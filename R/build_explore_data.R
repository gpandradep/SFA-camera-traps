# ============================================================
# SFA Camera Trap Project
# Build data objects for the Explore page
#
# This script assumes that:
#   1. survey_selected has already been defined
#   2. R/load_data.R has already been sourced
#
# Example:
# survey_selected <- "W8_2024"
# source(here("R", "load_data.R"))
# source(here("R", "build_explore_data.R"))
# ============================================================


# ------------------------------------------------------------
# 1. Station metadata for mapping
# ------------------------------------------------------------

explore_stations <- ct_table |>
  filter(component == "Ground") |>
  distinct(
    Station,
    component,
    longitude,
    latitude
  ) |>
  mutate(
    survey_id = survey_selected,

    treatment = case_when(
      str_detect(Station, regex("^Control", ignore_case = TRUE)) ~ "Control",
      str_detect(Station, regex("^Fish", ignore_case = TRUE)) ~ "Fish bait",
      str_detect(Station, regex("^Perfume", ignore_case = TRUE)) ~ "Perfume",
      TRUE ~ "Other"
    ),

    coordinate_available =
      !is.na(longitude) & !is.na(latitude)
  )


# ------------------------------------------------------------
# 2. Sampling effort by station and period
# ------------------------------------------------------------

explore_effort_30d <- effort_30d_by_station |>
  filter(component == "Ground") |>
  transmute(
    survey_id = survey_selected,
    Station,
    period = "30-day bait experiment",
    trap_nights = trap_nights_30d
  )

explore_effort_full <- survey_dates |>
  filter(component == "Ground") |>
  transmute(
    survey_id = survey_selected,
    Station,
    period = "Full survey",
    trap_nights = n_trap_nights_active
  )

explore_effort <- bind_rows(
  explore_effort_30d,
  explore_effort_full
)


# ------------------------------------------------------------
# 3. Independent records for the two sampling periods
# ------------------------------------------------------------

station_setup <- survey_dates |>
  filter(component == "Ground") |>
  distinct(
    Station,
    setup
  )

explore_records_full <- records |>
  filter(component == "Ground") |>
  transmute(
    survey_id = survey_selected,
    Station,
    Species,
    DateTime,
    Date,
    period = "Full survey"
  )

explore_records_30d <- records |>
  filter(component == "Ground") |>
  left_join(
    station_setup,
    by = "Station"
  ) |>
  filter(
    Date >= setup,
    Date <= setup + lubridate::days(30)
  ) |>
  transmute(
    survey_id = survey_selected,
    Station,
    Species,
    DateTime,
    Date,
    period = "30-day bait experiment"
  )

explore_records <- bind_rows(
  explore_records_30d,
  explore_records_full
)


# ------------------------------------------------------------
# 4. Events by station and species
#
# The full station x species grid is retained so stations with
# zero detections still appear on the map.
# ------------------------------------------------------------

explore_species_list <- records |>
  filter(component == "Ground") |>
  distinct(Species) |>
  arrange(Species)

station_species_grid <- tidyr::crossing(
  period = c(
    "30-day bait experiment",
    "Full survey"
  ),
  Station = explore_stations$Station,
  Species = explore_species_list$Species
)

events_station_species <- explore_records |>
  count(
    period,
    Station,
    Species,
    name = "independent_events"
  )

explore_station_species <- station_species_grid |>
  left_join(
    events_station_species,
    by = c(
      "period",
      "Station",
      "Species"
    )
  ) |>
  mutate(
    independent_events = replace_na(
      independent_events,
      0L
    )
  ) |>
  left_join(
    explore_effort,
    by = c(
      "period",
      "Station"
    )
  ) |>
  left_join(
    explore_stations |>
      select(
        Station,
        treatment,
        longitude,
        latitude,
        coordinate_available
      ),
    by = "Station"
  ) |>
  mutate(
    survey_id = survey_selected,

    events_per_100_trap_nights = if_else(
      trap_nights > 0,
      100 * independent_events / trap_nights,
      NA_real_
    )
  ) |>
  select(
    survey_id,
    period,
    Station,
    treatment,
    longitude,
    latitude,
    coordinate_available,
    Species,
    trap_nights,
    independent_events,
    events_per_100_trap_nights
  )


# ------------------------------------------------------------
# 5. Overall station summaries
#
# n_record_categories is calculated directly from the current
# Species field. It should NOT yet be interpreted as true
# species richness because the field includes categories such
# as Bird, Human, Bug, and Rodent.
# ------------------------------------------------------------

events_station_total <- explore_records |>
  count(
    period,
    Station,
    name = "independent_events"
  )

categories_station <- explore_records |>
  distinct(
    period,
    Station,
    Species
  ) |>
  count(
    period,
    Station,
    name = "n_record_categories"
  )

station_period_grid <- tidyr::crossing(
  period = c(
    "30-day bait experiment",
    "Full survey"
  ),
  Station = explore_stations$Station
)

explore_station_summary <- station_period_grid |>
  left_join(
    events_station_total,
    by = c(
      "period",
      "Station"
    )
  ) |>
  left_join(
    categories_station,
    by = c(
      "period",
      "Station"
    )
  ) |>
  mutate(
    independent_events = replace_na(
      independent_events,
      0L
    ),
    n_record_categories = replace_na(
      n_record_categories,
      0L
    )
  ) |>
  left_join(
    explore_effort,
    by = c(
      "period",
      "Station"
    )
  ) |>
  left_join(
    explore_stations |>
      select(
        Station,
        treatment,
        longitude,
        latitude,
        coordinate_available
      ),
    by = "Station"
  ) |>
  mutate(
    survey_id = survey_selected,

    events_per_100_trap_nights = if_else(
      trap_nights > 0,
      100 * independent_events / trap_nights,
      NA_real_
    )
  ) |>
  select(
    survey_id,
    period,
    Station,
    treatment,
    longitude,
    latitude,
    coordinate_available,
    trap_nights,
    independent_events,
    events_per_100_trap_nights,
    n_record_categories
  )


# ------------------------------------------------------------
# 6. Optional species metadata
#
# If data/metadata/species_metadata.csv exists, use it to
# calculate true observed species richness and standardized
# display names.
#
# Expected columns:
#   Species
#   Species_display
#   include_in_richness
# ------------------------------------------------------------

species_metadata_file <- here(
  "data",
  "metadata",
  "species_metadata.csv"
)

if (file.exists(species_metadata_file)) {

  species_metadata <- read_csv(
    species_metadata_file,
    show_col_types = FALSE
  )

  richness_station <- explore_records |>
    left_join(
      species_metadata,
      by = "Species"
    ) |>
    filter(
      include_in_richness %in% TRUE
    ) |>
    distinct(
      period,
      Station,
      Species_display
    ) |>
    count(
      period,
      Station,
      name = "observed_species_richness"
    )

  explore_station_summary <- explore_station_summary |>
    left_join(
      richness_station,
      by = c(
        "period",
        "Station"
      )
    ) |>
    mutate(
      observed_species_richness =
        replace_na(
          observed_species_richness,
          0L
        )
    )

  explore_station_species <- explore_station_species |>
    left_join(
      species_metadata |>
        select(
          Species,
          Species_display,
          include_in_richness
        ),
      by = "Species"
    )

} else {

  explore_station_summary <- explore_station_summary |>
    mutate(
      observed_species_richness = NA_integer_
    )

  explore_station_species <- explore_station_species |>
    mutate(
      Species_display = Species,
      include_in_richness = NA
    )
}


# ------------------------------------------------------------
# 7. Survey-level cards for Explore
# ------------------------------------------------------------

explore_survey_summary <- explore_station_summary |>
  group_by(
    survey_id,
    period
  ) |>
  summarise(
    n_stations = n_distinct(Station),
    stations_with_coordinates = sum(
      coordinate_available,
      na.rm = TRUE
    ),
    total_trap_nights = sum(
      trap_nights,
      na.rm = TRUE
    ),
    total_independent_events = sum(
      independent_events,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 8. QC: stations that cannot currently be mapped
# ------------------------------------------------------------

explore_missing_coordinates <- explore_stations |>
  filter(!coordinate_available) |>
  select(
    survey_id,
    Station,
    treatment
  )


# ------------------------------------------------------------
# Optional export
#
# Uncomment if you want browser-ready CSV files saved under
# data/explore/. One pair of files will be produced per survey.
# ------------------------------------------------------------

# explore_dir <- here("data", "explore")
# dir.create(explore_dir, recursive = TRUE, showWarnings = FALSE)
#
# write_csv(
#   explore_station_summary,
#   file.path(
#     explore_dir,
#     paste0(
#       survey_selected,
#       "_station_summary.csv"
#     )
#   )
# )
#
# write_csv(
#   explore_station_species,
#   file.path(
#     explore_dir,
#     paste0(
#       survey_selected,
#       "_station_species.csv"
#     )
#   )
# )
