# ============================================================
# SFA Camera Trap Projects
# Load data for one survey
#
# Works with:
#   - single-camera stations (e.g., W8_2024)
#   - paired-camera stations (e.g., CR_2025)
#   - surveys with or without canopy cameras
#   - different DateTime formats in recordTable.csv
# ============================================================

library(tidyverse)
library(here)

# ------------------------------------------------------------
# Helper: parse dates from different surveys
# ------------------------------------------------------------

parse_mixed_date <- function(x) {
  as.Date(
    lubridate::parse_date_time(
      as.character(x),
      orders = c(
        "ymd",
        "mdy"
      ),
      quiet = TRUE
    )
  )
}

# ------------------------------------------------------------
# 1. Select survey
# ------------------------------------------------------------

#survey_selected <- "CR_2025"

# ------------------------------------------------------------
# 2. File paths
# ------------------------------------------------------------

metadata_file <- here("data", "metadata", "surveys.csv")

survey_dir <- here(
  "data",
  "surveys",
  survey_selected
)

camtrap_dir <- file.path(survey_dir, "camtrapR")
report_dir <- file.path(camtrap_dir, "surveyReport")
timelapse_dir <- file.path(survey_dir, "timelapse")


# ------------------------------------------------------------
# 3. Survey metadata
# ------------------------------------------------------------

surveys <- read_csv(
  metadata_file,
  show_col_types = FALSE
)

survey <- surveys |>
  filter(survey_id == survey_selected)

if (nrow(survey) == 0) {
  stop("survey_selected was not found in data/metadata/surveys.csv")
}


# ------------------------------------------------------------
# 4. Camera operation table
# ------------------------------------------------------------

ct_raw <- read_csv(
  file.path(camtrap_dir, "CTtable.csv"),
  show_col_types = FALSE
)

# Some surveys (e.g., W8_2024) have one camera per station and
# therefore do not contain an explicit Camera column.
has_camera_id <- "Camera" %in% names(ct_raw)

ct_table <- ct_raw |>
  mutate(
    survey_id = survey_selected,
    
    SetupDate = parse_mixed_date(SetupDate),
    RetrievalDate = parse_mixed_date(RetrievalDate),
    
    across(
      matches("^Problem\\d+_(from|to)$"),
      parse_mixed_date
    ),
    
    component = if_else(
      str_detect(
        Station,
        regex("Canopy", ignore_case = TRUE)
      ),
      "Canopy",
      "Ground"
    )
  )

# Add a Camera identifier for downstream summaries when the
# original CTtable contains one camera per station.
if (!has_camera_id) {
  ct_table <- ct_table |>
    mutate(Camera = Station)
}

has_canopy <- any(ct_table$component == "Canopy")


station_coordinates <- ct_table |>
  distinct(
    Station,
    component,
    longitude,
    latitude
  ) |>
  mutate(
    treatment = case_when(
      str_detect(Station, regex("^Control", ignore_case = TRUE)) ~ "Control",
      str_detect(Station, regex("^Fish", ignore_case = TRUE)) ~ "Fish",
      str_detect(Station, regex("^Perfume", ignore_case = TRUE)) ~ "Perfume",
      str_detect(Station, regex("Canopy", ignore_case = TRUE)) ~ "Canopy",
      TRUE ~ "Other"
    ),
    coordinate_available = !is.na(longitude) & !is.na(latitude)
  )

# ------------------------------------------------------------
# 5. Sampling effort by station
# ------------------------------------------------------------

survey_dates <- read_csv(
  file.path(camtrap_dir, "survey_dates.csv"),
  show_col_types = FALSE
) |>
  mutate(
    survey_id = survey_selected,
    
    across(
      c(
        setup,
        retrieval,
        image_first,
        image_last
      ),
      parse_mixed_date
    ),
    
    component = if_else(
      str_detect(
        Station,
        regex("Canopy", ignore_case = TRUE)
      ),
      "Canopy",
      "Ground"
    )
  )


# ------------------------------------------------------------
# 6. Independent wildlife records
# ------------------------------------------------------------

records <- read_csv(
  file.path(camtrap_dir, "recordTable.csv"),
  show_col_types = FALSE
) |>
  mutate(
    survey_id = survey_selected,

    # Accept both ISO dates (e.g., CR_2025) and m/d/Y dates
    # (e.g., W8_2024).
    DateTime = lubridate::parse_date_time(
      as.character(DateTime),
      orders = c(
        "ymd HMS",
        "ymd HM",
        "mdy HMS",
        "mdy HM"
      )
    ),

    Date = as.Date(DateTime),

    component = if_else(
      str_detect(Station, regex("Canopy", ignore_case = TRUE)),
      "Canopy",
      "Ground"
    )
  )


# ------------------------------------------------------------
# 7. surveyReport outputs
# ------------------------------------------------------------

events_by_species <- read_csv(
  file.path(report_dir, "events_by_species.csv"),
  show_col_types = FALSE
) |>
  mutate(survey_id = survey_selected)

events_by_station <- read_csv(
  file.path(report_dir, "events_by_station.csv"),
  show_col_types = FALSE
) |>
  mutate(survey_id = survey_selected)

events_by_station2 <- read_csv(
  file.path(report_dir, "events_by_station2.csv"),
  show_col_types = FALSE
) |>
  mutate(survey_id = survey_selected)

species_by_station <- read_csv(
  file.path(report_dir, "species_by_station.csv"),
  show_col_types = FALSE
) |>
  mutate(survey_id = survey_selected)


# ------------------------------------------------------------
# 8. Daily camera-operation matrix
#
# For surveys with paired cameras:
#   - byCamera = FALSE
#   - allCamsOn = FALSE
#   - camerasIndependent = FALSE
#
# Thus, a station contributes one trap-night when at least one
# camera in the pair is operational.
#
# For surveys without an original Camera column, camtrapR assumes
# one camera per station.
# ------------------------------------------------------------

camera_operation_args <- list(
  CTtable = ct_table,
  stationCol = "Station",
  setupCol = "SetupDate",
  retrievalCol = "RetrievalDate",
  hasProblems = TRUE,
  dateFormat = "ymd"
)

if (has_camera_id) {
  camera_operation_args <- c(
    camera_operation_args,
    list(
      cameraCol = "Camera",
      byCamera = FALSE,
      allCamsOn = FALSE,
      camerasIndependent = FALSE
    )
  )
}

camera_operation <- do.call(
  camtrapR::cameraOperation,
  camera_operation_args
)


# ------------------------------------------------------------
# 9. Camera operation in long format
# ------------------------------------------------------------

station_info <- ct_table |>
  group_by(Station) |>
  summarise(
    setup = min(SetupDate, na.rm = TRUE),
    component = first(component),
    .groups = "drop"
  )

camera_operation_long <- camera_operation |>
  as.data.frame() |>
  tibble::rownames_to_column("Station") |>
  as_tibble() |>
  pivot_longer(
    -Station,
    names_to = "Date",
    values_to = "operation"
  ) |>
  mutate(
    Date = as.Date(Date)
  ) |>
  left_join(
    station_info,
    by = "Station"
  )


# ------------------------------------------------------------
# 10. Standardized 30-day bait-experiment effort
#
# The 30-day window begins separately on the deployment date
# of each station.
# ------------------------------------------------------------

effort_30d_by_station <- camera_operation_long |>
  filter(
    Date >= setup,
    Date <= setup + lubridate::days(30)
  ) |>
  group_by(Station, component) |>
  summarise(
    trap_nights_30d = sum(operation, na.rm = TRUE),
    .groups = "drop"
  )

effort_30d_summary <- effort_30d_by_station |>
  group_by(component) |>
  summarise(
    n_stations = n_distinct(Station),
    min_active_days = min(trap_nights_30d, na.rm = TRUE),
    mean_active_days = mean(trap_nights_30d, na.rm = TRUE),
    max_active_days = max(trap_nights_30d, na.rm = TRUE),
    total_trap_nights = sum(trap_nights_30d, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 11. Full-survey summaries
# ------------------------------------------------------------

camera_summary <- ct_table |>
  group_by(component) |>
  summarise(
    n_stations = n_distinct(Station),
    n_cameras = n_distinct(Camera),
    first_deployment = min(SetupDate, na.rm = TRUE),
    last_retrieval = max(RetrievalDate, na.rm = TRUE),
    .groups = "drop"
  )

effort_summary <- survey_dates |>
  group_by(component) |>
  summarise(
    n_stations = n_distinct(Station),
    min_active_days = min(n_trap_nights_active, na.rm = TRUE),
    mean_active_days = mean(n_trap_nights_active, na.rm = TRUE),
    max_active_days = max(n_trap_nights_active, na.rm = TRUE),
    total_trap_nights = sum(n_trap_nights_active, na.rm = TRUE),
    .groups = "drop"
  )

record_summary <- records |>
  group_by(component) |>
  summarise(
    n_events = n(),
    n_record_categories = n_distinct(Species),
    n_stations_with_records = n_distinct(Station),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 12. Ground component
# ------------------------------------------------------------

ground_camera_summary <- camera_summary |>
  filter(component == "Ground")

ground_effort_summary <- effort_summary |>
  filter(component == "Ground")

ground_effort_30d <- effort_30d_summary |>
  filter(component == "Ground")

ground_record_summary <- record_summary |>
  filter(component == "Ground")


# ------------------------------------------------------------
# 13. Optional canopy component
#
# These objects may contain zero rows for surveys without canopy
# cameras. Use has_canopy before referring to them in a Quarto
# page.
# ------------------------------------------------------------

canopy_camera_summary <- camera_summary |>
  filter(component == "Canopy")

canopy_effort_summary <- effort_summary |>
  filter(component == "Canopy")

canopy_effort_30d <- effort_30d_summary |>
  filter(component == "Canopy")

canopy_record_summary <- record_summary |>
  filter(component == "Canopy")


# ------------------------------------------------------------
# 14. Optional: raw Timelapse table
# ------------------------------------------------------------

# timelapse_records <- read_csv(
#   file.path(timelapse_dir, "record_Table_Timelapse.csv"),
#   show_col_types = FALSE
# ) |>
#   mutate(survey_id = survey_selected)
