# =============================================================================
# prep_data.R — Build app_data.rds for bright-spots-app-v2
#
# Run this script once (from the chronic-absenteeism project) whenever the
# underlying analysis data changes. It saves app_data.rds into the app's
# data/ folder, which is the only file the app itself reads.
#
# Universe: all schools in schools_2025_sector_results with improvement_pp >= 5
#   (~1,424 schools). Non-modeled schools are excluded.
#
# Joins:
#   - pd_group from schools_2025_final_candidates (310 bright spot schools)
#   - pct_decrease computed as (p_peak - p_current) / p_peak
# =============================================================================

library(tidyverse)

# Source the analysis scripts if objects are not already in the environment.
# Adjust paths as needed relative to your working directory.
if (!exists("schools_2025_sector_results")) {
  source("2025-pd-analysis-bb-viz.R")
}
if (!exists("bright_spot_candidates")) {
  source("2025-map.R")
}

# Clean pd_group levels (str_squish collapses all internal whitespace)
# Recode to short labels for display in the app
pd_lookup <- schools_2025_final_candidates |>
  select(agency_code, pd_group) |>
  mutate(
    pd_group = str_squish(as.character(pd_group)),
    pd_group = case_when(
      str_starts(pd_group, "Exceptional") ~ "Exceptional",
      str_starts(pd_group, "Strong") ~ "Strong",
      str_starts(pd_group, "Promising") ~ "Promising",
      .default = NA_character_
    )
  )

app_data <- schools_2025_sector_results |>
  # Apply the 5 pp improvement gate
  filter(improvement_pp >= 5) |>
  # Bring in bright-spot category
  left_join(pd_lookup, join_by(agency_code)) |>
  # Compute pct_decrease (positive = improvement) to match the threshold filter
  mutate(pct_decrease = (p_peak - p_current) / p_peak) |>
  # Keep only the columns the app needs
  select(
    agency_code,
    name,
    county,
    grade_band,
    poverty,
    pct_eds,
    den,
    year_peak,
    p_peak,
    p_current,
    improvement_pp,
    pct_decrease,
    expected_rate,
    residual_pp,
    quasi_std_residual,
    pd_group,
    latitude,
    longitude
  )

saveRDS(app_data, "bright-spots-app-v2/data/app_data.rds")
message(
  "Saved ",
  nrow(app_data),
  " schools to bright-spots-app-v2/data/app_data.rds"
)
