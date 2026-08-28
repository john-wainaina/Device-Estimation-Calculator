################################################################################
# 04_eligibility_calibration_by_level.R
# CPAP DEVICE ESTIMATION TOOL
#
# Purpose: Calibrate a single eligibility rate (%) per level of care, for use
#          as the calculator's eligibility parameter.
#
# Requires: cpap_facility_summary (from 03_facility_aggregation.R)
# Produces: eligibility_by_level
#
# Run order: 5th
#
# ------------------------------------------------------------------------------
# COVERAGE WINDOW RATIONALE
# ------------------------------------------------------------------------------
# Level II Basic / Level II Comprehensive: coverage 40-85%
# Level III:                                coverage 50-85%
#
# Lower bound excludes supply-constrained facilities, where CPAP scarcity
# may suppress both documented criteria and actual receipt (need is real
# but undercounted). Upper bound (>85%) is excluded because the hybrid
# "eligible = max(estimate, cpaps_done)" rule mechanically inflates
# eligibility at very high coverage -- verified directly: facilities >85%
# coverage showed eligibility roughly double (37-43%) that of facilities
# in the 40-85% window (14-22%), confirming this is an artifact, not a
# true clinical signal, and must be excluded from calibration.
################################################################################

library(tidyverse)

eligibility_by_level <- cpap_facility_summary %>%
  filter(
    !is.na(neo_level),
    total_eligible >= 30,
    case_when(
      neo_level %in% c("Level II Basic", "Level II Comprehensive") ~
        coverage_weighted >= 40 & coverage_weighted <= 85,
      neo_level == "Level III" ~
        coverage_weighted >= 50 & coverage_weighted <= 85,
      TRUE ~ FALSE
    )
  ) %>%
  group_by(neo_level) %>%
  summarise(
    n_facilities = n(),
    coverage_range = sprintf("%.0f%%-%.0f%%", min(coverage_weighted), max(coverage_weighted)),
    eligible_rate_median = median(eligible_percent_overall, na.rm = TRUE) / 100,
    eligible_rate_mean   = weighted.mean(eligible_percent_overall, total_admissions, na.rm = TRUE) / 100,
    eligible_rate_sd     = sd(eligible_percent_overall, na.rm = TRUE) / 100,
    eligible_rate_se     = eligible_rate_sd / sqrt(n_facilities),
    eligible_rate_p25    = quantile(eligible_percent_overall, 0.25, na.rm = TRUE) / 100,
    eligible_rate_p75    = quantile(eligible_percent_overall, 0.75, na.rm = TRUE) / 100,
    .groups = "drop"
  )

cat("========== ELIGIBILITY BY LEVEL OF CARE ==========\n")
print(eligibility_by_level)

# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------
cat(sprintf("\n04_eligibility_calibration_by_level.R complete. %d levels calibrated.\n",
            nrow(eligibility_by_level)))
