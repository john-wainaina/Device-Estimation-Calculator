################################################################################
# 07_los_blend_by_level.R
# CPAP DEVICE ESTIMATION TOOL
#
# Purpose: Compute the typical case-mix (% of admissions per weight band)
#          for each level of care, then blend it with the band-specific RMTL
#          LOS values (05_cpap_los_rmtl.R) to produce a single level-specific
#          LOS parameter for the calculator.
#
# Requires: cpap_facility_summary (from 03_facility_aggregation.R)
#           rmtl_by_band (from 05_cpap_los_rmtl.R)
# Produces: typical_casemix_by_level, blend_los_by_level
#
# Run order: 8th (after 05; independent of 06)
#
# ------------------------------------------------------------------------------
# DESIGN RATIONALE (Option B)
# ------------------------------------------------------------------------------
# LOS varies meaningfully by weight band (validated via competing-risks RMTL
# in 05_cpap_los_rmtl.R), but the calculator keeps a SIMPLE user input
# (monthly admissions + level of care only -- no per-facility case-mix
# entry required). To reconcile this, each level's LOS parameter is a
# case-mix-WEIGHTED BLEND of the six band-specific RMTL values, using the
# TYPICAL case-mix observed across facilities of that level in the data.
#
# This means: the calculator does not ask the user for their case-mix, but
# the LOS parameter it uses already reflects the case-mix typically seen at
# that level (e.g. Level III sees proportionally more ELBW/VLBW admissions,
# which is reflected in a slightly different LOS blend than Level II Basic).
################################################################################

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. TYPICAL CASE-MIX (%) BY LEVEL OF CARE
# ------------------------------------------------------------------------------

typical_casemix_by_level <- cpap_facility_summary %>%
  filter(!is.na(neo_level)) %>%
  group_by(neo_level) %>%
  summarise(
    n_facilities  = n(),
    pct_elbw      = mean(pct_elbw, na.rm = TRUE),        # <1000g
    pct_vlbw      = mean(pct_vlbw, na.rm = TRUE),         # 1000-1499g
    pct_lbw_lower = mean(pct_lbw_lower, na.rm = TRUE),    # 1500-1999g
    pct_lbw_upper = mean(pct_lbw_upper, na.rm = TRUE),    # 2000-2499g
    pct_normal    = mean(pct_normal, na.rm = TRUE),       # 2500-4000g
    pct_large     = mean(pct_large, na.rm = TRUE),        # 4001+g
    .groups = "drop"
  ) %>%
  mutate(
    # Normalize so each row sums to 100% (rounding/missingness safety)
    total_check = pct_elbw + pct_vlbw + pct_lbw_lower + pct_lbw_upper + pct_normal + pct_large,
    across(c(pct_elbw, pct_vlbw, pct_lbw_lower, pct_lbw_upper, pct_normal, pct_large),
           ~ .x / total_check * 100)
  ) %>%
  select(-total_check)

cat("========== TYPICAL CASE-MIX BY LEVEL ==========\n")
print(typical_casemix_by_level)

# Expected pattern: ELBW/VLBW share should increase from Level II Basic ->
# Level II Comprehensive -> Level III, consistent with referral of sicker,
# smaller babies to higher-level facilities.

# ------------------------------------------------------------------------------
# 2. BLEND LOS PER LEVEL (case-mix-weighted RMTL)
# ------------------------------------------------------------------------------
# los_level    = sum(pct_band/100 * rmtl_days_band)
# los_level_se = sqrt(sum((pct_band/100 * rmtl_se_band)^2))   [error propagation
#                for a weighted sum of independent estimates]

blend_los_by_level <- typical_casemix_by_level %>%
  rowwise() %>%
  mutate(
    los_median =
      pct_elbw/100      * rmtl_by_band$rmtl_days[rmtl_by_band$birthweight_cat == "<1000"] +
      pct_vlbw/100      * rmtl_by_band$rmtl_days[rmtl_by_band$birthweight_cat == "1000-1499"] +
      pct_lbw_lower/100 * rmtl_by_band$rmtl_days[rmtl_by_band$birthweight_cat == "1500-1999"] +
      pct_lbw_upper/100 * rmtl_by_band$rmtl_days[rmtl_by_band$birthweight_cat == "2000-2499"] +
      pct_normal/100    * rmtl_by_band$rmtl_days[rmtl_by_band$birthweight_cat == "2500-4000"] +
      pct_large/100     * rmtl_by_band$rmtl_days[rmtl_by_band$birthweight_cat == "4001+"],

    los_se = sqrt(
      (pct_elbw/100      * rmtl_by_band$rmtl_se[rmtl_by_band$birthweight_cat == "<1000"])^2 +
      (pct_vlbw/100      * rmtl_by_band$rmtl_se[rmtl_by_band$birthweight_cat == "1000-1499"])^2 +
      (pct_lbw_lower/100 * rmtl_by_band$rmtl_se[rmtl_by_band$birthweight_cat == "1500-1999"])^2 +
      (pct_lbw_upper/100 * rmtl_by_band$rmtl_se[rmtl_by_band$birthweight_cat == "2000-2499"])^2 +
      (pct_normal/100    * rmtl_by_band$rmtl_se[rmtl_by_band$birthweight_cat == "2500-4000"])^2 +
      (pct_large/100     * rmtl_by_band$rmtl_se[rmtl_by_band$birthweight_cat == "4001+"])^2
    )
  ) %>%
  ungroup() %>%
  select(neo_level, n_facilities, los_median, los_se,
         pct_elbw, pct_vlbw, pct_lbw_lower, pct_lbw_upper, pct_normal, pct_large)

cat("\n========== BLENDED LOS BY LEVEL (FINAL CALCULATOR PARAMETER) ==========\n")
print(blend_los_by_level %>% select(neo_level, n_facilities, los_median, los_se))

# NOTE: blended LOS values are typically close across levels (e.g. ~4.2-4.3
# days), even though case-mix differs meaningfully by level. This is
# expected: RMTL itself is fairly stable across weight bands (4.1-4.9 days),
# so modest case-mix shifts across levels produce only small differences in
# the blended result. LOS is genuinely empirically derived and weight-band-
# specific under the hood; it is not a coincidence that the blended values
# converge -- it reflects the underlying RMTL pattern, not an error.

# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------
cat(sprintf("\n07_los_blend_by_level.R complete. %d levels blended.\n",
            nrow(blend_los_by_level)))
