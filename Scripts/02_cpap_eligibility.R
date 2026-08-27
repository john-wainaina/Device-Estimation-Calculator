################################################################################
# 02_cpap_eligibility.R
# CPAP DEVICE ESTIMATION TOOL
#
# Purpose: Estimate CPAP eligibility per facility x weight band, using the
#          hybrid logic agreed for this tool.
#
# Requires: df (from 00_setup_and_data_cleaning.R)
# Produces: cpap_estimate (facility x weight-band eligibility/coverage table)
#
# Run order: 3rd
#
# ------------------------------------------------------------------------------
# HYBRID ELIGIBILITY LOGIC
# ------------------------------------------------------------------------------
# <1500g:      Criteria only. Indication (prematurity/ELBW) is near-universal
#              and not clinically debated -> no hybrid adjustment needed.
#
# 1500-1999g:  max(documented criteria, empirical rate).
#              Empirical rate is data-derived from facilities with 60-85%
#              CPAP coverage (neither supply-starved nor mechanically
#              inflated) -- this band's criteria were flagged as debatable
#              by clinical reviewers, so we anchor to revealed practice at
#              well-resourced facilities rather than trusting criteria alone.
#
# >=2000g:     max(expert-opinion rate, actual receipt).
#              Expert rates: 8% for 2000-2499g, 5% for >=2500g. No reliable
#              criteria field exists for these lower-risk babies.
#
# ALL BANDS:   eligible_final = max(estimate, cpaps_done).
#              A baby who received CPAP was, by definition, eligible --
#              this guarantees coverage <= 100% by construction and
#              protects against underestimating need in facilities that,
#              in practice, treat more babies than criteria/expert rates
#              alone would suggest.
################################################################################

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. FACILITY x WEIGHT-BAND x MONTH SUMMARY
# ------------------------------------------------------------------------------

cpap_monthly <- df %>%
  filter(!is.na(birthweight_cat)) %>%
  group_by(country, in_facid, birthweight_cat, Month) %>%
  summarise(
    Admissions = n(),
    Eligible_criteria = sum(CPAP_eligible == "Yes", na.rm = TRUE),
    cpaps_done = sum(in_cp_admin == 1, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 2. AGGREGATE TO FACILITY x WEIGHT-BAND LEVEL
# ------------------------------------------------------------------------------

cpap_estimate <- cpap_monthly %>%
  group_by(country, in_facid, birthweight_cat) %>%
  summarise(
    total_admissions = sum(Admissions, na.rm = TRUE),
    total_eligible_criteria = sum(Eligible_criteria, na.rm = TRUE),
    total_cpaps_done = sum(cpaps_done, na.rm = TRUE),
    eligible_percent_criteria = total_eligible_criteria / total_admissions * 100,
    coverage_percent_criteria = if_else(
      total_eligible_criteria > 0,
      total_cpaps_done / total_eligible_criteria * 100,
      NA_real_
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 3. DERIVE EMPIRICAL RATE FOR 1500-1999g (from 60-85% coverage facilities)
# ------------------------------------------------------------------------------

empirical_rate_1500_1999 <- cpap_estimate %>%
  filter(
    birthweight_cat == "1500-1999",
    coverage_percent_criteria >= 60,
    coverage_percent_criteria <= 85,
    total_eligible_criteria >= 30
  ) %>%
  summarise(rate = median(eligible_percent_criteria, na.rm = TRUE) / 100) %>%
  pull(rate)

cat(sprintf("Empirical (data-derived) rate for 1500-1999g: %.1f%%\n",
            empirical_rate_1500_1999 * 100))

# ------------------------------------------------------------------------------
# 4. APPLY HYBRID ELIGIBILITY LOGIC
# ------------------------------------------------------------------------------

cpap_estimate <- cpap_estimate %>%
  mutate(
    applied_rate = case_when(
      birthweight_cat %in% c("<1000", "1000-1499") ~ NA_real_,   # criteria only
      birthweight_cat == "1500-1999" ~ empirical_rate_1500_1999,
      birthweight_cat == "2000-2499" ~ 0.08,
      birthweight_cat %in% c("2500-4000", "4001+") ~ 0.05,
      TRUE ~ NA_real_
    ),

    eligible_initial = case_when(
      birthweight_cat %in% c("<1000", "1000-1499") ~ total_eligible_criteria,
      !is.na(applied_rate) ~ pmax(total_eligible_criteria,
                                    total_admissions * applied_rate,
                                    na.rm = TRUE),
      TRUE ~ total_eligible_criteria
    ),

    # Core hybrid rule: if CPAP was given, the baby was eligible.
    eligible_final   = pmax(eligible_initial, total_cpaps_done, na.rm = TRUE),
    eligible_percent = (eligible_final / total_admissions) * 100,
    coverage_percent = (total_cpaps_done / eligible_final) * 100
  )

# ------------------------------------------------------------------------------
# 5. VERIFICATION
# ------------------------------------------------------------------------------

cat(sprintf("Max coverage (should be <=100): %.1f%%\n",
            max(cpap_estimate$coverage_percent, na.rm = TRUE)))

cat("\nEligibility summary by weight category:\n")
cpap_estimate %>%
  group_by(birthweight_cat) %>%
  summarise(
    n_facilities = n(),
    median_eligible_pct = median(eligible_percent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------
cat(sprintf("\n02_cpap_eligibility.R complete. cpap_estimate: %d facility x weight-band rows.\n",
            nrow(cpap_estimate)))
