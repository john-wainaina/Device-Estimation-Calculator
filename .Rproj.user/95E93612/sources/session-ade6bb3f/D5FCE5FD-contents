################################################################################
# 08_final_parameters_and_calculator.R
# CPAP DEVICE ESTIMATION TOOL
#
# Purpose: Assemble all calibrated parameters (eligibility, LOS, surge,
#          maintenance) into the final calculator function, with a 95%
#          confidence interval on the device estimate.
#
# Requires: eligibility_by_level    (from 04_eligibility_calibration_by_level.R)
#           blend_los_by_level      (from 07_los_blend_by_level.R)
#           surge_params            (from 06_surge_maintenance_params.R)
#           equipment_params        (from 06_surge_maintenance_params.R)
#
# Produces: final_params            (single object holding all parameters)
#           estimate_cpap_devices() (the calculator function)
#
# Run order: 9th (last)
#
# ------------------------------------------------------------------------------
# CALCULATOR INPUTS (what the user provides)
# ------------------------------------------------------------------------------
#   monthly_admits : average monthly admissions at the hospital
#   level_of_care  : one of "Level II Basic", "Level II Comprehensive",
#                    "Level III"
#
# ------------------------------------------------------------------------------
# FORMULA (v1.1 — two-stage rounding, revised at a PI's request)
# ------------------------------------------------------------------------------
# v1.0 rounded once, at the very end, after multiplying concurrent demand,
# surge, and maintenance buffer all together. For small hospitals, the
# maintenance buffer (3.4% of a small number) was frequently well under 1
# device, and simply vanished once folded into a single shared ceiling --
# effectively giving small hospitals no equipment-failure buffer at all,
# even though a small hospital losing 1 device to breakdown loses a much
# larger share of its total capacity than a large hospital does.
#
# v1.1 fixes this with a TWO-STAGE rounding, not three independent ones:
#
#   base   = ceiling(daily_concurrent x surge)        <- demand + surge together
#   buffer = 0                          if base == 0  <- no phantom spare when
#          = max(1, ceiling(base x maintenance))       there is no real demand
#   devices_needed = base + buffer
#
# Concurrent demand and surge are DELIBERATELY combined and rounded together
# (not each on its own) -- rounding each of the three stages independently
# was evaluated and rejected: it compounds, since ceiling() only ever rounds
# up, so three separate ceilings stack three separate "rounding taxes" on
# top of each other, materially inflating the estimate (worked example:
# ~33% higher at low volume) rather than just fixing the buffer blind spot.
# The two-stage version isolates the fix to exactly the problem raised --
# the buffer disappearing at small scale -- without introducing that extra
# compounding. It also converges back to the v1.0 formula as hospital size
# grows (verified: at Level III / 2000 admits/month, v1.0 and v1.1 give an
# identical result, 171 devices), so this is a bounded, self-limiting
# change targeted at small hospitals, not a general inflation of every
# estimate.
#
# 95% CI uses the SAME two-stage transform on the continuous peak-demand
# value plus/minus 1.96 x SE, rather than computing the interval separately
# -- this guarantees the point estimate and both CI bounds are discretized
# identically, so the interval always contains the point estimate.
################################################################################

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. ASSEMBLE FINAL PARAMETER SET
# ------------------------------------------------------------------------------

final_params <- eligibility_by_level %>%
  select(neo_level, eligible_rate_median, eligible_rate_se, n_facilities) %>%
  rename(eligibility_n_facilities = n_facilities) %>%
  left_join(
    blend_los_by_level %>% select(neo_level, los_median, los_se, n_facilities),
    by = "neo_level"
  ) %>%
  rename(los_n_facilities = n_facilities) %>%
  mutate(
    surge_median = surge_params$surge_median,
    maintenance_buffer = equipment_params$maintenance_buffer
  )

cat("========== FINAL CALCULATOR PARAMETERS ==========\n")
print(final_params)

# ------------------------------------------------------------------------------
# 2. TWO-STAGE ROUNDING TRANSFORM (single source of truth -- point estimate
#    and CI bounds all pass through this exact same function)
# ------------------------------------------------------------------------------

devices_from_peak_demand <- function(peak_demand, maintenance_buffer) {
  peak_demand <- max(0, peak_demand)   # guard against a negative CI bound
  base <- ceiling(peak_demand)
  if (base <= 0) return(0)             # no real demand -> no phantom spare
  buffer <- max(1, ceiling(base * maintenance_buffer))
  base + buffer
}

# ------------------------------------------------------------------------------
# 3. ESTIMATION FUNCTION
# ------------------------------------------------------------------------------

estimate_cpap_devices <- function(monthly_admits, level_of_care) {

  valid_levels <- c("Level II Basic", "Level II Comprehensive", "Level III")

  if (!level_of_care %in% valid_levels) {
    cat("\n=== HOSPITAL LEVEL GUIDE (WHO-aligned) ===\n\n")
    cat("Level II Basic (WHO: Secondary - Special Care)\n")
    cat("  Small district, mission, CHAM hospitals.\n")
    cat("  Basic special care: CPAP, oxygen, IV fluids, antibiotics,\n")
    cat("  phototherapy, KMC. Limited lab, no mechanical ventilation.\n\n")

    cat("Level II Comprehensive (WHO: Secondary - Special Care)\n")
    cat("  Large district, regional referral hospitals.\n")
    cat("  All Level II Basic services plus mobile X-ray, blood\n")
    cat("  transfusion, advanced diagnostics.\n\n")

    cat("Level III (WHO: Tertiary - Intensive Care)\n")
    cat("  Zonal, teaching, national referral hospitals.\n")
    cat("  Mechanical ventilation, surfactant, TPN, ROP screening,\n")
    cat("  pediatric surgery, subspecialty services.\n\n")

    stop(sprintf("Invalid level '%s'. Use one of: %s",
                 level_of_care, paste(valid_levels, collapse = ", ")))
  }

  p <- final_params %>% filter(neo_level == level_of_care)

  elig    <- p$eligible_rate_median
  se_elig <- p$eligible_rate_se
  los     <- p$los_median
  se_los  <- p$los_se
  surge   <- p$surge_median
  maint   <- p$maintenance_buffer

  daily_concurrent <- (monthly_admits * elig * los) / 30
  peak_demand      <- daily_concurrent * surge   # continuous -- NOT rounded here

  # Point estimate: base + buffer, via the single shared transform
  devices_needed <- devices_from_peak_demand(peak_demand, maint)
  base_devices   <- ceiling(max(0, peak_demand))
  buffer_devices <- devices_needed - base_devices

  # Uncertainty propagated on the continuous peak_demand (same as v1.0's
  # with_maintenance quantity), then the SAME transform applied to each
  # bound -- guarantees devices_needed always sits inside [ci_lower, ci_upper]
  se_total <- peak_demand * sqrt((se_elig / elig)^2 + (se_los / los)^2)

  ci_lower <- devices_from_peak_demand(peak_demand - 1.96 * se_total, maint)
  ci_upper <- devices_from_peak_demand(peak_demand + 1.96 * se_total, maint)

  list(
    level_of_care     = level_of_care,
    monthly_admits    = monthly_admits,
    daily_concurrent  = round(daily_concurrent, 2),
    peak_demand       = round(peak_demand, 2),
    base_devices      = base_devices,
    buffer_devices    = buffer_devices,
    devices_needed    = devices_needed,
    ci_95_lower       = ci_lower,
    ci_95_upper       = ci_upper,
    parameters_used   = list(
      eligibility_rate = elig,
      los_days         = los,
      surge_factor     = surge,
      maintenance_pct  = maint
    )
  )
}

# ------------------------------------------------------------------------------
# 4. TEST CALCULATIONS
# ------------------------------------------------------------------------------

cat("\n========== TEST CALCULATIONS (v1.1 two-stage rounding) ==========\n")

test_cases <- expand.grid(
  level  = c("Level II Basic", "Level II Comprehensive", "Level III"),
  admits = c(50, 100, 150, 200),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(test_cases))) {
  r <- estimate_cpap_devices(test_cases$admits[i], test_cases$level[i])
  cat(sprintf("%s - %d admissions/month:\n", test_cases$level[i], test_cases$admits[i]))
  cat(sprintf("  Daily concurrent: %.2f -> Peak demand: %.2f\n",
              r$daily_concurrent, r$peak_demand))
  cat(sprintf("  Base: %d + Buffer: %d = %d devices (95%% CI: %d-%d)\n\n",
              r$base_devices, r$buffer_devices, r$devices_needed,
              r$ci_95_lower, r$ci_95_upper))
}

# ------------------------------------------------------------------------------
# 5. SANITY CHECK: v1.1 vs v1.0 (single-pass) SIDE BY SIDE
# ------------------------------------------------------------------------------
# Confirms the fix is bounded (biggest effect at small hospitals, converging
# to zero difference at large hospitals), not a general inflation.

estimate_cpap_devices_v1_0 <- function(monthly_admits, level_of_care) {
  p <- final_params %>% filter(neo_level == level_of_care)
  daily_concurrent <- (monthly_admits * p$eligible_rate_median * p$los_median) / 30
  peak_demand      <- daily_concurrent * p$surge_median
  with_maintenance <- peak_demand * (1 + p$maintenance_buffer)
  ceiling(with_maintenance)
}

cat("========== v1.0 vs v1.1 COMPARISON ==========\n")
comparison <- test_cases %>%
  rowwise() %>%
  mutate(
    v1_0_devices = estimate_cpap_devices_v1_0(admits, level),
    v1_1_devices = estimate_cpap_devices(admits, level)$devices_needed,
    difference   = v1_1_devices - v1_0_devices
  ) %>%
  ungroup()
print(comparison)

# ------------------------------------------------------------------------------
# 6. SAVE PARAMETERS (for calculator app / Excel export)
# ------------------------------------------------------------------------------

saveRDS(final_params, "cpap_calculator_parameters.rds")
cat("\nParameters saved to: cpap_calculator_parameters.rds\n")

# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------
cat("\n08_final_parameters_and_calculator.R complete.\n")
