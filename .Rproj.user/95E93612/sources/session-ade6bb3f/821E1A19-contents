################################################################################
# 09_surge_empirical_sensitivity.R
# CPAP DEVICE ESTIMATION TOOL
#
# Purpose: Derive an empirical surge factor (peak/average concurrent CPAP
#          demand) from patient-level admission/discharge data, as a
#          sensitivity check against the current flat expert-opinion value
#          (surge = 2.0). Does NOT replace the flat value in the calculator --
#          this is exploratory, to see how far the data-derived number sits
#          from the current assumption.
#
# Requires: df (from 00_setup_and_data_cleaning.R), fac_levels (from 01)
# Produces: surge_empirical_by_hospital, surge_empirical_by_level
#
# ------------------------------------------------------------------------------
# METHOD
# ------------------------------------------------------------------------------
# Concurrent CPAP-eligible occupancy only changes at admission/discharge
# events, so it's computed as a step function via a sweep-line approach
# (+1 at admission, -1 the day after discharge) rather than a slow day-by-day
# grid. Peak = max of the cumulative step function. Average = time-weighted
# mean of that same step function over the observation window.
#
# CAUTION -- SMALL-HOSPITAL INFLATION:
# Peak/average ratios are mechanically inflated at hospitals with very low
# average concurrent volume, purely from ordinary random variation (a
# hospital averaging 0.3 concurrent babies will often peak at 2-3 just by
# chance). Hospitals below a minimum average volume are excluded from the
# summary distributions below to avoid this artifact swamping the signal.
################################################################################

library(tidyverse)
library(data.table)

# ------------------------------------------------------------------------------
# 1. DEFINE CPAP-ELIGIBLE FLAG PER PATIENT (same rule used elsewhere in the
#    pipeline: a baby who received CPAP counts as having needed it)
# ------------------------------------------------------------------------------

analysis_start <- as.Date("2024-07-01")   # adjust to your reliable-data window
analysis_end   <- as.Date("2026-06-30")

patient_events <- df %>%
  filter(!is.na(in_doa), !is.na(in_dis_dod)) %>%
  mutate(
    cpap_eligible = if_else(CPAP_eligible == "Yes" | in_cp_admin == 1, 1, 0)
  ) %>%
  filter(cpap_eligible == 1, in_doa >= analysis_start, in_doa <= analysis_end) %>%
  select(country, in_facid, in_doa, in_dis_dod)

# ------------------------------------------------------------------------------
# 2. SWEEP-LINE EVENTS: +1 at admission, -1 the day after discharge
# ------------------------------------------------------------------------------

events <- bind_rows(
  patient_events %>% transmute(country, in_facid, date = in_doa, delta = 1L),
  patient_events %>% transmute(country, in_facid, date = in_dis_dod + 1, delta = -1L)
) %>%
  group_by(country, in_facid, date) %>%
  summarise(delta = sum(delta), .groups = "drop") %>%
  arrange(in_facid, date)

# ------------------------------------------------------------------------------
# 3. CUMULATIVE OCCUPANCY (STEP FUNCTION) PER HOSPITAL
# ------------------------------------------------------------------------------

occupancy <- events %>%
  group_by(in_facid) %>%
  mutate(
    level = cumsum(delta),
    next_date = lead(date, default = analysis_end + 1),
    segment_days = as.numeric(next_date - date)
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# 4. PEAK AND TIME-WEIGHTED AVERAGE PER HOSPITAL
# ------------------------------------------------------------------------------

surge_empirical_by_hospital <- occupancy %>%
  group_by(country, in_facid) %>%
  summarise(
    peak_concurrent = max(level, na.rm = TRUE),
    total_days      = sum(segment_days, na.rm = TRUE),
    mean_concurrent = sum(level * segment_days, na.rm = TRUE) / total_days,
    .groups = "drop"
  ) %>%
  mutate(
    surge_ratio_raw = if_else(mean_concurrent > 0, peak_concurrent / mean_concurrent, NA_real_)
  ) %>%
  left_join(fac_levels %>% select(in_facid, neo_level), by = "in_facid")

# ------------------------------------------------------------------------------
# 5. EXCLUDE SMALL-VOLUME HOSPITALS BEFORE SUMMARIZING (see caution above)
# ------------------------------------------------------------------------------

MIN_MEAN_CONCURRENT <- 2   # exclude hospitals averaging under 2 concurrent
# CPAP-eligible babies -- below this, peak/mean
# ratio is dominated by small-number noise, not
# meaningful surge

surge_reliable <- surge_empirical_by_hospital %>%
  filter(!is.na(surge_ratio_raw), mean_concurrent >= MIN_MEAN_CONCURRENT, !is.na(neo_level))

cat(sprintf("Hospitals with usable surge data: %d of %d (excluded %d below mean_concurrent < %d)\n",
            nrow(surge_reliable), nrow(surge_empirical_by_hospital),
            nrow(surge_empirical_by_hospital) - nrow(surge_reliable), MIN_MEAN_CONCURRENT))

# ------------------------------------------------------------------------------
# 6. DISTRIBUTION -- OVERALL AND BY LEVEL (no single number picked yet;
#    this is a sensitivity check, report the spread honestly)
# ------------------------------------------------------------------------------

cat("\n========== EMPIRICAL SURGE RATIO -- OVERALL ==========\n")
surge_reliable %>%
  summarise(
    n = n(),
    p25 = quantile(surge_ratio_raw, 0.25),
    median = median(surge_ratio_raw),
    mean = mean(surge_ratio_raw),
    p75 = quantile(surge_ratio_raw, 0.75),
    p90 = quantile(surge_ratio_raw, 0.90)
  ) %>%
  print()

cat("\n========== EMPIRICAL SURGE RATIO -- BY LEVEL OF CARE ==========\n")
surge_empirical_by_level <- surge_reliable %>%
  group_by(neo_level) %>%
  summarise(
    n = n(),
    p25 = quantile(surge_ratio_raw, 0.25),
    median = median(surge_ratio_raw),
    mean = mean(surge_ratio_raw),
    p75 = quantile(surge_ratio_raw, 0.75),
    p90 = quantile(surge_ratio_raw, 0.90),
    .groups = "drop"
  )
print(surge_empirical_by_level)

# ------------------------------------------------------------------------------
# 7. SENSITIVITY COMPARISON AGAINST THE CURRENT FLAT ASSUMPTION (2.0)
# ------------------------------------------------------------------------------

cat("\n========== SENSITIVITY: EMPIRICAL MEDIAN vs. CURRENT FLAT VALUE (2.0) ==========\n")
surge_empirical_by_level %>%
  mutate(
    flat_assumption = 2.0,
    diff_from_flat = median - flat_assumption,
    pct_diff = diff_from_flat / flat_assumption * 100
  ) %>%
  select(neo_level, n, median, flat_assumption, diff_from_flat, pct_diff) %>%
  print()




# ------------------------------------------------------------------------------
# DIAGNOSTIC: is the surge ratio gradient by level actually just a volume-scale
# effect, rather than a genuine level-specific difference?
# ------------------------------------------------------------------------------

cat("=== MEAN CONCURRENT VOLUME BY LEVEL (reliable subset) ===\n")
surge_reliable %>%
  group_by(neo_level) %>%
  summarise(
    n = n(),
    p25_volume = quantile(mean_concurrent, 0.25),
    median_volume = median(mean_concurrent),
    p75_volume = quantile(mean_concurrent, 0.75),
    .groups = "drop"
  ) %>%
  print()

cat("\n=== CORRELATION: surge ratio vs. hospital volume (pooled, all levels) ===\n")
print(cor.test(surge_reliable$mean_concurrent, surge_reliable$surge_ratio_raw, method = "spearman"))

cat("\n=== SURGE RATIO BY VOLUME BAND (pooled -- ignoring level) ===\n")
surge_reliable %>%
  mutate(volume_band = cut(mean_concurrent, breaks = c(0, 3, 6, 10, 20, Inf),
                           labels = c("2-3", "3-6", "6-10", "10-20", "20+"))) %>%
  group_by(volume_band) %>%
  summarise(n = n(), median_ratio = median(surge_ratio_raw), mean_ratio = mean(surge_ratio_raw), .groups = "drop") %>%
  print()



# ------------------------------------------------------------------------------
# EXPLORATORY: surge as a function of concurrent volume, not level
# (fit on the same 68-hospital reliable subset)
# ------------------------------------------------------------------------------

surge_volume_model <- lm(log(surge_ratio_raw) ~ log(mean_concurrent), data = surge_reliable)
summary(surge_volume_model)

# Predicted surge at a few representative volumes, for sanity-checking
predict_surge <- function(mean_concurrent_value) {
  exp(predict(surge_volume_model, newdata = data.frame(mean_concurrent = mean_concurrent_value)))
}

cat("Predicted surge at mean_concurrent = 2, 5, 10, 20, 30:\n")
sapply(c(2, 5, 10, 20, 30), predict_surge)
