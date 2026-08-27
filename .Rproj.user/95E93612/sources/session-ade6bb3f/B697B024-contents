################################################################################
# COMPARISON: NAIVE MEDIAN LOT vs. COMPETING-RISKS RMTL
# Demonstrates the value of accounting for death censoring and
# undocumented-wean censoring, rather than a naive raw duration summary.
################################################################################

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. NAIVE LOT — the "if we hadn't done any of this" baseline
# ------------------------------------------------------------------------------
# Simple median of observed cpap_duration_days, treating every episode as if
# it were a completed, uncensored observation (ignores death censoring
# entirely, and ignores that some "alive" episodes have no true end date).

naive_lot <- cpap_duration %>%
  group_by(birthweight_cat) %>%
  summarise(
    n = n(),
    naive_median_days = median(cpap_duration_days, na.rm = TRUE),
    naive_mean_days    = mean(cpap_duration_days, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 2. COMPARISON TABLE
# ------------------------------------------------------------------------------

lot_comparison <- naive_lot %>%
  left_join(rmtl_by_band, by = "birthweight_cat") %>%
  mutate(
    rmtl_ci = sprintf("%.2f-%.2f", rmtl_ci_low, rmtl_ci_high),
    abs_diff_days = naive_median_days - rmtl_days,
    pct_diff = (abs_diff_days / rmtl_days) * 100
  ) %>%
  select(
    birthweight_cat, n,
    naive_median_days,
    rmtl_days, rmtl_ci,
    abs_diff_days, pct_diff
  )

cat("========== NAIVE MEDIAN LOT vs. COMPETING-RISKS RMTL ==========\n")
print(lot_comparison, digits = 2)

# ------------------------------------------------------------------------------
# 3. PLAIN-LANGUAGE SUMMARY LINE PER BAND (for presentation/report use)
# ------------------------------------------------------------------------------

lot_comparison %>%
  mutate(
    direction = if_else(abs_diff_days > 0, "overestimates", "underestimates"),
    summary_line = sprintf(
      "%s: naive median = %.2f days, RMTL = %.2f days (95%% CI %s) -> naive %s true LOT by %.1f%% (%.2f days)",
      birthweight_cat, naive_median_days, rmtl_days, rmtl_ci, direction, abs(pct_diff), abs(abs_diff_days)
    )
  ) %>%
  pull(summary_line) %>%
  cat(sep = "\n")