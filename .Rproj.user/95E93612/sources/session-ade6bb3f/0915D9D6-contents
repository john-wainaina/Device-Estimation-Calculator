################################################################################
# 05_cpap_los_rmtl.R
# CPAP DEVICE ESTIMATION TOOL
#
# Purpose: Estimate CPAP length of treatment (LOS) per weight band, using a
#          competing-risks Restricted Mean Time Lost (RMTL) approach that
#          properly handles death censoring and undocumented-wean censoring.
#
# Requires: df (from 00_setup_and_data_cleaning.R)
# Produces: cpap_duration (episode-level analytic dataset)
#           rmtl_by_band  (validated LOS parameter per weight band, with SE/CI)
#
# Run order: 6th
#
# ------------------------------------------------------------------------------
# METHODOLOGY SUMMARY
# ------------------------------------------------------------------------------
# Problem with naive LOS (e.g. overall hospital LOS, or raw mean CPAP
# duration): mixes three different populations --
#   1. Babies who weaned successfully (the true treatment-duration signal)
#   2. Babies who died on CPAP (duration reflects time-to-death, not
#      treatment need -- this is death censoring)
#   3. Babies weaned early due to device scarcity (deflates LOS, and is
#      circular: scarcity -> shorter observed LOS -> underestimated need
#      -> recommend fewer devices -> perpetuates scarcity)
#
# Solution: competing-risks framing.
#   - event = 1: died on CPAP (competing event)
#   - event = 2: discharged alive WITH a documented CPAP end date
#                (confirmed successful wean -- event of interest)
#   - event = 0: discharged alive with NO documented CPAP end date
#                (true wean time unknown -- right-censored, NOT assumed
#                to equal the discharge date, since that would bias LOS
#                upward for babies actually weaned earlier but undocumented)
#
# RMTL ("restricted mean time on CPAP") = expected number of days a baby
# spends in the "still on CPAP" state (neither weaned nor died) within a
# fixed horizon (28 days). This is well-defined even for weight bands
# where the cumulative incidence of successful wean never reaches 50%
# (i.e., no true median exists, e.g. <1000g where ~77% mortality means
# most babies never reach "confirmed wean").
#
# Uncertainty: facility-level clustered bootstrap (not individual-level),
# since babies within the same facility aren't independent observations.
################################################################################

library(tidyverse)
library(survival)
library(cmprsk)

# ------------------------------------------------------------------------------
# 1. BUILD CPAP DURATION + OUTCOME EVENT CODING
# ------------------------------------------------------------------------------
# Requires: df$cpap_start / df$cpap_end (from 00_setup_and_data_cleaning.R)
#           df$outcome_label -- discharge outcome field ("Dead" / "Alive")
#           df$in_recid -- unique patient/admission identifier

cpap_duration <- df %>%
  filter(in_cp_admin == 1, !is.na(cpap_start)) %>%
  left_join(
    df %>% select(country, in_facid, in_recid, outcome_status = outcome_label),
    by = c("country", "in_facid", "in_recid")
  ) %>%
  mutate(
    has_cpap_end = !is.na(cpap_end),

    duration_end_dt = case_when(
      has_cpap_end ~ cpap_end,
      # No recorded CPAP end: use discharge/death date as the censoring point.
      # (For "Dead", this is a reasonable proxy -- CPAP is typically
      #  discontinued at death without being separately logged.
      #  For "Alive", this is only an upper bound -- handled via event=0
      #  below, NOT treated as a confirmed wean.)
      !has_cpap_end & outcome_status %in% c("Dead", "Alive") ~
        ymd_hms(paste(in_dis_dod, "00:00:00"), quiet = TRUE),
      TRUE ~ as.POSIXct(NA)
    ),

    cpap_duration_hrs  = as.numeric(difftime(duration_end_dt, cpap_start, units = "hours")),
    cpap_duration_days = cpap_duration_hrs / 24,

    # QC: drop negative durations (data entry error) and >60 days (implausible)
    qc_valid = cpap_duration_hrs >= 0 & cpap_duration_days <= 60 & !is.na(cpap_duration_days),

    outcome_event = case_when(
      outcome_status == "Dead" ~ 1,                          # died (competing event)
      outcome_status == "Alive" & has_cpap_end  ~ 2,          # confirmed successful wean
      outcome_status == "Alive" & !has_cpap_end ~ 0,          # censored -- true wean unknown
      TRUE ~ NA_real_
    )
  ) %>%
  filter(qc_valid, !is.na(outcome_event))

cat(sprintf(
  "CPAP LOS analytic sample: %d episodes\n  Died (event=1): %d\n  Confirmed wean (event=2): %d\n  Censored, alive no end-date (event=0): %d\n",
  nrow(cpap_duration),
  sum(cpap_duration$outcome_event == 1),
  sum(cpap_duration$outcome_event == 2),
  sum(cpap_duration$outcome_event == 0)
))

# ------------------------------------------------------------------------------
# 2. FACE-VALIDITY CHECK: MORTALITY BY FACILITY AND WEIGHT BAND
# ------------------------------------------------------------------------------
# Expect: no implausible facility-level outliers; mortality decreasing with
# increasing weight up to ~2000-2499g, then a modest upturn at >=2500g
# (term/near-term babies on CPAP typically have a different, non-prematurity
# indication -- e.g. birth asphyxia, meconium aspiration, sepsis -- which
# carries its own independent mortality risk).

cpap_duration %>%
  group_by(country, in_facid) %>%
  summarise(n = n(), died_pct = mean(outcome_event == 1) * 100, .groups = "drop") %>%
  arrange(desc(died_pct)) %>%
  print(n = Inf)

cpap_duration %>%
  group_by(birthweight_cat) %>%
  summarise(n = n(), died_pct = mean(outcome_event == 1) * 100, .groups = "drop") %>%
  print()

# ------------------------------------------------------------------------------
# 3. COMPETING-RISKS CUMULATIVE INCIDENCE (by weight band)
# ------------------------------------------------------------------------------

cif_fit <- cuminc(
  ftime   = cpap_duration$cpap_duration_days,
  fstatus = cpap_duration$outcome_event,
  group   = cpap_duration$birthweight_cat
)

# ------------------------------------------------------------------------------
# 4. RMTL ("restricted mean time on CPAP") PER WEIGHT BAND
# ------------------------------------------------------------------------------
# P(still on CPAP at time t) = 1 - CIF_death(t) - CIF_weaned(t)
# RMTL = area under that "still on CPAP" curve, up to the horizon.

compute_rmtl_oncpap <- function(cif_fit, weight_cats, horizon = 28) {
  out <- tibble(birthweight_cat = character(), rmtl_days = numeric())
  for (wc in weight_cats) {
    key_death <- paste(wc, "1"); key_wean <- paste(wc, "2")
    if (key_death %in% names(cif_fit) & key_wean %in% names(cif_fit)) {
      t_d <- cif_fit[[key_death]]$time; e_d <- cif_fit[[key_death]]$est
      t_w <- cif_fit[[key_wean]]$time;  e_w <- cif_fit[[key_wean]]$est
      grid <- sort(unique(c(0, t_d[t_d <= horizon], t_w[t_w <= horizon], horizon)))
      cif_at <- function(t_vec, e_vec, grid) {
        idx <- findInterval(grid, t_vec)
        ifelse(idx == 0, 0, e_vec[idx])
      }
      p_on_cpap <- pmax(0, 1 - cif_at(t_d, e_d, grid) - cif_at(t_w, e_w, grid))
      rmtl <- sum(diff(grid) * p_on_cpap[-length(p_on_cpap)])
      out <- out %>% add_row(birthweight_cat = wc, rmtl_days = rmtl)
    }
  }
  out
}

rmtl_point_estimates <- compute_rmtl_oncpap(cif_fit, levels(cpap_duration$birthweight_cat), horizon = 28)
cat("\n========== RMTL POINT ESTIMATES (28-day horizon) ==========\n")
print(rmtl_point_estimates)

# Robustness check: 60-day horizon should give near-identical values if the
# 28-day estimate isn't artificially truncating meaningful tail duration.
rmtl_60d_check <- compute_rmtl_oncpap(cif_fit, levels(cpap_duration$birthweight_cat), horizon = 60)
cat("\n========== RMTL ROBUSTNESS CHECK (60-day horizon) ==========\n")
print(rmtl_60d_check)

# ------------------------------------------------------------------------------
# 5. FACILITY-CLUSTERED BOOTSTRAP (SE / 95% CI)
# ------------------------------------------------------------------------------

compute_rmtl_single_band <- function(data, horizon = 28) {
  cif <- cuminc(ftime = data$cpap_duration_days, fstatus = data$outcome_event)
  key_death <- "1 1"; key_wean <- "1 2"
  if (!(key_death %in% names(cif)) | !(key_wean %in% names(cif))) return(NA_real_)
  t_d <- cif[[key_death]]$time; e_d <- cif[[key_death]]$est
  t_w <- cif[[key_wean]]$time;  e_w <- cif[[key_wean]]$est
  grid <- sort(unique(c(0, t_d[t_d <= horizon], t_w[t_w <= horizon], horizon)))
  cif_at <- function(t_vec, e_vec, grid) {
    idx <- findInterval(grid, t_vec)
    ifelse(idx == 0, 0, e_vec[idx])
  }
  p_on_cpap <- pmax(0, 1 - cif_at(t_d, e_d, grid) - cif_at(t_w, e_w, grid))
  sum(diff(grid) * p_on_cpap[-length(p_on_cpap)])
}

bootstrap_rmtl_by_band <- function(data, weight_cat, n_boot = 200, horizon = 28) {
  band_data <- data %>% filter(birthweight_cat == weight_cat)
  facilities <- unique(band_data$in_facid)
  point_est <- compute_rmtl_single_band(band_data, horizon)

  boot_ests <- map_dbl(1:n_boot, function(i) {
    sampled <- sample(facilities, length(facilities), replace = TRUE)
    boot_sample <- map_dfr(sampled, ~band_data %>% filter(in_facid == .x))
    tryCatch(compute_rmtl_single_band(boot_sample, horizon), error = function(e) NA_real_)
  })
  boot_ests <- boot_ests[!is.na(boot_ests)]

  tibble(
    birthweight_cat = weight_cat,
    rmtl_days    = point_est,
    rmtl_se      = sd(boot_ests, na.rm = TRUE),
    rmtl_ci_low  = quantile(boot_ests, 0.025, na.rm = TRUE),
    rmtl_ci_high = quantile(boot_ests, 0.975, na.rm = TRUE),
    n_valid_boot = length(boot_ests)
  )
}

set.seed(123)
rmtl_by_band <- map_dfr(
  levels(cpap_duration$birthweight_cat),
  ~bootstrap_rmtl_by_band(cpap_duration, .x, n_boot = 200, horizon = 28)
)

cat("\n========== VALIDATED LOS (RMTL) BY WEIGHT BAND ==========\n")
print(rmtl_by_band)

# ------------------------------------------------------------------------------
# 6. SAMPLE SIZE PER BAND (context for CI width -- smaller bands have wider CIs)
# ------------------------------------------------------------------------------

cpap_duration %>%
  group_by(birthweight_cat) %>%
  summarise(n_facilities = n_distinct(in_facid), n_babies = n(), .groups = "drop") %>%
  print()

# NOTE: "4001+" typically has the smallest n (both facilities and babies) of
# any band, and correspondingly the widest CI. This is expected -- see CI
# width relative to sample size above -- not a data quality problem.

# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------
cat(sprintf("\n05_cpap_los_rmtl.R complete. rmtl_by_band: %d weight bands.\n",
            nrow(rmtl_by_band)))
