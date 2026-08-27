# ============================================================================
# CPAP LENGTH OF TREATMENT (LOT) — COMPETING RISKS APPROACH
# ============================================================================

library(tidyverse)
library(lubridate)
library(survival)
library(cmprsk)
library(lme4)

# ============================================================================
# STEP 1: BUILD CPAP START/END DATETIMES
# ============================================================================

clean_time2 <- function(t) {
  t <- str_trim(as.character(t))
  t <- if_else(t %in% c("", "NA", "-3", "-1"), NA_character_, t)
  # pad to HH:MM:SS where needed — adjust pattern to match your raw format
  t
}

df <- df %>%
  mutate(
    in_doa       = as.Date(in_doa),
    in_dob       = as.Date(in_dob),
    in_cp_stdt_1 = as.Date(in_cp_stdt_1),
    in_cp_endt_1 = as.Date(in_cp_endt_1),
    
    in_tob        = clean_time2(in_tob),
    in_toa        = clean_time2(in_toa),
    in_cp_sttm_1  = clean_time2(in_cp_sttm_1),
    in_cp_entm_1  = clean_time2(in_cp_entm_1),
    
    # Combined datetimes
    birth_dt    = ymd_hms(paste(in_dob, in_tob), quiet = TRUE),
    admit_dt    = ymd_hms(paste(in_doa, in_toa), quiet = TRUE),
    cpap_start  = ymd_hms(paste(in_cp_stdt_1, in_cp_sttm_1), quiet = TRUE),
    cpap_end    = ymd_hms(paste(in_cp_endt_1, in_cp_entm_1), quiet = TRUE),
    
    # Fallback: if time-of-day missing but date present, use date only (midday assumption avoided —
    # flag instead so duration isn't silently wrong)
    cpap_start_dateonly_flag = is.na(cpap_start) & !is.na(in_cp_stdt_1),
    cpap_end_dateonly_flag   = is.na(cpap_end) & !is.na(in_cp_endt_1),
    
    cpap_start = if_else(is.na(cpap_start) & !is.na(in_cp_stdt_1),
                         as_datetime(in_cp_stdt_1), cpap_start),
    cpap_end   = if_else(is.na(cpap_end) & !is.na(in_cp_endt_1),
                         as_datetime(in_cp_endt_1), cpap_end)
  )

# ============================================================================
# STEP 2: CPAP DURATION WITH PROPER CENSORING TREATMENT --> (HOURS -> DAYS) WITH QC
# ============================================================================

cpap_duration <- df %>%
  filter(in_cp_admin == 1, !is.na(cpap_start)) %>%
  left_join(
    df %>% select(Country, in_facid, in_recid, outcome_status = outcome_label),
    by = c("Country", "in_facid", "in_recid")
  ) %>%
  mutate(
    has_cpap_end = !is.na(cpap_end),
    
    duration_end_dt = case_when(
      has_cpap_end ~ cpap_end,
      !has_cpap_end & outcome_status == "Dead"  ~ ymd_hms(paste(in_dis_dod, "00:00:00"), quiet = TRUE),
      !has_cpap_end & outcome_status == "Alive" ~ ymd_hms(paste(in_dis_dod, "00:00:00"), quiet = TRUE),
      TRUE ~ as.POSIXct(NA)
    ),
    
    cpap_duration_hrs  = as.numeric(difftime(duration_end_dt, cpap_start, units = "hours")),
    cpap_duration_days = cpap_duration_hrs / 24,
    
    qc_negative    = cpap_duration_hrs < 0,
    qc_implausible = cpap_duration_days > 60,
    qc_valid       = !qc_negative & !qc_implausible & !is.na(cpap_duration_days),
    
    outcome_event = case_when(
      outcome_status == "Dead" ~ 1,
      outcome_status == "Alive" & has_cpap_end  ~ 2,
      outcome_status == "Alive" & !has_cpap_end ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(qc_valid, !is.na(outcome_event))

cat(sprintf(
  "Final analytic sample: %d episodes\n  Died (event=1): %d\n  Confirmed wean (event=2): %d\n  Censored, alive no end-date (event=0): %d\n",
  nrow(cpap_duration),
  sum(cpap_duration$outcome_event == 1),
  sum(cpap_duration$outcome_event == 2),
  sum(cpap_duration$outcome_event == 0)
))


# Confirm mortality rate isn't concentrated in a few outlier facilities
cpap_duration %>%
  group_by(Country, in_facid) %>%
  summarise(n = n(), died_pct = mean(outcome_event == 1) * 100, .groups = "drop") %>%
  arrange(desc(died_pct)) |> 
  print(n = Inf)


# And compare to overall CPAP-recipient mortality by weight band —
# should be higher in <1000g, lower in larger babies, if this reflects real clinical risk
cpap_duration %>%
  group_by(birthweight_cat) %>%
  summarise(n = n(), died_pct = mean(outcome_event == 1) * 100, .groups = "drop")


cpap_duration_clean <- cpap_duration %>% filter(qc_valid)

# ============================================================================
# STEP 3: COMPETING RISKS — SEPARATE "SUCCESSFUL WEAN" FROM "DIED ON CPAP"
# ============================================================================
# event = 1: died (competing event — removes baby from being able to "wean")
# event = 2: discharged alive (event of interest — true successful CPAP course)
# event = 0: censored (still admitted / missing outcome / lost to follow-up)

cpap_duration_clean <- cpap_duration_clean %>%
  mutate(
    outcome_event = case_when(
      outcome_label == "Dead" ~ 1,
      outcome_label == "Alive" ~ 2,
      TRUE ~ 0
    )
  )

cat("\nOutcome distribution among CPAP episodes:\n")
print(table(cpap_duration_clean$outcome_event, useNA = "always"))

# Cause-specific cumulative incidence
cif_fit <- cuminc(
  ftime   = cpap_duration_clean$cpap_duration_days,
  fstatus = cpap_duration_clean$outcome_event,
  group   = cpap_duration_clean$birthweight_cat
)

# Extract median time-to-successful-wean per weight band (event code "2")
extract_median_cif <- function(cif_fit, weight_cats) {
  results <- list()
  for (wc in weight_cats) {
    key <- paste(wc, "2")   # group + event code 2 (discharge alive)
    if (key %in% names(cif_fit)) {
      time  <- cif_fit[[key]]$time
      est   <- cif_fit[[key]]$est
      # median = first time est crosses 0.5 * max(est reached)
      target <- 0.5 * max(est, na.rm = TRUE)
      med_time <- time[which(est >= target)[1]]
      results[[wc]] <- med_time
    }
  }
  results
}

median_los_by_weight <- extract_median_cif(
  cif_fit, 
  levels(cpap_duration_clean$birthweight_cat)
)

cat("\nMedian time-to-successful-wean (competing risks) by weight band:\n")
print(median_los_by_weight)


# Check the max CIF reached per band — flags where median may not exist
extract_cif_summary <- function(cif_fit, weight_cats) {
  out <- tibble(
    birthweight_cat = character(),
    median_days     = numeric(),
    max_cif_reached = numeric(),
    median_valid    = logical()
  )
  
  for (wc in weight_cats) {
    key <- paste(wc, "2")  # event code 2 = confirmed wean
    if (key %in% names(cif_fit)) {
      time <- cif_fit[[key]]$time
      est  <- cif_fit[[key]]$est
      max_est <- max(est, na.rm = TRUE)
      target  <- 0.5 * max_est
      
      # median only exists if the curve actually reaches 50% of its own max AND that max >= 0.5 overall
      if (max_est >= 0.5) {
        idx <- which(est >= 0.5)[1]  # true median = first time overall CIF hits 0.5, not 0.5*max
        med <- if (!is.na(idx)) time[idx] else NA_real_
      } else {
        med <- NA_real_
      }
      
      out <- out %>% add_row(
        birthweight_cat = wc,
        median_days     = med,
        max_cif_reached = max_est,
        median_valid    = max_est >= 0.5
      )
    }
  }
  out
}

cif_summary <- extract_cif_summary(cif_fit, levels(cpap_duration$birthweight_cat))
print(cif_summary)


library(survival)

compute_rmtl_oncpap <- function(cif_fit, weight_cats, horizon = 28) {
  out <- tibble(birthweight_cat = character(), rmtl_oncpap_days = numeric())
  
  for (wc in weight_cats) {
    key_death <- paste(wc, "1")  # event 1 = died
    key_wean  <- paste(wc, "2")  # event 2 = successful wean
    
    if (key_death %in% names(cif_fit) & key_wean %in% names(cif_fit)) {
      
      t_d <- cif_fit[[key_death]]$time; e_d <- cif_fit[[key_death]]$est
      t_w <- cif_fit[[key_wean]]$time;  e_w <- cif_fit[[key_wean]]$est
      
      # common time grid
      grid <- sort(unique(c(0, t_d[t_d <= horizon], t_w[t_w <= horizon], horizon)))
      
      # step-function lookup: cumulative incidence at each grid time
      cif_at <- function(t_vec, e_vec, grid) {
        idx <- findInterval(grid, t_vec)
        ifelse(idx == 0, 0, e_vec[idx])
      }
      
      cif_death_grid <- cif_at(t_d, e_d, grid)
      cif_wean_grid  <- cif_at(t_w, e_w, grid)
      
      # P(still on CPAP at time t) = 1 - CIF_death(t) - CIF_wean(t)
      p_on_cpap <- pmax(0, 1 - cif_death_grid - cif_wean_grid)
      
      # integrate via trapezoid rule
      rmtl <- sum(diff(grid) * p_on_cpap[-length(p_on_cpap)])
      
      out <- out %>% add_row(birthweight_cat = wc, rmtl_oncpap_days = rmtl)
    }
  }
  out
}

rmtl_oncpap <- compute_rmtl_oncpap(cif_fit, levels(cpap_duration$birthweight_cat), horizon = 28)
print(rmtl_oncpap)

rmtl_60 <- compute_rmtl_oncpap(cif_fit, levels(cpap_duration$birthweight_cat), horizon = 60)
print(rmtl_60)



# ============================================================================
# BOOTSTRAP RMTL WITH FACILITY-LEVEL CLUSTERING
# ============================================================================
library(boot)

compute_rmtl_single_band <- function(data, horizon = 28) {
  # requires: cpap_duration_days, outcome_event columns already in data
  cif <- cuminc(ftime = data$cpap_duration_days, fstatus = data$outcome_event)
  
  key_death <- "1 1"  # cuminc names groups "1" by default with single group
  key_wean  <- "1 2"
  
  if (!(key_death %in% names(cif)) | !(key_wean %in% names(cif))) return(NA_real_)
  
  t_d <- cif[[key_death]]$time; e_d <- cif[[key_death]]$est
  t_w <- cif[[key_wean]]$time;  e_w <- cif[[key_wean]]$est
  
  grid <- sort(unique(c(0, t_d[t_d <= horizon], t_w[t_w <= horizon], horizon)))
  cif_at <- function(t_vec, e_vec, grid) {
    idx <- findInterval(grid, t_vec)
    ifelse(idx == 0, 0, e_vec[idx])
  }
  cif_death_grid <- cif_at(t_d, e_d, grid)
  cif_wean_grid  <- cif_at(t_w, e_w, grid)
  p_on_cpap <- pmax(0, 1 - cif_death_grid - cif_wean_grid)
  
  sum(diff(grid) * p_on_cpap[-length(p_on_cpap)])
}

bootstrap_rmtl_by_band <- function(cpap_duration, weight_cat, n_boot = 200, horizon = 28) {
  
  band_data <- cpap_duration %>% filter(birthweight_cat == weight_cat)
  facilities <- unique(band_data$in_facid)
  
  point_est <- compute_rmtl_single_band(band_data, horizon)
  
  boot_ests <- numeric(n_boot)
  for (i in 1:n_boot) {
    sampled_facilities <- sample(facilities, length(facilities), replace = TRUE)
    boot_sample <- map_dfr(sampled_facilities, function(f) band_data %>% filter(in_facid == f))
    boot_ests[i] <- tryCatch(
      compute_rmtl_single_band(boot_sample, horizon),
      error = function(e) NA_real_
    )
  }
  
  boot_ests <- boot_ests[!is.na(boot_ests)]
  
  tibble(
    birthweight_cat = weight_cat,
    rmtl_point   = point_est,
    rmtl_se      = sd(boot_ests, na.rm = TRUE),
    rmtl_ci_low  = quantile(boot_ests, 0.025, na.rm = TRUE),
    rmtl_ci_high = quantile(boot_ests, 0.975, na.rm = TRUE),
    n_valid_boot = length(boot_ests)
  )
}

set.seed(123)
rmtl_with_ci <- map_dfr(
  levels(cpap_duration$birthweight_cat),
  ~bootstrap_rmtl_by_band(cpap_duration, .x, n_boot = 200, horizon = 28)
)

print(rmtl_with_ci)

cpap_duration %>%
  group_by(birthweight_cat) %>%
  summarise(n_facilities = n_distinct(in_facid), n_babies = n(), .groups = "drop")

# CASE-MIX MODEL


case_mix_data <- cpap_duration_clean %>%
  mutate(
    # Antenatal corticosteroids — 3-level: Yes / No / Not documented
    anc_steroids_cat = case_when(
      in_corts_admin == 1 ~ "Yes",
      in_corts_admin == 0 ~ "No",
      in_corts_admin %in% c(-1, -3) ~ "Not documented",
      TRUE ~ "Not documented"
    ) %>% factor(levels = c("No", "Yes", "Not documented")),  # "No" as reference
    
    temp_plausible = in_tmp_adm_cel >= 32 & in_tmp_adm_cel <= 42,
    hypothermia = case_when(
      !temp_plausible ~ NA_real_,
      in_tmp_adm_cel < 36.5 ~ 1,
      in_tmp_adm_cel >= 36.5 ~ 0
    ),
    
    infection = case_when(
      final_diagnosis_cat1_label == "Infection" ~ 1,
      final_diagnosis_cat1_label %in% c("Not readable", "Not recorded") ~ NA_real_,
      !is.na(final_diagnosis_cat1_label) ~ 0
    )
  ) %>%
  filter(!is.na(hypothermia), !is.na(infection))  # only drop on temp/infection now, NOT on ACS

cat(sprintf("Case-mix analytic sample: %d of %d episodes retained (%.1f%%)\n",
            nrow(case_mix_data), nrow(cpap_duration_clean),
            100 * nrow(case_mix_data) / nrow(cpap_duration_clean)))

cat("\nRetention by country (should now be much more balanced):\n")
cpap_duration_clean %>%
  mutate(retained = in_recid %in% case_mix_data$in_recid) %>%
  group_by(Country) %>%
  summarise(n = n(), pct_retained = mean(retained) * 100, .groups = "drop")

cat("\nACS documentation by country (transparency check):\n")
case_mix_data %>%
  group_by(Country) %>%
  count(anc_steroids_cat) %>%
  mutate(pct = n / sum(n) * 100) %>%
  print(n = 20)

cat(sprintf("Case-mix analytic sample: %d of %d episodes retained (%.1f%%)\n",
            nrow(case_mix_data), nrow(cpap_duration_clean),
            100 * nrow(case_mix_data) / nrow(cpap_duration_clean)))

cat("\nCovariate distribution in analytic sample:\n")



# Quick collinearity check — cross-tab confirms the near-collinearity numerically
case_mix_data %>%
  count(Country, anc_steroids_cat) %>%
  group_by(anc_steroids_cat) %>%
  mutate(pct_of_category_from_country = n / sum(n) * 100) %>%
  filter(anc_steroids_cat == "Not documented") %>%
  arrange(desc(pct_of_category_from_country))


# Fit RMTL regression per weight band
library(eventglm)

case_mix_data <- case_mix_data %>%
  mutate(
    outcome_event_fct = factor(
      outcome_event,
      levels = c(0, 1, 2),
      labels = c("censored", "died", "weaned")
    )
  )

fit_rmtl_by_band <- function(data, weight_cat, horizon = 28) {
  band_data <- data %>% filter(birthweight_cat == weight_cat)
  
  rmeanglm(
    Surv(cpap_duration_days, outcome_event_fct) ~
      anc_steroids_cat + hypothermia + infection + Country,
    data  = band_data,
    time  = horizon,
    cause = "weaned",   # referencing the factor label directly
    link  = "identity"
  )
}

bands_for_case_mix <- setdiff(levels(case_mix_data$birthweight_cat), "4001+")

rmtl_models <- map(
  bands_for_case_mix,
  ~fit_rmtl_by_band(case_mix_data, .x)
)
names(rmtl_models) <- bands_for_case_mix

walk2(rmtl_models, names(rmtl_models), function(m, band) {
  cat(sprintf("\n=== %s ===\n", band))
  print(summary(m))
})




cat("Horizon value being passed:", horizon, "\n")  # should print 28

# Check units of the outcome variable actually going into the model
band_data_check <- case_mix_data %>% filter(birthweight_cat == "1000-1499")
summary(band_data_check$cpap_duration_days)

reference_subgroup <- case_mix_data %>%
  filter(birthweight_cat == "1000-1499",
         Country == "Ethiopia",
         anc_steroids_cat == "No",
         hypothermia == 0,
         infection == 0)

cat("N in reference subgroup:", nrow(reference_subgroup), "\n")

cif_ref <- cuminc(ftime = reference_subgroup$cpap_duration_days,
                  fstatus = reference_subgroup$outcome_event)

# reuse your validated compute_rmtl_single_band-style logic here restricted to this subgroup
compute_rmtl_single_band(reference_subgroup, horizon = 28)



band_data_test <- case_mix_data %>% filter(birthweight_cat == "1000-1499")

test_fit <- rmeanglm(
  Surv(cpap_duration_days, outcome_event_fct) ~
    anc_steroids_cat + hypothermia + infection + Country,
  time  = 28,               # hard-coded, not via variable
  cause = "weaned",
  link  = "identity",
  data  = band_data_test
)

summary(test_fit)

# Inspect the actual pseudo-observations the model is fitting to —
# these should average out to roughly the true RMTL (~4-5 days) if working correctly
pseudo_vals <- attr(test_fit, "pseudo.vals")
if (!is.null(pseudo_vals)) {
  cat("Mean pseudo-observation value:", mean(pseudo_vals, na.rm = TRUE), "\n")
  cat("Range:", range(pseudo_vals, na.rm = TRUE), "\n")
}



library(pseudo)

fit_case_mix_manual <- function(data, weight_cat, horizon = 28) {
  band_data <- data %>% 
    filter(birthweight_cat == weight_cat) %>%
    filter(!is.na(anc_steroids_cat), !is.na(hypothermia), !is.na(infection))
  
  # pseudoci returns subject-level pseudo-observations for cumulative incidence
  # at the specified time point, for the cause of interest (2 = weaned)
  pseudo_obs <- pseudoci(
    time  = band_data$cpap_duration_days,
    event = band_data$outcome_event,  # numeric: 0=censored,1=died,2=weaned
    tmax  = horizon
  )
  
  # pseudo_obs$cause2 gives pseudo-values for cause=2 (weaned) at each requested time
  # take the value AT the horizon itself (last column) as the RMTL-equivalent pseudo-value
  band_data$pseudo_val <- pseudo_obs$cause2[, ncol(pseudo_obs$cause2)]
  
  cat(sprintf("[%s] Mean pseudo-value: %.2f (compare to manual RMTL)\n", 
              weight_cat, mean(band_data$pseudo_val, na.rm = TRUE)))
  
  # Now a completely standard GLM — identity link, quasi family for robust SEs,
  # clustered by facility via geepack to respect facility-level correlation
  library(geepack)
  geeglm(
    pseudo_val ~ anc_steroids_cat + hypothermia + infection + Country,
    id = in_facid,
    data = band_data,
    family = gaussian(link = "identity"),
    corstr = "exchangeable"
  )
}

test_manual <- fit_case_mix_manual(case_mix_data, "1000-1499", horizon = 28)
summary(test_manual)


library(pseudo)

band_data <- case_mix_data %>% 
  filter(birthweight_cat == "1000-1499") %>%
  filter(!is.na(anc_steroids_cat), !is.na(hypothermia), !is.na(infection))

# Generate pseudo-observations for cumulative incidence of cause=2 (weaned) at t=28
pseudo_obs <- pseudoci(
  time  = band_data$cpap_duration_days,
  event = band_data$outcome_event,   # 0=censored, 1=died, 2=weaned
  tmax  = 28
)

# INSPECT BEFORE PROCEEDING — confirm structure
str(pseudo_obs, max.level = 1)




# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# ============================================================================
# STEP 1: TYPICAL CASE-MIX (%) BY LEVEL OF CARE
# ============================================================================

# Uses cpap_facility_summary (or equivalent facility-level table with 
# pct_elbw, pct_vlbw, pct_lbw_lower, pct_lbw_upper, pct_normal, pct_large)
# joined to neo_level

typical_casemix_by_level <- cpap_facility_summary %>%
  filter(!is.na(neo_level)) %>%
  group_by(neo_level) %>%
  summarise(
    n_facilities = n(),
    pct_elbw      = mean(pct_elbw, na.rm = TRUE),       # <1000g
    pct_vlbw      = mean(pct_vlbw, na.rm = TRUE),        # 1000-1499g
    pct_lbw_lower = mean(pct_lbw_lower, na.rm = TRUE),   # 1500-1999g
    pct_lbw_upper = mean(pct_lbw_upper, na.rm = TRUE),   # 2000-2499g
    pct_normal    = mean(pct_normal, na.rm = TRUE),      # 2500-4000g
    pct_large     = mean(pct_large, na.rm = TRUE),       # 4001+g
    .groups = "drop"
  ) %>%
  mutate(
    # normalize so each row sums to 100% (rounding/missingness safety)
    total_check = pct_elbw + pct_vlbw + pct_lbw_lower + pct_lbw_upper + pct_normal + pct_large,
    across(c(pct_elbw, pct_vlbw, pct_lbw_lower, pct_lbw_upper, pct_normal, pct_large),
           ~ .x / total_check * 100)
  ) %>%
  select(-total_check)

cat("========== TYPICAL CASE-MIX BY LEVEL ==========\n")
print(typical_casemix_by_level)


# ============================================================================
# STEP 2: BLEND LOS PER LEVEL (weighted by typical case-mix)
# ============================================================================

# RMTL values from your validated competing-risks analysis
rmtl_params <- tibble(
  birthweight_cat = c("<1000", "1000-1499", "1500-1999", "2000-2499", "2500-4000", "4001+"),
  rmtl_days = c(4.19, 4.72, 4.68, 4.15, 4.16, 4.87),
  rmtl_se   = c(0.239, 0.396, 0.699, 0.538, 0.609, 0.825)
)

blend_los_by_level <- typical_casemix_by_level %>%
  rowwise() %>%
  mutate(
    # weighted mean LOS: sum(pct_band/100 * rmtl_band)
    los_median = pct_elbw/100      * rmtl_params$rmtl_days[rmtl_params$birthweight_cat == "<1000"] +
      pct_vlbw/100      * rmtl_params$rmtl_days[rmtl_params$birthweight_cat == "1000-1499"] +
      pct_lbw_lower/100 * rmtl_params$rmtl_days[rmtl_params$birthweight_cat == "1500-1999"] +
      pct_lbw_upper/100 * rmtl_params$rmtl_days[rmtl_params$birthweight_cat == "2000-2499"] +
      pct_normal/100    * rmtl_params$rmtl_days[rmtl_params$birthweight_cat == "2500-4000"] +
      pct_large/100     * rmtl_params$rmtl_days[rmtl_params$birthweight_cat == "4001+"],
    
    # propagated SE: sqrt(sum((weight * se_band)^2)) — standard weighted-sum SE propagation
    los_se = sqrt(
      (pct_elbw/100      * rmtl_params$rmtl_se[rmtl_params$birthweight_cat == "<1000"])^2 +
        (pct_vlbw/100      * rmtl_params$rmtl_se[rmtl_params$birthweight_cat == "1000-1499"])^2 +
        (pct_lbw_lower/100 * rmtl_params$rmtl_se[rmtl_params$birthweight_cat == "1500-1999"])^2 +
        (pct_lbw_upper/100 * rmtl_params$rmtl_se[rmtl_params$birthweight_cat == "2000-2499"])^2 +
        (pct_normal/100    * rmtl_params$rmtl_se[rmtl_params$birthweight_cat == "2500-4000"])^2 +
        (pct_large/100     * rmtl_params$rmtl_se[rmtl_params$birthweight_cat == "4001+"])^2
    )
  ) %>%
  ungroup() %>%
  select(neo_level, n_facilities, los_median, los_se,
         pct_elbw, pct_vlbw, pct_lbw_lower, pct_lbw_upper, pct_normal, pct_large)

cat("\n========== BLENDED LOS BY LEVEL ==========\n")
print(blend_los_by_level)


# ============================================================================
# STEP 3: FINAL CALCULATOR PARAMETERS (LOS now level-specific, RMTL-derived)
# ============================================================================

final_los_params <- blend_los_by_level %>%
  select(neo_level, los_median, los_se)

cat("\n========== FINAL LOS PARAMETERS FOR CALCULATOR ==========\n")
final_los_params %>%
  mutate(
    display = sprintf("  %-25s %.2f days (SE=%.3f)", neo_level, los_median, los_se)
  ) %>%
  pull(display) %>%
  cat(sep = "\n")
