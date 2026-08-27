
# ============================================================================
# 1. ELIGIBILITY BY WEIGHT CATEGORY (CRITERIA-BASED ONLY)
# ============================================================================

cpap_by_weight <- df %>%
  filter(!is.na(birthweight_cat)) %>%
  group_by(Country, in_facid, birthweight_cat) %>%
  summarise(
    admissions = n(),
    eligible_by_criteria = sum(CPAP_eligible == "Yes", na.rm = TRUE),
    cpaps_done = sum(in_cp_admin == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Expert rates for heavy babies
    eligible_by_expert = case_when(
      birthweight_cat == "2000-2499" ~ admissions * 0.08,
      birthweight_cat %in% c("2500-4000", "4001+") ~ admissions * 0.05,
      TRUE ~ NA_real_
    ),
    
    # STEP 1: Get initial estimate
    eligible_initial = case_when(
      birthweight_cat %in% c("<1000", "1000-1499", "1500-1999") ~ eligible_by_criteria,
      !is.na(eligible_by_expert) ~ eligible_by_expert,
      TRUE ~ eligible_by_criteria
    ),
    
    # STEP 2: CORE RULE - Use whichever is higher: estimate OR actual receipt
    # This guarantees: if baby got CPAP, they count as eligible
    eligible_final = pmax(eligible_initial, cpaps_done, na.rm = TRUE),
    
    # STEP 3: Calculate percentages
    eligible_percent = (eligible_final / admissions) * 100,
    
    # STEP 4: Coverage now GUARANTEED ≤100% by construction
    coverage_percent = (cpaps_done / eligible_final) * 100
  ) %>%
  select(Country, in_facid, birthweight_cat, admissions, 
         eligible_final, eligible_percent, coverage_percent, cpaps_done)


# ============================================================================
# 2. AGGREGATE TO FACILITY LEVEL
# ============================================================================

cpap_by_facility <- cpap_by_weight %>%
  group_by(Country, in_facid) %>%
  summarise(
    total_admissions = sum(admissions, na.rm = TRUE),
    total_eligible = sum(eligible_final, na.rm = TRUE),
    total_cpaps_done = sum(cpaps_done, na.rm = TRUE),
    eligible_percent_overall = (total_eligible / total_admissions) * 100,
    # Coverage from totals (also ≤100% by construction)
    coverage_overall = (total_cpaps_done / total_eligible) * 100,
    .groups = "drop"
  ) %>%
  left_join(fac_levels %>% select(in_facid, fac_name, facility_type, neo_level),
            by = "in_facid")

# ============================================================================
# 3. OPERATIONAL PARAMETERS (LOS & SURGE)
# ============================================================================

patient_ops <- df %>%
  filter(!is.na(LOS), !is.na(birthweight_cat), Month >= "2024-01-01") %>%
  mutate(cpap_eligible = ifelse(CPAP_eligible == "Yes" | in_cp_admin == 1, 1, 0)) %>%
  select(Country, in_facid, birthweight_cat, in_doa, in_dis_dod, Month, LOS, cpap_eligible)

date_range <- patient_ops %>%
  summarise(min = min(in_doa, na.rm = TRUE), max = max(in_dis_dod, na.rm = TRUE))

daily_concurrent <- patient_ops %>%
  distinct(Country, in_facid, birthweight_cat) %>%
  crossing(date = seq.Date(date_range$min, date_range$max, by = "day")) %>%
  filter(date >= "2024-01-01", date <= "2025-12-31") %>%
  left_join(patient_ops, by = c("Country", "in_facid", "birthweight_cat"),
            relationship = "many-to-many") %>%
  filter(date >= in_doa, date <= in_dis_dod, cpap_eligible == 1) %>%
  count(Country, in_facid, birthweight_cat, date, name = "concurrent") %>%
  complete(Country, in_facid, birthweight_cat, date, fill = list(concurrent = 0)) %>%
  mutate(month = floor_date(date, "month"))

monthly_ops <- daily_concurrent %>%
  group_by(Country, in_facid, birthweight_cat, month) %>%
  summarise(
    avg_concurrent = mean(concurrent, na.rm = TRUE),
    peak_concurrent = max(concurrent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    patient_ops %>%
      group_by(Country, in_facid, birthweight_cat, Month) %>%
      summarise(median_los = median(LOS, na.rm = TRUE), .groups = "drop"),
    by = c("Country", "in_facid", "birthweight_cat", "month" = "Month"))

operational_params <- monthly_ops %>%
  group_by(Country, in_facid, birthweight_cat) %>%
  summarise(
    mean_concurrent = mean(avg_concurrent, na.rm = TRUE),
    peak_concurrent = max(peak_concurrent, na.rm = TRUE),
    surge_ratio = peak_concurrent / mean_concurrent,
    median_los = median(median_los, na.rm = TRUE),
    .groups = "drop") %>%
  filter(!is.na(birthweight_cat)) %>%
  mutate(surge_ratio = pmin(pmax(surge_ratio, 1.2), 3.0))

# ============================================================================
# 4. EQUIPMENT MAINTENANCE BUFFER
# ============================================================================
device_data <- fread("C:/Users/phomw/OneDrive - Rice University/OneDrive/Documents/Rice360/Datasets/Contextual/device_data.csv")

equipment_params <- device_data %>%
  select(country, in_facid, visit_date, md_eqt_cp_cp_avl, md_eqt_cp_cp_fc) %>% 
  mutate(date = as.Date(visit_date, "%m/%d/%Y"),
         Month = floor_date(date, 'month'),
         total = md_eqt_cp_cp_avl,
         functional = md_eqt_cp_cp_fc) %>% 
  summarise(
    functional_rate = mean(functional / total, na.rm = TRUE),
    redundancy_factor = 1 - functional_rate)

# ============================================================================
# 5. CALIBRATE ELIGIBILITY BY LEVEL OF CARE
# ============================================================================

eligibility_by_level <- cpap_by_facility %>%
  filter(
    !is.na(neo_level),
    total_eligible >= 30,
    case_when(
      neo_level %in% c("Level I", "Level II") ~ coverage_overall >= 40 & coverage_overall <= 85,
      neo_level == "Level III" ~ coverage_overall >= 50 & coverage_overall <= 85,
      TRUE ~ FALSE
    )
  ) %>%
  group_by(neo_level) %>%
  summarise(
    n_facilities = n(),
    coverage_range = sprintf("%.0f%%-%.0f%%", 
                             min(coverage_overall), max(coverage_overall)),
    eligible_rate_median = median(eligible_percent_overall) / 100,
    eligible_rate_mean = weighted.mean(eligible_percent_overall, total_admissions) / 100,
    eligible_rate_sd = sd(eligible_percent_overall) / 100,
    eligible_rate_se = eligible_rate_sd / sqrt(n_facilities),
    eligible_rate_p25 = quantile(eligible_percent_overall, 0.25) / 100,
    eligible_rate_p75 = quantile(eligible_percent_overall, 0.75) / 100,
    .groups = "drop"
  )

# ============================================================================
# 6. FIXED OPERATIONAL PARAMETERS (EXPERT OPINION)
# ============================================================================

los_params <- list(
  los_median = 3.0,
  los_se = 0.5
)

surge_params <- list(
  surge_median = 2.0,
  surge_se = 0.0
)

equipment_params <- list(
  maintenance_buffer = 0.05
)
# ============================================================================
# 7. FINAL PARAMETERS SUMMARY
# ============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║          CPAP DEVICE CALCULATOR - FINAL PARAMETERS         ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat("ELIGIBILITY RATE BY LEVEL OF CARE:\n")
cat("─────────────────────────────────────────────────────────────\n")
eligibility_by_level %>%
  mutate(
    display = sprintf("  %-12s %.1f%% (SE=%.2f%%, n=%d, coverage %s)",
                      neo_level,
                      eligible_rate_median * 100,
                      eligible_rate_se * 100,
                      n_facilities,
                      coverage_range)
  ) %>%
  pull(display) %>%
  cat(sep = "\n")

cat("\n\nOPERATIONAL PARAMETERS:\n")
cat("─────────────────────────────────────────────────────────────\n")
cat(sprintf("  LOS:              %.1f days (SE=%.1f, expert opinion)\n", 
            los_params$los_median, los_params$los_se))
cat(sprintf("  Surge Factor:     %.1f (fixed, expert opinion)\n", 
            surge_params$surge_median))
cat(sprintf("  Maintenance:      %.1f%%\n\n", 
            equipment_params$maintenance_buffer * 100))

# ============================================================================
# 8. ESTIMATION FUNCTION
# ============================================================================

estimate_cpap_devices <- function(monthly_admits, level_of_care) {
  
  params <- eligibility_by_level %>% filter(neo_level == level_of_care)
  
  if (nrow(params) == 0) {
    stop("Invalid level of care. Use: Level I, Level II, or Level III")
  }
  
  elig <- params$eligible_rate_median
  se_elig <- params$eligible_rate_se
  los <- los_params$los_median
  se_los <- los_params$los_se
  surge <- surge_params$surge_median
  maint <- equipment_params$maintenance_buffer
  
  daily_concurrent <- (monthly_admits * elig * los) / 30
  peak_demand <- daily_concurrent * surge
  with_maintenance <- peak_demand * (1 + maint)
  devices_needed <- ceiling(with_maintenance)
  
  se_total <- with_maintenance * sqrt((se_elig / elig)^2 + (se_los / los)^2)
  
  ci_lower <- ceiling(devices_needed - 1.96 * se_total)
  ci_upper <- ceiling(devices_needed + 1.96 * se_total)
  
  list(
    devices = devices_needed,
    ci_lower = max(1, ci_lower),
    ci_upper = ci_upper,
    daily_concurrent = round(daily_concurrent, 2),
    peak_demand = round(peak_demand, 2),
    with_maintenance = round(with_maintenance, 2),
    se = round(se_total, 2),
    parameters = list(
      eligibility = elig,
      los = los,
      surge = surge,
      maintenance = maint
    )
  )
}

# Calculator - test
estimate_cpap_devices(200, "Level III")


# ============================================================================
# 9. SAVE PARAMETERS
# ============================================================================

final_params <- list(
  eligibility = eligibility_by_level,
  los = los_params,
  surge = surge_params,
  maintenance = equipment_params$maintenance_buffer,
  metadata = list(
    calibration_date = Sys.Date(),
    coverage_criteria = "Level I/II: 40-85%, Level III: 50-85%",
    min_eligible = 30,
    total_facilities = sum(eligibility_by_level$n_facilities),
    version = "1.0"
  )
)

