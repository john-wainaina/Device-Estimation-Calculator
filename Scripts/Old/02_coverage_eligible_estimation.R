cpap_estimate <- df %>%
  filter(!is.na(birthweight_cat)) %>%
  
  # ═══════════════════════════════════════════════════════════
  # STEP 1: Calculate RAW COUNTS for all weight categories
  # ═══════════════════════════════════════════════════════════
  group_by(Country, in_facid, birthweight_cat) %>%
  summarise(
    total_admissions = n(),
    
    # For <2000g: we have criteria-based eligibility
    eligible_by_criteria = sum(CPAP_eligible == "Yes", na.rm = TRUE),
    
    # For all categories: we have actual CPAP done
    cpaps_done = sum(in_cp_admin == 1, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  
  # ═══════════════════════════════════════════════════════════
  # STEP 2: Apply EXPERT RATES for ≥2000g categories
  # ═══════════════════════════════════════════════════════════
  mutate(
    # Define expert opinion rates
    expert_rate = case_when(
      birthweight_cat == "2000-2499" ~ 0.08,
      birthweight_cat == "2500-4000" ~ 0.05,
      birthweight_cat == "4001+" ~ 0.05,
      TRUE ~ NA_real_
    ),
    
    # Calculate expert-based eligible (only for ≥2000g)
    eligible_by_expert = case_when(
      !is.na(expert_rate) ~ total_admissions * expert_rate,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # ═══════════════════════════════════════════════════════════
  # STEP 3: HYBRID ELIGIBILITY - Handle ALL scenarios
  # ═══════════════════════════════════════════════════════════
  mutate(
    # Determine source of initial estimate
    estimate_source = case_when(
      birthweight_cat %in% c("<1000", "1000-1499", "1500-1999") ~ "criteria",
      birthweight_cat %in% c("2000-2499", "2500-4000", "4001+") ~ "expert",
      TRUE ~ "unknown"
    ),
    
    # Get the initial eligible estimate (criteria or expert)
    eligible_initial = case_when(
      estimate_source == "criteria" ~ eligible_by_criteria,
      estimate_source == "expert" ~ eligible_by_expert,
      TRUE ~ NA_real_
    ),
    
    # ───────────────────────────────────────────────────────────
    # CORE HYBRID LOGIC: Use higher of estimate vs. actual
    # ───────────────────────────────────────────────────────────
    eligible_final = case_when(
      # If initial estimate > cpaps done: use estimate (some didn't get CPAP)
      eligible_initial > cpaps_done ~ eligible_initial,
      
      # If initial estimate = cpaps done: perfect match, use either
      eligible_initial == cpaps_done ~ eligible_initial,
      
      # If initial estimate < cpaps done: estimate too low, use actual
      eligible_initial < cpaps_done ~ cpaps_done,
      
      # Safety: if estimate is NA but we have cpaps_done, use that
      is.na(eligible_initial) & cpaps_done > 0 ~ cpaps_done,
      
      # Otherwise: use initial estimate
      TRUE ~ eligible_initial
    ),
    
    # ───────────────────────────────────────────────────────────
    # Calculate FINAL METRICS
    # ───────────────────────────────────────────────────────────
    eligible_percent = (eligible_final / total_admissions) * 100,
    
    coverage_percent = case_when(
      eligible_final > 0 ~ (cpaps_done / eligible_final) * 100,
      TRUE ~ NA_real_
    ),
    
    # ───────────────────────────────────────────────────────────
    # DOCUMENTATION: What happened?
    # ───────────────────────────────────────────────────────────
    adjustment_logic = case_when(
      # Perfect match scenarios
      eligible_initial == cpaps_done & estimate_source == "criteria" ~ 
        "Criteria matched receipt (100% coverage)",
      eligible_initial == cpaps_done & estimate_source == "expert" ~ 
        "Expert rate matched receipt",
      
      # Estimate higher (some didn't get CPAP)
      eligible_initial > cpaps_done & estimate_source == "criteria" ~ 
        "Criteria used (coverage <100%)",
      eligible_initial > cpaps_done & estimate_source == "expert" ~ 
        "Expert rate used (coverage <100%)",
      
      # Estimate lower (under-identified need)
      eligible_initial < cpaps_done & estimate_source == "criteria" ~ 
        "Receipt used - criteria too low (was >100%)",
      eligible_initial < cpaps_done & estimate_source == "expert" ~ 
        "Receipt used - expert rate too low",
      
      # Edge cases
      is.na(eligible_initial) & cpaps_done > 0 ~ 
        "Receipt used - no initial estimate",
      
      TRUE ~ "No adjustment needed"
    ),
    
    # Gap size (for reporting)
    eligibility_gap = case_when(
      eligible_initial < cpaps_done ~ cpaps_done - eligible_initial,
      TRUE ~ 0
    ),
    
    # Flag if adjustment was made
    adjusted = eligible_final != eligible_initial & !is.na(eligible_initial)
  ) %>%
  
  # ═══════════════════════════════════════════════════════════
  # STEP 4: Round for presentation
  # ═══════════════════════════════════════════════════════════
  mutate(across(where(is.numeric), ~round(.x, 1))) %>% 
  
  select(-c(eligible_by_criteria, cpaps_done, expert_rate, 
            eligible_by_expert, estimate_source, eligible_initial,
            adjustment_logic, eligibility_gap, adjusted))

# Principles based model ---------------- 

cpap_parameters <- cpap_estimate %>%
  # filter(
  #   # Only high-performing facilities for calibration
  #   coverage_percent >= 80 & coverage_percent <= 100
  # ) %>%
  # group_by(birthweight_cat) %>%
  summarise(
    n_facilities = n(),
    
    # These become your model parameters
    mean_eligible_pct = mean(eligible_percent, na.rm = TRUE),
    median_eligible_pct = median(eligible_percent, na.rm = TRUE),
    sd_eligible_pct = sd(eligible_percent, na.rm = TRUE),
    
    # For confidence intervals
    min_eligible_pct = quantile(eligible_percent, 0.25, na.rm = TRUE),
    max_eligible_pct = quantile(eligible_percent, 0.75, na.rm = TRUE)
  )

# monthly concurrent demand analysis

patient_data_clean <- df %>%
  filter(!is.na(LOS), Month >= '2024-01-01' & Year <= '2025-12-01') %>%
  mutate(
    # CPAP eligible flag (from hybrid logic)
    cpap_eligible = case_when(
      CPAP_eligible == "Yes" ~ 1,
      in_cp_admin == 1 ~ 1,  # If got CPAP, was eligible
      TRUE ~ 0)
  ) %>%
  select(Country, in_facid, birthweight_cat,
         in_doa, in_dis_dod, Month,
         LOS, cpap_eligible)

# Step 2: Create date sequence (every day in dataset)
# Get overall date range
date_range <- patient_data_clean %>%
  summarise(
    min_date = min(in_doa, na.rm = TRUE),
    max_date = max(in_dis_dod, na.rm = TRUE)
  )

# Create daily grid for each facility
facility_dates <- patient_data_clean %>%
  distinct(Country, in_facid) %>%
  crossing(
    date = seq.Date(
      from = date_range$min_date,
      to = date_range$max_date,
      by = "day"
    )
  ) %>% 
  filter(date >= '2024-01-01' & date <= '2025-12-31')

# Step 3: Calculate daily concurrent eligible patients

# For each facility-date, count patients who were:
# - Admitted on or before that date
# - Discharged on or after that date
# - Eligible for CPAP

daily_concurrent <- facility_dates %>%
  left_join(
    patient_data_clean,
    by = c("Country", "in_facid"),
    relationship = "many-to-many"
  ) %>%
  filter(
    date >= in_doa,
    date <= in_dis_dod,
    cpap_eligible == 1
  ) %>%
  group_by(Country, in_facid, birthweight_cat, date) %>%  # ADD birthweight_cat
  summarise(
    concurrent_eligible = n(),
    .groups = "drop"
  )

# Complete with zeros (including weight category)
facility_dates_weight <- patient_data_clean %>%
  distinct(Country, in_facid, birthweight_cat) %>%
  crossing(
    date = seq.Date(
      from = date_range$min_date,
      to = date_range$max_date,
      by = "day"
    )
  )

daily_concurrent_complete <- facility_dates_weight %>%
  left_join(
    daily_concurrent, 
    by = c("Country", "in_facid", "birthweight_cat", "date")
  ) %>%
  mutate(
    concurrent_eligible = replace_na(concurrent_eligible, 0),
    year_month = floor_date(date, "month")
  )

# Step 4: Aggregate to monthly summaries

monthly_facility_data <- daily_concurrent_complete %>%
  group_by(Country, in_facid, birthweight_cat, year_month) %>%  # ADD birthweight_cat
  summarise(
    # Concurrent demand metrics
    avg_daily_concurrent = mean(concurrent_eligible, na.rm = TRUE),
    median_daily_concurrent = median(concurrent_eligible, na.rm = TRUE),
    peak_daily_concurrent = max(concurrent_eligible, na.rm = TRUE),
    min_daily_concurrent = min(concurrent_eligible, na.rm = TRUE),
    
    # Variability
    sd_daily_concurrent = sd(concurrent_eligible, na.rm = TRUE),
    
    # Days in month
    days_in_month = n(),
    
    .groups = "drop"
  ) %>%
  
  # Calculate surge ratio
  mutate(
    surge_ratio = case_when(
      avg_daily_concurrent > 0 ~ peak_daily_concurrent / avg_daily_concurrent,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # Remove Ethiopia's 2024 data as no records in the period
  filter(!(Country == "Ethiopia" & 
             year_month >= as.Date("2024-01-01") & 
             year_month <= as.Date("2024-12-31")))

# Step 5: Add admission counts for context

monthly_admissions <- patient_data_clean %>%
  group_by(Country, in_facid, birthweight_cat, Month) %>%  # ADD birthweight_cat
  summarise(
    monthly_admissions = n(),
    monthly_eligible = sum(cpap_eligible, na.rm = TRUE),
    
    # ADD LOS calculation here
    median_los = median(LOS, na.rm = TRUE),
    mean_los = mean(LOS, na.rm = TRUE),
    
    .groups = "drop"
  )

monthly_facility_data <- monthly_facility_data %>%
  rename("Month" = "year_month") %>%  
  left_join(
    monthly_admissions,
    by = c("Country", "in_facid", "birthweight_cat", "Month")  # ADD birthweight_cat
  )

operational_params <- monthly_facility_data %>%
  group_by(Country, in_facid, birthweight_cat) %>%  # This now works!
  summarise(
    # Average of monthly averages
    mean_daily_concurrent = mean(avg_daily_concurrent, na.rm = TRUE),
    
    # Maximum of monthly peaks (true peak across all time)
    peak_daily_concurrent = max(peak_daily_concurrent, na.rm = TRUE),
    
    # Overall surge ratio
    surge_ratio = peak_daily_concurrent / mean_daily_concurrent,
    
    # LOS metrics
    median_los = median(median_los, na.rm = TRUE),
    mean_los = mean(mean_los, na.rm = TRUE),
    
    # Number of months with data
    months_data = n(),
    
    .groups = "drop"
  ) %>% 
  # Remove NA birthweight categories
  filter(!is.na(birthweight_cat)) %>%
  
  # Cap extreme surge ratios (likely due to small numbers)
  mutate(
    surge_ratio_capped = case_when(
      surge_ratio > 3 ~ 2.0,  # Cap at 2.0 (conservative)
      surge_ratio < 1 ~ 1.2,  # Minimum 1.2 (20% surge)
      TRUE ~ surge_ratio
    ),
    
    # Flag facilities with capped ratios
    surge_capped = surge_ratio != surge_ratio_capped
  ) %>%
  
  # Check for reasonable LOS
  mutate(
    los_reasonable = case_when(
      birthweight_cat == "<1000" & median_los >= 10 & median_los <= 30 ~ TRUE,
      birthweight_cat == "1000-1499" & median_los >= 7 & median_los <= 21 ~ TRUE,
      birthweight_cat == "1500-1999" & median_los >= 5 & median_los <= 14 ~ TRUE,
      birthweight_cat == "2000-2499" & median_los >= 3 & median_los <= 10 ~ TRUE,
      birthweight_cat %in% c("2500-4000", "4001+") & median_los >= 2 & median_los <= 8 ~ TRUE,
      TRUE ~ FALSE
    )
  )

# From equipment data

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

# fac_adms
# fac_levels
# cpap_estimate 
# operational_params
# equipment_params

# Join datasets together

cpap_model_data <- cpap_estimate %>%
  # Join operational parameters
  left_join(
    operational_params %>%
      select(Country, in_facid, birthweight_cat,
             mean_daily_concurrent, peak_daily_concurrent,
             surge_ratio_capped, median_los, mean_los, months_data),
    by = c("Country", "in_facid", "birthweight_cat")
  ) %>%
  # Join facility characteristics
  left_join(
    fac_levels %>%
      select(in_facid, fac_name, facility_type, neo_level),
    by = "in_facid"
  ) %>%
  # Add equipment parameters (same for all)
  mutate(
    functional_rate = equipment_params$functional_rate,
    redundancy_factor = equipment_params$redundancy_factor
  ) %>%
  # Rename for clarity
  rename(
    surge_ratio = surge_ratio_capped
  )

# Check join quality
join_check <- cpap_model_data %>%
  summarise(
    rows_total = n(),
    rows_missing_operational = sum(is.na(surge_ratio)),
    rows_missing_facility = sum(is.na(neo_level)),
    facilities_unique = n_distinct(in_facid)
  )


# Aggregate to facility level (ONE ROW per facility)
cpap_facility_summary <- cpap_model_data %>%
  # Group by facility
  group_by(Country, in_facid, fac_name, facility_type, neo_level) %>%
  summarise(
    # ─────────────────────────────────────────────────────────
    # TOTALS (summed across all weight categories)
    # ─────────────────────────────────────────────────────────
    total_admissions = sum(total_admissions, na.rm = TRUE),
    total_eligible = sum(eligible_final, na.rm = TRUE),
    
    # ─────────────────────────────────────────────────────────
    # OVERALL RATES
    # ─────────────────────────────────────────────────────────
    eligible_percent_overall = (total_eligible / total_admissions) * 100,
    
    # ─────────────────────────────────────────────────────────
    # CASE MIX (admissions by weight as % of total)
    # ─────────────────────────────────────────────────────────
    admits_elbw = sum(total_admissions[birthweight_cat == "<1000"], na.rm = TRUE),
    admits_vlbw = sum(total_admissions[birthweight_cat == "1000-1499"], na.rm = TRUE),
    admits_lbw_lower = sum(total_admissions[birthweight_cat == "1500-1999"], na.rm = TRUE),
    admits_lbw_upper = sum(total_admissions[birthweight_cat == "2000-2499"], na.rm = TRUE),
    admits_normal = sum(total_admissions[birthweight_cat == "2500-4000"], na.rm = TRUE),
    admits_large = sum(total_admissions[birthweight_cat == "4001+"], na.rm = TRUE),
    
    pct_elbw = (admits_elbw / total_admissions) * 100,
    pct_vlbw = (admits_vlbw / total_admissions) * 100,
    pct_lbw_lower = (admits_lbw_lower / total_admissions) * 100,
    pct_lbw_upper = (admits_lbw_upper / total_admissions) * 100,
    pct_normal = (admits_normal / total_admissions) * 100,
    pct_large = (admits_large / total_admissions) * 100,
    
    # ─────────────────────────────────────────────────────────
    # OPERATIONAL METRICS (weighted or max)
    # ─────────────────────────────────────────────────────────
    
    # Total concurrent demand (sum across weights)
    total_mean_concurrent = sum(mean_daily_concurrent, na.rm = TRUE),
    total_peak_concurrent = sum(peak_daily_concurrent, na.rm = TRUE),
    
    # Overall surge ratio (facility-wide)
    surge_ratio_overall = total_peak_concurrent / total_mean_concurrent,
    
    # Weighted average LOS (weighted by eligible in each category)
    weighted_median_los = weighted.mean(
      median_los,
      w = eligible_final,
      na.rm = TRUE
    ),
    
    # ─────────────────────────────────────────────────────────
    # COVERAGE (weighted by eligible)
    # ─────────────────────────────────────────────────────────
    coverage_weighted = weighted.mean(
      coverage_percent,
      w = eligible_final,
      na.rm = TRUE
    ),
    
    # ─────────────────────────────────────────────────────────
    # DATA QUALITY
    # ─────────────────────────────────────────────────────────
    months_data = median(months_data, na.rm = TRUE),
    weight_categories_present = n(),
    
    .groups = "drop"
  ) %>%
  
  # ─────────────────────────────────────────────────────────
  # HIGH PERFORMER FLAG (for parameter extraction)
  # ─────────────────────────────────────────────────────────
  mutate(
    high_performer = case_when(
      coverage_weighted >= 80 & 
        coverage_weighted <= 100 &
        total_eligible >= 50 &  # Minimum sample size
        months_data >= 6 ~ TRUE,  # At least 6 months data
      TRUE ~ FALSE
    )
  )


# Get list of high performer facilities
high_perf_facilities <- cpap_facility_summary %>%
  filter(high_performer) %>%
  select(Country, in_facid)

# Extract parameters from their weight-stratified data
calibration_params <- cpap_model_data %>%
  
  # Filter to high performers only
  inner_join(high_perf_facilities, by = c("Country", "in_facid")) %>%
  
  # For <2000g categories with good coverage
  filter(
    birthweight_cat %in% c("<1000", "1000-1499", "1500-1999"),
    coverage_percent >= 70,
    coverage_percent <= 100
  ) %>%
  
  # Calculate parameters by weight category
  group_by(birthweight_cat) %>%
  summarise(
    n_facilities = n_distinct(in_facid),
    
    # Eligibility rates (α, β parameters)
    eligible_pct_mean = mean(eligible_percent, na.rm = TRUE),
    eligible_pct_median = median(eligible_percent, na.rm = TRUE),
    eligible_pct_p25 = quantile(eligible_percent, 0.25, na.rm = TRUE),
    eligible_pct_p75 = quantile(eligible_percent, 0.75, na.rm = TRUE),
    
    # LOS parameters
    los_median = median(median_los, na.rm = TRUE),
    los_mean = mean(median_los, na.rm = TRUE),
    los_p25 = quantile(median_los, 0.25, na.rm = TRUE),
    los_p75 = quantile(median_los, 0.75, na.rm = TRUE),
    
    # Surge ratios
    surge_mean = mean(surge_ratio, na.rm = TRUE),
    surge_median = median(surge_ratio, na.rm = TRUE),
    
    .groups = "drop"
  )


# Add expert-opinion parameters for ≥2000g
expert_params <- tibble(
  birthweight_cat = c("2000-2499", "2500-4000", "4001+"),
  eligible_pct_median = c(8, 5, 5),
  los_median = c(7, 5, 5),
  source = "expert opinion"
)


# Calculate CPAP device needs using the formula

# Extract specific parameters (median values)
alpha_elbw <- calibration_params %>% 
  filter(birthweight_cat == "<1000") %>% 
  pull(eligible_pct_median) / 100

alpha_vlbw <- calibration_params %>% 
  filter(birthweight_cat == "1000-1499") %>% 
  pull(eligible_pct_median) / 100

beta_lbw <- calibration_params %>% 
  filter(birthweight_cat == "1500-1999") %>% 
  pull(eligible_pct_median) / 100

gamma_upper <- 0.08  # 2000-2499g
delta_normal <- 0.05  # ≥2500g

# LOS parameters
los_elbw <- calibration_params %>% 
  filter(birthweight_cat == "<1000") %>% 
  pull(los_median)

los_vlbw <- calibration_params %>% 
  filter(birthweight_cat == "1000-1499") %>% 
  pull(los_median)

los_lbw <- calibration_params %>% 
  filter(birthweight_cat == "1500-1999") %>% 
  pull(los_median)

los_upper <- 7   # Expert
los_normal <- 5  # Expert

# Overall surge and redundancy
surge_factor <- calibration_params %>% 
  summarise(median(surge_median)) %>% 
  pull()

redundancy <- equipment_params$redundancy_factor

# Apply formula to each facility
cpap_facility_needs <- cpap_facility_summary %>%
  mutate(
    # Assume monthly admissions (divide total by months)
    monthly_admits = total_admissions / months_data,
    
    # Calculate daily concurrent eligible by weight
    daily_elbw = (monthly_admits * pct_elbw/100 * alpha_elbw * los_elbw) / 30,
    daily_vlbw = (monthly_admits * pct_vlbw/100 * alpha_vlbw * los_vlbw) / 30,
    daily_lbw_lower = (monthly_admits * pct_lbw_lower/100 * beta_lbw * los_lbw) / 30,
    daily_lbw_upper = (monthly_admits * pct_lbw_upper/100 * gamma_upper * los_upper) / 30,
    daily_normal = (monthly_admits * pct_normal/100 * delta_normal * los_normal) / 30,
    
    # Total average daily concurrent
    avg_concurrent_eligible = daily_elbw + daily_vlbw + daily_lbw_lower + 
      daily_lbw_upper + daily_normal,
    
    # Peak with surge
    peak_concurrent_eligible = avg_concurrent_eligible * surge_factor,
    
    # Devices needed (with redundancy)
    cpap_devices_needed = ceiling(peak_concurrent_eligible * (1 + redundancy)),
    
    # Alternative: Use their actual operational data
    cpap_devices_from_data = ceiling(total_peak_concurrent * (1 + redundancy))
  ) %>%
  
  select(Country, in_facid, fac_name, facility_type, neo_level,
         total_admissions, total_eligible, eligible_percent_overall,
         pct_elbw, pct_vlbw, pct_lbw_lower, pct_lbw_upper, pct_normal,
         avg_concurrent_eligible, peak_concurrent_eligible,
         cpap_devices_needed, cpap_devices_from_data,
         high_performer, coverage_weighted, months_data)


# Compare formula-based vs. data-based estimates

validation_compare <- cpap_facility_needs %>%
  mutate(
    difference = cpap_devices_needed - cpap_devices_from_data,
    pct_difference = (difference / cpap_devices_from_data) * 100
  ) %>%
  select(Country, in_facid, fac_name, neo_level,
         cpap_devices_needed, cpap_devices_from_data, 
         difference, pct_difference)

# Summary statistics
validation_summary <- validation_compare %>%
  summarise(
    n_facilities = n(),
    mean_formula = mean(cpap_devices_needed, na.rm = TRUE),
    mean_data = mean(cpap_devices_from_data, na.rm = TRUE),
    median_difference = median(difference, na.rm = TRUE),
    mean_abs_pct_diff = mean(abs(pct_difference), na.rm = TRUE),
    within_20pct = sum(abs(pct_difference) <= 20, na.rm = TRUE) / n() * 100
  )

# By level of care
validation_by_level <- validation_compare %>%
  group_by(neo_level) %>%
  summarise(
    n = n(),
    mean_formula = mean(cpap_devices_needed, na.rm = TRUE),
    mean_data = mean(cpap_devices_from_data, na.rm = TRUE),
    median_diff = median(difference, na.rm = TRUE)
  )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Estimating device needs - formula

#  `CPAP devices needed = (Monthly admissions × % eligible × LOS_days / 30) × Surge factor × (1 + Maintenance buffer)`

# 1. ELIGIBILITY RATE by level
eligibility_by_level <- cpap_estimate %>%
  left_join(fac_levels, by = "in_facid") %>%
  filter(coverage_percent >= 80) %>%  # High performers only
  group_by(neo_level) %>%
  summarise(
    eligible_rate = weighted.mean(eligible_percent, total_admissions) / 100
  )

# 2. AVERAGE LOS
avg_los <- operational_params %>%
  filter(!is.na(birthweight_cat)) %>%
  summarise(los = median(median_los, na.rm = TRUE)) %>%
  pull(los)

# 3. SURGE FACTOR  
surge <- operational_params %>%
  filter(!is.na(birthweight_cat), surge_ratio < 3) %>%
  summarise(surge = median(surge_ratio, na.rm = TRUE)) %>%
  pull(surge)

# -- Using the formula fr any facility ----------------------------------------


calculate_cpap <- function(monthly_admissions, level_of_care) {
  
  # eligibility rate for this level
  elig_rate <- eligibility_by_level %>% 
    filter(neo_level == level_of_care) %>% 
    pull(eligible_rate)
  
  # Calculate
  concurrent <- (monthly_admissions * elig_rate * avg_los) / 30
  peak <- concurrent * surge
  devices_needed <- ceiling(peak * 1.034)  # +3.4% maintenance buffer calculated from devc fn/av
  
  return(devices_needed)
}

# New facility
calculate_cpap(monthly_admissions = 180, level_of_care = "Level II")







