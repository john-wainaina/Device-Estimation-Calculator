
#########################################################
# ---------- DEVICE ESTIMATION TOOL ------------------- #
#########################################################

rm(list = ls())

library(tidyverse)
library(data.table)

# NID data -------------------------------------------- #

df <- fread("C:/Users/phomw/OneDrive - Rice University/OneDrive/Documents/Rice360/Datasets/Patient/overall_nid_for_nestit.csv") %>% 
  filter(in_rec_typ == 2)

# Facility ids
df <- df[!is.na(in_facid), ]
df[, in_facid := as.integer(in_facid)]

# country col
df[, Country := fcase(
  in_facid %in% c(100:199), 'Malawi',
  in_facid %in% c(217:276), 'Kenya',
  in_facid %in% c(300:399), 'Tanzania',
  in_facid %in% c(400:499), 'Nigeria',
  in_facid %in% c(500:599), 'Ethiopia')]

df[, Phase := fifelse(
  in_facid %in% c(100:136) | 
    in_facid %in% c(217:276) |
    in_facid %in% c(301:307) | 
    in_facid %in% c(401:411) | 
    in_facid %in% c(501:530), 'Phase I', 'Phase II')]

# Phase filter
#df <- df[Phase == 'Phase I', ]

# weights and documentation
df[, in_bwt := fifelse(in_bwt < 0 | is.na(in_bwt), NA_real_, in_bwt)]
df[, in_bwt := fifelse(in_bwt <= 10 & in_bwt >= 0, in_bwt * 1000, in_bwt)]
df[, in_bwt := fifelse(in_bwt < 400 | in_bwt > 6000, NA_real_, in_bwt)]


# Date cleaning
df[, in_dob := as.IDate(in_dob, format = "%Y-%m-%d")]
df[, in_doa := as.IDate(in_doa, format = "%Y-%m-%d")]
df[, in_dis_dod := as.IDate(in_dis_dod, format = "%Y-%m-%d")]

df[, LOS := as.numeric(as.Date(in_dis_dod) - as.Date(in_dob))]
df[, LOS := fifelse(LOS < 0 | LOS > 100, NA_real_, LOS)]

# Birth weight categories
df[, birthweight_cat := fcase(
  in_bwt >= 400 & in_bwt <= 999,   1,
  in_bwt >= 1000 & in_bwt <= 1499, 2,
  in_bwt >= 1500 & in_bwt <= 1999, 3,
  in_bwt >= 2000 & in_bwt <= 2499, 4,
  in_bwt >= 2500 & in_bwt <= 4000, 5,
  in_bwt > 4000 & in_bwt <= 6000,  6,
  is.na(in_bwt), NA_real_)]

df[, birthweight_cat := factor(birthweight_cat, levels = 1:6,
                               labels = c('<1000', '1000-1499', '1500-1999', '2000-2499',
                                          '2500-4000', '4001+'))]

df[, in_dx_prim_1 := fcase(
  in_dx_prim_1 %in% c(-1, -3), NA_character_,
  in_dx_prim_1 %in% c(1, 2, 3, 4, 5, 6), as.character(in_dx_prim_1),
  is.na(in_dx_prim_1), NA_character_
)]

df$in_dx_prim_1_label <- factor(df$in_dx_prim_1,
                                levels = c(1, 2, 3, 4, 5, 6),
                                labels = c("Congenital Malformations",
                                           "Prematurity","Infection","Intrapartum-related",
                                           "Jaundice", "Other"))
# create month col
set(df, j= 'Month', value = floor_date(df$in_doa, 'month'))
set(df, j= 'Year', value = floor_date(df$in_doa, 'year'))

df <- df[Year >= '2021-01-01' & Year <= '2026-01-01', ]

df <- df[!(Country == 'Ethiopia' & Year == '2024-01-01'), ]

df <- df %>% 
  mutate(in_o2_lo = ifelse(in_o2_lo < 50 | in_o2_lo > 100, NA, in_o2_lo), 
         respdist2 = ifelse(respdist2 == "", NA, respdist2),
         in_rds = ifelse(in_rds == -3 | in_rds == -1 | in_rds == "", NA, in_rds)) %>% 
  mutate(
    Prophylactic_CPAP = ifelse(in_bwt >= 500 & in_bwt <= 1499, "Yes", "No"),
    Treatment_CPAP = ifelse(
      (Country %in% c('Malawi', 'Tanzania', 'Nigeria', 'Ethiopia') & 
         in_o2_lo < 90 & respdist2 == 1 & in_bwt >= 1500 & in_bwt <= 1999) |
        (Country == "Kenya" & in_bwt >= 1500 & in_bwt <= 1999 & in_rds == 1), 
      "Yes", "No"
    ),
    CPAP_eligible = ifelse(Prophylactic_CPAP == 'Yes' | Treatment_CPAP == 'Yes', 'Yes', 'No')
  )

df <- df[, `:=` (in_tob = gsub("[^0-9:]", "", in_tob),
                 in_toa = gsub("[^0-9:]", "", in_toa),
                 in_cp_sttm_1 = gsub("[^0-9:]", "", in_cp_sttm_1),
                 in_cp_entm_1 = gsub("[^0-9:]", "", in_cp_entm_1))]

# Clean time function --------------------------------------------------------

clean_time <- function(time_vec) {
  require(stringr)
  
  time_vec <- str_to_lower(time_vec)
  time_vec <- str_replace_all(time_vec, "\\s+", "")  # Remove spaces
  time_vec <- str_replace_all(time_vec, "(hours?|hrs?|hs|hr)\\b", "")  # Remove hour labels
  time_vec <- str_replace_all(time_vec, "::", ":")   # Replace double colons
  time_vec <- str_replace_all(time_vec, "^0{3,}", "")  # Remove leading zeros like "0000"
  
  # Fix single-digit minute: "12:6" → "12:06"
  time_vec <- str_replace_all(time_vec, "([0-9]{1,2}):([0-9])\\b", "\\1:0\\2")
  
  # Handle AM/PM format
  ampm_pattern <- "(am|pm)$"
  is_ampm <- str_detect(time_vec, ampm_pattern) & !is.na(time_vec)
  if (any(is_ampm)) {
    parsed_ampm <- strptime(time_vec[is_ampm], format = "%I:%M%p")
    time_vec[is_ampm] <- format(parsed_ampm, "%H:%M:%S")
  }
  
  # Append seconds if time is HH:MM
  time_vec <- ifelse(str_detect(time_vec, "^[0-9]{1,2}:[0-9]{2}$"),
                     paste0(time_vec, ":00"),
                     time_vec)
  
  # Convert HHMM or HMM to HH:MM:SS
  time_vec <- ifelse(str_detect(time_vec, "^[0-9]{3,4}$"),
                     paste0(str_sub(time_vec, 1, 2), ":", str_sub(time_vec, 3, 4), ":00"),
                     time_vec)
  
  # Keep only valid HH:MM:SS where HH < 24, MM/SS < 60
  time_vec <- ifelse(
    str_detect(time_vec, "^([0-1]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"),
    time_vec,
    NA
  )
  
  return(time_vec)
}

# standardize time function to have HH:MM:SS -----------------------------------
standardize_time <- function(time_vec) {
  # Only keep valid HH:MM:SS format
  valid_format <- str_detect(time_vec, "^\\d{1,2}:\\d{2}:\\d{2}$") & !is.na(time_vec)
  
  # Parse times, suppress warnings for invalid ones
  parsed_times <- suppressWarnings(strptime(time_vec[valid_format], format = "%H:%M:%S"))
  
  # Assign back formatted values for valid times
  time_vec[valid_format] <- ifelse(!is.na(parsed_times),
                                   format(parsed_times, "%H:%M:%S"), NA)
  return(time_vec)
}

# Cleaning and standardize time vars --------------------------------------------
df$in_tob <- clean_time(df$in_tob)
df$in_tob <- standardize_time(df$in_tob)

df$in_toa <- clean_time(df$in_toa)
df$in_toa <- standardize_time(df$in_toa)


# Apply to the specified columns
df <- df %>%
  mutate(
    in_cp_stdt_1 = as.Date(in_cp_stdt_1),
    in_cp_endt_1 = as.Date(in_cp_endt_1),
  ) %>% 
  mutate( #return to character for joining with respective dates
    in_tob = as.character(in_tob),
    in_toa = as.character(in_toa),
    in_cp_sttm_1 = as.character(in_cp_sttm_1),
    in_cp_entm_1 = as.character(in_cp_entm_1)
  )

library(dplyr)
library(lubridate)

clean_dates <- function(x) {
  today <- Sys.Date()
  
  x %>%
    mutate(
      # Convert IDate to Date first
      across(c(in_dob, in_doa, in_cp_stdt_1, in_cp_endt_1, in_dis_dod), as.Date),
      
      # 1. Any date before 2021-01-01 → NA
      across(c(in_dob, in_doa, in_cp_stdt_1, in_cp_endt_1, in_dis_dod),
             ~ if_else(. < as.Date("2021-01-01"), as.Date(NA), .)),
      
      # 2. Any date after today → NA
      across(c(in_dob, in_doa, in_cp_stdt_1, in_cp_endt_1, in_dis_dod),
             ~ if_else(. > today, as.Date(NA), .)),
      
      # 3. Admission date must be >= date of birth and <= discharge
      in_doa = if_else(in_doa < in_dob | (!is.na(in_dis_dod) & in_doa > in_dis_dod),
                       as.Date(NA), in_doa),
      
      # 4. CPAP start date must be >= admission, >= date of birth, and <= discharge
      in_cp_stdt_1 = if_else(in_cp_stdt_1 < in_doa | in_cp_stdt_1 < in_dob |
                               (!is.na(in_dis_dod) & in_cp_stdt_1 > in_dis_dod),
                             as.Date(NA), in_cp_stdt_1),
      
      # 5. CPAP end date must be >= CPAP start date and <= discharge
      in_cp_endt_1 = if_else(in_cp_endt_1 < in_cp_stdt_1 |
                               (!is.na(in_dis_dod) & in_cp_endt_1 > in_dis_dod),
                             as.Date(NA), in_cp_endt_1)
    )
}

df <- clean_dates(df)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# - Average annual admissions 
fac_adms <- df %>% 
  group_by(Country, in_facid, Year) %>% 
  summarise(Admissions = n(), .groups = 'drop') %>% 
  group_by(Country, in_facid) %>% 
  summarise(Admissions = round(mean(Admissions, na.rm = T), 0), .groups = 'drop')

# - Birth weight categories frequencies

fac_bwt_freq <- df %>%
  group_by(Country, in_facid, birthweight_cat) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = 100 * n / sum(n))

# - Facility levels/types

fac_levels <- haven::read_dta("C:/Users/phomw/OneDrive - Rice University/OneDrive/Documents/Rice360/Student Projects/Meghan Paral/Data/Outputs/Facility levels_31_May_2024 (3).dta") %>% 
  select(in_facid = "id_recidfac",
         fac_name = "inf_id_facid",
         Country = "inf_id_ctry",
         facility_type = "inf_facid_typ")

# Levels of care from facility types 

fac_levels <- fac_levels %>%
  mutate(
    facility_type = str_to_lower(facility_type),
    
    neo_level = case_when(
      # ---------------- MALAWI ----------------
      Country == "Malawi" &
        str_detect(facility_type, "cham|mission|community") ~ "Level II Basic",
      
      Country == "Malawi" &
        str_detect(facility_type, "district") ~ "Level II Comprehensive",
      
      Country == "Malawi" &
        str_detect(facility_type, "regional|national") ~ "Level III",
      # ---------------- KENYA ----------------
      Country == "Kenya" &
        str_detect(facility_type, "maternity|regional") ~ "Level II Comprehensive",
      
      Country == "Kenya" &
        str_detect(facility_type, "national") ~ "Level III",
      
      # ---------------- TANZANIA ----------------
      Country == "Tanzania" &
        str_detect(facility_type, "regional") ~ "Level II Comprehensive",
      
      Country == "Tanzania" &
        str_detect(facility_type, "zonal|national") ~ "Level III",
      
      # ---------------- NIGERIA ----------------
      Country == "Nigeria" &
        str_detect(facility_type, "secondary") ~ "Level II Comprehensive",
      
      Country == "Nigeria" &
        str_detect(facility_type, "tertiary") ~ "Level III",
      # ---------------- DEFAULT ----------------
      TRUE ~ NA_character_
    )
  )

et_fac_levels <- df %>% 
  filter(Country == 'Ethiopia') %>% 
  select(Country, in_facid) %>% 
  distinct() %>% 
  mutate(neo_level = case_when(
    in_facid %in% c(512, 520, 515, 516, 508, 517, 522, 503) ~ "Level II Basic",
    in_facid %in% c(518, 521, 511, 505, 519) ~ "Level II Comprehensive",
    in_facid %in% c(509, 504, 501) ~ "Level III",
    TRUE ~ NA_character_),
    facility_type = "",
    fac_name = ""
  )

fac_levels <- bind_rows(fac_levels, et_fac_levels)

# ============================================================================
# CPAP ELIGIBILITY + CPAP DONE PER FACILITY (MONTHLY, THEN AGGREGATED)
# ============================================================================

# STEP 1: Facility x weight x month summary (unchanged)
cpap_monthly <- df %>%
  filter(!is.na(birthweight_cat)) %>%
  group_by(Country, in_facid, birthweight_cat, Month) %>%
  summarise(
    Admissions = n(),
    Eligible_criteria = sum(CPAP_eligible == "Yes", na.rm = TRUE),
    cpaps_done = sum(in_cp_admin == 1, na.rm = TRUE),
    .groups = "drop"
  )

# STEP 2: Aggregate to facility level (unchanged structure)
cpap_estimate <- cpap_monthly %>%
  group_by(Country, in_facid, birthweight_cat) %>%
  summarise(
    mean_monthly_admissions = mean(Admissions, na.rm = TRUE),
    mean_monthly_eligible_criteria = mean(Eligible_criteria, na.rm = TRUE),
    mean_monthly_cpaps_done = mean(cpaps_done, na.rm = TRUE),
    total_admissions   = sum(Admissions, na.rm = TRUE),
    total_eligible_criteria = sum(Eligible_criteria, na.rm = TRUE),
    total_cpaps_done   = sum(cpaps_done, na.rm = TRUE),
    eligible_percent_criteria = total_eligible_criteria / total_admissions * 100,
    coverage_percent_criteria = if_else(
      total_eligible_criteria > 0,
      total_cpaps_done / total_eligible_criteria * 100,
      NA_real_
    ),
    .groups = "drop"
  )

# ============================================================================
# STEP 3: DERIVE EMPIRICAL RATE FOR 1500-1999g FROM HIGH-COVERAGE FACILITIES
# (Same logic already used to justify 2000-2499g / >=2500g expert rates —
#  applied here as a DATA-DERIVED rate instead of a fixed expert guess,
#  since 1500-1999g criteria are debated but we do have volume of data)
# ============================================================================

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

# ============================================================================
# STEP 4: HYBRID ELIGIBILITY — CRITERIA (<1500g), EMPIRICAL/EXPERT (>=1500g)
# ============================================================================

cpap_estimate <- cpap_estimate %>%
  mutate(
    # rate to apply per weight band
    applied_rate = case_when(
      birthweight_cat %in% c("<1000", "1000-1499") ~ NA_real_,  # criteria only, no override
      birthweight_cat == "1500-1999"  ~ empirical_rate_1500_1999,
      birthweight_cat == "2000-2499"  ~ 0.08,
      birthweight_cat %in% c("2500-4000", "4001+") ~ 0.05,
      TRUE ~ NA_real_
    ),
    
    # initial eligible estimate: criteria for <1500g; 
    # for 1500g+, take the HIGHER of criteria-based vs. rate-based
    # (protects against undercounting when criteria are too conservative)
    eligible_initial = case_when(
      birthweight_cat %in% c("<1000", "1000-1499") ~ total_eligible_criteria,
      !is.na(applied_rate) ~ pmax(total_eligible_criteria, 
                                  total_admissions * applied_rate, 
                                  na.rm = TRUE),
      TRUE ~ total_eligible_criteria
    ),
    
    # ────────────────────────────────────────────────────────────────
    # CORE HYBRID RULE (as discussed): if a baby got CPAP, they were
    # eligible — regardless of what criteria/rate say. Guarantees
    # coverage <= 100% by construction, no separate capping needed.
    # ────────────────────────────────────────────────────────────────
    eligible_final = pmax(eligible_initial, total_cpaps_done, na.rm = TRUE),
    
    eligible_percent = (eligible_final / total_admissions) * 100,
    coverage_percent = (total_cpaps_done / eligible_final) * 100,
    
    mean_monthly_eligible = eligible_final / n_distinct(Country) * 0,  # placeholder removed below
  ) %>%
  select(-mean_monthly_eligible)

# Recompute mean_monthly_eligible properly (needs months_data count)
months_per_facility <- cpap_monthly %>%
  group_by(Country, in_facid, birthweight_cat) %>%
  summarise(months_data = n_distinct(Month), .groups = "drop")

cpap_estimate <- cpap_estimate %>%
  left_join(months_per_facility, by = c("Country", "in_facid", "birthweight_cat")) %>%
  mutate(mean_monthly_eligible = eligible_final / months_data) %>%
  forestmangr::round_df(1)

# ============================================================================
# VERIFICATION
# ============================================================================

cat(sprintf("\nMax coverage (should be <=100): %.1f%%\n", 
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


# THE LOGIC 
# <1500g: Eligibility is criteria-only, because the clinical indication (prematurity/extreme low birth weight) 
# is well-established and near-universal — there's little controversy here, so no hybrid adjustment is needed.
# 1500-1999g: Eligibility is the higher of (a) documented clinical criteria, 
# (b) an empirically observed rate from facilities with healthy CPAP coverage (60-85%,
# i.e., neither supply-starved nor artificially saturated), and (c) actual CPAP given.
# # ≥2000g: Eligibility is the higher of (a) a fixed expert-opinion rate (8% / 5%) 
# informed by clinical literature, and (b) actual CPAP given.
# All bands: Eligibility can never be lower than what was actually delivered 
# — eligible_final = max(estimate, cpaps_done)


