################################################################################
# 00_setup_and_data_cleaning.R
# CPAP DEVICE ESTIMATION TOOL
#
# Purpose: Load raw NID patient data, clean weights/dates/times, derive
#          birthweight categories, CPAP eligibility criteria, and CPAP
#          start/end datetimes.
#
# Produces: df  (cleaned patient-level data.table, used by all later scripts)
#
# Run order: 1st
################################################################################

library(tidyverse)
library(data.table)
library(lubridate)

# ------------------------------------------------------------------------------
# 1. LOAD RAW DATA
# ------------------------------------------------------------------------------

df <- fread(paste0(data_dir, "overall_nid_for_nestit.csv"))%>%
  filter(in_rec_typ == 2, !is.na(in_facid))

df[, in_facid := as.integer(in_facid)]

# ------------------------------------------------------------------------------
# 2. country & PHASE ASSIGNMENT
# ------------------------------------------------------------------------------

df[, Phase := fifelse(
  in_facid %in% c(100:136, 217:276, 301:307, 401:411, 501:530),
  "Phase I", "Phase II")]

# ------------------------------------------------------------------------------
# 3. BIRTH WEIGHT CLEANING & CATEGORIZATION
# ------------------------------------------------------------------------------

df[, in_bwt := fifelse(in_bwt < 0 | is.na(in_bwt), NA_real_, in_bwt)]
df[, in_bwt := fifelse(in_bwt <= 10 & in_bwt >= 0, in_bwt * 1000, in_bwt)]  # kg -> g typo fix
df[, in_bwt := fifelse(in_bwt < 400 | in_bwt > 6000, NA_real_, in_bwt)]     # implausible values

df[, birthweight_cat := fcase(
  in_bwt >= 400  & in_bwt <= 999,  1,
  in_bwt >= 1000 & in_bwt <= 1499, 2,
  in_bwt >= 1500 & in_bwt <= 1999, 3,
  in_bwt >= 2000 & in_bwt <= 2499, 4,
  in_bwt >= 2500 & in_bwt <= 4000, 5,
  in_bwt >  4000 & in_bwt <= 6000, 6)]

df[, birthweight_cat := factor(birthweight_cat, levels = 1:6,
    labels = c("<1000", "1000-1499", "1500-1999", "2000-2499", "2500-4000", "4001+"))]

# ------------------------------------------------------------------------------
# 4. DATE CLEANING & DERIVED FIELDS
# ------------------------------------------------------------------------------

df[, `:=`(
  in_dob     = as.IDate(in_dob),
  in_doa     = as.IDate(in_doa),
  in_dis_dod = as.IDate(in_dis_dod)
)]

df[, LOS := as.numeric(in_dis_dod - in_dob)]
df[, LOS := fifelse(LOS < 0 | LOS > 100, NA_real_, LOS)]

df[, `:=`(Month = floor_date(in_doa, "month"), Year = floor_date(in_doa, "year"))]

# Restrict to analytic window; drop Ethiopia 2024 (no records in period)
df <- df[Year >= "2021-01-01" & Year <= "2026-01-01"]
df <- df[!(country == "Ethiopia" & Year == "2024-01-01")]

# ------------------------------------------------------------------------------
# 5. DATE PLAUSIBILITY CHECKS (CRITICAL — do not skip)
# ------------------------------------------------------------------------------
# Nulls out dates that fail logical ordering/plausibility checks:
#   - before 2021-01-01 or after today -> NA
#   - admission before birth, or after discharge -> NA
#   - CPAP start before admission, before birth, or after discharge -> NA
#   - CPAP end before CPAP start, or after discharge -> NA
#
# IMPORTANT: this must run on the raw Date-only fields (in_cp_stdt_1,
# in_cp_endt_1), BEFORE they are combined with time-of-day into datetime
# fields in Section 7 below. Without this step, implausible in_cp_endt_1
# values are incorrectly retained as valid, which understates true
# censoring in 05_cpap_los_rmtl.R and biases LOS/RMTL estimates downward
# (confirmed empirically: omitting this step dropped the censored-episode
# rate from ~8.8% to ~0.2-1.5%, and shifted RMTL down by roughly 1 day
# across every weight band).

clean_dates <- function(x) {
  today <- Sys.Date()
  x %>%
    mutate(
      across(c(in_dob, in_doa, in_cp_stdt_1, in_cp_endt_1, in_dis_dod), as.Date),

      across(c(in_dob, in_doa, in_cp_stdt_1, in_cp_endt_1, in_dis_dod),
             ~ if_else(. < as.Date("2021-01-01"), as.Date(NA), .)),

      across(c(in_dob, in_doa, in_cp_stdt_1, in_cp_endt_1, in_dis_dod),
             ~ if_else(. > today, as.Date(NA), .)),

      in_doa = if_else(in_doa < in_dob | (!is.na(in_dis_dod) & in_doa > in_dis_dod),
                        as.Date(NA), in_doa),

      in_cp_stdt_1 = if_else(in_cp_stdt_1 < in_doa | in_cp_stdt_1 < in_dob |
                               (!is.na(in_dis_dod) & in_cp_stdt_1 > in_dis_dod),
                             as.Date(NA), in_cp_stdt_1),

      in_cp_endt_1 = if_else(in_cp_endt_1 < in_cp_stdt_1 |
                               (!is.na(in_dis_dod) & in_cp_endt_1 > in_dis_dod),
                             as.Date(NA), in_cp_endt_1)
    )
}

df <- clean_dates(df)

# ------------------------------------------------------------------------------
# 6. CPAP ELIGIBILITY CRITERIA (CLINICAL, <2000g)
# ------------------------------------------------------------------------------
# Prophylactic: 500-1499g (near-universal, well-established indication)
# Treatment: 1500-1999g, with documented respiratory distress / RDS
#   - Malawi, Tanzania, Nigeria, Ethiopia: SpO2 <90 + respdist2 == 1
#   - Kenya: RDS diagnosis (in_rds == 1)
# NOTE: this criteria-based flag is only the STARTING point for eligibility.
#       See 02_cpap_eligibility.R for the full hybrid logic (criteria vs.
#       empirical/expert rate vs. actual receipt) applied on top of this.

df <- df %>%
  mutate(
    in_o2_lo  = ifelse(in_o2_lo < 50 | in_o2_lo > 100, NA, in_o2_lo),
    respdist2 = ifelse(respdist2 == "", NA, respdist2),
    in_rds    = ifelse(in_rds %in% c(-3, -1, ""), NA, in_rds),

    Prophylactic_CPAP = ifelse(in_bwt >= 500 & in_bwt <= 1499, "Yes", "No"),

    Treatment_CPAP = ifelse(
      (country %in% c("Malawi", "Tanzania", "Nigeria", "Ethiopia") &
         in_o2_lo < 90 & respdist2 == 1 & in_bwt >= 1500 & in_bwt <= 1999) |
      (country == "Kenya" & in_bwt >= 1500 & in_bwt <= 1999 & in_rds == 1),
      "Yes", "No"
    ),

    CPAP_eligible = ifelse(Prophylactic_CPAP == "Yes" | Treatment_CPAP == "Yes", "Yes", "No")
  )

# ------------------------------------------------------------------------------
# 7. CPAP START/END DATETIME CONSTRUCTION (needed for LOS / RMTL analysis)
# ------------------------------------------------------------------------------
# Uses the PLAUSIBILITY-CHECKED in_cp_stdt_1 / in_cp_endt_1 from Section 5.

clean_time <- function(t) {
  t <- str_trim(as.character(t))
  if_else(t %in% c("", "NA", "-3", "-1"), NA_character_, t)
}

df <- df %>%
  mutate(
    in_tob       = clean_time(in_tob),
    in_toa       = clean_time(in_toa),
    in_cp_sttm_1 = clean_time(in_cp_sttm_1),
    in_cp_entm_1 = clean_time(in_cp_entm_1),

    cpap_start = ymd_hms(paste(in_cp_stdt_1, in_cp_sttm_1), quiet = TRUE),
    cpap_end   = ymd_hms(paste(in_cp_endt_1, in_cp_entm_1), quiet = TRUE),

    # Fallback to date-only (no time-of-day recorded) rather than dropping entirely
    cpap_start = if_else(is.na(cpap_start) & !is.na(in_cp_stdt_1),
                          as_datetime(in_cp_stdt_1), cpap_start),
    cpap_end   = if_else(is.na(cpap_end) & !is.na(in_cp_endt_1),
                          as_datetime(in_cp_endt_1), cpap_end)
  )

# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------
cat(sprintf("00_setup_and_data_cleaning.R complete. df: %d rows, %d facilities.\n",
            nrow(df), n_distinct(df$in_facid)))
