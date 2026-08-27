################################################################################
# 01_facility_metadata_levels.R
# CPAP DEVICE ESTIMATION TOOL
#
# Purpose: Build facility-level metadata, including WHO-aligned level of
#          care classification, and average annual admissions per facility.
#
# Requires: df (from 00_setup_and_data_cleaning.R)
# Produces: fac_levels, fac_adms
#
# Run order: 2nd
#
# ------------------------------------------------------------------------------
# LEVEL OF CARE CLASSIFICATION (WHO-aligned)
# ------------------------------------------------------------------------------
# Level II Basic:         Small district, mission, CHAM hospitals.
#                          WHO Level II (special care) with limited resources:
#                          CPAP, oxygen, IV fluids, antibiotics, phototherapy,
#                          KMC. Basic diagnostics only, no mechanical ventilation.
#
# Level II Comprehensive: Large district, regional referral hospitals.
#                          WHO Level II (special care) with comprehensive
#                          resources: all Level II Basic services plus mobile
#                          X-ray, blood transfusion, advanced diagnostics.
#
# Level III:               Tertiary, zonal, national referral hospitals.
#                          WHO Level III (intensive care): mechanical
#                          ventilation, surfactant therapy, surgery, TPN,
#                          ROP screening, subspecialty services.
#
# Reference: WHO (2020). Standards for improving the quality of care for
# small and sick newborns in health facilities. pp. 59-60 (Fig 3.2).
################################################################################

library(tidyverse)
library(haven)

# ------------------------------------------------------------------------------
# 1. FACILITY LEVELS — MALAWI, KENYA, TANZANIA, NIGERIA
# ------------------------------------------------------------------------------

fac_levels <- read_dta("C:/Users/phomw/OneDrive - Rice University/OneDrive/Documents/Rice360/Student Projects/Meghan Paral/Data/Outputs/Facility levels_31_May_2024 (3).dta") %>%
  select(in_facid = id_recidfac, fac_name = inf_id_facid,
         country = inf_id_ctry, facility_type = inf_facid_typ) %>%
  mutate(
    facility_type = str_to_lower(facility_type),
    neo_level = case_when(
      country == "Malawi"   & str_detect(facility_type, "cham|mission|community") ~ "Level II Basic",
      country == "Malawi"   & str_detect(facility_type, "district") ~ "Level II Comprehensive",
      country == "Malawi"   & str_detect(facility_type, "regional|national") ~ "Level III",

      country == "Kenya"    & str_detect(facility_type, "maternity|regional") ~ "Level II Comprehensive",
      country == "Kenya"    & str_detect(facility_type, "national") ~ "Level III",

      country == "Tanzania" & str_detect(facility_type, "regional") ~ "Level II Comprehensive",
      country == "Tanzania" & str_detect(facility_type, "zonal|national") ~ "Level III",

      country == "Nigeria"  & str_detect(facility_type, "secondary") ~ "Level II Comprehensive",
      country == "Nigeria"  & str_detect(facility_type, "tertiary") ~ "Level III",

      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------------------------
# 2. FACILITY LEVELS — ETHIOPIA (manually assigned; not in facility levels file)
# ------------------------------------------------------------------------------

et_fac_levels <- df %>%
  filter(country == "Ethiopia") %>%
  distinct(country, in_facid) %>%
  mutate(
    neo_level = case_when(
      in_facid %in% c(512, 520, 515, 516, 508, 517, 522, 503) ~ "Level II Basic",
      in_facid %in% c(518, 521, 511, 505, 519) ~ "Level II Comprehensive",
      in_facid %in% c(509, 504, 501) ~ "Level III"
    ),
    facility_type = "",
    fac_name = ""
  )

fac_levels <- bind_rows(fac_levels, et_fac_levels)

# ------------------------------------------------------------------------------
# 3. AVERAGE ANNUAL ADMISSIONS PER FACILITY
# ------------------------------------------------------------------------------

fac_adms <- df %>%
  group_by(country, in_facid, Year) %>%
  summarise(Admissions = n(), .groups = "drop") %>%
  group_by(country, in_facid) %>%
  summarise(Admissions = round(mean(Admissions, na.rm = TRUE), 0), .groups = "drop")

# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------
cat(sprintf("01_facility_metadata_levels.R complete. fac_levels: %d facilities (%d with level assigned).\n",
            nrow(fac_levels), sum(!is.na(fac_levels$neo_level))))
