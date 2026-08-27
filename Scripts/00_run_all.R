################################################################################
# 00_run_all.R
# CPAP DEVICE ESTIMATION TOOL — MASTER RUNNER
#
# Runs all scripts in sequence. Each script depends on objects created by
# the ones before it, all within a single R session (no objects are saved
# to disk between scripts) -- run this file top to bottom, or source()
# each numbered file in order from a fresh R session.
#
# Before running: update the file paths inside 00, 01, and 06 to point to
# your local copies of the raw data files.
################################################################################

# Clean workspace, console, and plots

rm(list = ls(), envir = .GlobalEnv);
cat("\014");
graphics.off()


script_dir <- paste0(getwd(), "/Scripts/") 

data_dir <- "C:/Users/phomw/OneDrive - Rice University/OneDrive/Documents/Rice360/Datasets/Patient/"

source(file.path(script_dir, "00_setup_and_data_cleaning.R"))
source(file.path(script_dir, "01_facility_metadata_levels.R"))
source(file.path(script_dir, "02_cpap_eligibility.R"))
source(file.path(script_dir, "03_facility_aggregation.R"))
source(file.path(script_dir, "04_eligibility_calibration_by_level.R"))
source(file.path(script_dir, "05_cpap_los_rmtl.R"))
source(file.path(script_dir, "06_surge_maintenance_params.R"))
source(file.path(script_dir, "07_los_blend_by_level.R"))
source(file.path(script_dir, "08_final_parameters_and_calculator.R"))

cat("\n================================================================\n")
cat("ALL SCRIPTS COMPLETE.\n")
cat("Key outputs in memory:\n")
cat("  eligibility_by_level   - eligibility rate by level of care\n")
cat("  rmtl_by_band           - validated LOS (RMTL) by weight band\n")
cat("  blend_los_by_level     - LOS blended by level's typical case-mix\n")
cat("  surge_params           - surge factor (flat, expert opinion)\n")
cat("  equipment_params       - maintenance buffer (flat, from audit data)\n")
cat("  final_params           - complete assembled parameter set\n")
cat("  estimate_cpap_devices()- the calculator function\n")
cat("================================================================\n")
