################################################################################
# 06_surge_maintenance_params.R
# CPAP DEVICE ESTIMATION TOOL
#
# Purpose: Set surge factor and maintenance buffer parameters.
#
# Requires: (device_data.csv, read directly in this script)
# Produces: surge_params, equipment_params
#
# Run order: 7th
#
# ------------------------------------------------------------------------------
# SURGE FACTOR
# ------------------------------------------------------------------------------
# Fixed at 2.0 (expert opinion), applied flat across all levels of care.
# Accounts for peak vs. average concurrent demand -- i.e. a facility must
# have capacity for roughly double its average daily concurrent CPAP need
# to handle admission surges without running out of devices.
#
# ------------------------------------------------------------------------------
# MAINTENANCE BUFFER
# ------------------------------------------------------------------------------
# Derived from equipment audit data: average proportion of CPAP devices
# non-functional at any given time (repairs, awaiting parts, calibration).
# This is a facility OPERATIONAL parameter, not patient-specific, so it is
# applied flat across all levels of care -- confirmed appropriate.
################################################################################

library(tidyverse)
library(data.table)

# ------------------------------------------------------------------------------
# 1. SURGE FACTOR (expert opinion, flat across levels)
# ------------------------------------------------------------------------------

surge_params <- list(
  surge_median = 2.0,
  surge_se     = 0.0
)

# ------------------------------------------------------------------------------
# 2. MAINTENANCE BUFFER (from equipment audit data, flat across levels)
# ------------------------------------------------------------------------------

device_data <- fread("C:/path/to/device_data.csv")

equipment_params <- device_data %>%
  mutate(
    functional = md_eqt_cp_cp_fc,
    total      = md_eqt_cp_cp_avl
  ) %>%
  summarise(
    functional_rate     = mean(functional / total, na.rm = TRUE),
    maintenance_buffer   = 1 - functional_rate
  )

cat(sprintf("Surge factor: %.1f (expert opinion, flat across levels)\n",
            surge_params$surge_median))
cat(sprintf("Maintenance buffer: %.1f%% (flat across levels)\n",
            equipment_params$maintenance_buffer * 100))

# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------
cat("\n06_surge_maintenance_params.R complete.\n")
