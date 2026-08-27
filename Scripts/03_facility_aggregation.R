################################################################################
# 03_facility_aggregation.R
# CPAP DEVICE ESTIMATION TOOL
#
# Purpose: Aggregate weight-band eligibility up to one row per facility,
#          including case-mix (% admissions per weight band) and overall
#          coverage. This is the table used both for eligibility calibration
#          (04) and for blending LOS by level of care (07).
#
# Requires: cpap_estimate (from 02_cpap_eligibility.R), fac_levels (from 01)
# Produces: cpap_facility_summary
#
# Run order: 4th
#
# ------------------------------------------------------------------------------
# IMPLEMENTATION NOTE — pivot_wider, not bracket-indexing
# ------------------------------------------------------------------------------
# Case-mix totals (admits_elbw, admits_vlbw, etc.) are built via pivot_wider,
# NOT via bracket-indexing inside summarise() (e.g.
# sum(total_admissions[birthweight_cat == "<1000"], na.rm = TRUE)).
#
# The bracket-indexing approach is vulnerable to silent row-misalignment if
# the join to fac_levels ever fans out (e.g. a duplicate in_facid). This was
# confirmed empirically: an earlier version of this script using bracket-
# indexing produced an impossible 100%/0% case-mix split (every facility
# showing 100% of admissions in exactly one weight band) once additional
# facilities were added to the pipeline. pivot_wider is immune to this
# failure mode because it never depends on row order within a group.
################################################################################

library(tidyverse)

# ------------------------------------------------------------------------------
# 1. DEFENSIVE DEDUPLICATION
# ------------------------------------------------------------------------------
# Guards against a facility appearing more than once in fac_levels (would
# otherwise fan out the join below and corrupt totals).

fac_levels <- fac_levels %>% distinct(in_facid, .keep_all = TRUE)

# ------------------------------------------------------------------------------
# 2. CASE-MIX TOTALS (WIDE FORMAT, ROBUST)
# ------------------------------------------------------------------------------

casemix_wide <- cpap_estimate %>%
  filter(!is.na(birthweight_cat)) %>%
  select(country, in_facid, birthweight_cat, total_admissions) %>%
  pivot_wider(
    names_from = birthweight_cat,
    values_from = total_admissions,
    values_fill = 0,
    names_prefix = "admits_"
  )

# ------------------------------------------------------------------------------
# 3. FACILITY-LEVEL SUMMARY
# ------------------------------------------------------------------------------

cpap_facility_summary <- cpap_estimate %>%
  group_by(country, in_facid) %>%
  summarise(
    total_admissions = sum(total_admissions, na.rm = TRUE),
    total_eligible    = sum(eligible_final, na.rm = TRUE),
    eligible_percent_overall = (total_eligible / total_admissions) * 100,
    coverage_weighted = weighted.mean(coverage_percent, w = eligible_final, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(casemix_wide, by = c("country", "in_facid")) %>%
  left_join(
    fac_levels %>% select(in_facid, fac_name, facility_type, neo_level),
    by = "in_facid"
  ) %>%
  mutate(
    pct_elbw      = `admits_<1000` / total_admissions * 100,
    pct_vlbw      = `admits_1000-1499` / total_admissions * 100,
    pct_lbw_lower = `admits_1500-1999` / total_admissions * 100,
    pct_lbw_upper = `admits_2000-2499` / total_admissions * 100,
    pct_normal    = `admits_2500-4000` / total_admissions * 100,
    pct_large     = `admits_4001+` / total_admissions * 100
  )

# ------------------------------------------------------------------------------
# DONE
# ------------------------------------------------------------------------------
cat(sprintf("03_facility_aggregation.R complete. cpap_facility_summary: %d facilities.\n",
            nrow(cpap_facility_summary)))
