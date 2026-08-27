# CPAP Device Estimation Tool — R Scripts

Organized, numbered pipeline reflecting the current validated methodology.
Run in numeric order (or use `00_run_all.R`) from a single R session — each
script depends on objects created by the ones before it.

## Files

| File | Purpose | Key outputs |
|---|---|---|
| `00_run_all.R` | Master runner — sources all scripts in order | — |
| `00_setup_and_data_cleaning.R` | Load raw NID data; clean weights, dates, birthweight categories; date plausibility checks; CPAP eligibility criteria; CPAP start/end datetimes | `df` |
| `01_facility_metadata_levels.R` | WHO-aligned facility level classification (Level II Basic / Level II Comprehensive / Level III); average annual admissions | `fac_levels`, `fac_adms` |
| `02_cpap_eligibility.R` | Hybrid eligibility logic per facility × weight band (criteria / empirical / expert rate, capped at actual receipt) | `cpap_estimate` |
| `03_facility_aggregation.R` | Aggregate to one row per facility: totals, case-mix %, coverage (via `pivot_wider`, not bracket-indexing) | `cpap_facility_summary` |
| `04_eligibility_calibration_by_level.R` | Calibrate eligibility rate (%) per level of care, using coverage-banded facility selection | `eligibility_by_level` |
| `05_cpap_los_rmtl.R` | Competing-risks RMTL — length of CPAP treatment per weight band, correcting for death censoring and undocumented-wean censoring, with facility-clustered bootstrap CIs | `cpap_duration`, `rmtl_by_band` |
| `06_surge_maintenance_params.R` | Surge factor (expert opinion, flat) and maintenance buffer (from equipment audit data, flat) | `surge_params`, `equipment_params` |
| `07_los_blend_by_level.R` | Typical case-mix (%) per level, blended with `rmtl_by_band` into level-specific LOS | `typical_casemix_by_level`, `blend_los_by_level` |
| `08_final_parameters_and_calculator.R` | Assembles all parameters; defines `estimate_cpap_devices()`; runs test cases; saves parameters to `.rds` | `final_params`, `estimate_cpap_devices()` |

## Before running

Update the file paths in:
- `00_setup_and_data_cleaning.R` (patient NID data)
- `01_facility_metadata_levels.R` (facility levels .dta file)
- `06_surge_maintenance_params.R` (device/equipment data)

## Methodology notes

- **Eligibility**: hybrid logic — clinical criteria for <1500g; criteria vs.
  empirical rate (60–85% coverage facilities) for 1500–1999g; expert rate
  (8%/5%) vs. actual receipt for ≥2000g. All bands capped so
  `eligible_final = max(estimate, cpaps_done)`, guaranteeing coverage ≤100%.
- **Eligibility calibration**: facilities selected by coverage window
  (40–85% for Level II Basic/Comprehensive, 50–85% for Level III) to
  exclude both supply-constrained facilities (<40/50%) and facilities where
  the hybrid rule mechanically inflates eligibility (>85%).
- **LOS**: competing-risks Restricted Mean Time Lost (RMTL), not naive
  mean/median duration — properly separates "died on CPAP" from
  "successfully weaned," and treats "discharged alive with no CPAP end
  date" as censored rather than assuming the wean date equals the
  discharge date. Facility-clustered bootstrap for SE/95% CI.
- **LOS blending**: the calculator keeps a simple input (monthly admissions
  + level of care only). LOS is band-specific under the hood, blended into
  a single level parameter using each level's *typical* case-mix — not a
  per-facility case-mix input (Option B design decision).
- **Surge factor**: 2.0, expert opinion, flat across all levels.
- **Maintenance buffer**: from equipment audit data, flat across all levels
  (facility operational parameter, not patient-specific).
- **Case-mix regression** (antenatal corticosteroids, hypothermia,
  infection, country) was explored as a secondary/exploratory analysis but
  is **not included** in this pipeline — the calculator uses population-level
  (marginal) LOS and eligibility estimates, which already reflect the
  case-mix present in the underlying data.
- **Levels of care**: WHO-aligned — Level II Basic and Level II
  Comprehensive both correspond to WHO Level II (special care), split by
  facility size/resource scope; Level III corresponds to WHO Level III
  (intensive care). See WHO (2020) *Standards for improving the quality of
  care for small and sick newborns in health facilities*, pp. 59–60.

## Known pitfalls fixed in this version

- **Date plausibility checks (`clean_dates()`) must run in Section 00**,
  before CPAP start/end datetimes are constructed. Omitting this step
  silently understates censoring in the LOS analysis (confirmed: censored
  episode rate dropped from ~8.8% to ~0.2–1.5%, and RMTL shifted down by
  ~1 day across every weight band, when this step was accidentally
  dropped during a script consolidation).
- **Case-mix aggregation must use `pivot_wider`, not bracket-indexing**
  inside `summarise()` (e.g. `sum(x[condition], na.rm=TRUE)`). The
  bracket-indexing form is vulnerable to silent row-misalignment if a
  join fans out (e.g. a duplicate facility ID) — confirmed to produce an
  impossible 100%/0% case-mix split under exactly that condition.
  `03_facility_aggregation.R` also defensively deduplicates `fac_levels`
  by `in_facid` before joining, as a second layer of protection.
