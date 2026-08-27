# Extract parameters with SE for 95% CI

# 1. Eligibility by level (with SE)
elig_params <- cpap_estimate %>%
  left_join(fac_levels, by = "in_facid") %>%
  filter(coverage_percent >= 85, coverage_percent <= 95, !is.na(neo_level)) %>%
  group_by(neo_level) %>%
  summarise(
    n_facilities = n(),
    eligible_rate = weighted.mean(eligible_percent, total_admissions, na.rm = TRUE) / 100,
    sd_eligible = sd(eligible_percent, na.rm = TRUE) / 100,
    se_eligible = sd_eligible / sqrt(n_facilities)
  )

print("ELIGIBILITY BY LEVEL:")
print(elig_params)

`# 2. LOS (with SE)
los_params <- operational_params %>%
  filter(!is.na(birthweight_cat)) %>%
  summarise(
    n = n(),
    avg_los = median(median_los, na.rm = TRUE),
    sd_los = sd(median_los, na.rm = TRUE),
    se_los = sd_los / sqrt(n)
  )

print("LOS PARAMETERS:")
print(los_params)

# 3. Surge factor (with SE)
surge_params <- operational_params %>%
  filter(!is.na(birthweight_cat), surge_ratio < 3) %>%
  summarise(
    n = n(),
    surge = median(surge_ratio, na.rm = TRUE),
    sd_surge = sd(surge_ratio, na.rm = TRUE),
    se_surge = sd_surge / sqrt(n)
  )

print("SURGE PARAMETERS:")
print(surge_params)

# 4. Maintenance buffer
maint_params <- equipment_params %>%
  select(redundancy_factor)

print("MAINTENANCE BUFFER:")
print(maint_params)

# Save for Excel
params_for_excel <- list(
  eligibility = elig_params,
  los = los_params,
  surge = surge_params,
  maintenance = maint_params$redundancy_factor
)

saveRDS(params_for_excel, "cpap_calculator_parameters.rds")
write.csv(elig_params, "eligibility_by_level.csv", row.names = FALSE)
```

**Run this code first and paste the output here so I can see the exact values.**
  
  ---
  
  ## **STEP 2: Excel calculator structure**
  
  Once I have the parameter values, I'll create an Excel file with this layout:

### **Sheet 1: CPAP Calculator**
```
┌─────────────────────────────────────────────────────────┐
│  NEONATAL CPAP DEVICE ESTIMATION TOOL                  │
└─────────────────────────────────────────────────────────┘

USER INPUTS:
┌─────────────────────────────────────────────────────────┐
│ Monthly Admissions:        [150]           (enter here) │
│ Level of Care:             [Level II ▼]    (dropdown)   │
└─────────────────────────────────────────────────────────┘

PARAMETERS (editable - change if needed):
┌─────────────────────────────────────────────────────────┐
│                        Level I  Level II  Level III     │
│ Eligibility Rate (%)     17%      15%       22%         │
│ Average LOS (days)        5        5         5          │
│ Surge Factor             2.0      2.0       2.0         │
│ Maintenance Buffer (%)   3.4%     3.4%      3.4%        │
└─────────────────────────────────────────────────────────┘

CALCULATION BREAKDOWN:
┌─────────────────────────────────────────────────────────┐
│ Daily concurrent demand    = 3.75                       │
│ Peak demand (with surge)   = 7.50                       │
│ With maintenance buffer    = 7.76                       │
└─────────────────────────────────────────────────────────┘

RESULT:
┌─────────────────────────────────────────────────────────┐
│                                                          │
│   CPAP DEVICES NEEDED:  8 devices                       │
│                                                          │
│   95% Confidence Interval:  6 to 10 devices             │
│                                                          │
└─────────────────────────────────────────────────────────┘

Sheet 2: Parameters & Formulas (for verification)
```

---

## **FORMULAS (I'll document these exactly):**
  
  ### **Cell locations:**
  - B5: Monthly admissions (user input)
- B6: Level of care (dropdown: Level I, Level II, Level III)
- B10:D10: Eligibility rates by level (user can edit)
- B11:D11: LOS by level (user can edit)
- B12:D12: Surge by level (user can edit)
- B13:D13: Maintenance by level (user can edit)

### **Calculation cells:**
```
Daily concurrent (B17):
  = (B5 * VLOOKUP(B6, A10:D13, ROW()-8, FALSE) * VLOOKUP(B6, A10:D13, ROW()-6, FALSE)) / 30

Peak demand (B18):
  = B17 * VLOOKUP(B6, A10:D13, 3, FALSE)

With maintenance (B19):
  = B18 * (1 + VLOOKUP(B6, A10:D13, 4, FALSE))

CPAP needed (B23):
  = ROUNDUP(B19, 0)

95% CI Lower (B24):
  = ROUNDUP(B23 - 1.96 * SE, 0)

95% CI Upper (B25):
  = ROUNDUP(B23 + 1.96 * SE, 0)