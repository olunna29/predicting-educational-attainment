# R/generate_data.R
# Script to generate a realistic macroeconomic and educational panel dataset

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

set.seed(42)

states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
  "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
  "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
  "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
  "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York",
  "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
  "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas",
  "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
)

regions <- list(
  "Northeast" = c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania"),
  "Midwest"   = c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"),
  "South"     = c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "West Virginia", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas"),
  "West"      = c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington")
)

state_region_map <- unlist(lapply(names(regions), function(reg) {
  setNames(rep(reg, length(regions[[reg]])), regions[[reg]])
}))

years <- 2004:2024

panel_grid <- expand.grid(state = states, year = years, stringsAsFactors = FALSE) |>
  arrange(state, year) |>
  mutate(region = state_region_map[state])

# Base state metrics with realistic regional variations
state_base <- tibble(
  state = states,
  base_years_schooling = rnorm(50, mean = 13.5, sd = 0.3),
  base_bachelors = rnorm(50, mean = 32, sd = 4),
  state_growth_rate = rnorm(50, mean = 0.22, sd = 0.04)
)

# Year macro shocks
year_macro <- tibble(
  year = years,
  nat_gdp_growth = c(3.8, 3.5, 2.9, 1.9, -0.1, -2.5, 2.6, 1.6, 2.2, 1.8, 2.5, 2.7, 1.7, 2.2, 2.9, 2.3, -2.8, 5.9, 2.1, 2.5, 2.8),
  nat_income_growth = c(2.4, 2.1, 2.8, 1.5, -0.8, -1.9, 1.2, 1.1, 1.8, 1.6, 2.1, 2.3, 1.4, 1.9, 2.5, 2.0, -1.5, 4.2, 1.8, 2.2, 2.4)
)

df <- panel_grid |>
  left_join(state_base, by = "state") |>
  left_join(year_macro, by = "year") |>
  mutate(
    # Add independent state-level macroeconomic components
    gdp_growth_pct = round(nat_gdp_growth + rnorm(n(), 0, 1.5), 2),
    income_growth_pct = round(nat_income_growth + rnorm(n(), 0, 1.2), 2),
    edu_investment_pct = round(2.0 + 0.3 * gdp_growth_pct + rnorm(n(), 0, 1.2), 2),
    
    # Calculate education outcomes conditioned on economic variables
    bachelors_plus_pct = round(
      base_bachelors + 
      (year - 2004) * state_growth_rate + 
      0.65 * income_growth_pct + 
      0.45 * gdp_growth_pct + 
      0.40 * edu_investment_pct + 
      rnorm(n(), 0, 1.2), 
      2
    ),
    bachelors_plus_pct = pmin(pmax(bachelors_plus_pct, 15), 65),
    
    hs_completion_pct = round(
      86 + (year - 2004) * 0.28 + 
      0.25 * gdp_growth_pct + 
      0.20 * edu_investment_pct + 
      rnorm(n(), 0, 0.8), 
      2
    ),
    hs_completion_pct = pmin(pmax(hs_completion_pct, 75), 98),
    
    avg_years_schooling = round(
      base_years_schooling + 
      0.04 * (year - 2004) + 
      0.095 * income_growth_pct + 
      0.065 * gdp_growth_pct + 
      0.055 * edu_investment_pct + 
      rnorm(n(), 0, 0.12), 
      2
    ),
    
    # Categorical Income Growth Tiers
    income_tier = case_when(
      income_growth_pct < 1.0 ~ "Low (<1.0%)",
      income_growth_pct <= 3.0 ~ "Moderate (1.0-3.0%)",
      TRUE ~ "High (3.0%+)"
    ),
    income_tier = factor(income_tier, levels = c("Low (<1.0%)", "Moderate (1.0-3.0%)", "High (3.0%+)")),
    
    # Categorical Education Tiers for individual level/state aggregation analysis
    education_tier = case_when(
      bachelors_plus_pct < 30.0 ~ "High School or Less",
      bachelors_plus_pct <= 38.0 ~ "Some College",
      TRUE ~ "Bachelor's+"
    ),
    education_tier = factor(education_tier, levels = c("High School or Less", "Some College", "Bachelor's+"))
  )

dir.create("data", showWarnings = FALSE)
saveRDS(df, "data/macro_education_panel.rds")
write.csv(df, "data/macro_education_panel.csv", row.names = FALSE)
cat("Dataset successfully generated with", nrow(df), "rows across 50 states and", length(years), "years.\n")
