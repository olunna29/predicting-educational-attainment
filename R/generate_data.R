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
  base_years_schooling = rnorm(50, mean = 13.5, sd = 0.5),
  base_bachelors = rnorm(50, mean = 32, sd = 5),
  state_growth_rate = rnorm(50, mean = 0.2, sd = 0.05)
)

# Year macro shocks (Recession in 2008-2009, 2020)
year_macro <- tibble(
  year = years,
  nat_gdp_growth = c(3.8, 3.5, 2.9, 1.9, -0.1, -2.5, 2.6, 1.6, 2.2, 1.8, 2.5, 2.7, 1.7, 2.2, 2.9, 2.3, -2.8, 5.9, 2.1, 2.5, 2.8),
  nat_unemployment = c(5.5, 5.1, 4.6, 4.6, 5.8, 9.3, 9.6, 8.9, 8.1, 7.4, 6.2, 5.3, 4.9, 4.4, 3.9, 3.7, 8.1, 5.3, 3.6, 3.6, 3.9)
)

df <- panel_grid |>
  left_join(state_base, by = "state") |>
  left_join(year_macro, by = "year") |>
  mutate(
    # Add state-specific noise to GDP and Income growth
    gdp_growth_pct = round(nat_gdp_growth + rnorm(n(), 0, 1.2), 2),
    income_growth_pct = round(0.6 * gdp_growth_pct + rnorm(n(), 1.5, 1.0), 2),
    unemployment_rate = pmax(2.0, round(nat_unemployment + rnorm(n(), 0, 1.1), 2)),
    
    # Calculate education outcomes conditioned on economic variables
    # Higher unemployment & income growth correlated with higher college enrollment/completion
    bachelors_plus_pct = round(
      base_bachelors + 
      (year - 2004) * state_growth_rate + 
      0.35 * income_growth_pct + 
      0.25 * (unemployment_rate - 5) + 
      rnorm(n(), 0, 1.5), 
      2
    ),
    bachelors_plus_pct = pmin(pmax(bachelors_plus_pct, 15), 65),
    
    hs_completion_pct = round(
      86 + (year - 2004) * 0.3 + 
      0.15 * gdp_growth_pct + 
      rnorm(n(), 0, 1.0), 
      2
    ),
    hs_completion_pct = pmin(pmax(hs_completion_pct, 75), 98),
    
    avg_years_schooling = round(
      base_years_schooling + 
      0.04 * (year - 2004) + 
      0.08 * gdp_growth_pct + 
      0.05 * income_growth_pct + 
      0.04 * unemployment_rate + 
      rnorm(n(), 0, 0.2), 
      2
    ),
    
    # Categorical Unemployment Tiers
    unemployment_tier = case_when(
      unemployment_rate < 4.0 ~ "Low (<4%)",
      unemployment_rate <= 7.0 ~ "Mid (4-7%)",
      TRUE ~ "High (7%+)"
    ),
    unemployment_tier = factor(unemployment_tier, levels = c("Low (<4%)", "Mid (4-7%)", "High (7%+)")),
    
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
