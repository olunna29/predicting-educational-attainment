# R/fit_final_models.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(nnet)
  library(broom)
})

cat("Loading data/gss_cleaned.rds...\n")
df <- readRDS("data/gss_cleaned.rds")

# Ensure educ_years is defined if missing
if (!"educ_years" %in% colnames(df)) {
  df <- df |> mutate(
    educ_years = case_when(
      educ == "No formal schooling" ~ 0, educ == "1st grade" ~ 1, educ == "2nd grade" ~ 2,
      educ == "3rd grade" ~ 3, educ == "4th grade" ~ 4, educ == "5th grade" ~ 5,
      educ == "6th grade" ~ 6, educ == "7th grade" ~ 7, educ == "8th grade" ~ 8,
      educ == "9th grade" ~ 9, educ == "10th grade" ~ 10, educ == "11th grade" ~ 11,
      educ == "12th grade" ~ 12, educ == "1 year of college" ~ 13, educ == "2 years of college" ~ 14,
      educ == "3 years of college" ~ 15, educ == "4 years of college" ~ 16, educ == "5 years of college" ~ 17,
      educ == "6 years of college" ~ 18, educ == "7 years of college" ~ 19, educ == "8 or more years of college" ~ 20,
      TRUE ~ NA_real_
    )
  )
}

# Prepare clean modeling sample
m_data <- df |>
  filter(
    !is.na(educ_cat),
    !is.na(educ_years),
    !is.na(parent_educ_avg),
    !is.na(incom16_num),
    !is.na(family16_clean),
    !is.na(res16_clean),
    !is.na(sibs_num),
    !is.na(childs_num),
    !is.na(age_num),
    !is.na(year_num),
    !is.na(sex_clean),
    !is.na(race_clean),
    !is.na(born_clean),
    !is.na(parborn_clean)
  )

cat("Modeling N:", nrow(m_data), "\n")

# Fit Multinomial Model
fit_multi <- multinom(
  educ_cat ~ parent_educ_avg + incom16_num + sibs_num + childs_num + 
    age_num + year_num + sex_clean + race_clean + res16_clean + family16_clean + born_clean + parborn_clean,
  data = m_data,
  trace = FALSE
)

# Fit OLS Baseline Model
fit_ols <- lm(
  educ_years ~ parent_educ_avg + incom16_num + sibs_num + childs_num + 
    age_num + year_num + sex_clean + race_clean + res16_clean + family16_clean + born_clean + parborn_clean,
  data = m_data
)

# Save fitted objects
saveRDS(df, "data/gss_cleaned.rds")
saveRDS(m_data, "data/gss_model_data.rds")
saveRDS(fit_multi, "data/gss_multinomial_model.rds")
saveRDS(fit_ols, "data/gss_ols_model.rds")

cat("Successfully fitted and saved multinomial and OLS models to data/\n")
