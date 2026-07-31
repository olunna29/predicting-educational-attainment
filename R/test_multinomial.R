# R/test_multinomial.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(nnet)
  library(broom)
})

raw_df <- read.csv("data/gss_data.csv", stringsAsFactors = FALSE)

clean_gss <- function(data) {
  data |>
    filter(!educ %in% c(".d:  Do not Know/Cannot Choose", ".n:  No answer", "")) |>
    mutate(
      educ_cat = case_when(
        educ %in% c("No formal schooling", "1st grade", "2nd grade", "3rd grade", "4th grade", 
                    "5th grade", "6th grade", "7th grade", "8th grade", "9th grade", 
                    "10th grade", "11th grade", "12th grade") ~ "High School or Less",
        educ %in% c("1 year of college", "2 years of college", "3 years of college") ~ "Some College",
        educ %in% c("4 years of college") ~ "Bachelor Degree",
        educ %in% c("5 years of college", "6 years of college", "7 years of college", "8 or more years of college") ~ "Graduate Degree",
        TRUE ~ NA_character_
      ),
      educ_cat = factor(educ_cat, levels = c("High School or Less", "Some College", "Bachelor Degree", "Graduate Degree")),
      
      paeduc_years = case_when(
        paeduc == "No formal schooling" ~ 0, paeduc == "1st grade" ~ 1, paeduc == "2nd grade" ~ 2,
        paeduc == "3rd grade" ~ 3, paeduc == "4th grade" ~ 4, paeduc == "5th grade" ~ 5,
        paeduc == "6th grade" ~ 6, paeduc == "7th grade" ~ 7, paeduc == "8th grade" ~ 8,
        paeduc == "9th grade" ~ 9, paeduc == "10th grade" ~ 10, paeduc == "11th grade" ~ 11,
        paeduc == "12th grade" ~ 12, paeduc == "1 year of college" ~ 13, paeduc == "2 years of college" ~ 14,
        paeduc == "3 years of college" ~ 15, paeduc == "4 years of college" ~ 16, paeduc == "5 years of college" ~ 17,
        paeduc == "6 years of college" ~ 18, paeduc == "7 years of college" ~ 19, paeduc == "8 or more years of college" ~ 20,
        TRUE ~ NA_real_
      ),
      maeduc_years = case_when(
        maeduc == "No formal schooling" ~ 0, maeduc == "1st grade" ~ 1, maeduc == "2nd grade" ~ 2,
        maeduc == "3rd grade" ~ 3, maeduc == "4th grade" ~ 4, maeduc == "5th grade" ~ 5,
        maeduc == "6th grade" ~ 6, maeduc == "7th grade" ~ 7, maeduc == "8th grade" ~ 8,
        maeduc == "9th grade" ~ 9, maeduc == "10th grade" ~ 10, maeduc == "11th grade" ~ 11,
        maeduc == "12th grade" ~ 12, maeduc == "1 year of college" ~ 13, maeduc == "2 years of college" ~ 14,
        maeduc == "3 years of college" ~ 15, maeduc == "4 years of college" ~ 16, maeduc == "5 years of college" ~ 17,
        maeduc == "6 years of college" ~ 18, maeduc == "7 years of college" ~ 19, maeduc == "8 or more years of college" ~ 20,
        TRUE ~ NA_real_
      ),
      parent_educ_avg = case_when(
        !is.na(paeduc_years) & !is.na(maeduc_years) ~ (paeduc_years + maeduc_years) / 2,
        !is.na(paeduc_years) ~ paeduc_years,
        !is.na(maeduc_years) ~ maeduc_years,
        TRUE ~ NA_real_
      ),
      sex_clean = ifelse(sex %in% c("FEMALE", "MALE"), sex, NA_character_),
      race_clean = ifelse(race %in% c("White", "Black", "Other"), race, NA_character_),
      res16_clean = case_when(
        res16 %in% c("FARM", "COUNTRY,NONFARM", "TOWN LT 50000", "50000 TO 250000", "BIG-CITY SUBURB", "CITY GT 250000") ~ res16,
        TRUE ~ NA_character_
      ),
      family16_clean = case_when(
        family16 == "Both own parents" ~ "Both Own Parents",
        family16 %in% c("Mother only", "Father only") ~ "Single Parent",
        family16 %in% c("Mother and stepparent", "Father and stepparent") ~ "Stepparent",
        family16 %in% c("Other", "Other arrangement with relatives (e.g., aunt and uncle, grandparents)", 
                        "Some other female relative (No male head)", "Some other male relative (No female head)") ~ "Other Relative",
        TRUE ~ NA_character_
      ),
      incom16_num = case_when(
        incom16 == "FAR BELOW AVERAGE" ~ 1,
        incom16 == "BELOW AVERAGE" ~ 2,
        incom16 == "AVERAGE" ~ 3,
        incom16 == "ABOVE AVERAGE" ~ 4,
        incom16 == "FAR ABOVE AVERAGE" ~ 5,
        TRUE ~ NA_real_
      ),
      born_clean = ifelse(born %in% c("YES", "NO"), born, NA_character_),
      parborn_clean = case_when(
        parborn == "Both born in the U.S." ~ "Both U.S.",
        parborn %in% c("Mother yes, father no", "Mother no, father yes", "Mother yes, father don\'t know", "Mother don\'t know, father yes") ~ "One U.S.",
        parborn == "Neither born in the U.S." ~ "Neither U.S.",
        TRUE ~ NA_character_
      ),
      sibs_num = suppressWarnings(as.numeric(ifelse(sibs == "6 or more", "6", sibs))),
      childs_num = suppressWarnings(as.numeric(ifelse(childs == "8 or more", "8", childs))),
      age_num = suppressWarnings(as.numeric(age)),
      year_num = as.numeric(year)
    )
}

df_clean <- clean_gss(raw_df)

m_data <- df_clean |>
  filter(
    !is.na(educ_cat),
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

cat("Complete modeling dataset size:", nrow(m_data), "\n")

# Full multinomial fit
fit_full <- multinom(
  educ_cat ~ parent_educ_avg + incom16_num + sibs_num + childs_num + 
    age_num + year_num + sex_clean + race_clean + res16_clean + family16_clean + born_clean + parborn_clean,
  data = m_data,
  trace = FALSE
)

cat("Full Model AIC:", AIC(fit_full), "Deviance:", deviance(fit_full), "\n")

# Variable Importance / LRT tests by dropping one variable at a time
terms <- c("parent_educ_avg", "incom16_num", "sibs_num", "childs_num", "age_num", 
           "year_num", "sex_clean", "race_clean", "res16_clean", "family16_clean", "born_clean", "parborn_clean")

lrt_res <- data.frame(Term = character(), LR_Chisq = numeric(), DF = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

full_dev <- deviance(fit_full)

for (t in terms) {
  f_sub <- as.formula(paste("educ_cat ~", paste(setdiff(terms, t), collapse = " + ")))
  fit_sub <- multinom(f_sub, data = m_data, trace = FALSE)
  dev_sub <- deviance(fit_sub)
  chisq <- dev_sub - full_dev
  df_diff <- fit_full$edf - fit_sub$edf
  p_val <- pchisq(chisq, df = df_diff, lower.tail = FALSE)
  lrt_res <- rbind(lrt_res, data.frame(Term = t, LR_Chisq = chisq, DF = df_diff, p_value = p_val))
}

lrt_res <- lrt_res |> arrange(desc(LR_Chisq))
cat("\nLikelihood Ratio Tests for Covariate Significance:\n")
print(lrt_res)

# Select top most statistically significant variables
top_vars <- lrt_res |> filter(p_value < 0.001) |> pull(Term)
cat("\nTop statistically significant predictors (p < 0.001):\n")
print(top_vars)

# Refit optimal/final model with top significant predictors
f_final <- as.formula(paste("educ_cat ~", paste(top_vars, collapse = " + ")))
fit_final <- multinom(f_final, data = m_data, trace = FALSE)

cat("\nFinal Selected Model AIC:", AIC(fit_final), "\n")
print(summary(fit_final))

# Save processed datasets for website compilation
saveRDS(df_clean, "data/gss_cleaned.rds")
saveRDS(m_data, "data/gss_model_data.rds")
saveRDS(fit_final, "data/gss_multinomial_model.rds")
cat("\nSaved processed data and model objects to data/\n")
