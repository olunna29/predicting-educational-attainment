# R/explore_gss.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(nnet)
  library(broom)
})

raw_df <- read.csv("data/gss_data.csv", stringsAsFactors = FALSE)
cat("Raw dataset dimensions:", dim(raw_df), "\n")

# Process educ
# Let's inspect unique educ values
cat("Unique educ values:\n")
print(table(raw_df$educ))

# Define clean educ categories (e.g., 4 tiers or 3 tiers for multinomial logistic regression)
# Or 4 categories: High School or Less, Some College, Bachelor's Degree, Graduate Degree
# Or 3 categories: High School or Less, Some College, Bachelor's+
df <- raw_df |>
  filter(!educ %in% c(".d:  Do not Know/Cannot Choose", ".n:  No answer", "")) |>
  mutate(
    educ_3cat = case_when(
      educ %in% c("No formal schooling", "1st grade", "2nd grade", "3rd grade", "4th grade", 
                  "5th grade", "6th grade", "7th grade", "8th grade", "9th grade", 
                  "10th grade", "11th grade", "12th grade") ~ "High School or Less",
      educ %in% c("1 year of college", "2 years of college", "3 years of college") ~ "Some College",
      educ %in% c("4 years of college", "5 years of college", "6 years of college", 
                  "7 years of college", "8 or more years of college") ~ "Bachelor Degree+",
      TRUE ~ NA_character_
    ),
    educ_4cat = case_when(
      educ %in% c("No formal schooling", "1st grade", "2nd grade", "3rd grade", "4th grade", 
                  "5th grade", "6th grade", "7th grade", "8th grade", "9th grade", 
                  "10th grade", "11th grade", "12th grade") ~ "High School or Less",
      educ %in% c("1 year of college", "2 years of college", "3 years of college") ~ "Some College",
      educ %in% c("4 years of college") ~ "Bachelor Degree",
      educ %in% c("5 years of college", "6 years of college", 
                  "7 years of college", "8 or more years of college") ~ "Graduate Degree",
      TRUE ~ NA_character_
    ),
    educ_years = case_when(
      educ == "No formal schooling" ~ 0,
      educ == "1st grade" ~ 1,
      educ == "2nd grade" ~ 2,
      educ == "3rd grade" ~ 3,
      educ == "4th grade" ~ 4,
      educ == "5th grade" ~ 5,
      educ == "6th grade" ~ 6,
      educ == "7th grade" ~ 7,
      educ == "8th grade" ~ 8,
      educ == "9th grade" ~ 9,
      educ == "10th grade" ~ 10,
      educ == "11th grade" ~ 11,
      educ == "12th grade" ~ 12,
      educ == "1 year of college" ~ 13,
      educ == "2 years of college" ~ 14,
      educ == "3 years of college" ~ 15,
      educ == "4 years of college" ~ 16,
      educ == "5 years of college" ~ 17,
      educ == "6 years of college" ~ 18,
      educ == "7 years of college" ~ 19,
      educ == "8 or more years of college" ~ 20,
      TRUE ~ NA_real_
    )
  )

cat("\nCleaned educ_4cat counts:\n")
print(table(df$educ_4cat, useNA="always"))

# Let's clean the covariates
df_clean <- df |>
  mutate(
    # Parent education numeric (years)
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
    # Highest parent education
    max_parent_educ = pmax(paeduc_years, maeduc_years, na.rm = TRUE),
    
    # Categoricals
    sex = ifelse(sex %in% c("FEMALE", "MALE"), sex, NA_character_),
    race = ifelse(race %in% c("White", "Black", "Other"), race, NA_character_),
    
    incom16_ord = case_when(
      incom16 == "FAR BELOW AVERAGE" ~ 1,
      incom16 == "BELOW AVERAGE" ~ 2,
      incom16 == "AVERAGE" ~ 3,
      incom16 == "ABOVE AVERAGE" ~ 4,
      incom16 == "FAR ABOVE AVERAGE" ~ 5,
      TRUE ~ NA_real_
    ),
    incom16_cat = ifelse(!is.na(incom16_ord), incom16, NA_character_),
    
    res16_cat = case_when(
      res16 %in% c("FARM", "COUNTRY,NONFARM", "TOWN LT 50000", "50000 TO 250000", "BIG-CITY SUBURB", "CITY GT 250000") ~ res16,
      TRUE ~ NA_character_
    ),
    
    family16_cat = case_when(
      family16 == "Both own parents" ~ "Both Own Parents",
      family16 %in% c("Mother only", "Father only") ~ "Single Parent",
      family16 %in% c("Mother and stepparent", "Father and stepparent") ~ "Stepparent",
      family16 %in% c("Other", "Other arrangement with relatives (e.g., aunt and uncle, grandparents)", 
                      "Some other female relative (No male head)", "Some other male relative (No female head)") ~ "Other Relative",
      TRUE ~ NA_character_
    ),
    
    born_cat = ifelse(born %in% c("YES", "NO"), born, NA_character_),
    
    parborn_cat = case_when(
      parborn == "Both born in the U.S." ~ "Both U.S.",
      parborn %in% c("Mother yes, father no", "Mother no, father yes", "Mother yes, father don\'t know", "Mother don\'t know, father yes") ~ "One U.S.",
      parborn == "Neither born in the U.S." ~ "Neither U.S.",
      TRUE ~ NA_character_
    ),
    
    sibs_num = suppressWarnings(as.numeric(ifelse(sibs == "6 or more", "6", sibs))),
    childs_num = suppressWarnings(as.numeric(ifelse(childs == "8 or more", "8", childs))),
    age_num = suppressWarnings(as.numeric(age)),
    year_num = as.numeric(year),
    
    agekdbrn_num = suppressWarnings(as.numeric(ifelse(agekdbrn == "17 and younger", "17", agekdbrn)))
  )

cat("\nSummary of complete cases for regression:\n")
model_df <- df_clean |>
  filter(!is.na(educ_4cat), !is.na(paeduc_years) | !is.na(maeduc_years), 
         !is.na(sex), !is.na(race), !is.na(incom16_ord), !is.na(family16_cat),
         !is.na(res16_cat), !is.na(sibs_num), !is.na(age_num), !is.na(year_num))

cat("Complete cases count:", nrow(model_df), "\n")

# Let's fit a full linear model first to see p-values of all covariates!
full_lm <- lm(educ_years ~ paeduc_years + maeduc_years + max_parent_educ + sex + race + 
                incom16_ord + factor(family16_cat) + factor(res16_cat) + 
                sibs_num + childs_num + age_num + year_num + factor(born_cat) + factor(parborn_cat), 
              data = df_clean)

cat("\nFull OLS Summary:\n")
print(summary(full_lm))
