# R/process_gss_data.R
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

cat("Reading data/gss_data.csv...\n")
raw_df <- read.csv("data/gss_data.csv", stringsAsFactors = FALSE)

clean_gss <- function(data) {
  data |>
    filter(!educ %in% c(".d:  Do not Know/Cannot Choose", ".n:  No answer", "")) |>
    mutate(
      # Primary Outcome (4 Tiers)
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
      
      # Continuous Schooling Years
      educ_years = case_when(
        educ == "No formal schooling" ~ 0, educ == "1st grade" ~ 1, educ == "2nd grade" ~ 2,
        educ == "3rd grade" ~ 3, educ == "4th grade" ~ 4, educ == "5th grade" ~ 5,
        educ == "6th grade" ~ 6, educ == "7th grade" ~ 7, educ == "8th grade" ~ 8,
        educ == "9th grade" ~ 9, educ == "10th grade" ~ 10, educ == "11th grade" ~ 11,
        educ == "12th grade" ~ 12, educ == "1 year of college" ~ 13, educ == "2 years of college" ~ 14,
        educ == "3 years of college" ~ 15, educ == "4 years of college" ~ 16, educ == "5 years of college" ~ 17,
        educ == "6 years of college" ~ 18, educ == "7 years of college" ~ 19, educ == "8 or more years of college" ~ 20,
        TRUE ~ NA_real_
      ),
      
      # Father Education
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
      
      # Mother Education
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
      
      # Average Parent Education
      parent_educ_avg = case_when(
        !is.na(paeduc_years) & !is.na(maeduc_years) ~ (paeduc_years + maeduc_years) / 2,
        !is.na(paeduc_years) ~ paeduc_years,
        !is.na(maeduc_years) ~ maeduc_years,
        TRUE ~ NA_real_
      ),
      
      # Sex & Race
      sex_clean = ifelse(sex %in% c("FEMALE", "MALE"), sex, NA_character_),
      race_clean = ifelse(race %in% c("White", "Black", "Other"), race, NA_character_),
      
      # Residence at age 16
      res16_clean = case_when(
        res16 %in% c("FARM", "COUNTRY,NONFARM", "TOWN LT 50000", "50000 TO 250000", "BIG-CITY SUBURB", "CITY GT 250000") ~ res16,
        TRUE ~ NA_character_
      ),
      res16_clean = factor(res16_clean, levels = c("FARM", "COUNTRY,NONFARM", "TOWN LT 50000", "50000 TO 250000", "BIG-CITY SUBURB", "CITY GT 250000")),
      
      # Family structure at age 16
      family16_clean = case_when(
        family16 == "Both own parents" ~ "Both Own Parents",
        family16 %in% c("Mother only", "Father only") ~ "Single Parent",
        family16 %in% c("Mother and stepparent", "Father and stepparent") ~ "Stepparent",
        family16 %in% c("Other", "Other arrangement with relatives (e.g., aunt and uncle, grandparents)", 
                        "Some other female relative (No male head)", "Some other male relative (No female head)") ~ "Other Relative",
        TRUE ~ NA_character_
      ),
      family16_clean = factor(family16_clean, levels = c("Both Own Parents", "Single Parent", "Stepparent", "Other Relative")),
      
      # Childhood Income Tier (Factor & Numeric)
      incom16_num = case_when(
        incom16 == "FAR BELOW AVERAGE" ~ 1,
        incom16 == "BELOW AVERAGE" ~ 2,
        incom16 == "AVERAGE" ~ 3,
        incom16 == "ABOVE AVERAGE" ~ 4,
        incom16 == "FAR ABOVE AVERAGE" ~ 5,
        TRUE ~ NA_real_
      ),
      incom16_cat = case_when(
        incom16 %in% c("FAR BELOW AVERAGE", "BELOW AVERAGE", "AVERAGE", "ABOVE AVERAGE", "FAR ABOVE AVERAGE") ~ incom16,
        TRUE ~ NA_character_
      ),
      incom16_cat = factor(incom16_cat, levels = c("FAR BELOW AVERAGE", "BELOW AVERAGE", "AVERAGE", "ABOVE AVERAGE", "FAR ABOVE AVERAGE")),
      
      # Respondent & Parental Nativity
      born_clean = ifelse(born %in% c("YES", "NO"), born, NA_character_),
      parborn_clean = case_when(
        parborn == "Both born in the U.S." ~ "Both U.S.",
        parborn %in% c("Mother yes, father no", "Mother no, father yes", "Mother yes, father don\'t know", "Mother don\'t know, father yes") ~ "One U.S.",
        parborn == "Neither born in the U.S." ~ "Neither U.S.",
        TRUE ~ NA_character_
      ),
      parborn_clean = factor(parborn_clean, levels = c("Both U.S.", "One U.S.", "Neither U.S.")),
      
      # Numeric Counts & Years
      sibs_num = suppressWarnings(as.numeric(ifelse(sibs == "6 or more", "6", sibs))),
      childs_num = suppressWarnings(as.numeric(ifelse(childs == "8 or more", "8", childs))),
      age_num = suppressWarnings(as.numeric(age)),
      year_num = as.numeric(year),
      agekdbrn_num = suppressWarnings(as.numeric(ifelse(agekdbrn == "17 and younger", "17", agekdbrn)))
    )
}

df_clean <- clean_gss(raw_df)
saveRDS(df_clean, "data/gss_cleaned.rds")
cat("Successfully processed and saved data/gss_cleaned.rds with all helper columns! (Rows:", nrow(df_clean), ")\n")
