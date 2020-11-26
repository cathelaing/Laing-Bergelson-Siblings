## Demographics: Updated 14th February 2020 ##

# This is the main file containing all demographics data, incorporating some data from the demographics questionnaires at 6 and 18 months,
# mothers' PVT scores, and CDI data (productive vocabulary) from 6-18 months. THis also includes infant productions from audio and video recordings in the Seedlings corpus.

# This is the first script to generate when arranging data for the siblings analysis. 

library(tidyverse)
library(forcats)

# Import data

PVT <- read_csv("Data/ParentPVT.csv") %>%
  mutate(subj = factor(subj)) %>%
  rename(PVTscore = uncorrected_standard_score) %>%
  select(-ed_pvt)

CDI <- read_csv("Data/CDI.csv") %>%
  mutate(subj = factor(subj)) %>%
  rename(Total.words = CDI_TotalProd,
        month = Month) %>%
  mutate(Log.Totalwords = log(Total.words+1)) %>%
  select(subj, month, Total.words, Log.Totalwords)

chidata <- read_feather("Data/basic_levels_Mar1518.feather") %>%   # Read in basic levels data and then join with CDI data to create a dataset of infant production
  filter(speaker == 'CHI') %>%
  dplyr::select(
    basic_level, 
    audio_video,
    subj, 
    month) %>%
  mutate(basic_level = str_to_lower(basic_level),
         subj = factor(subj))

CHIdata_tokens_video <- chidata %>% 
  filter(audio_video == "video") %>%
  group_by(subj, month) %>%
  tally() %>%
  rename("tokens_video" = "n")

CHIdata_types_video <- chidata %>% 
  filter(audio_video == "video") %>%
  group_by(subj, month) %>%
  summarise(types_video = n_distinct(basic_level))

CHIdata_tokens_audio <- chidata %>% 
  filter(audio_video == "audio") %>%
  group_by(subj, month) %>%
  tally() %>%
  rename("tokens_audio" = "n")

CHIdata_types_audio <- chidata %>% 
  filter(audio_video == "audio") %>%
  group_by(subj, month) %>%
  summarise(types_audio = n_distinct(basic_level))

ProductionData <- CDI %>%
  left_join(CHIdata_tokens_video) %>%
  left_join(CHIdata_types_video) %>%
  left_join(CHIdata_tokens_audio) %>%
  left_join(CHIdata_types_audio) %>%
  mutate(tokens_video = ifelse(month < 18 & is.na(tokens_video), 0, tokens_video),
         types_video = ifelse(month < 18 & is.na(types_video), 0, types_video),
         tokens_audio = ifelse(month < 18 & is.na(tokens_audio), 0, tokens_audio),
         types_audio = ifelse(month < 18 & is.na(types_audio), 0, types_audio))

## Demographics dataset: clean original data and join with PVT.csv

demographics <- read_csv("Data/demo_deid_cleaned.csv") %>%
  mutate(subj = factor(subj)) %>%
  dplyr::select(
    subj,
    sex,
    maternal_education_18mos,
    maternal_employment_18mos,
    maternal_employment_hours_18mos,
    paternal_employment_18mos,
    paternal_employment_hours_18mos,
    children_in_household_18mos_staff,
    children_in_household_6mos_staff
  ) %>%
  dplyr::rename(MOTedu = maternal_education_18mos,  ## rename columns
         MOTwork = maternal_employment_18mos,
         MOTworkhours = maternal_employment_hours_18mos,
         FATwork = paternal_employment_18mos,
         FATworkhours = paternal_employment_hours_18mos) %>%
  mutate(MOTedu = factor(MOTedu),  # rename variables according to key in shared folder
         MOTedu = fct_recode(MOTedu,
                             "High School" = "E",
                             "Some college" = "H",
                             "Assoc Degree" = "I",
                             "Bachelors Degree" = "J",
                             "Masters Degree" = "K",
                             "Doctorate" = "L"),
         MOTwork = ifelse(MOTwork %in% c("H","I"),"Other", MOTwork),       # Three mothers not classed as working FT, PT or Home; class as 'Other' or PT, as relevant
         MOTwork = ifelse(MOTwork == "B_E", "B", MOTwork), 
         MOTwork = factor(MOTwork),                          
         MOTwork = fct_recode(MOTwork,
                              "Full-time" = "A",
                              "Part-time" = "B",
                              "Home" = "C"),
         FATwork = ifelse(FATwork !="A" & FATwork !="B" & FATwork != "C", "Other",  # Recode fathers' work status so that fathers who don't work FT, PT,
                     FATwork),                                                      # or stay home (e.g. full-time students) are listed as 'other'
         FATwork = factor(FATwork),
         FATwork = fct_recode(FATwork,
                              "Full-time" = "A",
                              "Home" = "C"),
         Siblings18 = children_in_household_18mos_staff - 1,   # new variable for sibling number at 18 months 
         Siblings6 = children_in_household_6mos_staff - 1,      # new variable for sibling number at 6 months
         SibsYN = ifelse(Siblings6 == 0, "N", "Y"),             # binomial variable for slibling status (Y = siblings, N = No siblings)
         SibGroup6 = factor(Siblings6),                          # new variable for sibling group, 0, 1, 2+ siblings
         SibGroup6 = fct_recode(SibGroup6,
                               "None" = "0",
                               "One" = "1",
                               "2+" = "2",
                               "2+" = "3",
                               "2+" = "4"),
         SibGroup18 = factor(Siblings18),                          # new variable for sibling group, 0, 1, 2+ siblings
         SibGroup18 = fct_recode(SibGroup18,
                                "None" = "0",
                                "One" = "1",
                                "2+" = "2",
                                "2+" = "3",
                                "2+" = "4"),
         # SibsDiscrete = factor(Siblings),                       # discrete variable for sibling number at 6 months
         # SibsDiscrete = fct_recode(SibsDiscrete,
         #                       "None" = "0",
         #                       "One" = "1",
         #                       "Two" = "2",
         #                       "Three" = "3",
         #                       "Four" = "4"),
         MOTedulevel = fct_recode(MOTedu,
                                   "1" = "High School",
                                   "2" = "Some college",
                                   "3" = "Assoc Degree",
                                   "4" = "Bachelors Degree",
                                   "5" = "Masters Degree",
                                   "6" = "Doctorate"),
         MOTedulevel = as.numeric(MOTedulevel)) %>%
  dplyr::select(-children_in_household_6mos_staff, -children_in_household_18mos_staff) %>% 
  left_join(PVT)   # merge with PVT data

#write_csv(demographics, "Data/Demographics.csv", append = FALSE, col_names = TRUE)

SiblingsData <- demographics %>% 
  left_join(ProductionData) %>% 
  dplyr::select(                                      # re-order columns
    subj,
    month,
    sex,
    MOTedu,
    MOTedulevel,
    age_mom,
    PVTscore,
    MOTwork,
    MOTworkhours,
    FATwork,
    FATworkhours,
    SibsYN,
    Siblings6,
    Siblings18,
    SibGroup6,
    SibGroup18,
    #SibsDiscrete,
    Total.words,
    Log.Totalwords,
    tokens_video,
    types_video,
    tokens_audio,
    types_audio)  %>%
  mutate(Log.tokens_video = log(tokens_video+1),
         Log.types_video = log(types_video+1),
         Log.tokens_audio = log(tokens_audio+1),
         Log.types_audio = log(types_audio+1))

# save

#write_csv(SiblingsData, "Data/SiblingsData.csv", append = FALSE, col_names = TRUE)
