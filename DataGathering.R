
# This file extracts the relevant data from the basiclevels feather spreadsheet in the Seedlings folder

# Three main variables are extracted and joined with the Demographics spreadsheet: object presence, utterance type, and speaker type. 
# This script also summarizes in.cdi data. This relates to the 'ease of acquisition' variable in the analysis. 

source("Demographics.R")  

library(tidyverse)
library(readr)
library(feather)
library(stringi)
library(stringr)
library(forcats)

# work from basic levels spreadsheet

sibsdata_video <- read_csv("Data/all_basiclevel_randsubj.csv") %>%
  filter(audio_video =='video',   # Only use video data
    subj != 351 &                  # filter out one twin
         !(speaker %in% c('CHI', #remove infant productions
                          "EFA", "EFB", "EFS", "EFE", # exclude female experimenters
                          "EMM", # exclude male experimenters
                          "MBT", # exclude mother, brother and TV in unison
                          "GRY", # exclude grandfather and toy in unison 
                          "CHY"))) %>%  # exclude child and toy in unison  
  dplyr::select(
    utterance_type, 
    speaker, 
    object_present, 
    basic_level, 
    subj, 
    month) %>%
  mutate(basic_level = str_to_lower(basic_level),
         speaker = factor(speaker),
         speaker = fct_collapse(speaker,
                                "SIBLING" = c("BRO", "BR1", "BR2", "SIS", "SI1", "SI2", 
                                              "BTY", "SCU", "STY"),
                                "AUNT" = c("AUN", "AU2"),
                                "UNCLE" = c("UN2", "UNC"),
                                "BABYSITTER" = c("BSE", "BSJ", "BSK", "BSS"),
                                "FAT" = c("FTS", "FTY", "MFT", "MFV"), # 2 instances of MFT and MFV in the data, in both cases MOT is CG1 and MFT/MFV CG2
                                "GRP" = c("GP2", "GRP"),
                                "GRM" = c("GRA", "GTY", "GRM"),
                                "MOT" = c("MBR", "MCU", "MIS", "MTY"), 
                                #"MOT+FAT" = c("MFT", "MFV"),    
                                "COUSIN" = c("MC2", "FCO", "MCO", "FC2"),
                                "OTHER ADULT" = c("AF8", "AFA", 
                                                  "AFB", "AFC", "AFD","AFL", "AFM", 
                                                  "AFP", "AFR", "AFS", "AM1", "AM3",
                                                  "AMC", "AMR", "AF3", "AFN", "AMI"),
                                "OTHER CHILD" = c("CFC", "CFR",  
                                                  "CFA", "CFD", "CFE", 
                                                  "CFS", "CMD", "CME",
                                                  "CMH", "CML","CMO")),  # rename speakers
  
         subj = factor(subj),
         month = as.numeric(month)) %>%
  filter(month >9)

n_excluded <- read_csv("Data/all_basiclevel_randsubj.csv") %>%
  mutate(month = as.numeric(month)) %>%
  filter(audio_video =='video' &   # Only use video data
           subj != 351 &                  # filter out one twin
           month > 9 &
    (speaker %in% c("TOY",
                     "EFA", "EFB", "EFS", "EFE", # exclude female experimenters
                     "EMM", # exclude male experimenters
                     "MBT", # exclude mother, brother and TV in unison
                     "GRY" # exclude grandfather and toy in unison 
                      ) |
      (grepl("TV", speaker)))) %>%
  tally()

## Speaker type

speaker.type.all_video <- sibsdata_video %>%
  group_by(subj, month, speaker) %>%
  tally() %>%
  spread(speaker, n) %>%
  dplyr::select(-contains("TV"), -TOY) %>% # remove anything with TV and toy (UAT - aunt + uncle + TV)
  replace(is.na(.), 0) %>%
  rowwise() %>%
  mutate(All.speakers = sum(c_across(`OTHER ADULT`:`UNCLE`))) %>% 
  dplyr::select(subj, month, All.speakers)

speaker.type.cg1_video <- sibsdata_video %>%
  filter((!grepl("TV", speaker)) & !(speaker %in% c("TOY", "SIBLING"
                                                    , "OTHER CHILD", "COUSIN"
                                                    ))) %>%
  group_by(subj, month, speaker) %>%
  tally() %>%
  slice_max(n) %>%
  mutate(caregiver = "CG1")

speaker.type.cg2_video <- sibsdata_video %>%
  filter((!grepl("TV", speaker)) & 
           !(speaker %in% c("TOY", "SIBLING", 
                            "OTHER CHILD", "COUSIN", 
                            "GRO"))) %>% # remove speech from other kids and
  # groups ("GRO" - multiple speakers in unison - occured as CG2 for one infant)
  group_by(subj, month, speaker) %>%
  tally() %>%
  group_by(subj, month) %>%
  arrange(desc(n)) %>% 
  slice(2) %>%
  mutate(caregiver = "CG2")

speaker.type.sib_video <- sibsdata_video %>%
  group_by(subj, month, speaker) %>%
  filter(speaker == "SIBLING") %>%
  tally() %>%
  mutate(caregiver = "SIB")

speaker.type.n_video <- rbind(speaker.type.cg1_video,
                              speaker.type.cg2_video,
                              speaker.type.sib_video)

speaker.type.household_video <- speaker.type.n_video %>%
  group_by(subj, month) %>%
  summarise(n = sum(n)) %>%
  mutate(caregiver = "FAMILY")

speaker.type_video <- rbind(speaker.type.n_video, speaker.type.household_video) %>%
  left_join(SiblingsData) %>%
  mutate(Log.n = log(n+1),
         speaker = as.character(speaker),
         speaker = ifelse(caregiver == "FAMILY", "Total.input", speaker))

all.speaker.data_video <- speaker.type.household_video %>% left_join(speaker.type.all_video) %>%
  group_by(subj, month) %>%
  mutate(other.input = All.speakers - n,
         prop.other = other.input/All.speakers) %>%
  ungroup()

## Object presence: How much caregiver input relates to objects that are present in the infant's environment?

object.presence_video <- sibsdata_video %>%
  left_join(speaker.type.n_video) %>%
  filter(!is.na(caregiver)) %>%
  filter(object_present %in%
           c("y", "n")) %>%
  group_by(subj, month, object_present) %>%
  tally() %>%
  spread(object_present, n) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(Total = n + y,
         PC = y/Total) %>%
  dplyr::select(subj, month, y, PC) %>%
  rename(n = y) %>%  
  left_join(demographics) %>%
  mutate(Log.n = log(n+1)) 

# how many instances of object presence were unclear?

object.presence.unsure <- sibsdata_video %>%
    left_join(speaker.type.n_video) %>%
  filter(!is.na(caregiver)) %>%
  filter(object_present %in%
           c("y", "n", "u")) %>%
  group_by(subj, object_present) %>%
  tally() %>%
  spread(object_present, n) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(Total = n + y + u,
         PCu = u/Total) %>%
  summarise(totalu = sum(u),
            meanu = (mean(PCu))*100)

sib.ages <- read_csv("Data/SiblingAges.csv") %>%   # Read in data showing age differences between subj and siblings
  mutate(subj = factor(subj)) %>%
  filter( subj != 351)                  # filter out one twinv

# dataframe to show which recordings included speech from a sibling

sib.presence <- speaker.type_video %>% 
  dplyr::select(subj, month, caregiver, n, SibGroup) %>% 
  group_by(subj, month) %>%
  pivot_wider(names_from = caregiver, values_from = n) %>%
  mutate(sib.present = ifelse((SibGroup != "None" & !is.na(SIB)), "Sibling present", "Sibling not present"))

# dataframe to test for correlations between home-recordings and CDI word productions

#grabbing sibgroup and CDI from SiblingsData df
SibGroup_cdi <- SiblingsData %>% 
  filter(month==18) %>% 
  dplyr::select(subj, SibGroup,Total.words) %>% 
  rename(CDI = Total.words)

#grabbing type and token counts overall and for CHI

types_tokens <- read_csv("Data/all_basiclevel_randsubj.csv") %>%
  mutate(month = as.numeric(month)) %>%
  filter(!(speaker %in% c('CHI', "EFA", "EFB", "EFS", "EMM", "EFE", "GRO", "MBT")) &  # remove infant productions
           subj != 351 &
           month > 9
         ) %>%    
  dplyr::group_by(subj) %>%
  mutate(subj = as.factor(subj)) %>%
  summarise(numtokens = n(),
            numtypes = n_distinct(basic_level)) %>%
  replace(is.na(.), 0)

types_tokens_CHI <- read_csv("Data/all_basiclevel_randsubj.csv") %>%
  mutate(month = as.numeric(month)) %>%
  filter(speaker == "CHI" & 
           subj != 351 &
           month > 9
         ) %>%
  dplyr::group_by(subj) %>%
  mutate(subj = as.factor(subj)) %>%
  summarise(numtokens_chi = n(),
            numtypes_chi = n_distinct(basic_level)) %>%
  replace(is.na(.), 0)

types_tokens_overall_CDI_sibgroup<- types_tokens %>% 
  left_join(types_tokens_CHI) %>% 
  replace(is.na(.), 0) %>% # to replace the NAs for input
  mutate(subj=as.factor(subj)) %>% 
  left_join(SibGroup_cdi)

#write_csv(types_tokens_overall_CDI_sibgroup, "Data/types_tokens_overall_CDI_sibgroup.csv")

### create dataframes for analysis of audio data for supplementals:

sibsdata_audio <- read_csv("Data/all_basiclevel_randsubj.csv") %>%
  filter(audio_video =='audio',   # Only use video data
         subj != 351 &                  # filter out one twin
           !(speaker %in% c('CHI', #remove infant productions
                            "EFA", "EFB", "EFS", "EFE", # exclude female experimenters
                            "EMM", # exclude male experimenters
                            "MBT", # exclude mother, brother and TV in unison
                            "GRY", # exclude grandfather and toy in unison 
                            "CHY"))) %>%  # exclude child and toy in unison  
  dplyr::select(
    utterance_type, 
    speaker, 
    object_present, 
    basic_level, 
    subj, 
    month) %>%
  mutate(basic_level = str_to_lower(basic_level),
         speaker = factor(speaker),
         speaker = fct_collapse(speaker,
                                "SIBLING" = c("BRO", "BR1", "BR2", "SIS", "SI1", "SI2", 
                                              "BTY", "SCU", "STY", "SIU"),
                                "AUNT" = c("AUN", "AU2", "AU3", "AU4"),
                                "UNCLE" = c("UN2", "UNC", "GUN", "UN3", "UN4"),
                                "BABYSITTER" = c("BSE", "BSJ", "BSK", "BSS", "BSC", "BSB", "BSD", "BSL"),
                                "FAT" = c("FTS", "FTY", "MFT", "MFV"), # 2 instances of MFT and MFV in the data, in both cases MOT is CG1 and MFT/MFV CG2
                                "GRP" = c("GP2", "GRP", "GGP", "SGP"),
                                "GRM" = c("GRA", "GTY", "GRM", "GGM"),
                                "MOT" = c("MBR", "MCU", "MIS", "MTY", "MTT"), 
                                #"MOT+FAT" = c("MFT", "MFV"),    
                                "COUSIN" = c("MC2", "COU", "FCO", "MCO", "FC3", "MC3", "CCU", "FC2"),
                                "OTHER ADULT" = c("AF2", "AF4", "AF5", "AF6", "AF7", "AF8", "AFA", 
                                                  "AFB", "AFC", "AFD", "AFH", "AFJ", "AFL", "AFM", 
                                                  "AFP", "AFR", "AFS", "AFT", "AM1", "AM2", "AM3", "AM6",
                                                  "AMB","AMC","AMG","AMR", "X12", "AF1", "AF3", "AF9", "AFE",
                                                  "AFG", "AFK", "AFN", "AFY", "AM4", "AM5", "AMA", "AME",
                                                  "AMI", "AMJ", "AMK", "AMM", "AMS", "AMT", "ADM"),
                                "OTHER CHILD" = c("CFC", "CFR", "CFZ", 
                                                  "CM1", "CM2", "CF1", "CFA", "CFB", "CFD", "CFE", "CFH",
                                                  "CFK", "CFL", "CFM","CFP", "CFS", "CH1", "CMD", "CME",
                                                  "CMH", "CMJ", "CML", "CMM", "CMO","CMT")),  # rename speakers
         
         subj = factor(subj),
         month = as.numeric(month)) %>%
  filter(month >9)

## Speaker type

speaker.type.cg1_audio <- sibsdata_audio %>%
  filter((!grepl("TV", speaker)) & !(speaker %in% c("TOY", "SIBLING"
                                                    , "OTHER CHILD", "COUSIN"
  ))) %>%
  group_by(subj, month, speaker) %>%
  tally() %>%
  slice_max(n) %>%
  mutate(caregiver = "CG1")

speaker.type.cg2_audio <- sibsdata_audio %>%
  filter((!grepl("TV", speaker)) & 
           !(speaker %in% c("TOY", "SIBLING", 
                            "OTHER CHILD", "COUSIN", 
                            "GRO"))) %>% # remove speech from other kids and
  # groups ("GRO" - multiple speakers in unison - occured as CG2 for one infant)
  group_by(subj, month, speaker) %>%
  tally() %>%
  group_by(subj, month) %>%
  arrange(desc(n)) %>% 
  slice(2) %>%
  mutate(caregiver = "CG2")

speaker.type.sib_audio <- sibsdata_audio %>%
  group_by(subj, month, speaker) %>%
  filter(speaker == "SIBLING") %>%
  tally() %>%
  mutate(caregiver = "SIB")

speaker.type.n_audio <- rbind(speaker.type.cg1_audio, 
                              speaker.type.cg2_audio, 
                              speaker.type.sib_audio)

speaker.type.household_audio <- speaker.type.n_audio %>%
  group_by(subj, month) %>%
  summarise(n = sum(n)) %>%
  mutate(caregiver = "FAMILY")

speaker.type_audio <- rbind(speaker.type.n_audio, speaker.type.household_audio) %>%
  left_join(SiblingsData) %>%
  mutate(Log.n = log(n+1),
         speaker = as.character(speaker),
         speaker = ifelse(caregiver == "FAMILY", "Total.input", speaker))

object.presence_audio <- sibsdata_audio %>%
  left_join(speaker.type.n_audio) %>%
  filter(!is.na(caregiver)) %>%
  filter(object_present %in%
           c("y", "n")) %>%
  group_by(subj, month, object_present) %>%
  tally() %>%
  spread(object_present, n) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(Total = n + y,
         PC = y/Total) %>%
  dplyr::select(subj, month, y, PC) %>%
  rename(n = y) %>%  
  left_join(demographics) %>%
  mutate(Log.n = log(n+1)) 
