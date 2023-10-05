## Data gathering: Updated March 2022 ##

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

sibsdata <- read_csv("Data/all_basiclevel_randsubj.csv") %>%
  filter(#audio_video =='video',   # Only use video data
         !(speaker %in% c('CHI', "EFA", "EFB", "EFS", "EMM", "EFE", "GRO", "MBT"))) %>%    # remove infant productions
  dplyr::select(
    audio_video,
    utterance_type, 
    speaker, 
    object_present, 
    basic_level, 
    subj, 
    month) %>%
  mutate(basic_level = str_to_lower(basic_level),
         speaker = factor(speaker),
         speaker = fct_collapse(speaker,
                                "SIBLING" = c("BRO", "BR1", "BR2", "SIS", "SI1", "SI2", "BTY", "SCU", "STY", "SIU"),
                                "AUNT" = c("AUN", "AU2"),
                                "UNCLE" = c("UN2", "UNC"),
                                "BABYSITTER" = c("BSE", "BSJ", "BSK", "BSS", "BSC", "BSB", "BSD", "BSL"),
                                "FAT" = c("FTS", "FTY", "MFT", "MFV"), # 2 instances of MFT and MFV in the data, in both cases MOT is CG1 and MFT/MFV CG2
                                "GRP" = c("GP2"),
                                "GRM" = c("GRA", "GTY"),
                                "MOT" = c("MBR", "MCU", "MIS", "MTY"), 
                                #"MOT+FAT" = c("MFT", "MFV"),    
                                "COUSIN" = c("MC2", "COU", "FCO", "MCO"),
                                "OTHER ADULT" = c("AF2", "AF4", "AF5", "AF6", "AF7", "AF8", "AFA", 
                                                  "AFB", "AFC", "AFD", "AFH", "AFJ", "AFL", "AFM", 
                                                  "AFP", "AFR", "AFS", "AFT", "AM1", "AM2", "AM3", "AM6",
                                                  "AMB","AMC","AMG","AMR", "X12"),
                                "OTHER CHILD" = c("CFC", "CFR", "CFZ", 
                                                  "CM1", "CM2")),  # rename speakers
  
         subj = factor(subj),
         month = as.numeric(month))

## Speaker type

speaker.type.all <- sibsdata %>%
  group_by(subj, month, audio_video, speaker) %>%
  tally() %>%
  spread(speaker, n) %>%
  dplyr::select(-contains("TV"), -TOY) %>%
  replace(is.na(.), 0) %>%
  rowwise() %>%
  mutate(All.speakers = sum(c_across(ADM:X13))) %>%  # not including unknown speakers from audio data
  dplyr::select(subj, month, audio_video, All.speakers)

speaker.type.cg1 <- sibsdata %>%
  filter((!grepl("TV", speaker)) & !(speaker %in% c("TOY", "SIBLING"))) %>%
  group_by(subj, month, audio_video, speaker) %>%
  tally() %>%
  slice_max(n) %>%
  mutate(caregiver = "CG1")

speaker.type.cg2 <- sibsdata %>%
  filter((!grepl("TV", speaker)) & !(speaker %in% c("TOY", "SIBLING"))) %>%
  group_by(subj, month, audio_video, speaker) %>%
  tally() %>%
  group_by(subj, month, audio_video) %>%
  arrange(desc(n)) %>% 
  slice(2) %>%
  mutate(caregiver = "CG2")

speaker.type.sib <- sibsdata %>%
  group_by(subj, month, audio_video, speaker) %>%
  filter(speaker == "SIBLING") %>%
  tally() %>%
  mutate(caregiver = "SIB")

speaker.type.n <- rbind(speaker.type.cg1, speaker.type.cg2, speaker.type.sib)

speaker.type.household <- speaker.type.n %>%
  group_by(subj, month, audio_video) %>%
  summarise(n = sum(n)) %>%
  mutate(caregiver = "FAMILY")


speaker.type <- rbind(speaker.type.n, speaker.type.household) %>%
  left_join(SiblingsData) %>%
  mutate(Log.n = log(n+1),
         speaker = as.character(speaker),
         speaker = ifelse(caregiver == "FAMILY", "Total.input", speaker)) %>%
  filter(!(speaker %in% c("OTHER CHILD", "COUSIN")))

all.speaker.data <- speaker.type.household %>% left_join(speaker.type.all)


## Object presence: How much caregiver input relates to objects that are present in the infant's environment?

object.presence <- sibsdata %>%
  left_join(speaker.type.n) %>%
  filter(!is.na(caregiver)) %>%
  filter(object_present %in%
           c("y", "n")) %>%
  group_by(subj, month, audio_video, object_present) %>%
  tally() %>%
  spread(object_present, n) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(Total = n + y,
         PC = y/Total) %>%
  dplyr::select(subj, month, audio_video, y, PC) %>%
  rename(n = y) %>%  
  left_join(demographics) %>%
  mutate(Log.n = log(n+1)) 

#check <- object.presence %>% group_by(subj, audio_video) %>% tally() %>% filter(audio_video == "video" & n<12) 
# 129 has no video data in month 6 but that's not a problem because we start at month 10

# how many instances of object presence were unclear?

object.presence.unsure <- sibsdata %>%
  left_join(speaker.type.n) %>%
  filter(!is.na(caregiver)) %>%
  filter(object_present %in%
           c("y", "n", "u")) %>%
  group_by(subj, month, audio_video, object_present) %>%
  tally() %>%
  spread(object_present, n) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  mutate(Total = n + y + u,
         PCu = u/Total) %>%
  filter(audio_video == "video") %>%
  summarise(totalu = sum(u),
            meanu = (mean(PCu))*100)


sib.ages <- read_csv("Data/SiblingAges.csv") %>%   # Read in data showing age differences between subj and siblings
  mutate(subj = factor(subj))

# dataframe to show which recordings included speech from a sibling

sib.presence <- speaker.type %>% filter(audio_video == "video") %>% 
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
  filter(!(speaker %in% c('CHI', "EFA", "EFB", "EFS", "EMM", "EFE", "GRO", "MBT")) &  # remove infant productions
           subj != 351
         ) %>%    
  dplyr::group_by(subj) %>%
  mutate(subj = as.factor(subj)) %>%
  summarise(numtokens = n(),
            numtypes = n_distinct(basic_level)) %>%
  replace(is.na(.), 0)

types_tokens_CHI <- read_csv("Data/all_basiclevel_randsubj.csv") %>%
  filter(speaker == "CHI" & 
           subj != 351
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
