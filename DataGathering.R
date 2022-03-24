## Data gathering: Updated 14th February 2020 ##

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
  filter(audio_video =='video',   # Only use video data
         speaker != 'CHI') %>%    # remove infant productions
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
                                "SIBLING" = c("BRO", "BR1", "BR2", "SIS", "SI1", "SI2")),  # rename speakers
         subj = factor(subj))

wordlist <- read_csv("Data/in_cdi_Wordlist.csv") # Read in CDI wordlist that matches sibsdata$basic_level with words on the CDI


## Utterance type: What kinds of utterances occur in the infants' inputs?

utterance.type.n <- sibsdata %>%
  filter(speaker %in% c("MOT", "FAT", "SIBLING"))  %>%    # Remove other speakers from data
  filter(utterance_type %in%
           c("d", "i", "n", "q", "r", "s", "u"))  %>% 
    group_by(subj, month, utterance_type) %>%
  tally() %>%
  spread(utterance_type, n) %>%
  ungroup() %>%
  replace(is.na(.), 0) 

# Turn rows to columns for both N and PC, create new datasets to keep it manageable

utterance.type.PC <- utterance.type.n %>%
  mutate(Total.input = (d + i + n + q + r + s + u),         # create % of each utterance type   
         PCd = d/Total.input,                             
         PCi = i/Total.input,
         PCn = n/Total.input,                              
         PCq = q/Total.input,
         PCr = r/Total.input,
         PCs = s/Total.input,
         PCu = u/Total.input) %>% 
  dplyr::select(subj, month, PCd,PCi, PCn, PCq, PCr, PCs, PCu) %>%
  gather(`PCd`,`PCi`, `PCn`, `PCq`, `PCr`, `PCs`, `PCu`, 
         key = "TypePC", 
         value = "PC") %>% 
  dplyr::select(subj, month, TypePC, PC) %>%  # Remove unnecessary columns 
  mutate(TypePC = factor(TypePC),
         TypePC = fct_recode(TypePC,
                             "d" = "PCd",
                             "i" = "PCi",
                             "n" = "PCn",
                             "q" = "PCq",
                             "r" = "PCr",
                             "s" = "PCs",
                             "u" = "PCu")) %>%
  rename(Type = TypePC)

utterance.type <- utterance.type.n %>% 
  gather(`d`,`i`, `n`, `q`, `r`, `s`, `u`, 
         key = "Type", 
         value = "n") %>%
  mutate(Type = factor(Type)) %>%
  dplyr::select(subj, month, Type, n) %>%  
  left_join(utterance.type.PC) %>%
  left_join(demographics) %>%      # Combine with demographics spreadsheet
  mutate(Log.n = log(n+1),
         # Add a column to show which factors represent joint engagement (R, S, Q)
         JE = ifelse((Type=='r'| Type=='s' | Type=='q'), T, F))



## Object presence: How much caregiver input relates to objects that are present in the infant's environment?

object.presence <- sibsdata %>%
  filter(speaker %in% c("MOT", "FAT", "SIBLING"))  %>%    # Remove other speakers from data
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


## Speaker type

# Spread information across columns

speaker.type.n <- sibsdata %>%
  group_by(subj, month, speaker) %>%
  tally() %>%
  spread(speaker, n) %>%
  dplyr::select(-contains("TV"), -TOY) %>%
  replace(is.na(.), 0) %>%
  rowwise() %>%
  mutate(All.speakers = sum(c_across(AF3:UNC))) %>%
  ungroup() %>%
  mutate(Family.input = (MOT + FAT + SIBLING)) %>%
  dplyr::select(subj, month, MOT, FAT, SIBLING, Family.input, All.speakers)
  
speaker.type <- speaker.type.n %>%
  gather(`MOT`,`FAT`, `SIBLING`, 
         key = "Speaker", 
         value = "n") %>% 
  left_join(demographics) %>%
  mutate(Log.n = log(n+1))

# CDI data: is the word the caregiver produces deemed to be 'learnable' in early acquisition (i.e. is it on the CDI)?

# queries <- sibsdata %>%            # with updated basic_levels.feather I need to first go through and classify words that are not in the original wordlist doc
#   left_join(wordlist) %>%
#   select(-CDIform, -object) %>%
#   filter(is.na(in_cdi) & speaker %in% c("MOT", "FAT", "SIBLING")) %>%
#   distinct(basic_level, .keep_all = TRUE) %>%
#   write_csv("cdi_queries_Mar22.csv")

in.cdi <- sibsdata %>%            # create dataset that classifies each basic_level as matching or not matching CDI list in wordlist
  left_join(wordlist) %>%
  select(-CDIform, -object) %>%
  filter(!is.na(in_cdi)) %>%               # filter these for now while wating for final basic_levels, but remove filter once queries have been sorted
  group_by(subj, month, in_cdi) %>%
  tally() %>%
  spread(in_cdi, n) %>%
  replace(is.na(.), 0) %>%
  ungroup() %>%
  rename(n = `TRUE`,
         False = `FALSE`) %>%
  mutate(Total = False + n,
         PC = n/Total,
         Log.n = log(n+1)) %>%
  dplyr::select(subj, month, n, PC) %>%
  left_join(demographics)                        # Combine with demographics data


sib.ages <- read_csv("Data/SiblingAges.csv") %>%   # Read in data showing age differences between subj and siblings
  mutate(subj = factor(subj))

