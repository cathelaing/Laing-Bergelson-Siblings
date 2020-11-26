# This script generates the demographics information about the infants' siblings. For those infants who have siblings,
# the Sibling_ages.csv dataset identifies how old those siblings are. Siblings who are listed as 'Older' are more than 3 years
# older than the infant; siblings listed as 'Younger' are three years older than the infant or less. Note that 'Younger' does 
# not mean that the sibling is younger than the infant.

# This script should be run after the Demographics and Data_gathering scripts:

#source("C:/Users/catherine/Box Sync/LaingScriptchecks/Siblings/Demographics gathering.R")
#source("C:/Users/catherine/Box Sync/LaingScriptchecks/Siblings/Data gatherine_useme_v2.R")


library(forcats)
library(readr)
library(tidyverse)

CDI <- read_csv("H:/Data/Siblings/Data/CSVs for analysis/CDI.csv") %>%
  mutate(subj = factor(subj),
         subj = fct_recode(subj,
                           "01" = "1",
                           "02" = "2",
                           "03" = "3",
                           "04" = "4",
                           "06" = "6",
                           "07"= "7",
                           "08" = "8",
                           "09" = "9"))


sib.data <- read_csv("H:/Data/Siblings/Data/CSVs for analysis/Sibling_ages.csv") %>%
  mutate(subj = factor(subj),
         subj = fct_recode(subj,
                             "01" = "1",
                             "02" = "2",
                             "03" = "3",
                             "04" = "4",
                             "06" = "6",
                             "07"= "7",
                             "08" = "8",
                             "09" = "9")) %>%
  group_by(subj, Group, SibsYN, Siblings, SibGroup, age.diff.d) %>%
  filter(Group %in%
           c("Younger", "Older")) %>%
  tally() %>%
  spread(Group, n) %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  left_join(CDI, by='subj')

write_csv(sib.data, "H:/Data/Siblings/Data/CSVs for analysis/SibsAgeData.csv", append = FALSE, col_names = TRUE) # Save

