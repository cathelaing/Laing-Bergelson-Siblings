library(tidyverse)
library(feather)

# set.seed(2422)
# 
# subnums <- as.data.frame(runif(46, min=100, max=999))
# 
# subnums <- subnums %>% rename(number = 1) 
# subnums$number <- round(subnums$number, digits=0)
# 
# #sort(subnums$number) # check for duplicates
# 
# subnums <- subnums %>% mutate(subj = c(1:46)) %>%
#   mutate(subj = factor(subj),
#          subj = fct_recode(subj,
#                            "01" = "1",
#                            "02" = "2",
#                            "03" = "3",
#                            "04" = "4",
#                            "05" = "5",
#                            "06" = "6",
#                            "07" = "7",
#                            "08" = "8",
#                            "09" = "9"))

setwd("C:/Code/Laing-Bergelson-Siblings/BLAB_DATA/BLAB_data_3-2022")


# write_csv(subnums, "data/random_subj_JUL282018.csv")
# 
subnums <- read_csv("random_subj_JUL282018.csv")

subnums <- subnums %>% filter(subj != "05" & subj != "24")

all_basiclevel_randsubj <- read_csv("all_basiclevel.csv") %>%
  mutate(subj = factor(subj),
         subj = fct_recode(subj,
                           "01" = "1",
                           "02" = "2",
                           "03" = "3",
                           "04" = "4",
                           "06" = "6",
                           "07" = "7",
                           "08" = "8",
                           "09" = "9")) %>% left_join(subnums) %>% select(-subj) %>% rename(subj = number) %>%
  dplyr::select(-SubjectNumber, -id) %>%
  write_csv("all_basiclevel_randsubj.csv")

all_basiclevel_NA_randsubj <- read_csv("all_basiclevel_NA.csv") %>%
  mutate(subj = factor(subj),
         subj = fct_recode(subj,
                           "01" = "1",
                           "02" = "2",
                           "03" = "3",
                           "04" = "4",
                           "06" = "6",
                           "07" = "7",
                           "08" = "8",
                           "09" = "9")) %>% left_join(subnums) %>% select(-subj) %>% rename(subj = number) %>%
  dplyr::select(-SubjectNumber, -id) %>%
  write_csv("all_basiclevel_NA_randsubj.csv")


