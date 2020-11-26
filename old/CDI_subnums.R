library(feather)
library(dplyr)

subnums <- read_csv("C:/Users/Daniel & Catherine/Box Sync/Laing&BergelsonWork/random_subj_JUL282018.csv") %>% 
  filter(subj != "05" & subj != "24") %>% 
  mutate(subj = factor(subj))

CDI <- read_csv("cdi.csv") %>% 
  dplyr::select(SubjectNumber, CDI_TotalProd) %>%
  separate(col = SubjectNumber, into = c("SubjectNumber", "Month"), sep = "_") %>%
  filter(SubjectNumber < 47 & SubjectNumber!=05 & SubjectNumber!=24 & SubjectNumber != '08semns1' & SubjectNumber != '08semns2') %>%  # Remove infants who are not in the main Seedlings data
  rename("subj" = "SubjectNumber") %>%
  left_join(subnums) %>%
  select(-subj) %>%
  rename("subj" = "number")

write_csv(CDI, "C:/Users/Daniel & Catherine/Box Sync/Laing&BergelsonSiblings/Data/CDI.csv")

basic_levels_data <- read_feather("Data/all_basiclevelMar1518.feather") %>%
  separate(col = SubjectNumber, into = c("subj", "month"), sep = "_") %>%
  left_join(subnums) %>%
  select(-subj) %>%
  rename("subj" = "number")

write_feather(basic_levels_data, "C:/Users/Daniel & Catherine/Box Sync/Laing&BergelsonSiblings/Data/basic_levels_Mar1518.feather")
