library(janitor)
library(kableExtra)
library(knitr)
library(magrittr)

table.sibling.number <- 
  SiblingsData %>% filter(month == 18 & subj != 794) %>%
  group_by(Siblings6, sex) %>% tally() %>%
  spread(sex, n) %>%
  replace(is.na(.), 0) %>%
  rename(
    "Female" = `F`,
    "Male" = "M",
    "n Siblings" = Siblings6
  ) %>%
  mutate(Total = sum(Female + Male, na.rm=T, digits = 0)) %>%
  adorn_totals("row")

table.data.summary.sibgroup <- 
  SiblingsData %>% filter(month==18) %>%
  group_by(SibGroup6) %>%
  summarise(mean = mean(Total.words, na.rm=T),
            sd = sd(Total.words, na.rm=T))  %>%
  mutate(Variable = "Productive Vocabulary 18m") %>%
  select(Variable, SibGroup6, mean, sd)

table.data.summary.speaker <- 
  speaker.type %>% group_by(SibGroup6) %>%
  summarise(mean = mean(n),
            sd = sd(n)) %>%
  mutate(Variable = "N Input utterances, 10-17 months") %>%
  select(Variable, SibGroup6, mean, sd)

table.data.summary.cdi <- 
  in.cdi %>% group_by(SibGroup6) %>%
  summarise(mean = mean(PC),
            sd = sd(PC)) %>%
  mutate(Variable = "% early-acquired words in input") %>%
  select(Variable, SibGroup6, mean, sd)

table.data.summary.object <- 
  object.presence %>% group_by(SibGroup6) %>%
  summarise(mean = mean(PC),
            sd = sd(PC)) %>%
  mutate(Variable = "% object presence in input") %>%
  select(Variable, SibGroup6, mean, sd)

table.data.summary.mean <- rbind(table.data.summary.sibgroup, table.data.summary.speaker, table.data.summary.cdi, table.data.summary.object) %>% 
  select(Variable, SibGroup6, mean) %>%
  spread(SibGroup6, mean) %>%
  rename("none m" = "None",
         "1 m" = "One",
         "2 m" = "2+")

table.data.summary.sd <- rbind(table.data.summary.sibgroup, table.data.summary.speaker, table.data.summary.cdi, table.data.summary.object) %>% 
  select(Variable, SibGroup6, sd) %>%
  spread(SibGroup6, sd) %>%
  rename("none sd" = "None",
         "1 sd" = "One",
         "2 sd" = "2+")

table.data.summary <- table.data.summary.mean %>% left_join(table.data.summary.sd) %>%
  select(Variable, `none m`, `none sd`, `1 m`, `1 sd`, `2 m`, `2 sd`)# %>%
  # rename("mean" = "none m",
  #        "sd" = "none sd",
  #        "mean" = "1 m",
  #        "sd" = "1 sd",
  #        "mean" = "2 m",
  #        "sd" = "2 sd")
