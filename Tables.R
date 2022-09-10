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
  group_by(SibGroup) %>%
  summarise(mean = mean(Total.words, na.rm=T),
            sd = sd(Total.words, na.rm=T))  %>%
  mutate(Variable = "Productive Vocabulary 18m (CDI)") %>%
  select(Variable, SibGroup, mean, sd)

table.data.summary.speaker <- 
  speaker.type %>% 
  filter(audio_video == "video"  & 
           Speaker == "Family.input") %>%
  group_by(SibGroup) %>%
  summarise(mean = mean(n),
            sd = sd(n)) %>%
  mutate(Variable = "N Input utterances, 10-17 months") %>%
  select(Variable, SibGroup, mean, sd)

# table.data.summary.cdi <- 
#   in.cdi %>% 
#   filter(audio_video == "video") %>%
#   group_by(SibGroup) %>%
#   summarise(mean = mean(PC),
#             sd = sd(PC)) %>%
#   mutate(Variable = "% early-acquired words in input") %>%
#   select(Variable, SibGroup, mean, sd)

table.data.summary.object <- 
  object.presence %>% 
  filter(audio_video == "video") %>%
  group_by(SibGroup) %>%
  summarise(mean = mean(PC),
            sd = sd(PC)) %>%
  mutate(Variable = "% object presence in input, 10-17 months") %>%
  select(Variable, SibGroup, mean, sd)

table.data.summary.mean <- rbind(table.data.summary.sibgroup, table.data.summary.speaker, table.data.summary.object) %>% 
  select(Variable, SibGroup, mean) %>%
  spread(SibGroup, mean) %>%
  rename("none m" = "None",
         "1 m" = "One",
         "2 m" = "2+")

table.data.summary.sd <- rbind(table.data.summary.sibgroup, table.data.summary.speaker, table.data.summary.object) %>% 
  select(Variable, SibGroup, sd) %>%
  spread(SibGroup, sd) %>%
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
