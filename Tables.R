library(janitor)
library(kableExtra)
#library(knitr)
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
           caregiver == "FAMILY") %>%
  group_by(SibGroup) %>%
  summarise(mean = mean(n),
            sd = sd(n)) %>%
  mutate(Variable = "N Input utterances, 10-17 months") %>%
  select(Variable, SibGroup, mean, sd)

table.data.summary.object <- 
  object.presence %>% 
  filter(audio_video == "video") %>%
  group_by(SibGroup) %>%
  summarise(mean = (mean(PC)*100),
            sd = (sd(PC)*100)) %>%
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
  select(Variable, `none m`, `none sd`, `1 m`, `1 sd`, `2 m`, `2 sd`)

# sibling presence

sib.presence.summary <- sib.presence %>%
  filter(audio_video == "video" & SibGroup != "None") %>%
  #mutate(sib.present = ifelse(is.na(SIB), "Sibling not present", "Sibling present")) %>%
  group_by(subj, sib.present) %>%
  tally() %>%
  pivot_wider(names_from = sib.present, values_from = n) %>%
  mutate(`Sibling not present` = ifelse(is.na(`Sibling not present`), 0, `Sibling not present`)) %>%
  summarise(PC.present = `Sibling present`/(`Sibling not present` + `Sibling present`),
            total.present = sum(`Sibling present`)) %>%
  ungroup() %>%
  summarise(mean.present = mean(PC.present),
            sd.present = sd(PC.present),
            total.present = sum(total.present))

sib.presence.table.input <-
  speaker.type %>%
  filter(audio_video == "video" & SibGroup != "None" & (caregiver %in% c("CG1","CG2"))) %>%
  left_join(sib.presence) %>%
  group_by(SibGroup, sib.present) %>%
  summarise("Mean" = mean(n),
            "SD" = sd(n)) %>%
  rename(
    "Sibling presence" = "sib.present"
  )%>%
  mutate(Variable = "N Input utterances, 10-17 months")

sib.presence.table.OP <-
  object.presence %>%
  filter(audio_video == "video" & SibGroup != "None") %>%
  left_join(sib.presence) %>%
  group_by(SibGroup, sib.present) %>%
  summarise("Mean" = (mean(PC, na.rm=T)*100),
            "SD" = (sd(PC, na.rm=T)*100)) %>%
  rename(
    "Sibling presence" = "sib.present"
  )%>%
  mutate(Variable = "% object presence in input, 10-17 months")

sib.presence.table_mean <- rbind(sib.presence.table.input, sib.presence.table.OP) %>%
  dplyr::select(Variable, SibGroup, `Sibling presence`, Mean) %>%
  pivot_wider(names_from = SibGroup, values_from = Mean) %>%
  rename("1 m" = "One",
         "2 m" = "2+")

sib.presence.table_sd <- rbind(sib.presence.table.input, sib.presence.table.OP) %>%
  dplyr::select(Variable, SibGroup, `Sibling presence`, SD) %>%
  pivot_wider(names_from = SibGroup, values_from = SD) %>%
  rename("1 sd" = "One",
         "2 sd" = "2+")

sib.presence.table.data.summary <- sib.presence.table_mean %>% left_join(sib.presence.table_sd) %>%
  select(Variable, `Sibling presence`, `1 m`, `1 sd`, `2 m`, `2 sd`) %>%
  mutate(Variable = ifelse(`Sibling presence` == "Sibling present", "", Variable))

