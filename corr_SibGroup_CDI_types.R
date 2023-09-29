library(tidyverse)
library(blabr)
install.packages("blabr")

# for converting subject numbers, NOT PUSHING this file for data anonymity
random_subjnums <- read_csv("Data/random_subnums.csv")

#grabbing sibgroup and CDI from Catherine's SiblingsData df
randsubj_SibGroup_cdi <- SiblingsData %>% 
  filter(month==18) %>% 
  dplyr::select(subj, SibGroup,Total.words) %>% 
  rename(cdi = Total.words)


#grabbing type and token counts overall and for CHI
seedlings_nouns <- get_seedlings_nouns('v1.0.0') %>% 
  rename("subj" = "child") %>% 
  filter(audio_video=="video")


types_tokens <- seedlings_nouns %>%
  filter(speaker != "CHI") %>%
  dplyr::group_by(subj) %>%
  summarise(numtokens = n(),
            numtypes = n_distinct(basic_level))

types_tokens_CHI <- seedlings_nouns %>%
  filter(speaker == "CHI") %>%
  dplyr::group_by(subj) %>%
  summarise(numtokens_chi = n(),
            numtypes_chi = n_distinct(basic_level))

types_tokens_overall_CDI_sibgroup<- types_tokens %>% 
  left_join(types_tokens_CHI) %>% 
  replace(is.na(.), 0) %>% # to replace the NAs for input
  left_join(random_subjnums) %>%
  dplyr::select(-subj) %>%
  rename(subj = number) %>% 
  mutate(subj=as.factor(subj)) %>% 
  left_join(randsubj_SibGroup_cdi)
 
rm(random_subjnums)

write_csv(types_tokens_overall_CDI_sibgroup, "Data/types_tokens_overall_CDI_sibgroup.csv")

figure_sibgroup_numtypes_cdi <- types_tokens_overall_CDI_sibgroup %>% 
  ggplot(aes(numtypes_chi, cdi))+geom_point(shape=3)+stat_smooth(method="lm")+
  facet_wrap(~SibGroup, scales="free", nrow=3)
figure_sibgroup_numtypes_cdi

