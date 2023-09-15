library(papaja)
library(tidyverse)
library(dplyr)
library(tibble)
library(lmerTest)
library(afex)
#library(citr)
library(feather)

source("DataGathering.R")

demographics <- demographics %>% filter(subj != 351)
sib.ages <- sib.ages %>% filter(subj != 351)
SiblingsData <- SiblingsData %>% filter(subj != 351)
CDI <- CDI %>% filter(subj != 351)
speaker.type <- speaker.type %>% filter(subj != 351)
object.presence <- object.presence %>% filter(subj != 351)

stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data = fun, colour = "red", geom = geom, width = 0.2, ...)
}

# editor comment 2: does having an older sibling lead to more IDS?

# note that these corrs include multiple datapoints for the same infant if the child has more than 1 sib. 
# this might not be the best way to do it

# include input with sibling

speaker.type %>% filter(caregiver == "FAMILY" & audio_video == "video") %>% 
  group_by(subj) %>% 
  summarise(mean_input = mean(n)) %>% 
  left_join(sib.ages) %>%
  filter(!(is.na(Group))) %>%
  summarise(cor_older = cor(mean_input, age.diff.d, method = "spearman")) # r = -.29

# remove input from sibling

speaker.type %>% filter(caregiver %in% c("CG1", "CG2") & audio_video == "video") %>% 
  group_by(subj) %>% 
  summarise(mean_input = mean(n)) %>% 
  left_join(sib.ages) %>%
  filter(!(is.na(Group))) %>%
  summarise(cor_older = cor(mean_input, age.diff.d, method = "spearman")) # r = -.27

# now look only at sibling input

speaker.type %>% filter(caregiver == "SIB" & audio_video == "video") %>% 
  group_by(subj) %>% 
  summarise(mean_input = mean(n)) %>% 
  left_join(sib.ages) %>%
  filter(!(is.na(Group))) %>%
  summarise(cor_older = cor(mean_input, age.diff.d, method = "spearman")) # r = .113

# Reviewer 1 comment 4: is it possible to analyse this measure in terms of whether or not a sibling was present in the room with the infant?

sib.presence <- speaker.type %>% filter(audio_video == "video") %>% 
  dplyr::select(subj, month, caregiver, n, SibGroup) %>% 
  group_by(subj, month) %>%
  pivot_wider(names_from = caregiver, values_from = n) %>%
  mutate(sib.present = ifelse((SibGroup != "None" & !is.na(SIB)), T, F))


sib.presence.summary <- sib.presence %>%
  filter(audio_video == "video" & SibGroup != "None") %>%
  mutate(sib.present = ifelse(is.na(SIB), F, T)) %>%
  group_by(subj, sib.present) %>%
  tally() %>%
  pivot_wider(names_from = sib.present, values_from = n) %>%
  mutate(`FALSE` = ifelse(is.na(`FALSE`), 0, `FALSE`)) %>%
  summarise(PC.present = `TRUE`/(`FALSE` + `TRUE`)) %>%
  ungroup() %>%
  summarise(mean.present = mean(PC.present),
            sd.present = sd(PC.present))

sib.present.video <- sib.presence %>% filter(audio_video == "video" & SibGroup != "None") %>% 
  dplyr::select(subj, month, caregiver, n, SibGroup)

object.sib.presence <- object.presence %>% 
  filter(audio_video == "video") %>% 
  left_join(sib.presence)

ggplot(subset(object.sib.presence, SibGroup != "None"), aes(x = sib.present, y = PC, fill = sib.present)) +
  geom_point(aes(colour = SibGroup), shape= 1, 
             position = position_jitter(.1), size=1.5, 
             stroke = 1.1, alpha = .5) +
  geom_violin(alpha = .1) +
 # stat_summary(fun.y=mean, geom = "point", aes(group = subj), shape=1, size=1.5, stroke = 1, alpha = .3, position = position_jitter(.03)) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", shape=17, size=1, aes(colour = SibGroup), position = position_jitter(.1)) +
  theme_bw() +
  ylim(0,1)

ggplot(object.sib.presence, aes(x = SibGroup, y = PC, fill = SibGroup)) +
  geom_point(aes(colour = sib.present), shape= 1, 
             position = position_jitter(.1), size=1.5, 
             stroke = 1.1, alpha = .5) +
  geom_violin(alpha = .1) +
  # stat_summary(fun.y=mean, geom = "point", aes(group = subj), shape=1, size=1.5, stroke = 1, alpha = .3, position = position_jitter(.03)) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", shape=17, size=1, aes(colour = sib.present), position = position_jitter(.1)) +
  theme_bw() +
  ylim(0,1)

# Reviewer 1 comment 5: is there a change over time across measures?

# input

Figure.input <- ggplot(data = subset(speaker.type, caregiver == "FAMILY"), mapping = aes(x=as.numeric(month), y=Log.n)) +
  geom_point(aes(colour = SibGroup), shape = 1, size = 3, position = position_jitter(.15)) + 
  geom_smooth(aes(colour = SibGroup, fill = SibGroup),size=2) + 
  ylab('Total number of words heard in input') +
  xlab('Age in Months') +
  scale_x_continuous(breaks = c(6:18)) +
  scale_y_continuous(breaks = c(0, log(11), log(51), log(151), log(301)), labels = c(0, 10, 50, 150, 300)) +
  theme_bw() + 
  theme(axis.text=element_text(size=18),
        text = element_text(size=18),
        legend.position = c(.15, .85),
        legend.title = element_blank()) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", 
               aes(fill = SibGroup), color = "black", shape = 24,
               position = position_dodge(.2))

# object presence

Figure.objpresence <- ggplot(data = object.presence, mapping = aes(x=as.numeric(month), y=PC)) +
  geom_point(aes(colour = SibGroup), shape = 1, size = 3, position = position_jitter(.15)) + 
  geom_smooth(aes(colour = SibGroup, fill = SibGroup),size=2) + 
  ylab('Total proprtion of object presence') +
  xlab('Age in Months') +
  scale_x_continuous(breaks = c(6:18)) +
  #scale_y_continuous(breaks = c(0, log(11), log(51), log(151), log(301)), labels = c(0, 10, 50, 150, 300)) +
  theme_bw() + 
  theme(axis.text=element_text(size=18),
        text = element_text(size=18),
        legend.position = c(.15, .85),
        legend.title = element_blank()) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", 
               aes(fill = SibGroup), color = "black", shape = 24,
               position = position_dodge(.2))

# Reviewer 1 comment 6: 6.	Would it be possible to run a mediation analysis between number of siblings, language input and vocabulary?

# n sibs --> language input --> vocabulary

# first test relationship between language input ~ vocabulary

input.vocab <- lmerTest::lmer(Log.Totalwords ~ Log.n + month + sex + (1|subj), data=subset(speaker.type, audio_video == "video" &
                                                                                        caregiver == "FAMILY"), REML=FALSE)
summary(input.vocab) # no effect

# continuing with analysis despite lack of effect
# evidence of any mediation: needs to be an effect of sibling group on input words

input.sibs <- lmerTest::lmer(Log.Totalwords ~ SibGroup  + month + sex + (1|subj), data=subset(speaker.type, audio_video == "video" & 
                                                                                              caregiver == "FAMILY"), REML=FALSE)
summary(input.sibs) # sig effect for 2+

# test for effect of mediator (SibGroup) on vocabulary by controlling for input words

input.sibs.vocab <- lmerTest::lmer(Log.Totalwords ~ SibGroup + Log.n + month + sex + (1|subj), 
                                   data=subset(speaker.type, audio_video == "video" & 
                                               caregiver == "FAMILY"), REML=FALSE)
summary(input.sibs.vocab)

# sib group has an effect on total vocabulary size
# number of input words has no effect, consistent with the above
# I think this suggests that number of siblings mediates the effect of language input on the data, but this is complicated by the fact that
# input didn't initially predict vocab size anyway.

library(mediation)

results <- mediate(input.sibs, input.sibs.vocab, treat='SibGroup', mediator='Log.n', boot=T)
summary(results) # no mediation effect


input.vocab <- lmerTest::lmer(Log.Totalwords ~ Log.n + month + sex + (1|subj), data=subset(speaker.type, audio_video == "video" &
                                                                                        caregiver == "FAMILY"), REML=FALSE)
summary(input.vocab) # no effect

# continuing with analysis despite lack of effect
# evidence of any mediation: needs to be an effect of sibling group on input words

input.sibs <- lmerTest::lmer(Log.Totalwords ~ SibGroup  + month + sex + (1|subj), data=subset(speaker.type, audio_video == "video" & 
                                                                                              caregiver == "FAMILY"), REML=FALSE)
summary(input.sibs) # sig effect for 2+

# test for effect of mediator (SibGroup) on vocabulary by controlling for input words

input.sibs.vocab <- lmerTest::lmer(Log.Totalwords ~ SibGroup + Log.n + month + sex + (1|subj), 
                                   data=subset(speaker.type, audio_video == "video" & 
                                               caregiver == "FAMILY"), REML=FALSE)
summary(input.sibs.vocab)

# reporting bias for infants with no siblings

# run correlations between words produced in the video/audio data and CDI scores

ProductionData %>%
  filter(month == 17) %>%
 summarise(cdi_tok_vid = cor(Total.words, tokens_video, method = "spearman", use = "complete.obs"),
            cdi_typ_vid = cor(Total.words, types_video, method = "spearman", use = "complete.obs"),
            cdi_tok_aud = cor(Total.words, tokens_audio, method = "spearman", use = "complete.obs"),
            cdi_typ_aud = cor(Total.words, types_audio, method = "spearman", use = "complete.obs"))
  
