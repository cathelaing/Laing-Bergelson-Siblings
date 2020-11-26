library(dplyr)
library(forcats)
library(lme4)
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
  group_by(subj, SibsYN, Siblings, SibGroup, Group) %>%
  filter(Group %in%
           c("Younger", "Older")) %>%
  tally() %>%
  spread(Group, n) %>%
  ungroup() %>%
  replace(is.na(.), 0) %>%
  left_join(CDI, by='Infant')



# Plot language development as a function of # of YOUNGER siblings

sib.data1.full$Younger <- as.factor(sib.data1.full$Younger)

Plot1 <- ggplot(sib.data1.full, mapping=aes(x=Age, y=Total.words, colour=Younger)) +
  geom_point() +
  geom_smooth()
plot(Plot1)

# And OLDER siblings

sib.data1.full$Older <- as.factor(sib.data1.full$Older)

Plot2 <- ggplot(sib.data1.full, mapping=aes(x=Age, y=Total.words, colour=Older)) +
  geom_point() +
  geom_smooth()
plot(Plot2)

# Does number of Younger siblings have an effect on overall language development?

Model1 <- lmer(Log.Totalwords ~ Younger + Age + (1|Infant), data=sib.data1.full, REML=FALSE)
Model1.null <- lmer(Log.Totalwords ~ Age + (1|Infant), data=sib.data1.full, REML=FALSE)
anova(Model1, Model1.null) # Not significant

# Test at 18 months

eighteen <- subset(sib.data1.full, Age==18)

Model1b <- lm(Log.Totalwords ~ Younger, data=eighteen)
summary(Model1b) # Not significant

# Does number of Older siblings have an effect on overall language development?

Model2 <- lmer(Log.Totalwords ~ Older + Age + (1|Infant), data=sib.data1.full, REML=FALSE)
Model2.null <- lmer(Log.Totalwords ~ Age + (1|Infant), data=sib.data1.full, REML=FALSE)
anova(Model2, Model2.null) # Significant, p=.002
summary(Model2)

# Having 1, 2, or 4 siblings more than 3 years older than the infant has a significant negative effect on language acquisition in the first 18 months


# Test for correlation between vocab at 18 months and mean sibling age

# Calculate mean age difference for each infant

sib.data2 <- subset(sib.data, SibsYN=='Y')
detach(package:plyr)

sib.data2 <- sib.data2 %>%
  group_by(Infant, Siblings, SibGroup) %>%
  summarise(meanDiff = mean(age.diff.d))

# Join with CDI data

sib.data2.full <- sib.data2 %>%
  left_join(CDI, by='Infant')

cor.test(subset(sib.data2.full, Age==18)$meanDiff, subset(sib.data2.full, Age==18)$Total.words, method='spearman') 

# Significant (p=.01) correlation between words acquired by 18 months and mean age of siblings

Plot3 <- ggplot(subset(sib.data2.full, Age==18), mapping=aes(x=meanDiff, y=Total.words)) +
                  geom_smooth() +
  geom_point(aes(colour=SibGroup), size=3)

plot(Plot3)

# Infant 32 has a meanDiff of over 5000 - test without this infant

Plot4 <- ggplot(subset(sib.data2.full, Age==18 & meanDiff<5000), mapping=aes(x=meanDiff, y=Total.words)) +
  geom_smooth()
plot(Plot4)

cor.test(subset(sib.data2.full, Age==18 & meanDiff<5000)$meanDiff, subset(sib.data2.full, Age==18 & meanDiff<5000)$Total.words, method='spearman') 

# The effect remains when this infant is removed (p=.004)

# Having siblings who are more than 3 years older seems to be an important factor in this data. How does the language development of infants with no siblings
# compare to those who have siblings less than 3 years older?

# For this analysis I need to remove all infants who have older siblings, but also those who have BOTH older AND younger siblings, since they will distort findings

# Which infants have older AND younger siblings?

sib.data1 %>%
  mutate(both = ((Older >0) & (Younger >0)), T, F) %>%
  filter(both==T)

# Infants 3, 13, 15, 36, 40 and 44 have both older and younger siblings

sib.data$Group[is.na(sib.data$Group)] <- 'None'
sib.data3 <- subset(sib.data, Group!='Older')
sib.data3 <- subset(sib.data3, Infant !='03' & Infant !='13' & Infant !='15' & Infant !='36'& Infant !='40' & Infant !='44')

sib.data3 <- sib.data3 %>%
  left_join(CDI, by='Infant')

# Test sibling status

Plot5 <- ggplot(sib.data3, mapping=aes(x=Age, y=Total.words, colour=SibsYN)) +
  geom_smooth()
plot(Plot5)

Model3 <- lmer(Log.Totalwords ~ SibsYN + Age + (1|Infant), data=sib.data3, REML=FALSE)
Model3.null <- lmer(Log.Totalwords ~ Age + (1|Infant), data=sib.data3, REML=FALSE)
anova(Model3, Model3.null) # Not significant

# Test at 18 months

Model3b <- lm(Log.Totalwords ~ SibsYN, data=subset(sib.data3, Age==18))
summary(Model3b) # Not significant

# Test sibling group

Model4 <- lmer(Log.Totalwords ~ SibGroup + Age + (1|Infant), data=sib.data3, REML=FALSE)
Model4.null <- lmer(Log.Totalwords ~ Age + (1|Infant), data=sib.data3, REML=FALSE)
anova(Model4, Model4.null) # Not significant

Plot6 <- ggplot(sib.data3, mapping=aes(x=Age, y=Total.words, colour=SibGroup)) +
  geom_smooth()
plot(Plot6)

# This is impossible to really unpick because none of the infants with only Younger siblings have more than 1 sibling, and so the analysis is the same
# as the original comparison between One and None groups, which was not significantly different

# Re-run this with infants who only have Older siblings, just in case this is of interest

# Infants 3, 13, 15, 36, 40 and 44 have both older and younger siblings

sib.data4 <- subset(sib.data, Group!='Younger')
sib.data4 <- subset(sib.data4, Infant !='03' & Infant !='13' & Infant !='15' & Infant !='36'& Infant !='40' & Infant !='44')

sib.data4 <- sib.data4 %>%
  left_join(CDI, by='Infant')

# There probably isn't much point to this since almost all of the infants with siblings are in the 2+ group

Plot7 <- ggplot(sib.data4, mapping=aes(x=Age, y=Total.words, colour=SibGroup)) +
  geom_smooth()
plot(Plot7)

# Summary: These analyses suggest that older siblings might have a more detrimental effect on language development than
# younger siblings, but due to the nature of the data this is impossible to pick apart fully. The infants with more siblings
# are those with the oldest siblings, and so it is unclear whether their slower lexical acquisition is a function of
# sibling number or sibling age. 
