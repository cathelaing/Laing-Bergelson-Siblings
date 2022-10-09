library(Hmisc)

Figure.SibGroup <- ggplot(data = SiblingsData, mapping = aes(x=as.numeric(month), y=Log.Totalwords)) +
  geom_point(aes(colour = SibGroup), shape = 1, size = 3, position = position_jitter(.15)) + 
  geom_smooth(aes(colour = SibGroup, fill = SibGroup),size=2) + 
  ylab('Productive vocabulary (n words)') +
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

speaker.type2 <- speaker.type %>% 
  filter(audio_video == "video") %>%
  mutate(speaker = ifelse(speaker == "MT2", "MOT", speaker)) %>%
  group_by(subj, month, speaker, SibGroup) %>%
  summarise(mean.n = mean(n))

Figure.speaker.count <- ggplot(subset(speaker.type2, speaker %in% c("MOT", "FAT", "SIBLING", "Total.input")), 
                               aes(x=speaker, y=mean.n, color = speaker)) +
  stat_summary(fun= mean, geom = "point", aes(group = subj), shape = 1, size = 3, position = position_jitter(.1)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", shape = 17, size = 1) +
  facet_wrap(~SibGroup, ncol=3) +
  scale_x_discrete(limits = c("MOT", "FAT", "SIBLING", "Total.input"), labels = c("Mother", "Father", "Sibling", "Total input")) +
  ylab("Number of object words produced") +
  xlab("Speaker") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5))


Figure.object.presence <- ggplot(data=subset(object.presence, audio_video == "video"), 
                                 aes(x=SibGroup, y=PC, color = SibGroup, fill = SibGroup)) +
  geom_violin(alpha = .3) +
  stat_summary(fun.y=mean, geom = "point", aes(group = subj), shape=1, size=1.5, stroke = 1, position = position_jitter(.03)) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", shape=17, size=.5, colour='black') + 
  scale_x_discrete(name=element_blank(),limits=c("None", "One", "2+")) + 
  theme_bw(base_size = 18) +
  ylab('% input words with object presence') +
  theme(legend.position='none')