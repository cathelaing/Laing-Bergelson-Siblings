library(Hmisc)

Figure.SibGroup <- ggplot(data = SiblingsData, mapping = aes(x=as.numeric(month), y=Log.Totalwords)) +
  geom_point(aes(colour = SibGroup18), shape = 1, size = 3, position = position_jitter(.15)) + 
  geom_smooth(aes(colour = SibGroup18),size=2) + 
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
                aes(fill = SibGroup18), color = "black", shape = 24)

Figure.speaker.count <- ggplot(subset(speaker.type, Speaker %in% c("MOT", "FAT", "SIBLING")), aes(x=Speaker, y=n, color = Speaker)) +
  stat_summary(fun.y = mean, geom = "point", aes(group = subj), shape = 1, size = 3, position = position_jitter(.1)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", shape = 17, size = 1) +
  facet_wrap(~SibGroup6, ncol=3) +
  scale_x_discrete(limits = c("MOT", "FAT", "SIBLING"), labels = c("Mother", "Father", "Sibling")) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")


Figure.in.cdi <- ggplot(data=in.cdi, aes(x=SibGroup6, y=PC, colour=SibGroup6, fill = SibGroup6)) + 
  geom_violin(alpha = .3) + 
  stat_summary(fun=mean, geom = "point", aes(group = subj), shape=1, size=1.5, stroke = 1, position = position_jitter(.03)) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", shape=17, size=.5, colour='black') + 
  scale_colour_discrete(name="Sibling group",limits=c("None", "One", "2+"), labels=c("None", "One", "2+")) + 
  scale_x_discrete(name=element_blank(),limits=c("None", "One", "2+")) + 
  expand_limits(y=c(0, 1)) + 
  theme_bw(base_size = 18) +
  ylab('% early-acquired words in input') + 
  theme(legend.position='none')


Figure.object.presence <- ggplot(data=object.presence, aes(x=SibGroup6, y=PC, color = SibGroup6, fill = SibGroup6)) +
  geom_violin(alpha = .3) +
  stat_summary(fun.y=mean, geom = "point", aes(group = subj), shape=1, size=1.5, stroke = 1, position = position_jitter(.03)) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", shape=17, size=.5, colour='black') + 
  scale_x_discrete(name=element_blank(),limits=c("None", "One", "2+")) + 
  theme_bw(base_size = 18) +
  ylab('% input words with object presence') +
  theme(legend.position='none')