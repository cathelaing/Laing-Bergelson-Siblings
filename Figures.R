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
                aes(fill = SibGroup), color = "black", shape = 21,
               position = position_dodge(.2))

speaker.type2 <- speaker.type %>% 
  filter(audio_video == "video") %>%
  mutate(speaker = ifelse(speaker == "MT2", "MOT", speaker)) %>%
  filter(speaker %in% c("MOT", "FAT", "SIBLING")) %>%
  #group_by(subj, month, speaker, SibGroup) %>% 
  #eb2cl: above line was the error, by keeping month in the group you're not actually taking the mean 
  #so then the error bars in the graph below were over all the data from every month not just overall mean
  group_by(subj, speaker, SibGroup) %>%
  summarise(mean.n = mean(n))
# eb2cl: you could tell it was wrong by looking at the output of speaker.type2 which had basically just as many 
#rows as speaker.type %>% filter(audio_video==video) instead of only about 3 per kid

speaker.type.fig <- speaker.type %>% 
  filter(audio_video == "video") %>%
  mutate(speaker = ifelse(speaker == "MT2", "MOT", speaker)) %>%
  filter(speaker %in% c("MOT", "FAT", "SIBLING")) %>%
  group_by(subj, SibGroup, speaker) %>%
    summarise(speaker.mean = mean(n)) %>%
  ungroup() %>%
  group_by(subj, SibGroup) %>%
  summarise(mean.n = sum(speaker.mean)) %>%  
  ## not actually mean - this is the total of the average input from each speaker
  ## labelling this way for generating the figure below
  mutate(speaker = "FAMILY") %>%
  rbind(speaker.type2)

Figure.speaker.count_fixed <- ggplot(subset(speaker.type.fig, speaker %in% c("MOT", "FAT", "SIBLING", "FAMILY")), 
                               aes(x=speaker, y=mean.n, color = speaker)) +
  #geom_point(shape = 1, size = 2, position = position_jitter(.1)) +
  stat_summary(fun= mean, geom = "point", aes(group = subj), shape = 2, size = 2, position = position_jitter(.1)) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", 
               aes(fill = speaker), color = "black", shape = 24) +
  facet_wrap(~SibGroup, ncol=3) +
  scale_x_discrete(limits = c("MOT", "FAT", "SIBLING", "FAMILY"), labels = c("Mother", "Father", "Sibling", "Total input")) +
  ylab("Number of object words produced") +
  xlab("Speaker") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5))

# Figure.object.presence <- ggplot(data=subset(object.presence, audio_video == "video"),
#                                  aes(x=SibGroup, y=(PC*100), color = SibGroup, fill = SibGroup)) +
#   geom_violin(alpha = .3) +
#   stat_summary(fun.y=mean, geom = "point", aes(group = subj), shape=1, size=1.5, stroke = 1, position = position_jitter(.03)) +
#   stat_summary(fun.data=mean_cl_boot, geom="pointrange", shape=17, size=.5, colour='black') +
#   scale_x_discrete(name=element_blank(),limits=c("None", "One", "2+")) +
#   theme_bw(base_size = 18) +
#   ylab('% input words with object presence') +
#   theme(legend.position='none')
#################
#eb2cl: i think this what you want instead?
object.presence_fixed <- subset(object.presence, audio_video=="video") %>% 
  group_by(subj, SibGroup) %>%
  summarise(mean.objpresence = mean(PC))

Figure.object.presence_fixed <- ggplot(data=object.presence_fixed, 
       aes(x=SibGroup, y=(mean.objpresence*100), color = SibGroup, fill = SibGroup)) +
  geom_violin(alpha = .3) +
  geom_point(shape = 5, size = 2, position = position_jitter(width = .1)) +
  #stat_summary(fun.y=mean, geom = "point", aes(group = subj), shape=1, size=1.5, stroke = 1, position = position_jitter(.03)) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", shape=23, size=.5, colour='black') + 
  scale_x_discrete(name=element_blank(),limits=c("None", "One", "2+")) + 
  theme_bw(base_size = 18) +
  ylab('% input words with object presence') +
  theme(legend.position='none')

#################
sib.presence.OP <-
  object.presence %>%
  filter(audio_video == "video" & SibGroup != "None") %>%
  left_join(sib.presence)

Figure.object.presence.sibling <- ggplot(sib.presence.OP, 
                                         aes(x=SibGroup, y=(PC*100), shape = SibGroup, color = sib.present)) +
  geom_point(aes(group=sib.present), size = 3, position=position_dodge(-.2)) +
  stat_summary(fun.data=mean_cl_boot, geom = "pointrange", aes(shape=SibGroup, fill = sib.present), 
               size=1, position = position_dodge(-.2), colour = "grey32", show.legend = FALSE) +
  stat_summary(fun.y = mean, aes(group=sib.present, colour=sib.present), geom='line', size=.8, 
               position = position_dodge(-.2), show.legend = FALSE) +
  scale_shape_manual(values=c(23,25)) +
  xlab("Sibling Group") +
  ylab('% input words with object presence') +
  #ggtitle("Object presence") +
  theme_bw(base_size=11) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size=14),
        legend.title = element_blank(),
        legend.position="bottom") +
  guides(shape = FALSE,
         color = guide_legend(override.aes = list(shape=15))) +
  facet_wrap(vars(month), nrow=2)

sib.presence.input <-
  speaker.type %>%
  filter(audio_video == "video" & SibGroup != "None"& caregiver %in% c("CG1","CG2")) %>%
  left_join(sib.presence)

Figure.input.sibling <- ggplot(sib.presence.input, 
                                         aes(x=SibGroup, y=(n), shape = SibGroup, color = sib.present)) +
  geom_point(aes(group=sib.present), size = 3, position=position_dodge(-.2)) +
  stat_summary(fun.data=mean_cl_boot, geom = "pointrange", aes(shape=SibGroup, fill = sib.present), 
               size=1, position = position_dodge(-.2), colour = "grey32", show.legend = FALSE) +
  stat_summary(fun.y = mean, aes(group=sib.present, colour=sib.present), geom='line', size=.8,
               position = position_dodge(-.2), show.legend = FALSE) +
  scale_shape_manual(values=c(23,25)) +
  xlab("Sibling Group") +
  ylab('N object words produced by caregivers') +
  theme_bw(base_size=11) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size=14),
        legend.title = element_blank(),
        legend.position="bottom") +
  guides(shape = FALSE,
         color = guide_legend(override.aes = list(shape=15))) +
  facet_wrap(vars(month), nrow=2)

