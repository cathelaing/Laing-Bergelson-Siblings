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
  #geom_point(shape = 1, size = 2, position = position_jitter(.1)) +
  stat_summary(fun= mean, geom = "point", aes(group = subj), shape = 1, size = 2, position = position_jitter(.1)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", shape = 17, size = 1) +
  facet_wrap(~SibGroup, ncol=3) +
  scale_x_discrete(limits = c("MOT", "FAT", "SIBLING", "Total.input"), labels = c("Mother", "Father", "Sibling", "Total input")) +
  ylab("Number of object words produced") +
  xlab("Speaker") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 0.5))


Figure.object.presence <- ggplot(data=subset(object.presence, audio_video == "video"), 
                                 aes(x=SibGroup, y=(PC*100), color = SibGroup, fill = SibGroup)) +
  geom_violin(alpha = .3) +
  stat_summary(fun.y=mean, geom = "point", aes(group = subj), shape=1, size=1.5, stroke = 1, position = position_jitter(.03)) +
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", shape=17, size=.5, colour='black') + 
  scale_x_discrete(name=element_blank(),limits=c("None", "One", "2+")) + 
  theme_bw(base_size = 18) +
  ylab('% input words with object presence') +
  theme(legend.position='none')

sib.presence.OP <-
  object.presence %>%
  filter(audio_video == "video" & SibGroup != "None") %>%
  left_join(sib.presence)

Figure.object.presence.sibling <- ggplot(sib.presence.OP, 
                                         aes(x=SibGroup, y=(PC*100), shape = SibGroup, color = sib.present)) +
  geom_point(aes(group=sib.present), size = 5, position=position_dodge(-.2)) +
  stat_summary(fun.data=mean_cl_boot, geom = "pointrange", aes(shape=SibGroup, fill = sib.present), 
               size=1.5, position = position_dodge(-.2), colour = "grey32", show.legend = FALSE) +
  stat_summary(fun.y = mean, aes(group=sib.present, colour=sib.present), geom='line', size=.8, 
               position = position_dodge(-.2), show.legend = FALSE) +
  scale_shape_manual(values=c(23,25)) +
  xlab("Sibling Group") +
  ylab('% input words with object presence') +
  ggtitle("Object presence") +
  theme_bw(base_size=11) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size=14),
        legend.position = "none")
  #       legend.title = element_blank(),
  #       legend.justification=c(1,1), legend.position=c(.99,1)) +
  # guides(shape = FALSE,
  #        color = guide_legend(override.aes = list(shape=15)))

sib.presence.input <-
  speaker.type %>%
  filter(audio_video == "video" & SibGroup != "None"& caregiver %in% c("CG1","CG2")) %>%
  left_join(sib.presence)

Figure.input.sibling <- ggplot(sib.presence.input, 
                                         aes(x=SibGroup, y=(n), shape = SibGroup, color = sib.present)) +
  geom_point(aes(group=sib.present), size = 5, position=position_dodge(-.2)) +
  stat_summary(fun.data=mean_cl_boot, geom = "pointrange", aes(shape=SibGroup, fill = sib.present), 
               size=1.5, position = position_dodge(-.2), colour = "grey32", show.legend = FALSE) +
  stat_summary(fun.y = mean, aes(group=sib.present, colour=sib.present), geom='line', size=.8,
               position = position_dodge(-.2), show.legend = FALSE) +
  scale_shape_manual(values=c(23,25)) +
  xlab("Sibling Group") +
  ylab('Number of object words produced by caregivers') +
  ggtitle("Caregiver input") +
  theme_bw(base_size=11) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size=14),
        legend.title = element_blank(),
        legend.justification=c(1,1), legend.position=c(.99,.99)) +
  guides(shape = FALSE,
         color = guide_legend(override.aes = list(shape=15)))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#sib.presence.plots <- multiplot(Figure.input.sibling, Figure.object.presence.sibling, cols=2)
