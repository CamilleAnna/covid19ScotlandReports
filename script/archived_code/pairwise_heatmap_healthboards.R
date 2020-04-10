
# EPIDEMIC PROGRESSION -  SCOTTISH HB ----


setwd('/Users/s1687811/Documents/GitHub/covid19/')
today<- Sys.Date() - 2
source('/Users/s1687811/Documents/GitHub/covid19/script/sourced_functions_doublingTime_reports.R') 
load(paste0('/Users/s1687811/Documents/GitHub/covid19/output/AG_briefing_analysis_output_', today, '.RData'))

test.HB<- d.cases %>%
  gather('Health_board', 'cumCases', 2:ncol(.)) %>%
  left_join(pops.shb, by = 'Health_board') %>%
  mutate(cumCases10k = cumCases * (10000/popsize)) %>%
  select(-cumCases, -popsize) %>%
  spread('Health_board', 'cumCases10k')


# Do it for all pairwise comparison (could use it to make a heatmap..?)
pairwise.comp.HB<- 
  expand.grid(
    data.frame(focal = colnames(test.HB)[-1],
               versus = colnames(test.HB[-1]))
  )
pairwise.comp.HB$epidemic.diff<- NA
pairwise.comp.HB$focal<- as.character(pairwise.comp.HB$focal)
pairwise.comp.HB$versus<- as.character(pairwise.comp.HB$versus)
for(i in 1:nrow(pairwise.comp.HB)){
  
  pairwise.comp.HB$epidemic.diff[i]<- epidemic.diff(test.HB, pairwise.comp.HB$focal[i], pairwise.comp.HB$versus[i])
  
}


pairwise.comp.HB.df<-
  pairwise.comp.HB[order(pairwise.comp.HB$epidemic.diff, na.last = TRUE, decreasing = TRUE), ]
pairwise.comp.HB.df$epidemic.diff.text<- formatC(pairwise.comp.HB.df$epidemic.diff, digits = 1, format = "f")


# THE PROBLEM OF HB ORDERING LIKELY OCCURS HERE, WHEN DEFINING THE ORDER OF FACTORS
pairwise.comp.HB.df$focal<- factor(pairwise.comp.HB.df$focal, levels = rev(unique(as.character(pairwise.comp.HB.df$focal))))

pairwise.comp.HB.df$versus<- factor(pairwise.comp.HB.df$versus, levels = unique(as.character(pairwise.comp.HB.df$versus)))

upper<- pairwise.comp.HB.df[-which(na.omit(pairwise.comp.HB.df$epidemic.diff) < 0),]




ggplot(pairwise.comp.HB.df, aes(x = versus, y = focal, fill= epidemic.diff)) +
  geom_tile()+
  xlab('')+ylab('')+
  scale_fill_gradient2(low="forestgreen", high="firebrick", mid = "white", midpoint = 0) +
  #geom_text(aes(label = epidemic.diff.text, x = `compared to`, y = `focal country`), size = 10, fontface = 2)+
  scale_x_discrete(position = "top")+
  theme_bw()+
  labs(colour = 'yo')+
  theme(legend.position="bottom",
        #legend.text = element_text(size = 35),
        legend.title = element_text(face = 2, size = 10),
        panel.border= element_rect(color = 'black', size = 0.5),
        axis.text.y = element_text(face="bold", colour="black", size=15),
        axis.text.x = element_text(colour="black", face="bold", size=15, angle = 90, hjust = 0, vjust = 0.5),
        #axis.title.y = element_text(face="bold", colour="black", size=25),
        #axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))



ggplot(upper, aes(x = versus, y = focal, fill= epidemic.diff)) +
  geom_tile()+
  xlab('')+ylab('')+
  scale_fill_gradient(low="white", high="firebrick") +
  #geom_text(aes(label = epidemic.diff.text, x = `compared to`, y = `focal country`), size = 10, fontface = 2)+
  scale_x_discrete(position = "top")+
  theme_bw()+
  labs(colour = 'yo')+
  theme(legend.position="bottom",
        #legend.text = element_text(size = 35),
        legend.title = element_text(face = 2, size = 10),
        panel.border= element_rect(color = 'black', size = 0.5),
        axis.text.y = element_text(face="bold", colour="black", size=15),
        axis.text.x = element_text(colour="black", face="bold", size=15, angle = 90, hjust = 0, vjust = 0.5),
        #axis.title.y = element_text(face="bold", colour="black", size=25),
        #axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.line.x = element_line(color="black", size = 0.5),
        plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5),
        panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))





