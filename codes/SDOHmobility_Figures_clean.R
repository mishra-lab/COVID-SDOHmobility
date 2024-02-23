library(ggplot2)
library(tidyverse)
library(lubridate)
library(cowplot)
library(grid)
library(viridis)
library(openxlsx)

# setwd("C:/Users/wangs/OneDrive/Documents/GitHub/SDOH-Mobility/Rfiles")
setwd("C:/Users/WangSiyi/Documents/GitHub/SDOH-Mobility/Rfiles")



mobilityCT_PHU <- read.csv("../Data/AggregatedMobility/mobilityCT_PHU.csv")
mobilityCT_PHU$wk_day_1 <- as.Date(mobilityCT_PHU$wk_day_1)
mobilityCT_PHU$PHU_ID <- as.factor(mobilityCT_PHU$PHU_ID)


mobilityCT_ATIPPEQ <- read.csv("../Data/AggregatedMobility/mobilityCT_ATIPPEQ.csv")
# Recode ATIPPE Quintile from high to low levels
mobilityCT_ATIPPEQ = mobilityCT_ATIPPEQ %>%
  mutate(ATIPPE_quintile=as.numeric(case_when(ATIPPE_quintile == "1" ~"5",
                                              ATIPPE_quintile == "2" ~"4",
                                              ATIPPE_quintile == "3" ~"3",
                                              ATIPPE_quintile == "4" ~"2",
                                              ATIPPE_quintile == "5" ~"1")))
mobilityCT_ATIPPEQ$wk_day_1 <- as.Date(mobilityCT_ATIPPEQ$wk_day_1)
mobilityCT_ATIPPEQ$ATIPPE_quintile <- as.factor(mobilityCT_ATIPPEQ$ATIPPE_quintile)


mobilityCT_EssentialServiceQ <-read.csv("../Data/AggregatedMobility/mobilityCT_EssentialServiceQ.csv")
mobilityCT_EssentialServiceQ$wk_day_1 <- as.Date(mobilityCT_EssentialServiceQ$wk_day_1)
mobilityCT_EssentialServiceQ$employ_sales.trades.manufacturing.agriculture_quintile <- as.factor(mobilityCT_EssentialServiceQ$employ_sales.trades.manufacturing.agriculture_quintile)



mobilityCT_PHU <- mobilityCT_PHU %>% mutate(PHUnames=case_when(PHU_ID == "2230" ~"Durham",
                                                               PHU_ID == "2236" ~"Halton",
                                                               PHU_ID == "2253" ~"Peel",
                                                               PHU_ID == "3895" ~"Toronto",
                                                               PHU_ID == "2270" ~"York"))

############################################

# 1239 CTs
mcSDOH <-read.csv("../Data/AggregatedMobility/mcSDOH_ct_0wklag.csv")
length(unique(mcSDOH$CTID))

# Recode ATIPPE Quintile from high to low levels
mcSDOH = mcSDOH %>%
  mutate(ATIPPE_quintile=as.numeric(case_when(ATIPPE_quintile == "1" ~"5",
                                              ATIPPE_quintile == "2" ~"4",
                                              ATIPPE_quintile == "3" ~"3",
                                              ATIPPE_quintile == "4" ~"2",
                                              ATIPPE_quintile == "5" ~"1")))


# For weekly COVID-19 cases we exclude LTCH cases
covidCT = subset(mcSDOH, week >= 9 & week <= 51) %>%
  group_by(covid_wk_day_1) %>%
  summarise(Iwk=sum(Iwk)-sum(Iwk_LTCH),
            population=sum(CT_pop),
            infect_rate=Iwk/population)
covidCT$covid_wk_day_1 <- as.Date(covidCT$covid_wk_day_1)


covidCT_PHU = subset(mcSDOH, week >= 9 & week <= 51) %>%
  group_by(PHU, HUID, covid_wk_day_1) %>%
  summarise(Iwk=sum(Iwk)-sum(Iwk_LTCH),
            population=sum(CT_pop),
            infect_rate=Iwk/population)
covidCT_PHU$covid_wk_day_1 <- as.Date(covidCT_PHU$covid_wk_day_1)


covidCT_ATIPPEQ = subset(mcSDOH, week >= 9 & week <= 51) %>%
  group_by(ATIPPE_quintile,covid_wk_day_1) %>%
  summarise(Iwk=sum(Iwk)-sum(Iwk_LTCH),
            population=sum(CT_pop),
            infect_rate=Iwk/population)
covidCT_ATIPPEQ$covid_wk_day_1 <- as.Date(covidCT_ATIPPEQ$covid_wk_day_1)
covidCT_ATIPPEQ$ATIPPE_quintile <- as.factor(covidCT_ATIPPEQ$ATIPPE_quintile)


covidCT_EssentialServiceQ = subset(mcSDOH, week >= 9 & week <= 51) %>%
  group_by(employ_sales.trades.manufacturing.agriculture_quintile,covid_wk_day_1) %>%
  summarise(Iwk=sum(Iwk)-sum(Iwk_LTCH),
            population=sum(CT_pop),
            infect_rate=Iwk/population)

covidCT_EssentialServiceQ$covid_wk_day_1 <- as.Date(covidCT_EssentialServiceQ$covid_wk_day_1)
covidCT_EssentialServiceQ$employ_sales.trades.manufacturing.agriculture_quintile <-
  as.factor(covidCT_EssentialServiceQ$employ_sales.trades.manufacturing.agriculture_quintile)


###############################################################################
# fourty = unique(mobilitySDOH_CT$CTID)
# thirtynine = unique(mcSDOH$CTID)
# thirtynine = unique(policy1$CTID)
# no_match = fourty[is.na(match(fourty,thirtynine))]
# 
# missing = mobilitySDOH_CT %>% filter(CTID == as.character(5350009)) %>% filter(week %in% c(9:11,13:15))
# missing = mobilitySDOH_CT %>% filter(CTID == as.character(5350378.18)) # for covid

####################### Figure 1 ############################

# timeline of COVID-19 policies

restriction1 <- grobTree(grid::textGrob(expression(1^st~Restriction), rot = 0, gp=gpar(fontsize=13, col = 'red')))
restriction2 <- grobTree(grid::textGrob(expression(2^nd~Restriction), rot = 0, gp=gpar(fontsize=13, col = 'red')))

p1 <- ggplot(covidCT,aes(x=covid_wk_day_1,y=Iwk)) + 
  geom_line(size=1.2, alpha = 0.4)+ xlab("Week (2020)") + ylab("Greater Toronto Area weekly new COVID-19 cases") +
  ylim(0,15000)+
  scale_x_date(breaks = seq(as.Date("2020-02-23"), as.Date("2020-12-13"), by = "3 week"), 
               date_labels = "%b %d") +
  # Make annotation above the box in the margin
  coord_cartesian(clip = "off") +
  annotation_custom(restriction1, 
                    xmin = as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    ymin =17000) +
  annotation_custom(restriction2, 
                    xmin = as.Date("Dec 01, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Dec 01, 2020", format="%b %d, %Y"), 
                    ymin =17000) +
  
  geom_vline(xintercept = as.numeric(as.Date("Mar 17, 2020", format="%b %d, %Y")), linetype=2, colour="red", size = 1) +
  annotate(geom = "text", label = expression(GTA(Mar~17)), color = "red",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Mar 17, 2020", format="%b %d, %Y"),
           y =12000,
           size = 4) +
  
  geom_vline(xintercept = as.numeric(as.Date("Nov 23, 2020", format="%b %d, %Y")), linetype=2, colour="red", size = 1) +
  annotate(geom = "text", label = expression(paste(Toronto,",",Peel~(Nov~23))), color = "red",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Nov 23, 2020", format="%b %d, %Y"),
           y =9950,
           size = 4) +
  
  geom_vline(xintercept = as.numeric(as.Date("Dec 14, 2020", format="%b %d, %Y")), linetype=2, colour="firebrick3", size = 1)+
  annotate(geom = "text", label = expression(York~(Dec~14)), color = "firebrick3",
           angle = 90,
           hjust = 0,
           vjust = 1,
           x =as.Date("Dec 14, 2020", format="%b %d, %Y"),
           y =11880,
           size = 4,
           alpha = 0.75) +
  
  geom_vline(xintercept = as.numeric(as.Date("Dec 26, 2020", format="%b %d, %Y")), linetype=2, colour="firebrick3", size = 1)+
  annotate(geom = "text", label = expression(paste(Durham,",",Halton~(Dec~26))), color = "firebrick3",
           angle = 90,
           hjust = 0,
           vjust = 1,
           x =as.Date("Dec 26, 2020", format="%b %d, %Y"),
           y =9380,
           size = 4,
           alpha = 0.75) +
  
  # Add shades for pre/post periods for two restrictions
  # Pre-lockdown for 1st restriction
  annotate('rect', xmin=as.Date("Feb 23, 2020", format="%b %d, %Y"), xmax=as.Date("Mar 14, 2020", format="%b %d, %Y"), ymin=-Inf, ymax=Inf, alpha=.2, fill='blue') + 
  annotate(geom = "text", label = "Pre-restriction period", color = "blue",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Mar 05, 2020", format="%b %d, %Y"),
           y =3000,
           size = 5,
           alpha = .65) +
  
  # Post-lockdown for 1st restriction
  annotate('rect', xmin=as.Date("Mar 22, 2020", format="%b %d, %Y"), xmax=as.Date("Apr 11, 2020", format="%b %d, %Y"), ymin=-Inf, ymax=Inf, alpha=.2, fill='red') + 
  annotate(geom = "text", label = "Post-restriction period", color = "red",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Apr 01, 2020", format="%b %d, %Y"),
           y =3000,
           size = 5,
           alpha = .65) +
  
  # Pre-lockdown for 2nd restriction
  annotate('rect', xmin=as.Date("Nov 01, 2020", format="%b %d, %Y"), xmax=as.Date("Nov 21, 2020", format="%b %d, %Y"), ymin=-Inf, ymax=Inf, alpha=.2, fill='blue') + 
  annotate(geom = "text", label = "Pre-restriction period for Toronto and Peel", color = "blue",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Nov 11, 2020", format="%b %d, %Y"),
           y =500,
           size = 5,
           alpha = .65) +
  # Post-lockdown for 2nd restriction
  annotate('rect', xmin=as.Date("Nov 29, 2020", format="%b %d, %Y"), xmax=as.Date("Dec 19, 2020", format="%b %d, %Y"), ymin=-Inf, ymax=Inf, alpha=.2, fill='red') +
  annotate(geom = "text", label = "Post-restriction period for Toronto and Peel", color = "red",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Dec 08, 2020", format="%b %d, %Y"),
           y =500,
           size = 5,
           alpha = .65) +
  
  
  
  theme(plot.margin=margin(t=40,
                           r=20),
        panel.background=element_rect(fill="white",colour="grey20"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 11),
        axis.text.y = element_text(size = 11),
        #plot.title = element_text(hjust=0.5, face="bold", size=34),
        axis.title = element_text(hjust=0.5, size=13),
        legend.title = element_text(hjust=0, size=11),
        legend.text=element_text(size=11),
        legend.key = element_rect(fill="white",colour=NA),
        legend.key.size = unit(1, 'cm'),
        legend.key.width= unit(1.2, 'cm')) 
p1

ggsave(p1, file= "../Plots/MobilitySDOH_Figures/new/covid_timelines.png", width = 9, height = 6)
# png("../Plots/MobilitySDOH_Figures/new/covid_timelines.png", width = 1100, height = 700, units = "px",
#     res = 120)
# p1
# dev.off()

########################
########### Appendix Figure A3 detailed policy timeline for the second restriction ############################

# Separate policy timeline plot

# 2nd restriction
# Descriptive
restriction1 <- grobTree(grid::textGrob(expression(1^st~Restriction), rot = 0, gp=gpar(fontsize=11, col = 'red')))
restriction2 <- grobTree(grid::textGrob(expression(2^nd~Restriction), rot = 0, gp=gpar(fontsize=11, col = 'red')))

# 2nd restriction
# Descriptive
covidCT_TorPee = subset(mcSDOH, week >= 40 & week <= 51) %>%
  filter(PHU %in% c("City of Toronto Health Unit","Peel Regional Health Unit"))%>%
  group_by(covid_wk_day_1) %>%
  summarise(Iwk=sum(Iwk)-sum(Iwk_LTCH),
            population=sum(CT_pop),
            infect_rate=Iwk/population)
covidCT_TorPee$covid_wk_day_1 <- as.Date(covidCT_TorPee$covid_wk_day_1)

pa31 <- ggplot(covidCT_TorPee,
             aes(x=covid_wk_day_1,y=Iwk)) + 
  geom_line(size=1.2, alpha = 0.4)+ xlab("Week (2020)") + ylab("Weekly new COVID-19 cases for intervention group \n(Toronto and Peel)") +
  ylim(0,15000)+
  scale_x_date(breaks = seq(as.Date("2020-09-27"), as.Date("2020-12-26"), by = "2 week"), 
               date_labels = "%b %d") +
  # Make annotation above the box in the margin
  coord_cartesian(clip = "off") +
  annotation_custom(restriction2, 
                    xmin = as.Date("Nov 23, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Nov 23, 2020", format="%b %d, %Y"), 
                    ymin =17000) +
  
  
  geom_vline(xintercept = as.numeric(as.Date("Nov 23, 2020", format="%b %d, %Y")), linetype=2, colour="red", size = 1) +
  annotate(geom = "text", label = expression(paste(Toronto,",",Peel~(Nov~23))), color = "red",
           angle = 90,
           hjust = 0,
           vjust = 1,
           x =as.Date("Nov 23, 2020", format="%b %d, %Y"),
           y =7000,
           size = 4) +
  
  # Pre-lockdown for 2nd restriction
  annotate('rect', xmin=as.Date("Nov 01, 2020", format="%b %d, %Y"), xmax=as.Date("Nov 21, 2020", format="%b %d, %Y"), ymin=-Inf, ymax=Inf, alpha=.2, fill='blue') + 
  annotate(geom = "text", label = "Pre-restriction period", color = "blue",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Nov 11, 2020", format="%b %d, %Y"),
           y =3000,
           size = 5,
           alpha = .65) +
  # Post-lockdown for 2nd restriction
  annotate('rect', xmin=as.Date("Nov 29, 2020", format="%b %d, %Y"), xmax=as.Date("Dec 19, 2020", format="%b %d, %Y"), ymin=-Inf, ymax=Inf, alpha=.2, fill='red') + 
  annotate(geom = "text", label = "Post-restriction period", color = "red",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Dec 08, 2020", format="%b %d, %Y"),
           y =3000,
           size = 5,
           alpha = .65) +
  theme(plot.margin=margin(t=40, r=10, l=5),
        panel.background=element_rect(fill="white",colour="grey20"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10),
        #plot.title = element_text(hjust=0.5, face="bold", size=34),
        axis.title = element_text(hjust=0.5, size=10),
        legend.title = element_text(hjust=0, size=10),
        legend.text=element_text(size=10),
        legend.key = element_rect(fill="white",colour=NA),
        legend.key.size = unit(1, 'cm'),
        legend.key.width= unit(1.2, 'cm')) 
pa31



ggsave(pa31, file= "../Plots/MobilitySDOH_Figures/covid_timelines2_TorPeel_partial.png", width = 5, height = 4.5)
# png("../Plots/MobilitySDOH_Figures/covid_timelines2_TorPeel_partial.png", width = 5600, height = 2800, units = "px",
#     res = 300)
# p1
# dev.off()
#####################

# Control group
covidCT_DurHalYrk = subset(mcSDOH, week >= 40 & week <= 51) %>%
  filter(PHU %in% c("Durham Regional Health Unit","Halton Regional Health Unit", "York Regional Health Unit")) %>%
  group_by(covid_wk_day_1) %>%
  summarise(Iwk=sum(Iwk)-sum(Iwk_LTCH),
            population=sum(CT_pop),
            infect_rate=Iwk/population)
covidCT_DurHalYrk$covid_wk_day_1 <- as.Date(covidCT_DurHalYrk$covid_wk_day_1)

pa32 <- ggplot(covidCT_DurHalYrk,
             aes(x=covid_wk_day_1,y=Iwk)) + 
  geom_line(size=1.2, alpha = 0.4)+ xlab("") + ylab("Weekly new COVID-19 cases for control group \n(York, Durham and Halton)") +
  ylim(0,15000)+
  scale_x_date(breaks = seq(as.Date("2020-09-27"),as.Date("2020-12-26"), by = "2 week"), 
               date_labels = "%b %d") +
  # Make annotation above the box in the margin
  coord_cartesian(clip = "off") +
  
  annotation_custom(restriction2, 
                    xmin = as.Date("Dec 15, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Dec 15, 2020", format="%b %d, %Y"), 
                    ymin =17000) +
  geom_vline(xintercept = as.numeric(as.Date("Dec 14, 2020", format="%b %d, %Y")), linetype=2, colour="firebrick3", size = 1)+
  annotate(geom = "text", label = expression(York~(Dec~14)), color = "firebrick3",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Dec 14, 2020", format="%b %d, %Y"),
           y =10000,
           size = 4,
           alpha = 1) +
  
  geom_vline(xintercept = as.numeric(as.Date("Dec 26, 2020", format="%b %d, %Y")), linetype=2, colour="firebrick3", size = 1)+
  annotate(geom = "text", label = expression(paste(Durham,",",Halton~(Dec~26))), color = "firebrick3",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Dec 26, 2020", format="%b %d, %Y"),
           y =6000,
           size = 4,
           alpha = 1) +
  
  # Add shades for pre/post periods for two restrictions
  
  
  # Pre-lockdown for 2nd restriction
  annotate('rect', xmin=as.Date("Nov 01, 2020", format="%b %d, %Y"), xmax=as.Date("Nov 21, 2020", format="%b %d, %Y"), ymin=-Inf, ymax=Inf, alpha=.2, fill='blue') + 
  annotate(geom = "text", label = "Pre-restriction period", color = "blue",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Nov 11, 2020", format="%b %d, %Y"),
           y =3000,
           size = 5,
           alpha = .65) +
  # Post-lockdown for 2nd restriction
  annotate('rect', xmin=as.Date("Nov 29, 2020", format="%b %d, %Y"), xmax=as.Date("Dec 19, 2020", format="%b %d, %Y"), ymin=-Inf, ymax=Inf, alpha=.2, fill='blue') + 
  annotate(geom = "text", label = "Pre-restriction period", color = "blue",
           angle = 90,
           hjust = 0,
           vjust = 0,
           x =as.Date("Dec 08, 2020", format="%b %d, %Y"),
           y =3000,
           size = 5,
           alpha = .65) +
  theme(plot.margin=margin(t=40, r=10, l=5),
        panel.background=element_rect(fill="white",colour="grey20"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10),
        #plot.title = element_text(hjust=0.5, face="bold", size=34),
        axis.title = element_text(hjust=0.5, size=10),
        legend.title = element_text(hjust=0, size=10),
        legend.text=element_text(size=10),
        legend.key = element_rect(fill="white",colour=NA),
        legend.key.size = unit(1, 'cm'),
        legend.key.width= unit(1.2, 'cm')) 
pa32

ggsave(pa32, file= "../Plots/MobilitySDOH_Figures/covid_timelines2_DurHalYrk_onecurve_partial.png", width = 5, height = 4.5)

# png("../Plots/MobilitySDOH_Figures/covid_timelines2_DurHalYrk_onecurve_partial.png", width = 2800, height = 2800, units = "px",
#     res = 300)
# p2
# dev.off()
####################### 
# Make panel

library(ggpubr)

pa3 = ggarrange(pa31,pa32,
                    align = "hv",
                    nrow = 2,
                    ncol = 1,
                    common.legend = F,
                    legend = NULL,
                    labels = 'AUTO')

pa3


ggsave(file= "../Plots/MobilitySDOH_Figures/COVID_timeline2_appendix_onecurve_partial.png", bg = 'white',                                     
       width = 5, height = 8.5, limitsize = FALSE, pa3)


# labels_with_superscript <-  c(as.character(as.expression("Public health units that had "*2^nd~restriction)),
#                               as.character(expression("Control group for "*2^nd~restriction)))
# 
# 
# # png for SDOH mobility and covid panel
# library(cowplot)
# png("../Plots/MobilitySDOH_Figures/COVID_timeline2_appendix_onecurve_partial.png", width = 6000, height = 3000, units = "px",
#     res = 300)
# plot_grid( p1, p2,nrow=1, ncol=2,
#            scale=0.9) +
#   draw_plot_label(labels_with_superscript, x = c(-0.08, 0.45), y = c(1, 1), size = 22, parse = TRUE)
# dev.off()



####################### Figure 2 ############################

# Flow chart for data process

# Flow diagram

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)


flowchart <- grViz("digraph flowchart {
      graph [layout =  dot, fontsize = 11]
      splines = false;
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, fontsize = 11] 
      
      tab1 [label = '5 PHUs in the Greater Toronto Area \n(N=1261 CTs)']
      tab2 [label = 'socioeconomic factors data \n(N=1254 CTs)']
      tab3 [label = 'weekly mobility data \n(N=1240 CTs, week=42)']
      tab4 [label = 'weekly mobility data merged \nwith socioeconomic factor \nin 5 PHUs in Greater Toronto Area \n(N=1240 CTs, week=42)']
      tab6 [label = <1<FONT POINT-SIZE='8'><SUp>st</SUp></FONT>  restriction: <br/>5 PHUs, 6 weeks <br/>(N = 1239 CTs)> ]
      tab7 [label = <2<FONT POINT-SIZE='8'><SUp>nd</SUp></FONT>  restriction: <br/>Toronto/Peel PHUs, <br/>6 weeks <br/>(N = 803 CTs)>]
      tab9 [label = <2<FONT POINT-SIZE='8'><SUp>nd</SUp></FONT>  restriction: <br/>5 PHUs, 6 weeks <br/>(N = 1240 CTs)>]
      
      node [fontname = Helvetica, shape = ellipse, fontsize = 11]
      tab5 [label = 'descriptive analyses']
      tab8 [label = 'difference-in-differences \nanalyses by mixed-effect models']


      # edge definitions with the node IDs
      tab1 -> tab2[headlabel = '    exclude CTs without \nsocioeconomic \nfactor data (N=7)', labeldistance=6, labelangle=75, minlen = 1.5, fontname = Helvetica, fontsize = 11];
      tab1 -> tab3[label = ' exclude CTs without \n socioeconomic factor \ndata (N=7) and mobility \ndata (N=14)', labeldistance=5.5, labelangle=68, minlen = 1.5, fontname = Helvetica, fontsize = 11];
      tab2 -> tab4[label = '         merge', labelloc = b, minlen = 1.5, fontname = Helvetica, fontsize = 11];
      tab3 -> tab4[label = '', minlen = 1.5];
      tab4 -> tab5[label = '', minlen = 1.5];
      tab5 -> tab6[headlabel = 'exclude CTs without at \nleast one mobility data for \npre- and post-restriction \nperiods (N=1)', labeldistance=6, labelangle=75, minlen = 2, fontname = Helvetica, fontsize = 11];
      tab5 -> tab7[label = '', minlen = 2];
      tab4 -> tab8;
      tab8 -> tab9[label = '', minlen = 2];
      }

      ")
flowchart

# setwd("C:/Users/WangSiyi/Documents/GitHub/SDOH-Mobility/Rfiles")
# png("../Plots/MobilitySDOH_Figures/Flow_Chart.png", width = 550, height = 600, units = "px",
#     res = 300)
# flowchart
# dev.off()

# export_graph(flowchart,
#              file_name = "../Plots/MobilitySDOH_Figures/Flow_Chart.png",
#              file_type = "png")
# 
# save_graph(x=flowchart, file="../Plots/MobilitySDOH_Figures/Flow_Chart.png")

# Export Function for lavaanPlot Obtained from github
# https://github.com/alishinski/lavaanPlot/blob/export_plots/R/plotExportFunctions.R

save_png <- function(plot, path){
  DiagrammeRsvg::export_svg(plot) %>%
    charToRaw() %>%
    rsvg::rsvg() %>%
    png::writePNG(path)
}

save_png(flowchart, "../Plots/MobilitySDOH_Figures/Flow_Chart.png")


####################### Figure 3 ############################

# Four-panel figures for both epidemic curves and mobility patterns

type <- c("2020" = "solid", "2019" = "dashed")
restriction1 <- grobTree(grid::textGrob(expression(1^st~restriction), rot = 0, gp=gpar(fontsize=11)))
restriction2 <- grobTree(grid::textGrob(expression(2^nd~restriction), rot = 0, gp=gpar(fontsize=11)))

mobilityCT_ATIPPEQ_r <- subset(mobilityCT_ATIPPEQ, week >= 9 & week <= 51)
mobilityCT_EssentialServiceQ_r <- subset(mobilityCT_EssentialServiceQ, week >= 9 & week <= 51)

# ATIPPE Quintile COVID curve
# Reorder quintile levels so that the legend is from 5 to 1
covidCT_ATIPPEQ$ATIPPE_quintile <- factor(covidCT_ATIPPEQ$ATIPPE_quintile, 
                                          levels = rev(levels(covidCT_ATIPPEQ$ATIPPE_quintile)))
# # Infection rate not weekly cases
# p31 <- ggplot(covidCT_ATIPPEQ,aes(x=covid_wk_day_1,y=infect_rate, colour=ATIPPE_quintile)) +
#   geom_line(size=0.85)+ xlab("") + ylab("Weekly new COVID-19 cases") +
#   ylim(0,0.0027)+
#   # Reverse legend quintile from top to bottom to align with the line graph (visually easier on the eye that way)
#   scale_color_viridis(discrete = TRUE, direction = -1, name = "Income quintile",
#                       labels = rev(c("1 = Highest \n      income", "2", "3", "4", "5 = Lowest \n      income"))) +
#   scale_x_date(breaks = seq(as.Date("2020-02-23"), as.Date("2020-12-13"), by = "6 week"),
#                date_labels = "%b %d") +
#   # scale_x_date(date_breaks = "6 week", date_labels = "%b %d") +
#   # Make annotation above the box in the margin
#   coord_cartesian(clip = "off") +
#   geom_vline(xintercept = as.numeric(as.Date("Mar 16, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
#   geom_vline(xintercept = as.numeric(as.Date("Nov 23, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
#   annotation_custom(restriction1,
#                     xmin = as.Date("Mar 16, 2020", format="%b %d, %Y"),
#                     xmax =as.Date("Mar 16, 2020", format="%b %d, %Y"),
#                     ymin =4650) +
#   annotation_custom(restriction2,
#                     xmin = as.Date("Nov 10, 2020", format="%b %d, %Y"),
#                     xmax =as.Date("Nov 10, 2020", format="%b %d, %Y"),
#                     ymin =4650) +
# 
#   theme(plot.margin=margin(t=50),
#         panel.background=element_rect(fill="white",colour="grey20"),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
#         axis.text.y = element_text(size = 10),
#         #plot.title = element_text(hjust=0.5, face="bold", size=34),
#         axis.title = element_text(hjust=0.5, size=11),
#         legend.title = element_text(hjust=0, size=10),
#         legend.text=element_text(size=10),
#         legend.key = element_rect(fill="white",colour=NA),
#         legend.key.size = unit(0.7, 'cm'),
#         legend.key.width= unit(0.8, 'cm'))
# p31
# 
# p32 <- ggplot(covidCT_EssentialServiceQ,aes(x=covid_wk_day_1,y=infect_rate,
#                                             colour=employ_sales.trades.manufacturing.agriculture_quintile)) +
#   geom_line(size=0.85)+ xlab("") + ylab("Weekly new COVID-19 cases") +
#   ylim(0,0.0027)+
#   # Reverse legend quintile from top to bottom to align with the line graph (visually easier on the eye that way)
#   scale_color_viridis(discrete = TRUE, direction = -1, name = "Essential worker \nquintile",
#                       labels = rev(c("1 = Lowest \n   proportion", "2", "3", "4", "5 = Highest \n   proportion"))) +
#   scale_x_date(breaks = seq(as.Date("2020-02-23"), as.Date("2020-12-13"), by = "6 week"),
#                date_labels = "%b %d") +
#   # scale_x_date(date_breaks = "6 week", date_labels = "%b %d") +
#   # Make annotation above the box in the margin
#   coord_cartesian(clip = "off") +
#   geom_vline(xintercept = as.numeric(as.Date("Mar 16, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
#   geom_vline(xintercept = as.numeric(as.Date("Nov 23, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
#   annotation_custom(restriction1,
#                     xmin = as.Date("Mar 16, 2020", format="%b %d, %Y"),
#                     xmax =as.Date("Mar 16, 2020", format="%b %d, %Y"),
#                     ymin =4650) +
#   annotation_custom(restriction2,
#                     xmin = as.Date("Nov 10, 2020", format="%b %d, %Y"),
#                     xmax =as.Date("Nov 10, 2020", format="%b %d, %Y"),
#                     ymin =4650) +
#   theme(plot.margin=margin(t=50),
#         panel.background=element_rect(fill="white",colour="grey20"),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
#         axis.text.y = element_text(size = 10),
#         #plot.title = element_text(hjust=0.5, face="bold", size=34),
#         axis.title = element_text(hjust=0.5, size=11),
#         legend.title = element_text(hjust=0, size=10),
#         legend.text=element_text(size=10),
#         legend.key = element_rect(fill="white",colour=NA),
#         legend.key.size = unit(0.7, 'cm'),
#         legend.key.width= unit(0.8, 'cm'))
# 
# p32
# 

p31 <- ggplot(covidCT_ATIPPEQ,aes(x=covid_wk_day_1,y=Iwk, colour=ATIPPE_quintile)) + 
  geom_line(size=0.85)+ xlab("") + ylab("Weekly new COVID-19 cases") +
  ylim(0,4000)+
  # Reverse legend quintile from top to bottom to align with the line graph (visually easier on the eye that way)
  scale_color_viridis(discrete = TRUE, direction = -1, name = "Income quintile", 
                      labels = rev(c("1 = Highest \n      income", "2", "3", "4", "5 = Lowest \n      income"))) +
  scale_x_date(breaks = seq(as.Date("2020-02-23"), as.Date("2020-12-13"), by = "6 week"), 
               date_labels = "%b %d") +
  # scale_x_date(date_breaks = "6 week", date_labels = "%b %d") +
  # Make annotation above the box in the margin
  coord_cartesian(clip = "off") +
  geom_vline(xintercept = as.numeric(as.Date("Mar 16, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
  geom_vline(xintercept = as.numeric(as.Date("Nov 23, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
  annotation_custom(restriction1, 
                    xmin = as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    ymin =4650) +
  annotation_custom(restriction2, 
                    xmin = as.Date("Nov 10, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Nov 10, 2020", format="%b %d, %Y"), 
                    ymin =4650) +
  
  theme(plot.margin=margin(t=50),
        panel.background=element_rect(fill="white",colour="grey20"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10),
        #plot.title = element_text(hjust=0.5, face="bold", size=34),
        axis.title = element_text(hjust=0.5, size=11),
        legend.title = element_text(hjust=0, size=10),
        legend.text=element_text(size=10),
        legend.key = element_rect(fill="white",colour=NA),
        legend.key.size = unit(0.7, 'cm'),
        legend.key.width= unit(0.8, 'cm')) 
p31


# EssentialServices Quintile COVID curve
covidCT_EssentialServiceQ$employ_sales.trades.manufacturing.agriculture_quintile <- 
  factor(covidCT_EssentialServiceQ$employ_sales.trades.manufacturing.agriculture_quintile, 
         levels = rev(levels(covidCT_EssentialServiceQ$employ_sales.trades.manufacturing.agriculture_quintile)))

p32 <- ggplot(covidCT_EssentialServiceQ,aes(x=covid_wk_day_1,y=Iwk,
                                           colour=employ_sales.trades.manufacturing.agriculture_quintile)) + 
  geom_line(size=0.85)+ xlab("") + ylab("Weekly new COVID-19 cases") +
  ylim(0,4000)+
  # Reverse legend quintile from top to bottom to align with the line graph (visually easier on the eye that way)
  scale_color_viridis(discrete = TRUE, direction = -1, name = "Essential worker \nquintile", 
                      labels = rev(c("1 = Lowest \n   proportion", "2", "3", "4", "5 = Highest \n   proportion"))) +
  scale_x_date(breaks = seq(as.Date("2020-02-23"), as.Date("2020-12-13"), by = "6 week"), 
               date_labels = "%b %d") +
  # scale_x_date(date_breaks = "6 week", date_labels = "%b %d") +
  # Make annotation above the box in the margin
  coord_cartesian(clip = "off") +
  geom_vline(xintercept = as.numeric(as.Date("Mar 16, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
  geom_vline(xintercept = as.numeric(as.Date("Nov 23, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
  annotation_custom(restriction1, 
                    xmin = as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    ymin =4650) +
  annotation_custom(restriction2, 
                    xmin = as.Date("Nov 10, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Nov 10, 2020", format="%b %d, %Y"), 
                    ymin =4650) +
  theme(plot.margin=margin(t=50),
        panel.background=element_rect(fill="white",colour="grey20"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10),
        #plot.title = element_text(hjust=0.5, face="bold", size=34),
        axis.title = element_text(hjust=0.5, size=11),
        legend.title = element_text(hjust=0, size=10),
        legend.text=element_text(size=10),
        legend.key = element_rect(fill="white",colour=NA),
        legend.key.size = unit(0.7, 'cm'),
        legend.key.width= unit(0.8, 'cm')) 

p32


ggsave(p31, file= "../Plots/MobilitySDOH_Figures/covid_ATIPPEQ.png", width = 6, height = 5)
ggsave(p32, file= "../Plots/MobilitySDOH_Figures/covid_EssentialServicesQ.png", width = 6, height = 5)

# png("../Plots/MobilitySDOH_Figures/covid_ATIPPEQ.png", width = 700, height = 600, units = "px",
#     res = 120)
# p31
# dev.off()
# 
# 
# png("../Plots/MobilitySDOH_Figures/covid_EssentialServicesQ.png", width = 700, height = 600, units = "px",
#     res = 120)
# p32
# dev.off()

#######################

# ATIPPE Quintile
p33 <- ggplot(mobilityCT_ATIPPEQ_r) + 
  # Make annotation above the box in the margin
  coord_cartesian(clip = "off") +
  geom_line(size=0.85,
            aes(x=wk_day_1,y=100-percent_stay, colour=ATIPPE_quintile, linetype="2020"))+
  geom_line(size=0.85,
            aes(x=wk_day_1, y=100-base2019_percent_stay, colour=ATIPPE_quintile, linetype="2019"), alpha = 0.4) +
  labs(x="", y= "Mobility metric \n (% of devices that went outside home)", colour = "Mobility") +
  geom_vline(xintercept = as.numeric(as.Date("Mar 16, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
  # geom_text(size=6, aes(x=as.Date("Mar 16, 2020", format="%b %d, %Y"),
  #               y=37),label=expression(1^st~Restriction),hjust=0.3, parse=TRUE)+
  geom_vline(xintercept = as.numeric(as.Date("Nov 23, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
  # geom_text(size=6, aes(x=as.Date("Nov 23, 2020", format="%b %d, %Y"),
  #               y=45,label=expression(2^nd~Restriction),
  #           hjust=0.7, parse = TRUE)+
  annotation_custom(restriction1, 
                    xmin = as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    ymin =92) +
  annotation_custom(restriction2, 
                    xmin = as.Date("Nov 10, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Nov 10, 2020", format="%b %d, %Y"), 
                    ymin =92) +
  
  scale_color_viridis(discrete = TRUE, name = "Income quintile", 
                      labels = c("1 = Highest \n      income", "2", "3", "4", "5 = Lowest \n      income")) +
  scale_linetype_manual(values = type, name = " ") +
  # ggtitle("GTA Mobility Stratified by After-Tax Income (2019 and 2020)")+
  # scale_x_date(date_breaks = "6 week", date_labels = "%b %d") +
  scale_x_date(breaks = seq(as.Date("2020-02-23"), as.Date("2020-12-13"), by = "6 week"), 
               date_labels = "%b %d") +
  theme(plot.margin=margin(t=50),
        panel.background=element_rect(fill="white",colour="grey20"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10),
        #plot.title = element_text(hjust=0.5, face="bold", size=34),
        axis.title = element_text(hjust=0.5, size=11),
        legend.title = element_text(hjust=0, size=10),
        legend.text=element_text(size=10),
        legend.key = element_rect(fill="white",colour=NA),
        legend.key.size = unit(0.7, 'cm'),
        legend.key.width= unit(0.8, 'cm')) 
p33





# EssentialServices Quintile
p34 <- ggplot(mobilityCT_EssentialServiceQ_r) + 
  # Make annotation above the box in the margin
  coord_cartesian(clip = "off") +
  geom_line(size=0.85,
            aes(x=wk_day_1,y=100-percent_stay, 
                colour=employ_sales.trades.manufacturing.agriculture_quintile,
                linetype="2020"))+
  geom_line(size=0.85,
            aes(x=wk_day_1, y=100-base2019_percent_stay, 
                colour=employ_sales.trades.manufacturing.agriculture_quintile, 
                linetype="2019"), alpha = 0.4) +
  labs(x="", y="Mobility metric \n (% of devices that went outside home)", colour = "Mobility") +
  geom_vline(xintercept = as.numeric(as.Date("Mar 16, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
  # geom_text(size=6, aes(x=as.Date("Mar 16, 2020", format="%b %d, %Y"),
  #                       y=37),label=expression(1^{st}~Restriction),hjust=0.3)+
  geom_vline(xintercept = as.numeric(as.Date("Nov 23, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
  # geom_text(size=6, aes(x=as.Date("Nov 23, 2020", format="%b %d, %Y"),
  #                       y=45),label=expression(2^{nd}~Restriction),hjust=0.7)+
  
  annotation_custom(restriction1, 
                    xmin = as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    ymin =91) +
  annotation_custom(restriction2, 
                    xmin = as.Date("Nov 10, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Nov 10, 2020", format="%b %d, %Y"), 
                    ymin =91) +
  
  scale_color_viridis(discrete = TRUE, name = "Essential worker \nquintile", 
                      labels = c("1 = Lowest \n   proportion", "2", "3", "4", "5 = Highest \n   proportion")) +
  scale_linetype_manual(values = type, name = " ") +
  # ggtitle("GTA Mobility Stratified by Essential Worker (2019 and 2020)")+
  # scale_x_date(date_breaks = "6 week", date_labels = "%b %d") +
  scale_x_date(breaks = seq(as.Date("2020-02-23"), as.Date("2020-12-13"), by = "6 week"), 
               date_labels = "%b %d") +
  theme(plot.margin=margin(t=50),
        panel.background=element_rect(fill="white",colour="grey20"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 10),
        axis.text.y = element_text(size = 10),
        #plot.title = element_text(hjust=0.5, face="bold", size=34),
        axis.title = element_text(hjust=0.5, size=11),
        legend.title = element_text(hjust=0, size=10),
        legend.text=element_text(size=10),
        legend.key = element_rect(fill="white",colour=NA),
        legend.key.size = unit(0.7, 'cm'),
        legend.key.width= unit(0.8, 'cm')) 
p34


ggsave(p33, file= "../Plots/MobilitySDOH_Figures/ATIPPEQ_10curves.png", width = 6, height = 5)
ggsave(p34, file= "../Plots/MobilitySDOH_Figures/EssentialServicesQ_10curves.png", width = 6, height = 5)

# png("../Plots/MobilitySDOH_Figures/ATIPPEQ_10curves.png", width = 700, height = 600, units = "px",
#     res = 120)
# p33
# dev.off()
# 
# 
# png("../Plots/MobilitySDOH_Figures/EssentialServicesQ_10curves.png", width = 700, height = 600, units = "px",
#     res = 120)
# p34
# dev.off()


##################### Make panels #################

library(ggpubr)
p3 = ggarrange(p31,p32,p33,p34,
                    align = "hv",
                    nrow = 2,
                    ncol = 2,
                    common.legend = F,
                    # legend = "right",
                    heights = c(1,1),
                    widths = c(1,1),
                    labels = "AUTO")

p3
ggsave(p3, file= "../Plots/MobilitySDOH_Figures/SDOH_CovidMobility.png", width = 9, height = 7)



# # png for SDOH mobility and covid panel
# library(cowplot)
# png("../Plots/MobilitySDOH_Figures/SDOH_CovidMobility.png", width = 1400, height = 1150, units = "px",
#     res = 120)
# plot_grid( p31, p32, p33, p34, nrow=2, ncol=2, 
#            labels = c("A", "B", "C", "D"),
#            label_size = 13,
#            scale=0.9)
# dev.off()

##############################################

####################### Figure 4 and Appendix Figure A5 ############################

# Figure 4
# Forest plot for Table 4


library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(dplyr)
library(ggforce)


table4<-read.csv("../new_version/did.csv")
nrow(table4)

col2 = c('#0072B2',
         '#D55E00')

table4$domain = factor(table4$domain,levels = c("Pre-restriction mobility difference","Adjusted mobility change following restriction"))
table4a = table4 %>% filter(domain == "Pre-restriction mobility difference")
table4b = table4 %>% filter(domain == "Adjusted mobility change following restriction")

p41 <-ggplot(table4a[c(1:5),], aes(y= difference, x = reorder(label_level,5:1), color = label_head)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(.8)) +
  ## This is not a OR or RR so we don't want log scale of y
  # scale_y_log10(limits = c(-1,7),
  #               breaks = c(-1, 1, 3, 5, 7),
  #               minor_breaks = NULL) +
  scale_y_continuous(limits = c(-1,7),
                     breaks = c(-1,0, 1, 3, 5, 7),
                     minor_breaks = NULL) +
  geom_hline(yintercept = 0, linetype=2) +
  scale_color_manual(name = "Model",
                     values = '#0072B2')+
  scale_x_discrete(breaks = table4a[c(1:5),]$label_level,
                   
                   labels = c(expression(bold("Income (Q1=Highest)")),"Q2 vs Q1","Q3 vs Q1","Q4 vs Q1","Q5 vs Q1"))+
  coord_flip() +
  labs(title="", x ='', y = "Difference in mobility between quintiles") +
  theme_bw()+
  theme(legend.position = "none",
        plot.margin = margin(t = 15, 
                             r = 30,
                             b = 15, 
                             l = 15)) +
  facet_wrap(~ domain ,nrow = 1)

p41

p42 <-ggplot(table4a[c(6:10),], aes(y= difference, x = reorder(label_level,5:1), color = label_head)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(.8)) +
  ## This is not a OR or RR so we don't want log scale of y
  # scale_y_log10(limits = c(-1,7),
  #               breaks = c(-1, 1, 3, 5, 7),
  #               minor_breaks = NULL) +
  scale_y_continuous(limits = c(-1,7),
                     breaks = c(-1,0, 1, 3, 5, 7),
                     minor_breaks = NULL) +
  geom_hline(yintercept = 0, linetype=2) +
  scale_color_manual(name = "Model",
                     values = '#D55E00')+
  scale_x_discrete(breaks = table4a[c(6:10),]$label_level,
                   
                   labels = c(expression(bold("Proportion essential \nworkers (Q1=Lowest)")),"Q2 vs Q1","Q3 vs Q1","Q4 vs Q1","Q5 vs Q1"))+
  coord_flip() +
  labs(title="", x ='', y = "Difference in mobility between quintiles") +
  theme_bw()+
  theme(legend.position = "none",
        plot.margin = margin(t = 15, 
                             r = 30,
                             b = 15, 
                             l = 15)) +
  facet_wrap(~ domain ,nrow = 1)

p42

##################### Make panels #################

# png for forest plot panel 1
library(cowplot)
# png("../Plots/MobilitySDOH_Figures/Forest_plot_Table4a.png", width = 950, height = 500, units = "px",
#     res = 120)
png("../new_version/Forest_plot_Table4a.png", width = 950, height = 500, units = "px",
    res = 120)
plot_grid( p41, p42,
           labels = c("A", "B"),
           label_size = 13,
           scale=1,
           rel_widths = c(1,1)) 
dev.off()



################################## 

# Appendix A5



pa51 <-ggplot(table4b[c(1:6),], aes(y= difference, x = reorder(label_level,6:1), color = label_head)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(.8)) +
  ## This is not a OR or RR so we don't want log scale of y
  # scale_y_log10(limits = c(-1,7),
  #               breaks = c(-1, 1, 3, 5, 7),
  #               minor_breaks = NULL) +
  scale_y_continuous(limits = c(-4,4),
                     breaks = c(-4, -2, 0, 2, 4),
                     minor_breaks = NULL) +
  geom_hline(yintercept = 0, linetype=2) +
  scale_color_manual(name = "Model",
                     values = '#0072B2')+
  # scale_color_discrete(name = "Model",
  #                      breaks = c("Unadjusted", "Age and sex adjusted","Non-SDOH adjusted","Full adjusted"),
  #                      labels = c("Unadjusted", "Age and sex adjusted","Demographic and baseline health adjusted","Demographic, baseline health,\nand other SDOH adjusted"))+
  scale_x_discrete(breaks = table4b[c(1:6),]$label_level,
                   
                   labels = c(expression(bold("Income (Q1=Highest)")),"Q1: Post vs Pre","Q2: Post vs Pre","Q3: Post vs Pre","Q4: Post vs Pre","Q5: Post vs Pre"))+
  coord_flip(xlim = c(1,6), clip = 'off') +
  # coord_cartesian(ylim = c(-4,4), clip = "off") +
  # annotate(geom = 'text', label = '<- Reduced mobility', 
  #          y = -4,
  #          x = -1,
  #          vjust = -3,
  #          hjust = 0,
  #          size = 3)+
  # annotate(geom = 'text', label = ' Increased mobility ->', 
  #          y = 1,
  #          x = -1,
  #          vjust = -3,
#          hjust = 0,
#          size = 3)+
labs(title="", x ='', y = "Difference") +
  # Add a second axis
  geom_segment(x = -0.6, y = -4, xend = -0.6, yend = 4,color = 'black',
               arrow = arrow(length = unit(0.03, "npc"), ends = "both"))+
  annotate("segment",x=-0.55,xend=-0.65, y=0,yend=0, size=0.6)+
  annotate("text",x=-0.8, y=-2.2,size=3.5, label = 'Reduced mobility')+
  annotate("text",x=-0.8, y=2.2,size=3.5, label = 'Increased mobility')+
  
  theme_bw()+
  theme(legend.position = "none",
        plot.margin = margin(t = 10, 
                             r = 10,
                             b = 45, 
                             l = 10)) +
  facet_wrap(~ domain ,nrow = 1)

pa51



pa52 <-ggplot(table4b[c(7:12),], aes(y= difference, x = reorder(label_level,6:1), color = label_head)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(.8)) +
  ## This is not a OR or RR so we don't want log scale of y
  # scale_y_log10(limits = c(-1,7),
  #               breaks = c(-1, 1, 3, 5, 7),
  #               minor_breaks = NULL) +
  scale_y_continuous(limits = c(-4,4),
                     breaks = c(-4, -2, 0, 2, 4),
                     minor_breaks = NULL) +
  geom_hline(yintercept = 0, linetype=2) +
  scale_color_manual(name = "Model",
                     values = '#D55E00')+
  # scale_color_discrete(name = "Model",
  #                      breaks = c("Unadjusted", "Age and sex adjusted","Non-SDOH adjusted","Full adjusted"),
  #                      labels = c("Unadjusted", "Age and sex adjusted","Demographic and baseline health adjusted","Demographic, baseline health,\nand other SDOH adjusted"))+
  scale_x_discrete(breaks = table4b[c(7:12),]$label_level,
                   
                   labels = c(expression(bold("Proportion essential \nworkers (Q1=Lowest)")),"Q1: Post vs Pre","Q2: Post vs Pre","Q3: Post vs Pre","Q4: Post vs Pre","Q5: Post vs Pre"))+
  coord_flip(xlim = c(1,6), clip = 'off') +
  # coord_cartesian(ylim = c(-4,4), clip = "off") +
  # annotate(geom = 'text', label = '<- Reduced mobility', 
  #          y = -4,
  #          x = -1,
  #          vjust = -3,
  #          hjust = 0,
  #          size = 3)+
  # annotate(geom = 'text', label = ' Increased mobility ->', 
  #          y = 1,
  #          x = -1,
  #          vjust = -3,
#          hjust = 0,
#          size = 3)+
labs(title="", x ='', y = "Difference") +
  # Add a second axis
  geom_segment(x = -0.6, y = -4, xend = -0.6, yend = 4,color = 'black',
               arrow = arrow(length = unit(0.03, "npc"), ends = "both"))+
  annotate("segment",x=-0.55,xend=-0.65, y=0,yend=0, size=0.6)+
  annotate("text",x=-0.8, y=-2.2,size=3.5, label = 'Reduced mobility')+
  annotate("text",x=-0.8, y=2.2,size=3.5, label = 'Increased mobility')+
  
  theme_bw()+
  theme(legend.position = "none",
        plot.margin = margin(t = 10, 
                             r = 10,
                             b = 45, 
                             l = 10)) +
  facet_wrap(~ domain ,nrow = 1)

pa52


##################### Make panel for Table 4b #################

# png for forest plot panel
library(cowplot)
# png("../Plots/MobilitySDOH_Figures/Forest_plot_Table4b.png",width = 1000, height = 550, units = "px",
#     res = 120)
png("../new_version/Forest_plot_Table4b.png",width = 1100, height = 550, units = "px",
    res = 120)
plot_grid( pa51, pa52,
           labels = c("A", "B"),
           label_size = 13,
           scale=1,
           rel_widths = c(1,1)) 
dev.off()


#############################################################
################# Appendix Figures ##########################
##############################################################

####################### Appendix Figure A1, A6, A7, maps ############################


library(sf)
library(ggplot2)

setwd("C:/Users/WangSiyi/Documents/GitHub/SDOH-Mobility/Rfiles")

# Geo information
ct_sf <- read_sf("../Data/Map/lct_000b16a_e.shp")
ON_ct_sf <- subset(ct_sf, PRNAME == "Ontario")

###################### Appendix Figure A1 PHU ############################

# GTA map

# GTA CT data
income_occ_CT <- read.xlsx("../Data/COVID19 Modeling - income-occ by CT - 2020-12-16.xlsx", na.strings = ".")
income_occ_CT$CTid = as.character(format(round(income_occ_CT$`CMACT.-.CMA.and.Census.Tract`/100, 2), nsmall = 2))
gta_ct_sf <-  merge(ON_ct_sf, income_occ_CT, by.x='CTUID', by.y = 'CTid', all.x = FALSE, all.y = TRUE)


# PHU region map

phu_ct <- ggplot(data = gta_ct_sf) +
  geom_sf(aes(fill = PHU_name),
          colour = "grey25")+
  scale_fill_viridis_d(alpha = 0.8,
                       name = 'Public health unit',
                       begin = 0.15,
                       end = 0.95,
                       labels = c('Toronto', 'Durham', 'Halton', 'Peel','York')) +
  theme_void()+
  theme(legend.position = c(0.8, 0.2))
phu_ct


ggsave(file= "../Plots/MobilitySDOH_Figures/Map_phu_full.png", bg = 'white',                                     
       width = 5, height = 5.6, limitsize = FALSE, phu_ct)


###################### Appendix Figure A2 SDOH map ############################

# Original sdoh data
SDOHdata <- read.csv("../Data/SDOHranking_CT.csv")
SDOHdata$CTid = as.character(format(round(SDOHdata$CTid, 2), nsmall = 2))
# SDOH_ct_sf <-  merge(ON_ct_sf, SDOHdata, by.x='CTUID', by.y = 'CTid', all.x = FALSE, all.y = TRUE)
SDOH_ct_sf <- left_join(
  gta_ct_sf,
  SDOHdata,
  by = c('CTUID'= 'CTid'),
  keep = NULL
)

SDOH_ct_sf = SDOH_ct_sf %>%
  mutate(ATIPPE_quintile=as.numeric(case_when(ATIPPE_quintile == "1" ~"5",
                                              ATIPPE_quintile == "2" ~"4",
                                              ATIPPE_quintile == "3" ~"3",
                                              ATIPPE_quintile == "4" ~"2",
                                              ATIPPE_quintile == "5" ~"1")))




# Income quintile map
SDOH_ct_sf$ATIPPE_quintile = as.factor(SDOH_ct_sf$ATIPPE_quintile)
income_quintile_ct <- ggplot(data = SDOH_ct_sf) +
  geom_sf(aes(fill = ATIPPE_quintile),
          colour = 'grey25') +
  scale_fill_viridis_d(name = 'Income quintile', 
                       alpha = 0.8,
                       begin = 0.15,
                       end = 0.95,
                       labels = c("1 = Highest income", "2", "3", "4", "5 = Lowest income","Missing"),
                       na.value = "grey75") +
  theme_void()+
  theme(legend.position = c(0.83, 0.2),
        legend.title = element_text(hjust=0, size=9),
        legend.text=element_text(size=9),
        legend.key = element_rect(fill="white",colour=NA),
        legend.key.size = unit(0.34, 'cm'),
        legend.key.width= unit(0.34, 'cm'))

income_quintile_ct

ggsave(file= "../Plots/MobilitySDOH_Figures/Map_income_quintile.png", bg = 'white',                                     
       width = 4, height = 4.3, limitsize = FALSE, income_quintile_ct)


# Essential worker quintile map
SDOH_ct_sf$employ_sales.trades.manufacturing.agriculture_quintile = as.factor(SDOH_ct_sf$employ_sales.trades.manufacturing.agriculture_quintile)
ew_quintile_ct <- ggplot(data = SDOH_ct_sf) +
  geom_sf(aes(fill = employ_sales.trades.manufacturing.agriculture_quintile),
          colour = "grey25") +
  scale_fill_viridis_d(#direction = -1, 
                       alpha = 0.8,
                       begin = 0.15,
                       end = 0.95,
                       name = "Essential worker quintile", 
                       na.value = 'grey75',
                       labels = c("1 = Lowest proportion", "2", "3", "4", "5 = Highest proportion", "Missing")) +
  theme_void()+
  theme(legend.position = c(0.83, 0.2),
        legend.title = element_text(hjust=0, size=9),
        legend.text=element_text(size=9),
        legend.key = element_rect(fill="white",colour=NA),
        legend.key.size = unit(0.34, 'cm'),
        legend.key.width= unit(0.34, 'cm'))
ew_quintile_ct

ggsave(file= "../Plots/MobilitySDOH_Figures/Map_EW_quintile.png", bg = 'white',                                     
       width = 4, height = 4.3, limitsize = FALSE, ew_quintile_ct)

########## Make panel ########

# 
# library(ggpubr)
# pa2 = ggarrange(income_quintile_ct,ew_quintile_ct,
#                align = "hv",
#                nrow = 1,
#                ncol = 2,
#                common.legend = F,
#                # legend = "right",
#                heights = c(1,1),
#                widths = c(1,1),
#                labels = "AUTO")
# 
# pa2
# ggsave(pa2, file= "../Plots/MobilitySDOH_Figures/SDOH_CovidMobility.png", 
#        limitsize = FALSE,width = 7.5, height = 5)


library(cowplot)
png("../Plots/MobilitySDOH_Figures/Map_EWandIncome_quintile.png",width = 2100, height = 850, units = "px",
    res = 300)
plot_grid( income_quintile_ct, ew_quintile_ct,
           labels = c("A", "B"),
           label_size = 13,
           scale=1,
           rel_widths = c(1,1))
dev.off()

#################
###################### Appendix Figure A2 Mobility map ############################
# Mobility map

# 1239 CTs
mcSDOH <-read.csv("../Data/AggregatedMobility/mcSDOH_ct_0wklag.csv")
# Recode ATIPPE Quintile from high to low levels
mcSDOH = mcSDOH %>%
  mutate(ATIPPE_quintile=as.numeric(case_when(ATIPPE_quintile == "1" ~"5",
                                              ATIPPE_quintile == "2" ~"4",
                                              ATIPPE_quintile == "3" ~"3",
                                              ATIPPE_quintile == "4" ~"2",
                                              ATIPPE_quintile == "5" ~"1"))) %>%
  mutate(mobility = 100-percent_stay)

# Transfer numeric to character and there are two digits after the decimal
mcSDOH$CTID = as.character(format(round(mcSDOH$CTID, 2), nsmall = 2))
# mcSDOH_ct_sf <-  merge(ON_ct_sf, mcSDOH, by.x='CTUID', by.y = 'CTID', all.x = FALSE, all.y = TRUE)
mcSDOH_ct_sf <- left_join(
  gta_ct_sf,
  mcSDOH,
  by = c('CTUID'= 'CTID'),
  keep = NULL
)

# Check missing. No missing
missing = subset(mcSDOH_ct_sf, is.na(PRUID))


#############
# policy 1
policy1_base <- mcSDOH %>%
  filter(week %in% c(9:11,13:15)) %>%
  mutate(policy=as.factor(ifelse(week < 12, '0', '1'))) %>%
  group_by(CTID, policy) %>%
  # select(CTUID, week,mobility, prop_at_home)
  summarise(mobility_mean = mean(mobility,na.rm=F),
            percent_stay_mean = mean(percent_stay))

policy1_frame0 = unique(mcSDOH_ct_sf %>%
                          select(CTUID, geometry) %>%
                          mutate(policy = 0))
policy1_frame1 = unique(mcSDOH_ct_sf %>%
                          select(CTUID, geometry) %>%
                          mutate(policy = 1))
policy1_frame = rbind(policy1_frame0, policy1_frame1) %>%
  mutate(policy = as.factor(policy))

policy1 <-  left_join(policy1_frame,
                      policy1_base,
                      by = c('CTUID'= 'CTID', 'policy' = 'policy'),
                      keep = NULL)

sum(is.na(policy1$mobility_mean))
missing = subset(policy1, is.na(mobility_mean))
table(missing$CTUID)
summary(subset(policy1, policy == 0)$mobility_mean)
summary(subset(policy1, policy == 1)$mobility_mean)
#########################
# Policy 2
policy2_base <- mcSDOH %>%
  filter(HUID %in% c(3895,2253)) %>%
  filter(week %in% c(45,46,47,49,50,51)) %>%
  mutate(policy=as.factor(ifelse(week < 48, '0', '1'))) %>%
  group_by(CTID, policy) %>%
  # select(CTUID, week,mobility, percent_at_home)
  summarise(mobility_mean = mean(mobility,na.rm=F))

policy2_frame0 = unique(mcSDOH_ct_sf %>%
                          filter(PHU_ID.x %in% c(3895,2253)) %>%
                          select(CTUID, geometry) %>%
                          mutate(policy = 0))
policy2_frame1 = unique(mcSDOH_ct_sf %>%
                          filter(PHU_ID.x %in% c(3895,2253)) %>%
                          select(CTUID, geometry) %>%
                          mutate(policy = 1))
policy2_frame = rbind(policy2_frame0, policy2_frame1) %>%
  mutate(policy = as.factor(policy))

policy2 <-  left_join(policy2_frame,
                      policy2_base,
                      by = c('CTUID'= 'CTID', 'policy' = 'policy'),
                      keep = NULL)

sum(is.na(policy2$mobility_mean))
summary(subset(policy2, policy == 0)$mobility_mean)
summary(subset(policy2, policy == 1)$mobility_mean)
##############################

# strip_labels <- expression(
#   '0' = A~Mobility~before~1^st~restriction,
#   '1' = B~Mobility~after~1^st~restriction
# )
# 
# # Helper function to help pluck values from expression list
# label_expressions <- function(values) {
#   stopifnot(is.expression(values))
#   as_labeller(function(x) {
#     if (is.null(names(values))) {
#       x <- seq_along(x)
#       if (length(x)!=length(values)) warning(paste0("Number of labels(",
#                                                     length(values), ") does not match number of values (", length(x), ")"))
#     }
#     as.list(values[x])
#   }, default=identity)
# }

mobi_ct_1_pre <- ggplot(data = policy1 %>% filter(policy == 0)) +
  geom_sf(aes(fill = mobility_mean, colour = 'grey30'))+
  scale_fill_viridis_c(direction = -1, 
                       alpha = 0.8,
                       name = 'Mobility',
                       na.value = "grey75",
                       limits = range(c(min(policy1$mobility_mean,na.rm=T),max(policy1$mobility_mean,na.rm=T)))) +
  # Add separate colour scale bar for missing value
  scale_colour_manual(values=NA, name = "Missing", labels = "") +
  guides(colour=guide_legend("Missing", override.aes=list(fill="grey75")))+
  theme_void()+
  theme(text = element_text(size = 10),
        legend.position = 'right',
        # legend.position = "none",
        legend.direction = 'vertical',
        # # Remove the facet label
        # strip.text.x = element_blank(),
        strip.text.x = element_text(hjust = 0, margin=margin(l=0))) 
# facet_wrap(~policy, labeller = NULL)
# facet_wrap(~policy,
#            labeller = labeller(policy = label_expressions(strip_labels)))
mobi_ct_1_pre



mobi_ct_1_post <- ggplot(data = policy1 %>% filter(policy == 1)) +
  geom_sf(aes(fill = mobility_mean, colour = 'grey30'))+
  scale_fill_viridis_c(direction = -1, 
                       alpha = 0.8,
                       name = 'Mobility',
                       na.value = "grey75",
                       limits = range(c(min(policy1$mobility_mean,na.rm=T),max(policy1$mobility_mean,na.rm=T)))) +
  scale_colour_manual(values=NA, name = "Missing", labels = "") +
  guides(colour=guide_legend("Missing", override.aes=list(fill="grey75")))+
  theme_void()+
  theme(text = element_text(size = 10),
        legend.position = 'right',
        # legend.position = "none",
        legend.direction = 'vertical',
        # # Remove the facet label
        # strip.text.x = element_blank(),
        strip.text.x = element_text(hjust = 0, margin=margin(l=0))) 
# facet_wrap(~policy, labeller = NULL)
# facet_wrap(~policy,
#            labeller = labeller(policy = label_expressions(strip_labels)))
mobi_ct_1_post


# Label
# library(gtable)
# g <- ggplotGrob(mobi_ct_1)
# strips <- g$layout[grep("strip-t", g$layout$name), ]
# titles <- lapply(paste0('(', letters[seq_len(nrow(strips))], ")"),
#                  textGrob, x = 0, hjust = 0, vjust = 1)
# g <- gtable_add_grob(g, grobs = titles,
#                      t = strips$t, b = strips$b - 2,
#                      l = strips$l, r = strips$r)
# grid.newpage()
# grid.draw(g)

# png("../Plots/MobilitySDOH_Figures/Map_mobility_1.png", width = 2500, height = 2300, units = "px",
#     res = 300)
# mobi_ct_1
# dev.off()

# ggsave(file= "../Plots/MobilitySDOH_Figures/Map_mobility_1.png", bg = 'white',                                     
#        width = 7.5, height = 4.6, limitsize = FALSE, mobi_ct_1)

ggsave(file= "../Plots/MobilitySDOH_Figures/Map_mobility_1_pre.png", bg = 'white',                                     
       width = 3.5, height = 4, limitsize = FALSE, mobi_ct_1_pre)

ggsave(file= "../Plots/MobilitySDOH_Figures/Map_mobility_1_post.png", bg = 'white',                                     
       width = 3.5, height = 4, limitsize = FALSE, mobi_ct_1_post)

#####################################
# 2nd restriction

# policy2 = mcSDOH_ct_sf %>%
#   filter(week %in% c(45,46,47,49,50,51)) %>%
#   mutate(policy=as.factor(ifelse(week < 48, '0', '1'))) %>%
#   group_by(CTUID, policy, geometry) %>%
#   summarise(mobility_mean = mean(mobility))

# Only Toronto and Peel PHUs
# strip_labels <- expression(
#   '0' = Mobility~before~2^nd~restriction~`for`~Toronto~and~Peel,
#   '1' = Mobility~after~2^nd~restriction~`for`~Toronto~and~Peel
# )
# 
mobi_ct_2_pre <- ggplot(data = policy2 %>% filter(policy == 0)) +
  geom_sf(aes(fill = mobility_mean,
          colour = 'grey30'))+
  scale_fill_viridis_c(direction = -1,
                       alpha = 0.8,
                       name = 'Mobility',
                       limits = range(c(min(policy1$mobility_mean,na.rm=T),max(policy1$mobility_mean,na.rm=T))),
                       na.value = 'grey75') +
  scale_colour_manual(values=NA, name = "Missing", labels = "") +
  guides(colour=guide_legend("Missing", override.aes=list(fill="grey75")))+
  theme_void()+
  theme(text = element_text(size = 10),
        legend.position = 'right',
        # legend.position = "none",
        legend.direction = 'vertical',
        # # Remove the facet label
        # strip.text.x = element_blank(),
        strip.text.x = element_text(hjust = 0, margin=margin(l=0))) 
# facet_wrap(~policy, labeller = NULL)
# facet_wrap(~policy,
#            labeller = labeller(policy = label_expressions(strip_labels)))
mobi_ct_2_pre


mobi_ct_2_post <- ggplot(data = policy2 %>% filter(policy == 1)) +
  geom_sf(aes(fill = mobility_mean,
          colour = 'grey30'))+
  scale_fill_viridis_c(direction = -1,
                       alpha = 0.8,
                       name = 'Mobility',
                       limits = range(c(min(policy1$mobility_mean,na.rm=T),max(policy1$mobility_mean,na.rm=T))),
                       na.value = 'grey75') +
  scale_colour_manual(values=NA, name = "Missing", labels = "") +
  guides(colour=guide_legend("Missing", override.aes=list(fill="grey75")))+
  theme_void()+
  theme(text = element_text(size = 10),
        legend.position = 'right',
        # legend.position = "none",
        legend.direction = 'vertical',
        # # Remove the facet label
        # strip.text.x = element_blank(),
        strip.text.x = element_text(hjust = 0, margin=margin(l=0))) 
# facet_wrap(~policy, labeller = NULL)
# facet_wrap(~policy,
#            labeller = labeller(policy = label_expressions(strip_labels)))
mobi_ct_2_post




# ggsave(file= "../Plots/MobilitySDOH_Figures/Map_mobility_TorPee_2.png", bg = 'white',                                     
#        width = 7.5, height = 5.6, limitsize = FALSE, mobi_ct_2)

ggsave(file= "../Plots/MobilitySDOH_Figures/Map_mobility_2_pre.png", bg = 'white',                                     
       width = 3.5, height = 4, limitsize = FALSE, mobi_ct_2_pre)

ggsave(file= "../Plots/MobilitySDOH_Figures/Map_mobility_2_post.png", bg = 'white',                                     
       width = 3.5, height = 4, limitsize = FALSE, mobi_ct_2_post)




#######

min(policy1$mobility_mean,na.rm=T);max(policy1$mobility_mean,na.rm=T)
min(policy2$mobility_mean,na.rm=T);max(policy2$mobility_mean,na.rm=T)

library(ggpubr)

mobi_ct = ggarrange(mobi_ct_1_pre,mobi_ct_1_post,
                    mobi_ct_2_pre,mobi_ct_2_post,
                    align = "hv",
                    nrow = 2,
                    ncol = 2,
                    common.legend = T,
                    legend = 'right',
                    labels = 'AUTO')

mobi_ct

# png("../Plots/MobilitySDOH_Figures/Map_mobility.png", width = 2500, height = 2300, units = "px",
#     res = 300)
# mobi_ct
# dev.off()

ggsave(file= "../Plots/MobilitySDOH_Figures/Map_mobility.png", bg = 'white',                                     
       width = 7.5, height = 6.5, limitsize = FALSE, mobi_ct)

###############################
# Overall baseline vs current
colors <- c("Current" = "", "Baseline" = "blue")

mobilityGTA <-read.csv("../Data/AggregatedMobility/AggregatedMobility_GTA.csv") %>%
  filter(w_o_y >= 9 & w_o_y <= 51) %>%
  mutate(wk_day_1 = as.Date(wk_day_1))


p0 <- ggplot(mobilityGTA) + 
  # Make annotation above the box in the margin
  coord_cartesian(clip = "off") +
  geom_line(size=1.2,
            aes(x=wk_day_1,y=100-Weighted_percent_stay,linetype="2020"), color = 'blue')+
  geom_line(size=1.2,
            aes(x=wk_day_1, y=100-Weighted_base2019_percent_stay, linetype="2019"), color = 'blue', alpha = 0.4) +
  labs(x="", y= "Mobility metric \n (% of devices that went outside home)", colour = "Mobility") +
  geom_vline(xintercept = as.numeric(as.Date("Mar 16, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
  # geom_text(size=6, aes(x=as.Date("Mar 16, 2020", format="%b %d, %Y"),
  #               y=37),label=expression(1^st~Restriction),hjust=0.3, parse=TRUE)+
  geom_vline(xintercept = as.numeric(as.Date("Nov 23, 2020", format="%b %d, %Y")), linetype=2, colour="black")+
  # geom_text(size=6, aes(x=as.Date("Nov 23, 2020", format="%b %d, %Y"),
  #               y=45,label=expression(2^nd~Restriction),
  #           hjust=0.7, parse = TRUE)+
  annotation_custom(restriction1, 
                    xmin = as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Mar 16, 2020", format="%b %d, %Y"), 
                    ymin =88) +
  annotation_custom(restriction2, 
                    xmin = as.Date("Nov 23, 2020", format="%b %d, %Y"), 
                    xmax =as.Date("Nov 23, 2020", format="%b %d, %Y"), 
                    ymin =88) +
  
  scale_linetype_manual(values = type, name = " ") +
  # ggtitle("GTA Mobility Comparison (2019 and 2020)")+
  scale_x_date(date_breaks = "6 week", date_labels = "%b %d") +
  
  theme(plot.margin=margin(t=50),
        panel.background=element_rect(fill="white",colour="grey20"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 23),
        axis.text.y = element_text(size = 23),
        #plot.title = element_text(hjust=0.5, face="bold", size=34),
        axis.title = element_text(hjust=0.5, size=26),
        legend.title = element_text(hjust=0, size=20),
        legend.text=element_text(size=20),
        legend.key = element_rect(fill="white",colour=NA),
        legend.key.size = unit(1, 'cm'),
        legend.key.width= unit(1.2, 'cm')
  ) 
p0

png("../Plots/MobilitySDOH_Figures/GTAmobility_comparison.png", width = 3300, height = 2800, units = "px",
    res = 300)
p0
dev.off()




###########################
###################### Appendix Figure A4 Correlation ############################

#################### Spearman correlation coefficient ###################
mobilitySDOH_CT <- read.csv("../Data/mobilitySDOH_CT.csv")
length(unique(mobilitySDOH_CT$CTID))

mobilitySDOH_CT = mobilitySDOH_CT %>%
  mutate(ATIPPE_quintile=as.numeric(case_when(ATIPPE_quintile == "1" ~"5",
                                              ATIPPE_quintile == "2" ~"4",
                                              ATIPPE_quintile == "3" ~"3",
                                              ATIPPE_quintile == "4" ~"2",
                                              ATIPPE_quintile == "5" ~"1")),
         ATIPPE_quintile_withinPHU=as.numeric(case_when(ATIPPE_quintile_withinPHU == "1" ~"5",
                                                        ATIPPE_quintile_withinPHU == "2" ~"4",
                                                        ATIPPE_quintile_withinPHU == "3" ~"3",
                                                        ATIPPE_quintile_withinPHU == "4" ~"2",
                                                        ATIPPE_quintile_withinPHU == "5" ~"1")))

# Reduce the repeated time-independent data
SDOHcorr = unique(mobilitySDOH_CT %>% select(CTID, CT_pop, 
                                             ATIPPE, ATIPPE_quintile,
                                             employ_sales.trades.manufacturing.agriculture, 
                                             employ_sales.trades.manufacturing.agriculture_quintile))

# Spearman correlation is -0.62 for ATIPPE and ESW
cor.test(SDOHcorr$ATIPPE,
         SDOHcorr$employ_sales.trades.manufacturing.agriculture,
         method = "spearman",
         exact = FALSE) 

# Spearman correlation is 0.58 for two SDOH quintiles (reversed income from high to low)
cor.test(SDOHcorr$ATIPPE_quintile,
         SDOHcorr$employ_sales.trades.manufacturing.agriculture_quintile,
         method = "spearman",
         exact = FALSE) 


# Scatterplot
library(car)

par(mar = c(5.1, 4.1, 4.1, 4.4))
scatterplot(SDOHcorr$ATIPPE,
            SDOHcorr$employ_sales.trades.manufacturing.agriculture,
            xlab="Income",
            ylab="Proportion essential workers ",
            grid = FALSE,
            boxplot = FALSE,
            regLine = FALSE,
            cex = 0.6)
pa41 <- recordPlot()
dev.off()



scatterplot(SDOHcorr$ATIPPE_quintile,
            SDOHcorr$employ_sales.trades.manufacturing.agriculture_quintile,
            xlab="Income quintile",
            ylab="Essential worker quintile",
            grid=FALSE,
            boxplot = FALSE,
            regLine = FALSE,
            cex = 0.6)
pa42 <- recordPlot()
dev.off()




library(cowplot)
png("../Plots/MobilitySDOH_Figures/scatterplot_two.png",width = 2200, height = 1200, units = "px",
    res = 300)
plot_grid( pa41, pa42,
           labels = c("A", "B"),
           label_size = 13,
           nrow=1,
           ncol=2,
           scale=1,
           rel_widths = c(1.1,1))
dev.off()

png("../Plots/MobilitySDOH_Figures/scatterplotQ.png", width = 1200, height =900, units = "px",
    res = 200)
scatterplot(SDOHcorr$ATIPPE_quintile,
            SDOHcorr$employ_sales.trades.manufacturing.agriculture_quintile,
            xlab="Income Quintile",
            ylab="Essential Worker Quintile",
            grid=FALSE,
            boxplot = FALSE,
            regLine = FALSE)

dev.off()

png("../Plots/MobilitySDOH_Figures/scatterplot.png", width = 1200, height =900, units = "px",
    res = 200)
scatterplot(SDOHcorr$ATIPPE,
            SDOHcorr$employ_sales.trades.manufacturing.agriculture,
            xlab="Income",
            ylab="Proportion Essential Worker ",
            grid = FALSE,
            boxplot = FALSE,
            regLine = FALSE)
dev.off()
##############################






####################



###############################




