require(lubridate)
require(dplyr)
require(openxlsx)
require(tidyverse)
require(ggplot2)
require(lme4)
require(lmerTest)

# setwd("C:/Users/wangs/OneDrive/Documents/GitHub/SDOH-Mobility/Data")
setwd("C:/Users/WangSiyi/Documents/GitHub/SDOH-Mobility/Rfiles")


mobilitySDOH_CT=read_csv("../Data/mobilitySDOH_CT.csv",
                         col_types=cols( PHU_ID=col_character(),
                                         w_o_y=col_integer(),
                                         ATIPPE_quintile = col_factor()
                         )	
)

# 1240 CTs
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


mobilitySDOH_CT$ATIPPE_quintile = as.factor(mobilitySDOH_CT$ATIPPE_quintile)
mobilitySDOH_CT$employ_sales.trades.manufacturing.agriculture_quintile =
  as.factor(mobilitySDOH_CT$employ_sales.trades.manufacturing.agriculture_quintile)


PHU_ID <- c("2230","2236","3895","2253","2270")
RedPolicy <- ymd(c("2020-11-23", "2020-11-16","2020-11-10", "2020-11-06","2020-11-16"))
weekRed <- epiweek(RedPolicy)
Closure <- ymd(c("2020-12-26", "2020-12-26","2020-11-23","2020-11-23","2020-12-14"))
weekGrey <- epiweek(Closure)
policydate <- data.frame(PHU_ID,RedPolicy,weekRed,Closure,weekGrey)

mobilitySDOH_CT <- left_join(mobilitySDOH_CT, policydate, by="PHU_ID")



###################

policy3did = mobilitySDOH_CT %>%
  filter(!is.na(base_2019_percent_stay), week %in% c(45,46,47,49,50,51)) %>%
  # The policy start week is not counted as the week under policy
  mutate(policy = ifelse(epiweek(wk_day_1) <= epiweek(Closure),0,1),
         trt = ifelse(PHU_ID %in% c("3895","2253"), 1, 0),
         post = ifelse(week %in% c(45:47), 0, 1),
         # policy = ifelse(epiweek(wk_day_1) <= epiweek(Closure),FALSE,TRUE),
         # percent_stay is the adjusted mobility here
         # adjusted mobility = current_mobility - 2019_mobility = (100-current_percent_stay) - (100-2019_percent_stay) = 2019_percent_stay - current_percent_stay
         # adjusted percent-stay = current_percent_stay - 2019_percent_stay = (100-current_mobility) - (100-2019_mobility) = 2019_mobility - current_mobility
         percent_stay=base_2019_percent_stay-percent_stay,
         ess_quintile=factor(`employ_sales.trades.manufacturing.agriculture_quintile`,levels=as.character(1:5)),
         CTID = as.character(CTID)
  )
length(unique(policy3did$CTID))


#################################
# Proposed model 1 (with categorical time variables)
did1_categorical = lmer(percent_stay ~ 
                          (1|CTID) +
                          (1|PHU_ID)+
                          as.factor(week)+
                          as.factor(trt)+
                          as.factor(policy), 
                        data=policy3did,
                        REML = FALSE)

summary(did1_categorical)
confint(did1_categorical)
anova(did1_categorical, test = "Chisq")

##################################



# Proposed model 2 by income (with categorical time variables)
did2income_categorical = lmer(percent_stay ~ 
                                (1|CTID) +
                                (1|PHU_ID) +
                                as.factor(policy)*as.factor(ATIPPE_quintile)+
                                as.factor(trt)*as.factor(ATIPPE_quintile)+
                                as.factor(week)*as.factor(ATIPPE_quintile), 
                              data=policy3did,
                              REML = FALSE)
summary(did2income_categorical)

anova(did2income_categorical, test = "Chisq")


# Proposed model 2 by proportion essential worker (with categorical time variables)
did2ess_categorical = lmer(percent_stay ~ 
                             (1|CTID) +
                             (1|PHU_ID) +
                             as.factor(policy)*as.factor(ess_quintile)+
                             as.factor(trt)*as.factor(ess_quintile)+
                             as.factor(week)*as.factor(ess_quintile), 
                           data=policy3did,
                           REML = FALSE)
summary(did2ess_categorical)

anova(did2ess_categorical, test = "Chisq")



#######################
# Table 6 for income quintile

fitMain = lmer(percent_stay ~ 
                                (1|CTID) +
                                (1|PHU_ID) +
                                as.factor(policy)*as.factor(ATIPPE_quintile)+
                                as.factor(trt)*as.factor(ATIPPE_quintile)+
                                as.factor(week)*as.factor(ATIPPE_quintile), 
                              data=policy3did,
                              REML = FALSE)
summary(fitMain)


library(multcomp)


CI_l <- confint(fitMain,parm = rownames(summary(fitMain)$coef)[1:6], level=0.95)
table4_l <- cbind(summary(fitMain)$coef[c(1:6),c(1,4,5)], CI_l)
round(table4_l,2)




# right
k <- rbind(c(rep(0,1),1,rep(0,10),1, rep(0,27)),
           c(rep(0,1),1,rep(0,11),1, rep(0,26)),
           c(rep(0,1),1,rep(0,12),1, rep(0,25)),
           c(rep(0,1),1,rep(0,13),1, rep(0,24)))
rownames(k) <- c("beta2 + beta13",
                 "beta2 + beta14",
                 "beta2 + beta15",
                 "beta2 + beta16")

summary(fitMain)$coef[c(13,14,15,16),c(1,4,5)]
k

test <- glht(fitMain, linfct=k)
results <- summary(test)

table4_r <- cbind(results[[9]][[3]], results[[9]][[5]], results[[9]][[6]])
colnames(table4_r) <- c("Estimate", "z value", "p value")
CI_r <- confint(test)[[9]]

table4_r <- cbind(table4_r, CI_r[,c(2,3)])
round(table4_r,2)

# Save Table 4
write.csv(round(table4_l,2), "../new_version/Table4_income_categorical_left.csv")
write.csv(round(table4_r,2), "../new_version/Table4_income_categorical_right.csv")



###########
# Table 6 for essential quintile

fitMain.ess = lmer(percent_stay ~ 
                             (1|CTID) +
                             (1|PHU_ID) +
                             as.factor(policy)*as.factor(ess_quintile)+
                             as.factor(trt)*as.factor(ess_quintile)+
                             as.factor(week)*as.factor(ess_quintile), 
                           data=policy3did,
                           REML = FALSE)
summary(fitMain.ess)

library(multcomp)

CI.ess_l <- confint(fitMain.ess,parm = rownames(summary(fitMain.ess)$coef)[1:6], level=0.95)
table4.ess_l <- cbind(summary(fitMain.ess)$coef[c(1:6),c(1,4,5)], CI.ess_l)
round(table4.ess_l,2)

# right

k <- rbind(c(rep(0,1),1,rep(0,10),1, rep(0,27)),
           c(rep(0,1),1,rep(0,11),1, rep(0,26)),
           c(rep(0,1),1,rep(0,12),1, rep(0,25)),
           c(rep(0,1),1,rep(0,13),1, rep(0,24)))
rownames(k) <- c("beta2 + beta13",
                 "beta2 + beta14",
                 "beta2 + beta15",
                 "beta2 + beta16")

summary(fitMain.ess)$coef[c(13,14,15,16),c(1,4,5)]
k

test.ess <- glht(fitMain.ess, linfct=k)
results.ess <- summary(test.ess)

table4.ess_r <- cbind(results.ess[[9]][[3]], results.ess[[9]][[5]], results.ess[[9]][[6]])
colnames(table4.ess_r) <- c("Estimate", "z value", "p value")
CI.ess_r <- confint(test.ess)[[9]]

table4.ess_r <- cbind(table4.ess_r, CI.ess_r[,c(2,3)])
round(table4.ess_r,2)

# Save Table 4
write.csv(round(table4.ess_l,2), "../new_version/Table4_esw_categorical_left.csv")
write.csv(round(table4.ess_r,2), "../new_version/Table4_esw_categorical_right.csv")



#################### 
# Forest figures

# Figure 3
# Forest plot for Table 4


library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(dplyr)
library(ggforce)


table4<-read.csv("../new_version/did2_categorical_forest.csv")
nrow(table4)

col2 = c('#0072B2',
         '#D55E00')

table4$domain = factor(table4$domain,levels = c("Pre-restriction mobility difference","Adjusted mobility change following restriction"))
table4a = table4 %>% filter(domain == "Pre-restriction mobility difference")
table4b = table4 %>% filter(domain == "Adjusted mobility change following restriction")



# Appendix A5



pa51 <-ggplot(table4b[c(1:6),], aes(y= difference, x = reorder(label_level,6:1), color = label_head)) +
  geom_point(position = position_dodge(.5)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position = position_dodge(.8)) +
  ## This is not a OR or RR so we don't want log scale of y
  # scale_y_log10(limits = c(-1,7),
  #               breaks = c(-1, 1, 3, 5, 7),
  #               minor_breaks = NULL) +
  scale_y_continuous(limits = c(-4.5,4.5),
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
  scale_y_continuous(limits = c(-4.5,4.5),
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
png("../new_version/Forest_plot_did_categorical.png",width = 1100, height = 550, units = "px",
    res = 120)
plot_grid( pa51, pa52,
           labels = c("A", "B"),
           label_size = 13,
           scale=1,
           rel_widths = c(1,1)) 
dev.off()







#################### Comparison #############################
################################################
# Using binary time variable

# Proposed model 1 (with binary time variables)
did1_binary = lmer(percent_stay ~ 
                     (1|CTID) +
                     (1|PHU_ID)+
                     as.factor(post)+
                     as.factor(trt)+
                     as.factor(policy), 
                   data=policy3did,
                   REML = FALSE)

summary(did1_binary)


did2income_binary = lmer(percent_stay ~ 
                     (1|CTID) +
                     (1|PHU_ID)+
                     as.factor(trt)*as.factor(ATIPPE_quintile)+
                     as.factor(post)*as.factor(ATIPPE_quintile)+
                     as.factor(policy)*as.factor(ATIPPE_quintile),
                   data=policy3did,
                   REML = FALSE)

summary(did2income_binary)
anova(did2income_binary, test = "Chisq")


did2ess_binary = lmer(percent_stay ~ 
                             (1|CTID) +
                             (1|PHU_ID) +
                             as.factor(policy)*as.factor(ess_quintile)+
                             as.factor(trt)*as.factor(ess_quintile)+
                             as.factor(week)*as.factor(ess_quintile), 
                           data=policy3did,
                           REML = FALSE)
summary(did2ess_binary)

anova(did2ess_binary, test = "Chisq")


####################################################

#######################
# New table and figures

# Table 4 for income quintile

fitMain = lmer(percent_stay ~ 
                 (1|CTID) +
                 (1|PHU_ID) +
                 as.factor(policy)*as.factor(ATIPPE_quintile)+
                 as.factor(trt)*as.factor(ATIPPE_quintile)+
                 as.factor(post)*as.factor(ATIPPE_quintile), 
               data=policy3did,
               REML = FALSE)
summary(fitMain)


library(multcomp)


CI_l <- confint(fitMain,parm = rownames(summary(fitMain)$coef)[1:6], level=0.95)
table4_l <- cbind(summary(fitMain)$coef[c(1:6),c(1,4,5)], CI_l)
round(table4_l,2)




# right
k <- rbind(c(rep(0,1),1,rep(0,6),1, rep(0,11)),
           c(rep(0,1),1,rep(0,7),1, rep(0,10)),
           c(rep(0,1),1,rep(0,8),1, rep(0,9)),
           c(rep(0,1),1,rep(0,9),1, rep(0,8)))
rownames(k) <- c("beta2 + beta9",
                 "beta2 + beta10",
                 "beta2 + beta11",
                 "beta2 + beta12")

summary(fitMain)$coef[c(9,10,11,12),c(1,4,5)]
k

test <- glht(fitMain, linfct=k)
results <- summary(test)

table4_r <- cbind(results[[9]][[3]], results[[9]][[5]], results[[9]][[6]])
colnames(table4_r) <- c("Estimate", "z value", "p value")
CI_r <- confint(test)[[9]]

table4_r <- cbind(table4_r, CI_r[,c(2,3)])
round(table4_r,2)

# Save Table 4
write.csv(round(table4_l,2), "../new_version/Table4_income_binary_left.csv")
write.csv(round(table4_r,2), "../new_version/Table4_income_binary_right.csv")



###########



# Table 4 for essential quintile

fitMain.ess = lmer(percent_stay ~ 
                     (1|CTID) +
                     (1|PHU_ID) +
                     as.factor(policy)*as.factor(ess_quintile)+
                     as.factor(trt)*as.factor(ess_quintile)+
                     as.factor(post)*as.factor(ess_quintile), 
                   data=policy3did,
                   REML = FALSE)
summary(fitMain.ess)

library(multcomp)

CI.ess_l <- confint(fitMain.ess,parm = rownames(summary(fitMain.ess)$coef)[1:6], level=0.95)
table4.ess_l <- cbind(summary(fitMain.ess)$coef[c(1:6),c(1,4,5)], CI.ess_l)
round(table4.ess_l,2)

# right
k <- rbind(c(rep(0,1),1,rep(0,6),1, rep(0,11)),
           c(rep(0,1),1,rep(0,7),1, rep(0,10)),
           c(rep(0,1),1,rep(0,8),1, rep(0,9)),
           c(rep(0,1),1,rep(0,9),1, rep(0,8)))
rownames(k) <- c("beta2 + beta9",
                 "beta2 + beta10",
                 "beta2 + beta11",
                 "beta2 + beta12")

summary(fitMain.ess)$coef[c(9,10,11,12),c(1,4,5)]
k

test.ess <- glht(fitMain.ess, linfct=k)
results.ess <- summary(test.ess)

table4.ess_r <- cbind(results.ess[[9]][[3]], results.ess[[9]][[5]], results.ess[[9]][[6]])
colnames(table4.ess_r) <- c("Estimate", "z value", "p value")
CI.ess_r <- confint(test.ess)[[9]]

table4.ess_r <- cbind(table4.ess_r, CI.ess_r[,c(2,3)])
round(table4.ess_r,2)

# Save Table 4
write.csv(round(table4.ess_l,2), "../new_version/Table4_esw_binary_left.csv")
write.csv(round(table4.ess_r,2), "../new_version/Table4_esw_binary_right.csv")


