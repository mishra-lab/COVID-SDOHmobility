
library(lubridate)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(lme4)


# Source file working directory
# setwd("C:/Users/wangs/OneDrive/Documents/GitHub/SDOH-Mobility/Rfiles")
setwd("C:/Users/WangSiyi/Documents/GitHub/SDOH-Mobility/Rfiles")


# mobilityCT <- read.csv("D:/UT/Master/COVID-19/mobility/update/20210301/ctuid_level_weekly_20210404_20210410.csv", fileEncoding="UTF-8-BOM")
mobilityCT <- read.csv("../Data/ctuid_level_weekly_20210425_20210501.csv", fileEncoding="UTF-8-BOM")
length(unique(mobilityCT$CTID))
income_occ_CT <- read.xlsx("../Data/COVID19 Modeling - income-occ by CT - 2020-12-16.xlsx", na.strings = ".")

source("./SDOHmobility_Functions_new.R")


SDOHdata <- ReadSDOH(income_occ_CT)


################ Add quintiles or tertiles #############################
# One SDOH must first get rank within PHU and then rank over GTA.
# Otherwise, the column of the first will replace the second

# Rank within PHU
SDOHdata <- PopWeightedQuintile_withinPHU(data=SDOHdata, quintileCol = "ATIPPE", weightedCol = "CT_pop", PHU = "PHU_ID")
table(SDOHdata$ATIPPE_quintile_withinPHU)


# Rank over GTA
SDOHdata <- PopWeightedQuintile(data=SDOHdata, quintileCol = "ATIPPE", weightedCol = "CT_pop")
table(SDOHdata$ATIPPE_quintile)

SDOHdata <- PopWeightedQuintile(data=SDOHdata, quintileCol = "employ_sales/trades/manufacturing/agriculture", weightedCol = "CT_pop")
table(SDOHdata$`employ_sales/trades/manufacturing/agriculture_quintile`)


# Save the data set
write.csv(SDOHdata, file="../Data/SDOHranking_CT.csv", row.names = F, quote = F, na = "NA")



################## Link SDOH and Mobility data #####################
SDOHdata <- read.csv("../Data/SDOHranking_CT.csv")

mobilitySDOH_CT <- arrange(merge(mobilityCT, SDOHdata, by.x="CTID", by.y = "CTid"), by=wk_day_1)
# There are 1240 CTids in this combined file
length(unique(mobilitySDOH_CT$CTID))


# Result: Device coverage, no sdoh data (N=7)
no_SDOH = income_occ_CT[is.na(match(as.character(as.numeric(income_occ_CT$`CMACT.-.CMA.and.Census.Tract`)),
                                    as.character(100*as.numeric(SDOHdata$CTid)))),]
min(no_SDOH$CT_pop);max(no_SDOH$CT_pop)

no_mobility = SDOHdata[is.na(match(as.character(100*as.numeric(SDOHdata$CTid)),
                                   as.character(100*as.numeric(unique(mobilitySDOH_CT$CTID))))),]
min(no_mobility$CT_pop);max(no_mobility$CT_pop);median(no_mobility$CT_pop)
min(no_mobility$ATIPPE);max(no_mobility$ATIPPE);median(no_mobility$ATIPPE)
min(no_mobility$`employ_sales/trades/manufacturing/agriculture`);max(no_mobility$`employ_sales/trades/manufacturing/agriculture`);median(no_mobility$`employ_sales/trades/manufacturing/agriculture`)

match_mobility = SDOHdata[!is.na(match(as.character(100*as.numeric(SDOHdata$CTid)),
                                       as.character(100*as.numeric(unique(mobilitySDOH_CT$CTID))))),]
min(match_mobility$CT_pop);max(match_mobility$CT_pop);median(match_mobility$CT_pop)
min(match_mobility$ATIPPE);max(match_mobility$ATIPPE);median(match_mobility$ATIPPE)
min(match_mobility$`employ_sales/trades/manufacturing/agriculture`);max(match_mobility$`employ_sales/trades/manufacturing/agriculture`);median(match_mobility$`employ_sales/trades/manufacturing/agriculture`)

################### Add Time Index (Week) and Base_percent_change#########################


# Add a column of time
mobilitySDOH_CT <- mobilitySDOH_CT %>% arrange(year, w_o_y)
week <- cbind(unique(mobilitySDOH_CT$wk_day_1),c(1:length(unique(mobilitySDOH_CT$wk_day_1))))
colnames(week) <- c("wk_day_1","week")
mobilitySDOH_CT <- merge(mobilitySDOH_CT, week, by="wk_day_1")
mobilitySDOH_CT <- mobilitySDOH_CT %>% arrange(CTID,wk_day_1)

# Add base_percent_change columns
mobilitySDOH_CT <- mobilitySDOH_CT %>%
  mutate(base2019_percent_change_percent_stay = 100*(percent_stay-base_2019_percent_stay)/base_2019_percent_stay,
         base2019_percent_change_prop_at_home = 100*(prop_at_home-base_2019_prop_at_home)/base_2019_prop_at_home)

## Outcome: mobility
# Add mobility=1-prop_at_home columns
mobilitySDOH_CT$mobility <- 100-mobilitySDOH_CT$percent_stay

# # check relationship between mobility and percent_stay
# A = mobilitySDOH_CT %>% select(mobility, percent_stay)


# Save the data set
write.csv(mobilitySDOH_CT, file="../Data/mobilitySDOH_CT.csv", row.names = F, quote = F, na = "NA")




############### Load data set and Reset the ATIPPE Quintile Levels ###########################
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

# # Check quintile order: Q1 highest income level
# B = mobilitySDOH_CT %>% select(ATIPPE, ATIPPE_quintile, ATIPPE_quintile_withinPHU)

#################### Appendix Table 1 with Device penetration ###########################

# Note the mean devices is a time-dependent variable and we can change the study period by function
table1ATIPPEQ <- Table1_new(data = mobilitySDOH_CT, SDOHcol = "ATIPPE_quintile")
table1ESWQ <- Table1_new(data = mobilitySDOH_CT, SDOHcol = "employ_sales.trades.manufacturing.agriculture_quintile")


######### Table 2: Overall mobility change without SDOH #############


policy1gta <- mobilitySDOH_CT[!is.na(mobilitySDOH_CT$base_2019_percent_stay),] %>% 
  filter(week %in% c(9:11,13:15)) %>%
  mutate(policy=ifelse(week < 12, 0, 1), # Should use week instead of w_o_y since there are repeated w_o_y in 2020 and 2021
         gta = 1)
length(unique(policy1gta$CTID))


GTA_closure1_mobility <- Table2_new(data = policy1gta, SDOHcol = "gta", Mobilitycol = "mobility")
write.csv(GTA_closure1_mobility, file="../Tables/Table2_new/Overall_closure1_mobility.csv", row.names = F, quote = F, na = "NA")

# 2nd restriction

PHU_ID <- c("2230","2236","3895","2253","2270")
RedPolicy <- c("2020-11-23", "2020-11-16","2020-11-10", "2020-11-06","2020-11-16")
weekRed <- epiweek(RedPolicy)
Closure <- c("2020-12-26", "2020-12-26","2020-11-23","2020-11-23","2020-12-14")
weekGrey <- epiweek(Closure)
policydate <- data.frame(cbind(PHU_ID,RedPolicy,weekRed,Closure,weekGrey))


mobilitySDOH_CT <- merge(mobilitySDOH_CT, policydate, by="PHU_ID")




# Grey policy
# policy 3
policy3 <-mobilitySDOH_CT[!is.na(mobilitySDOH_CT$base_2019_percent_stay),] %>% 
  filter(week <= as.numeric(weekGrey)+3 & week>= as.numeric(weekGrey)-3 & week != as.numeric(weekGrey)) %>%
  mutate(policy=ifelse(as.Date(wk_day_1) <= as.Date(Closure), 0,1))
length(unique(policy3$CTID))

# Grey policy with Peel and Toronto
# policy3PeelToronto
policy3PeelToronto <- policy3 %>% 
  filter(PHU_ID %in% c("3895","2253"))%>%
  mutate(control = 1)
length(unique(policy3PeelToronto$CTID))

TorontoPeel_closure2_mobility <- Table2_new(data = policy3PeelToronto, SDOHcol = "control", Mobilitycol = "mobility")
write.csv(TorontoPeel_closure2_mobility, file="../Tables/Table2_new/Overall_closure2_mobility_TorPee.csv", row.names = F, quote = F, na = "NA")

#################### Table 2 First Closure ######################

# 268 rows with missing baseline in 2019 in mobilitySDOH_CT

# policy 1 with 35 missing baselines
# 1 CT doesn't have base_2019_percent_stay data for all weeks
policy1 <- mobilitySDOH_CT[!is.na(mobilitySDOH_CT$base_2019_percent_stay),] %>% 
  filter(week %in% c(9:11,13:15)) %>%
  mutate(policy=ifelse(w_o_y < 12, 0, 1))

# Check before generating table 2
length(unique(policy1$CTID))
missingcheck = table(policy1$CTID, policy1$policy)

ATIPPEQ_closure1_percent_stay <- Table2_new(data = policy1, SDOHcol = "ATIPPE_quintile", Mobilitycol = "percent_stay")
ATIPPEQ_closure1_mobility <- Table2_new(data = policy1, SDOHcol = "ATIPPE_quintile", Mobilitycol = "mobility")
ATIPPEQ_closure1_prop_at_home <- Table2_new(data = policy1, SDOHcol = "ATIPPE_quintile", Mobilitycol = "prop_at_home")

ATIPPEQ_withinPHU_closure1_percent_stay <- Table2_new(data = policy1, SDOHcol = "ATIPPE_quintile_withinPHU", Mobilitycol = "percent_stay")

EssentialServiceQ_closure1_percent_stay <- Table2_new(data = policy1, SDOHcol = "employ_sales.trades.manufacturing.agriculture_quintile", Mobilitycol = "percent_stay")
EssentialServiceQ_closure1_mobility <- Table2_new(data = policy1, SDOHcol = "employ_sales.trades.manufacturing.agriculture_quintile", Mobilitycol = "mobility")

#################### Table 2 Second Closure #########################

# PHU_ID <- c("2230","2236","3895","2253","2270")
# RedPolicy <- c("2020-11-23", "2020-11-16","2020-11-10", "2020-11-06","2020-11-16")
# weekRed <- epiweek(RedPolicy)
# Closure <- c("2020-12-26", "2020-12-26","2020-11-23","2020-11-23","2020-12-14")
# weekGrey <- epiweek(Closure)
# policydate <- data.frame(cbind(PHU_ID,RedPolicy,weekRed,Closure,weekGrey))
# 
# 
# mobilitySDOH_CT <- merge(mobilitySDOH_CT, policydate, by="PHU_ID")


# # Red policy
# # policy 2
# policy2 <- mobilitySDOH_CT[!is.na(mobilitySDOH_CT$base_2019_percent_stay),] %>% 
#   filter(week <= as.numeric(weekRed)+3 & week>= as.numeric(weekRed)-3 & week != as.numeric(weekRed)) %>%
#   mutate(policy=ifelse(as.Date(wk_day_1) <= as.Date(RedPolicy), 0,1))
# length(unique(policy2$CTID))
# 
# 
# # Grey policy
# # policy 3
# policy3 <-mobilitySDOH_CT[!is.na(mobilitySDOH_CT$base_2019_percent_stay),] %>% 
#   filter(week <= as.numeric(weekGrey)+3 & week>= as.numeric(weekGrey)-3 & week != as.numeric(weekGrey)) %>%
#   mutate(policy=ifelse(as.Date(wk_day_1) <= as.Date(Closure), 0,1))
# length(unique(policy3$CTID))

## Note policy 1 has 1239 CTs and policy 3 has 1240 CTs since CT 5350009 only have mobility data for the second restriction but not the first restriction
# diff = subset(policy3, CTID == 5350009)

# Grey policy with Peel and Toronto
# policy3PeelToronto
policy3PeelToronto <- policy3 %>% 
  filter(PHU_ID %in% c("3895","2253")) 
length(unique(policy3PeelToronto$CTID))

# Check before generating table 2
length(unique(policy3PeelToronto$CTID))
missingcheck = table(policy3PeelToronto$CTID, policy3PeelToronto$policy)

# Red
ATIPPEQ_red_percent_stay <- Table2_new(data = policy2, SDOHcol = "ATIPPE_quintile", Mobilitycol = "percent_stay")
ATIPPEQ_red_mobility <- Table2_new(data = policy2, SDOHcol = "ATIPPE_quintile", Mobilitycol = "mobility")
ATIPPEQ_red_prop_at_home <- Table2_new(data = policy2, SDOHcol = "ATIPPE_quintile", Mobilitycol = "prop_at_home")

ATIPPEQ_withinPHU_red_percent_stay <- Table2_new(data = policy2, SDOHcol = "ATIPPE_quintile_withinPHU", Mobilitycol = "percent_stay")

EssentialServiceQ_red_percent_stay <- Table2_new(data = policy2, SDOHcol = "employ_sales.trades.manufacturing.agriculture_quintile", Mobilitycol = "percent_stay")
EssentialServiceQ_red_mobility <- Table2_new(data = policy2, SDOHcol = "employ_sales.trades.manufacturing.agriculture_quintile", Mobilitycol = "mobility")


# Grey GTA
ATIPPEQ_closure2_percent_stay <- Table2_new(data = policy3, SDOHcol = "ATIPPE_quintile", Mobilitycol = "percent_stay")
ATIPPEQ_closure2_mobility <- Table2_new(data = policy3, SDOHcol = "ATIPPE_quintile", Mobilitycol = "mobility")
ATIPPEQ_closure2_prop_at_home <- Table2_new(data = policy3, SDOHcol = "ATIPPE_quintile", Mobilitycol = "prop_at_home")

ATIPPEQ_withinPHU_closure2_percent_stay <- Table2_new(data = policy3, SDOHcol = "ATIPPE_quintile_withinPHU", Mobilitycol = "percent_stay")

EssentialServiceQ_closure2_percent_stay <- Table2_new(data = policy3, SDOHcol = "employ_sales.trades.manufacturing.agriculture_quintile", Mobilitycol = "percent_stay")
EssentialServiceQ_closure2_mobility <- Table2_new(data = policy3, SDOHcol = "employ_sales.trades.manufacturing.agriculture_quintile", Mobilitycol = "mobility")


# Grey Toronto/Peel only
ATIPPEQ_closure2_percent_stay_TorPee <- Table2_new(data = policy3PeelToronto, SDOHcol = "ATIPPE_quintile", Mobilitycol = "percent_stay")
ATIPPEQ_closure2_mobility_TorPee <- Table2_new(data = policy3PeelToronto, SDOHcol = "ATIPPE_quintile", Mobilitycol = "mobility")
ATIPPEQ_closure2_prop_at_home_TorPee <- Table2_new(data = policy3PeelToronto, SDOHcol = "ATIPPE_quintile", Mobilitycol = "prop_at_home")


ATIPPEQ_withinPHU_closure2_percent_stay_TorPee <- Table2_new(data = policy3PeelToronto, SDOHcol = "ATIPPE_quintile_withinPHU", Mobilitycol = "percent_stay")

EssentialServiceQ_closure2_percent_stay_TorPee <- Table2_new(data = policy3PeelToronto, SDOHcol = "employ_sales.trades.manufacturing.agriculture_quintile", Mobilitycol = "percent_stay")
EssentialServiceQ_closure2_mobility_TorPee <- Table2_new(data = policy3PeelToronto, SDOHcol = "employ_sales.trades.manufacturing.agriculture_quintile", Mobilitycol = "mobility")

############################# Save Table 1 and Table 2 ######################################
# Save the tables
write.csv(table1ATIPPEQ, file="../Tables/ATIPPEQ_table1.csv", row.names = F, quote = F, na = "NA")
write.csv(table1ESWQ, file="../Tables/ESWQ_table1.csv", row.names = F, quote = F, na = "NA")

write.csv(ATIPPEQ_closure1_percent_stay, file="../Tables/Table2_new/ATIPPEQ_closure1_percent_stay.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_red_percent_stay, file="../Tables/Table2_new/ATIPPEQ_red_percent_stay.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_closure2_percent_stay, file="../Tables/Table2_new/ATIPPEQ_closure2_percent_stay.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_closure2_percent_stay_TorPee, file="../Tables/Table2_new/ATIPPEQ_closure2_percent_stay_TorPee.csv", row.names = F, quote = F, na = "NA")


write.csv(ATIPPEQ_closure1_mobility, file="../Tables/Table2_new/ATIPPEQ_closure1_mobility.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_red_mobility, file="../Tables/Table2_new/ATIPPEQ_red_mobility.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_closure2_mobility, file="../Tables/Table2_new/ATIPPEQ_closure2_mobility.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_closure2_mobility_TorPee, file="../Tables/Table2_new/ATIPPEQ_closure2_mobility_TorPee.csv", row.names = F, quote = F, na = "NA")


write.csv(ATIPPEQ_closure1_prop_at_home, file="../Tables/Table2_new/ATIPPEQ_closure1_prop_at_home.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_red_prop_at_home, file="../Tables/Table2_new/ATIPPEQ_red_prop_at_home.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_closure2_prop_at_home, file="../Tables/Table2_new/ATIPPEQ_closure2_prop_at_home.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_closure2_prop_at_home_TorPee, file="../Tables/Table2_new/ATIPPEQ_closure2_prop_at_home_TorPee.csv", row.names = F, quote = F, na = "NA")


write.csv(ATIPPEQ_withinPHU_closure1_percent_stay, file="../Tables/Table2_new/ATIPPEQ_withinPHU_closure1_percent_stay.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_withinPHU_red_percent_stay, file="../Tables/Table2_new/ATIPPEQ_withinPHU_red_percent_stay.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_withinPHU_closure2_percent_stay, file="../Tables/Table2_new/ATIPPEQ_withinPHU_closure2_percent_stay.csv", row.names = F, quote = F, na = "NA")
write.csv(ATIPPEQ_withinPHU_closure2_percent_stay_TorPee, file="../Tables/Table2_new/ATIPPEQ_withinPHU_closure2_percent_stay_TorPee.csv", row.names = F, quote = F, na = "NA")

write.csv(EssentialServiceQ_closure1_percent_stay, file="../Tables/Table2_new/EssentialServiceQ_closure1_percent_stay.csv", row.names = F, quote = F, na = "NA")
write.csv(EssentialServiceQ_red_percent_stay, file="../Tables/Table2_new/EssentialServiceQ_red_percent_stay.csv", row.names = F, quote = F, na = "NA")
write.csv(EssentialServiceQ_closure2_percent_stay, file="../Tables/Table2_new/EssentialServiceQ_closure2_percent_stay.csv", row.names = F, quote = F, na = "NA")
write.csv(EssentialServiceQ_closure2_percent_stay_TorPee, file="../Tables/Table2_new/EssentialServiceQ_closure2_percent_stay_TorPee.csv", row.names = F, quote = F, na = "NA")

write.csv(EssentialServiceQ_closure1_mobility, file="../Tables/Table2_new/EssentialServiceQ_closure1_mobility.csv", row.names = F, quote = F, na = "NA")
write.csv(EssentialServiceQ_red_mobility, file="../Tables/Table2_new/EssentialServiceQ_red_mobility.csv", row.names = F, quote = F, na = "NA")
write.csv(EssentialServiceQ_closure2_mobility, file="../Tables/Table2_new/EssentialServiceQ_closure2_mobility.csv", row.names = F, quote = F, na = "NA")
write.csv(EssentialServiceQ_closure2_mobility_TorPee, file="../Tables/Table2_new/EssentialServiceQ_closure2_mobility_TorPee.csv", row.names = F, quote = F, na = "NA")

############################ Summary Tables for SDOHs ##########################################

SDOHdata <- read.csv("../Data/SDOHranking_CT.csv")

# Recode ATIPPE Quintile from high to low levels
SDOHdata = SDOHdata %>%
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


table_ESW_ATIPPE <-  SDOHdata %>% 
  group_by(employ_sales.trades.manufacturing.agriculture_quintile) %>% 
  summarise(total_pop = sum(CT_pop),
            N = length(unique(CTid)),
            mean = mean(employ_sales.trades.manufacturing.agriculture, na.rm = T),
            median = median(employ_sales.trades.manufacturing.agriculture, na.rm = T),
            min = min(employ_sales.trades.manufacturing.agriculture, na.rm = T),
            max = max(employ_sales.trades.manufacturing.agriculture, na.rm = T),
            median_ATIPPE = median(ATIPPE),
            min_ATIPPE = min(ATIPPE),
            max_ATIPPE = max(ATIPPE))
table_ESW_ATIPPE <- round(table_ESW_ATIPPE,2)


table_ATIPPE_ESW <-  SDOHdata %>% 
  group_by(ATIPPE_quintile) %>% 
  summarise(total_pop = sum(CT_pop),
            N = length(unique(CTid)),
            mean = mean(ATIPPE, na.rm = T),
            median = median(ATIPPE, na.rm = T),
            min = min(ATIPPE, na.rm = T),
            max = max(ATIPPE, na.rm = T),
            median_ESW = median(employ_sales.trades.manufacturing.agriculture),
            min_ESW = min(employ_sales.trades.manufacturing.agriculture),
            max_ESW = max(employ_sales.trades.manufacturing.agriculture))
table_ATIPPE_ESW <- round(table_ATIPPE_ESW,2)



ATIPPE_summary <- round(CT_level_characteristics(data=SDOHdata, 
                                                 byvar_raw = "ATIPPE",
                                                 rank_type = "quintile"),
                        0)


EssentialServices_summary <- CT_level_characteristics(data=SDOHdata, 
                                                      byvar_raw = "employ_sales.trades.manufacturing.agriculture",
                                                      rank_type = "quintile")




# Save tables
write.csv(ATIPPE_summary, file="../Tables/ATIPPE_summary.csv", row.names = F, quote = F, na = "NA")
write.csv(EssentialServices_summary, file="../Tables/EssentialServices_summary.csv", row.names = F, quote = F, na = "NA")

write.csv(table_ESW_ATIPPE, file="../Tables/EssentialServices_ATIPPE_summary.csv", row.names = F, quote = F, na = "NA")
write.csv(table_ATIPPE_ESW, file="../Tables/ATIPPE_EssentialServices_summary.csv", row.names = F, quote = F, na = "NA")




#################### Spearman correlation coefficient ###################
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
scatterplot(SDOHcorr$ATIPPE_quintile,
            SDOHcorr$employ_sales.trades.manufacturing.agriculture_quintile,
            xlab="Income Quintile",
            ylab="Essential Worker Quintile",
            grid=FALSE,
            boxplot = FALSE,
            regLine = FALSE)

scatterplot(SDOHcorr$ATIPPE,
            SDOHcorr$employ_sales.trades.manufacturing.agriculture,
            xlab="Income",
            ylab="Proportion Essential Worker ",
            grid = FALSE,
            boxplot = FALSE,
            regLine = FALSE)


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


############### Missing CTs check (1254 to 1240 when merge mobility data) ###############

missingCT = SDOHdata[!SDOHdata$CTid %in% SDOHcorr$CTID,]
summary(missingCT)
summary(SDOHcorr)

missingCTsummary = rbind(summary(missingCT)[c(1,3,6),c(4,6,11)], 
                         summary(SDOHcorr)[c(1,3,6),c(2,3,5)])
rownames(missingCTsummary)= c(rep('missing',3), rep('current',3))
missingCTsummary


# Save tables
write.csv(missingCTsummary, file="../Tables/missingCTsummary.csv", row.names = F, quote = F, na = "NA")
