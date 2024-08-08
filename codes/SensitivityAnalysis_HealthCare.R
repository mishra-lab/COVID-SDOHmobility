
library(lubridate)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(lme4)


# Source file working directory
setwd("C:/Users/WangSiyi/Documents/GitHub/SDOH-Mobility/Rfiles")


# mobilityCT <- read.csv("D:/UT/Master/COVID-19/mobility/update/20210301/ctuid_level_weekly_20210404_20210410.csv", fileEncoding="UTF-8-BOM")
mobilityCT <- read.csv("../Data/ctuid_level_weekly_20210425_20210501.csv", fileEncoding="UTF-8-BOM")
length(unique(mobilityCT$CTID))
income_occ_CT <- read.xlsx("../Data/COVID19 Modeling - income-occ by CT - 2020-12-16.xlsx", na.strings = ".")

source("./SDOHmobility_Functions_new.R")


SDOHdata <- ReadSDOH(income_occ_CT)

########################## Sensitivity analysis ####################

# Conduct the all the analysis using 'essential_wHealth_quintile' instead of 'employ_sales.trades.manufacturing.agriculture' in SDOHmobility_Results.R, SDOHmobility_Figures.R, and SDOHmobility_DID.R
