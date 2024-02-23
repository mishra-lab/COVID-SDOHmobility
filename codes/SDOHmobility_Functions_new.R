library(lubridate)
library(dplyr)
library(openxlsx)
library(tidyverse)
library(ggplot2)
library(lme4)

######################## Read Files ########################################
ReadSDOH <- function(SDOHdata){
  # Give better column names
  colnames(SDOHdata) <- c("PHU_ID","PHU_name", "CTid", "CT_pop", "median_tot_household_income",
                          "median_aftertax_household_income", "households", "1_person", "2_person", "3_person",
                          "4_person", "5+person", "CT_tot_incBT", "CT_tot_incAT", "single_person_equiv",
                          "BTIPPE", "ATIPPE", "employ_sales", "employ_trades", "employ_manufacturing",
                          "employ_agriculture", "employ_sales/trades/manufacturing/agriculture",
                          "employ_executive/managerial/professional", "employ_health", "employ_business",
                          "employ_eduaction/las/govt")
  
  # Keep useful columns related to income and essential services
  SDOHdata <- SDOHdata[,c(1:4,16:22)]
  # Remove Before tax income = 0
  SDOHdata[,5] <- ifelse(as.numeric(SDOHdata[,5]) <= 0, NA, SDOHdata[,5])
  SDOHdata <- SDOHdata[!is.na(SDOHdata[,5]),]
  
  SDOHdata[,3] <- as.numeric(SDOHdata[,3])/100
  
  return(SDOHdata)
}




################################# Ranking Functions ##################################
# Function to make population weighted quintile of SDOH
PopWeightedQuintile <- function(data, quintileCol, weightedCol){
  
  data[,quintileCol] <- as.numeric(data[,quintileCol])
  data[,quintileCol] <- ifelse(data[,quintileCol]<0, NA, data[,quintileCol])
  data <- data[!is.na(data[,quintileCol]),]
  data <- arrange(data, !!sym(quintileCol))
  cum_pop <- cumsum(data[,weightedCol])
  cum_pop_percent <- cum_pop/sum(data[,weightedCol])
  data$quintile <- NULL
  
  
  for (i in 1:dim(data)[1]){
    
    if (cum_pop_percent[i] <= 0.2) {
      data$quintile[i] <- 1
    } else if (cum_pop_percent[i] > 0.2 & cum_pop_percent[i] <= 0.4) {
      data$quintile[i] <- 2
    } else if (cum_pop_percent[i] > 0.4 & cum_pop_percent[i] <= 0.6) {
      data$quintile[i] <- 3
    } else if (cum_pop_percent[i] > 0.6 & cum_pop_percent[i] <= 0.8) {
      data$quintile[i] <- 4
    } else {data$quintile[i] <- 5}
    
  }
  
  names(data)[names(data)=="quintile"] <- paste0(quintileCol, "_quintile")
  return(data)
  
}



# Function to make population weighted quintile of SDOH ranking within PHU
PopWeightedQuintile_withinPHU <- function(data, quintileCol, weightedCol, PHU){
  columnnames <- c(quintileCol,weightedCol)
  
  data[,PHU] <- as.factor(data[,PHU])
  split_data <- split(data, data[,PHU])
  
  phu_subset <- NULL
  split_data_i <- NULL
  ranked_phu_subset <- NULL
  
  for (i in 1:nlevels(data[,PHU])) {
    split_data_i <- split_data[[i]]
    ranked_phu_subset <- PopWeightedQuintile(data=split_data_i, quintileCol = columnnames[1], weightedCol = columnnames[2])
    phu_subset <- rbind(phu_subset, ranked_phu_subset)
    
  }
  
  names(phu_subset)[names(phu_subset)==paste0(quintileCol, "_quintile")] <- paste0(quintileCol, "_quintile_withinPHU")
  return(phu_subset)
}



# Function to make population weighted tertile of SDOH
PopWeightedTertile <- function(data, tertileCol, weightedCol){
  
  data[,tertileCol] <- as.numeric(data[,tertileCol])
  data[,tertileCol] <- ifelse(data[,tertileCol]<0, NA, data[,tertileCol])
  data <- data[!is.na(data[,tertileCol]),]
  data <- arrange(data, !!sym(tertileCol))
  cum_pop <- cumsum(data[,weightedCol])
  cum_pop_percent <- cum_pop/sum(data[,weightedCol])
  data$tertile <- NULL
  
  
  for (i in 1:dim(data)[1]){
    
    if (cum_pop_percent[i] <= 0.33) {
      data$tertile[i] <- 1
    } else if (cum_pop_percent[i] > 0.33 & cum_pop_percent[i] <= 0.67) {
      data$tertile[i] <- 2
    } else {data$tertile[i] <- 3}
    
  }
  
  names(data)[names(data)=="tertile"] <- paste0(tertileCol, "_tertile")
  return(data)
  
}




# Function to make population weighted tertile of SDOH ranking within PHU
PopWeightedtertile_withinPHU <- function(data, tertileCol, weightedCol, PHU){
  columnnames <- c(tertileCol,weightedCol)
  
  data[,PHU] <- as.factor(data[,PHU])
  split_data <- split(data, data[,PHU])
  
  phu_subset <- NULL
  split_data_i <- NULL
  ranked_phu_subset <- NULL
  
  for (i in 1:nlevels(data[,PHU])) {
    split_data_i <- split_data[[i]]
    ranked_phu_subset <- PopWeightedTertileQuintile(data=split_data_i, tertileCol = columnnames[1], weightedCol = columnnames[2])
    phu_subset <- rbind(phu_subset, ranked_phu_subset)
    
  }
  
  names(phu_subset)[names(phu_subset)==paste0(tertileCol, "_tertile")] <- paste0(tertileCol, "_tertile_withinPHU")
  return(phu_subset)
}





################## Table Functions ################################

# Table 1
# Table1 <- function(data, SDOHcol){
# 
#   table1 <- mobilitySDOH_CT %>%
#     group_by(!!sym(SDOHcol)) %>%
#     summarise(n=length(unique(CTID)),
#               mean_devices=mean(avg_num_devices),
#               # sd_devices=sd(avg_num_devices),
#               mean_population=mean(CT_pop),
#               sd_population=sd(CT_pop),
#               prop_of_device = mean_devices/mean_population)
# 
#   return(round(table1, 2))
# 
# }

Table1_new <- function(data, SDOHcol){
  
  table1_pop = mobilitySDOH_CT %>%
    select(CTID, CT_pop, !!sym(SDOHcol)) %>%
    distinct() %>%
    group_by(!!sym(SDOHcol)) %>%
    summarise(mean_population=mean(CT_pop),
              sd_population=sd(CT_pop)
              ) %>%
    select(mean_population, sd_population)
  
  table1 <- mobilitySDOH_CT %>%
    # Select only the data in whole study period
    # filter(week >= 9 & week <= 51) %>%
    filter(week %in% c(45,46,47,49,50,51)) %>%
    mutate(prop_of_device = avg_num_devices/CT_pop) %>%
    group_by(!!sym(SDOHcol)) %>%
    summarise(n=length(unique(CTID)),
              mean_devices=mean(avg_num_devices),
              sd_devices=sd(avg_num_devices),
              # mean_population=mean(CT_pop),
              # sd_population=sd(CT_pop),
              mean_prop_of_device = mean(prop_of_device),
              sd_prop_of_device = sd(prop_of_device))
              # quintile_prop_of_device = mean_devices/mean_population)
  
  return(round(cbind(table1,table1_pop), 2))
  
}



# Table 2_new
Table2_new <- function(data, SDOHcol, Mobilitycol){
  
 
  table2_whole <- data %>%
    # Group the pre- and post- mobility
    group_by(!!sym(SDOHcol),CTID, policy) %>%
    summarise(percent_stay = mean(percent_stay),
              base_2019_percent_stay = mean(base_2019_percent_stay),
              mobility = 100 - percent_stay,
              base_2019_mobility = 100 - base_2019_percent_stay,
              prop_at_home = mean(prop_at_home),
              base_2019_prop_at_home = mean(base_2019_prop_at_home)) %>%
    # Generate the adjusted mobility
    mutate(adjusted_percent_stay = percent_stay - base_2019_percent_stay,
           adjusted_mobility = mobility - base_2019_mobility,
           adjusted_prop_at_home = prop_at_home - base_2019_prop_at_home,
    ) %>%
    ungroup() %>%
    # Long format to wide format
    pivot_wider(names_from = policy, values_from = c(percent_stay, base_2019_percent_stay, adjusted_percent_stay,
                                                     mobility, base_2019_mobility, adjusted_mobility,
                                                     prop_at_home, base_2019_prop_at_home, adjusted_prop_at_home)) %>%
    # Generate pre- and post- mobility change
    mutate(percent_stay_absolute_change = percent_stay_1 - percent_stay_0,
           percent_stay_adjusted_change = adjusted_percent_stay_1 - adjusted_percent_stay_0,
           mobility_absolute_change = mobility_1 - mobility_0,
           mobility_adjusted_change = adjusted_mobility_1 - adjusted_mobility_0,
           prop_at_home_absolute_change = mobility_1 - mobility_0,
           prop_at_home_adjusted_change = adjusted_prop_at_home_1 - adjusted_prop_at_home_0) %>%
    group_by(!!sym(SDOHcol)) %>%
    summarise(percent_stay_0 = mean(percent_stay_0),
              adjusted_percent_stay_0 = mean(adjusted_percent_stay_0),
              percent_stay_1 = mean(percent_stay_1),
              adjusted_percent_stay_1 = mean(adjusted_percent_stay_1),
              percent_stay_absolute_change = mean(percent_stay_absolute_change),
              percent_stay_adjusted_change = mean(percent_stay_adjusted_change),
              
              mobility_0 = mean(mobility_0),
              adjusted_mobility_0 = mean(adjusted_mobility_0),
              mobility_1 = mean(mobility_1),
              adjusted_mobility_1 = mean(adjusted_mobility_1),
              mobility_absolute_change = mean(mobility_absolute_change),
              mobility_adjusted_change = mean(mobility_adjusted_change),
              
              prop_at_home_0 = mean(prop_at_home_0),
              adjusted_prop_at_home_0 = mean(adjusted_prop_at_home_0),
              prop_at_home_1 = mean(prop_at_home_1),
              adjusted_prop_at_home_1 = mean(adjusted_prop_at_home_1),
              prop_at_home_absolute_change = mean(prop_at_home_absolute_change),
              prop_at_home_adjusted_change = mean(prop_at_home_adjusted_change)
    )
  
  

 
  table2_percent_stay <- table2_whole %>%
    select(contains("quintile"), contains("percent_stay"))
  colnames(table2_percent_stay) <- c(SDOHcol, "Crude pre-restriction","Adjusted pre-restriction",
                                     "Crude post-restriction","Adjusted post-restriction",
                                     "Absolute change","Adjusted change")
  
  table2_mobility <- table2_whole %>%
    select(contains("quintile"), contains("mobility"))
  colnames(table2_mobility) <- c(SDOHcol, "Crude pre-restriction","Adjusted pre-restriction",
                                 "Crude post-restriction","Adjusted post-restriction",
                                 "Absolute change","Adjusted change")
  
  table2_prop_at_home <- table2_whole %>%
    select(contains("quintile"), contains("prop_at_home"))
  colnames(table2_prop_at_home) <- c(SDOHcol, "Crude pre-restriction","Adjusted pre-restriction",
                                     "Crude post-restriction","Adjusted post-restriction",
                                     "Absolute change","Adjusted change")
  
  if (Mobilitycol=="percent_stay"){
    return(round(table2_percent_stay,1))
  } else if (Mobilitycol=="prop_at_home"){
    return(round(table2_prop_at_home,1))
  } else {return(round(table2_mobility,1))}
  
  

}


####################### Summary Tables #################################

CT_level_characteristics <- function(data, byvar_raw, rank_type){
  if(rank_type == "tertile"){
    byvar = paste0(byvar_raw, "_rank_tert")
  }
  if(rank_type == "quintile"){
    byvar = paste0(byvar_raw, "_quintile")
  }
  if(rank_type == "decile"){
    byvar = paste0(byvar_raw, "_rank_deci")
  }
  
  byvar <- as.name(byvar)                         # Create quosure
  byvar_raw <- as.name(byvar_raw)                 # Create quosure  
  
  CT_level =  data %>% 
    group_by(!!byvar) %>% 
    summarise(total_pop = sum(CT_pop),
              N = length(unique(CTid)),
              mean = mean(!!byvar_raw, na.rm = T),
              median = median(!!byvar_raw, na.rm = T),
              min = min(!!byvar_raw, na.rm = T),
              max = max(!!byvar_raw, na.rm = T))
  
  return(round(CT_level, 2))
}