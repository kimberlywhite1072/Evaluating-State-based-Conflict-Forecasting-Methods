########## MASTER THESIS CODE 13 ##########
########## Kimberly A. White #############


########## STAGING AREA & OBJECTIVE ##########

### Load packages
library(stringr)
library(lubridate)
library(dplyr)
library(dat)
library(mgcv)
library(countreg)
library(pryr)
library(data.table)
library(DEoptim)
library(markovchain)
library(tidyr)
library(gbm)

### Set working directory
setwd("C:/Users/ra46bel/Downloads")

### Clear environment
rm(list = ls())

### Objective: Run a Markov simulation and compare to Model C3 results


########## IMPORTING / TEEING-UP DATASETS FOR SUBSEQUENT CODE ##########

### Country level 
load("cm_model_data_final.RData")

cm_data <- cm_model_data
rm(cm_model_data)

# Ensure the data is sorted in chronological order by 'country_id.'
cm_data <- cm_data %>%
  arrange(country_id, year, month)

# Convert 'ged_dummy_sb' column to numeric
cm_data$ged_dummy_sb <- as.numeric(cm_data$ged_dummy_sb)

# Subset the dataset to only keep the relevant columns
cm_data <- subset(cm_data, select = c(month, year, ged_dummy_sb, country_id))

### PRIO-Grid cell level 
load("pgm_model_data_final.RData")

pgm_data <- pgm_model_data
rm(pgm_model_data)

# Ensure the data is sorted in chronological order by 'country_id.'
pgm_data <- pgm_data %>%
  arrange(pg_id, year, month)

# Convert 'ged_dummy_sb' column to numeric
pgm_data$ged_dummy_sb <- as.numeric(pgm_data$ged_dummy_sb)

# Subset the dataset to only keep the relevant columns
pgm_data <- subset(pgm_data, select = c(month, year, ged_dummy_sb, pg_id))



########## RUN MARKOV SIMULATION AT COUNTRY LEVEL FOR S = 2 (TWO MONTHS IN FUTURE) ##########

# Create a lagged version of 'ged_dummy_sb' to calculate transition probabilities
cm_data <- cm_data %>%
  group_by(country_id) %>%
  mutate(lagged_ged_dummy_sb = lag(ged_dummy_sb)) %>%
  ungroup()

# Drop rows with missing values (all January 1990 values)
cm_data <- tidyr::drop_na(cm_data)

# Calculate the transition matrix based on 'lagged_ged_dummy_sb' and 'ged_dummy_sb'
transition_matrix <-cm_data %>%
  group_by(lagged_ged_dummy_sb, ged_dummy_sb) %>%
  summarise(transitions = n()) %>%
  pivot_wider(names_from = ged_dummy_sb, values_from = transitions, values_fill = 0) %>%
  ungroup()

# Convert counts to probabilities in the transition matrix
transition_matrix$`1` <- transition_matrix$`1` / (transition_matrix$`1` + transition_matrix$`0`)
transition_matrix$`0` <- 1 - transition_matrix$`1`

# Function to simulate the next value on transition probabilities
simulate_next_state <- function(current_state) {
  if (current_state == 1) {
    sample(c(1, 0), 1, prob = c(transition_matrix$`1`[1], transition_matrix$`0`[1]))
  } else {
    sample(c(1, 0), 1, prob = c(transition_matrix$`1`[2], transition_matrix$`0`[2]))
  }
}

# Initialize a list to store the simulated data
simulated_data_list <- list()

# Group by 'country_id' and simulate the next two months
simulated_country_data <- cm_data %>%
  group_by(country_id) %>%
  mutate(
    simulated_ged_dummy_sb = list(
      c(ged_dummy_sb, 
        replicate(2, {
          current_state <- last(ged_dummy_sb)
          simulate_next_state(current_state)
        })
      )
    )
  ) %>%
  ungroup()

# Subset the 'simulated_country_data' to keep only 'country_id' and 'simulated_ged_dummy_sb'
grouped_data <- subset(simulated_country_data, select = c(country_id, simulated_ged_dummy_sb))

# Remove duplicate rows to ensure one row per country
grouped_data <- distinct(grouped_data)

# Modify the lists to remove the first two values
grouped_data$simulated_ged_dummy_sb <- map(grouped_data$simulated_ged_dummy_sb, ~ .[-c(1:2)])

# Expand the list of simulated states to individual rows in the dataset
expanded_data <- unnest(grouped_data, cols = simulated_ged_dummy_sb)

# Merge the expanded data with the original 'cm_data' using 'country_id'
cm_data <- cbind(cm_data, expanded_data)

# Subset for only relevant variables
cm_data <- subset(cm_data, select = c(month, year, country_id, ged_dummy_sb, simulated_ged_dummy_sb))
cm_data_markov <- cm_data



########## RUN MARKOV SIMULATION AT PRIO-GRID CELL LEVEL FOR S = 2 (TWO MONTHS IN FUTURE) ##########

# Create a lagged version of 'ged_dummy_sb' to calculate transition probabilities
pgm_data <- pgm_data %>%
  group_by(pg_id) %>%
  mutate(lagged_ged_dummy_sb = lag(ged_dummy_sb)) %>%
  ungroup()

# Drop rows with missing values (all January 1990 values)
pgm_data <- tidyr::drop_na(pgm_data)

# Calculate the transition matrix based on 'lagged_ged_dummy_sb' and 'ged_dummy_sb'
transition_matrix <-pgm_data %>%
  group_by(lagged_ged_dummy_sb, ged_dummy_sb) %>%
  summarise(transitions = n()) %>%
  pivot_wider(names_from = ged_dummy_sb, values_from = transitions, values_fill = 0) %>%
  ungroup()

# Convert counts to probabilities in the transition matrix
transition_matrix$`1` <- transition_matrix$`1` / (transition_matrix$`1` + transition_matrix$`0`)
transition_matrix$`0` <- 1 - transition_matrix$`1`

# Function to simulate the next value on transition probabilities
simulate_next_state <- function(current_state) {
  if (current_state == 1) {
    sample(c(1, 0), 1, prob = c(transition_matrix$`1`[1], transition_matrix$`0`[1]))
  } else {
    sample(c(1, 0), 1, prob = c(transition_matrix$`1`[2], transition_matrix$`0`[2]))
  }
}

# Initialize a list to store the simulated data
simulated_data_list <- list()

# Group by 'pg_id' and simulate the next two months
simulated_pg_data <- pgm_data %>%
  group_by(pg_id) %>%
  mutate(
    simulated_ged_dummy_sb = list(
      c(ged_dummy_sb, 
        replicate(2, {
          current_state <- last(ged_dummy_sb)
          simulate_next_state(current_state)
        })
      )
    )
  ) %>%
  ungroup()

# Subset the 'simulated_pg_data' to keep only 'pg_id' and 'simulated_ged_dummy_sb'
grouped_data <- subset(simulated_pg_data, select = c(pg_id, simulated_ged_dummy_sb))

# Remove duplicate rows to ensure one row per pg
grouped_data <- distinct(grouped_data)

# Modify the lists to remove the first two values
grouped_data$simulated_ged_dummy_sb <- map(grouped_data$simulated_ged_dummy_sb, ~ .[-c(1:2)])

# Expand the list of simulated states to individual rows in the dataset
expanded_data <- unnest(grouped_data, cols = simulated_ged_dummy_sb)

# Merge the expanded data with the original 'pgm_data' using 'pg_id'
pgm_data <- cbind(pgm_data, expanded_data)

# Subset for only relevant variables
pgm_data <- subset(pgm_data, select = c(month, year, pg_id, ged_dummy_sb, simulated_ged_dummy_sb))
pgm_data_markov <- pgm_data


########## COMPARE MARKOV SIMULATION RESULTS TO GBM WITH DATASET 3 ##########


########## CROSS VALIDATION FOR MODEL C3 ##########

load("cm_model_data_final.RData")
load("pgm_model_data_final.RData")

cm_data <- cm_model_data
pgm_data <- pgm_model_data
rm(cm_model_data)
rm(pgm_model_data)

cm_data = cm_data[order(cm_data$month_id,cm_data$country_id), ]
pgm_data = pgm_data[order(pgm_data$month_id, pgm_data$pg_id), ]

# Rename columns without temporal aggregation to lag_1 (corresponds to temporal
# aggregation over one month, i.e. no aggregation)

cm_data <- cm_data %>%
  rename_with(~paste0(., "_lag_1"), 
              c("gryrtotal_smallarms",
                "gryrtotal_lightweapons",
                "gryrtotal_explosives",
                "gryrtotal_mcw",
                "gryrtotal_other_undefinied",
                "noofgroups",
                "overlapping",
                "change_year",
                "status_excluded",
                "reg_aut",
                "recruitment",
                "NL_mean",
                "log_population",
                "temp_anomaly",
                "precip_anomaly"))

pgm_data <- pgm_data %>%
  rename_with(~paste0(., "_lag_1"), 
              c("neighbor1_fatality_bin_high",
                "neighbor2_appear_bin_low",
                "neighbor2_transborder_bin_low",
                "overlapping",
                "nearest_dist",
                "status_excluded",
                "reg_aut",
                "recruitment",
                "NL_mean",
                "log_population",
                "temp_anomaly",
                "precip_anomaly"))

# Include date variable (new version)
cm_data$date = as.Date(cm_data$date)
min_month = min(pgm_data$date)
cm_data$month_rescaled =  cm_data$month_id - min(cm_data$month_id)
cm_data$date_new = ymd(min_month)  + months(cm_data$month_rescaled)
cm_data$delay_sb = cm_data$ged_dummy_sb
cm_data$name_fac = factor(cm_data$name_fac)
min_month = min(pgm_data$date)
pgm_data$month_rescaled =  pgm_data$month_id - min(pgm_data$month_id)
pgm_data$date_new = ymd(min_month)  + months(pgm_data$month_rescaled)
pgm_data$date = as.Date(pgm_data$date)
# Check the country_ids and country_names
tmp_data <- data.table::data.table(cm_data)[, .(country_name, country_id)]
# Check for each country_id/country_name tuple which id is used most often and use this
tmp_data = tmp_data[, .(country_id = names(table(country_id))[which.max(table(country_id))]), by=country_name]
cm_data$country_id = tmp_data$country_id[match(cm_data$country_name,tmp_data$country_name)]
pgm_data$country_id = tmp_data$country_id[match(pgm_data$country_name,tmp_data$country_name)]


########## DATA PREPARATION FOR MODEL C3 ##########

prepare_model_cm = function(cm_data,S){
  setDT(cm_data)
  cm_data = cm_data[order(cm_data$country_id, cm_data$month_id), ]
  month_max =  max(cm_data$month_id)
  cm_data[, future_target:=c(fat_dummy_sb[-(1:S)],rep(NA, S)), by=country_id]
  cm_data[, date_target:=c(date[-(1:S)],rep(NA, S)), by=country_id]
  cm_data_lag = cm_data[!is.na(future_target)]
  cm_data_lag$fat_dummy_sb = cm_data_lag$future_target
  return(cm_data_lag)
}

prepare_model_pgm = function(pgm_data,S){
  pgm_data <- pgm_data[order(pgm_data$pg_id, pgm_data$month_id), ]
  month_max =  max(pgm_data$month_id)
  setDT(pgm_data)
  pgm_data[, future_fat_dummy_sb:=c(fat_dummy_sb[-(1:S)],rep(NA, S)), by=pg_id]
  pgm_data[, future_fat_best_sb:=c(fat_best_sb[-(1:S)],rep(NA, S)), by=pg_id]
  pgm_data[, date_target:=c(date[-(1:S)],rep(NA, S)), by=pg_id]
  pgm_data_lag = pgm_data[!is.na(future_fat_dummy_sb)]
  return(pgm_data_lag)
}

data_prep = function(cm_data,pgm_data, S) {
  cm_data_1 = prepare_model_cm(cm_data =cm_data,S = S) 
  data_stage_1 = cm_data_1
  tmp_data = cm_data_1[fat_dummy_sb == 1, c("country_id", "key_cm")]
  pgm_data_1 = prepare_model_pgm(pgm_data,S = S)
  data_stage_2 = pgm_data_1[key_cm %in% tmp_data$key_cm]
  data_stage_3 = data_stage_2[future_fat_dummy_sb>0]
  test_data_stage_1 = data_stage_1[date_target == max(date_target)]
  calibrate_data_stage_1 = data_stage_1[date_target == max(date_target) - months(S)]
  train_data_stage_1 = data_stage_1[date_target < (max(date_target) - months(S))]
  do_nothing_data_1 = data_stage_1[date_target %within% interval(max(date_target) - months(S) + months(1),
                                                                 max(date_target) - months(1))]
  test_data_stage_2 = data_stage_2[date == max(date)]
  calibrate_data_stage_2 = data_stage_2[date_target == (max(date_target) - months(S))]
  train_data_stage_2 = data_stage_2[date_target < (max(date_target) - months(S))]
  do_nothing_data_2 = data_stage_2[date_target %within% interval(max(date_target) - months(S) + months(1),
                                                                 max(date_target) - months(1))]
  test_data_stage_3 = data_stage_3[date == max(date)]
  calibrate_data_stage_3 = data_stage_3[date_target == (max(date_target) - months(S))]
  train_data_stage_3 = data_stage_3[date_target < (max(date_target) - months(S))]
  do_nothing_data_3 = data_stage_3[date_target %within% interval(max(date_target) - months(S) + months(1),
                                                                 max(date_target) - months(1))]
  test_two_stage = pgm_data_1[date == max(date)]
  calibrate_two_stage = pgm_data_1[date_target == (max(date_target) - months(S))]
  train_two_stage = pgm_data_1[date_target < (max(date_target) - months(S))]
  all_pgm_data_train = pgm_data_1[date_target <= (max(date_target) - months(S))]
  pgm_calibrate_1 = pgm_data_1[date_target  == (max(date_target) - months(S))]
  cm_calibrate_1 =cm_data_1[date_target  == (max(date_target) - months(S))]
  pgm_data_1 = pgm_data_1[date == max(date)]
  cm_data_1 =cm_data_1[date == max(date)]
  
  return(list(stage_1 = data_stage_1,stage_2 = data_stage_2,stage_3 = data_stage_3, 
              test_data_stage_1 = test_data_stage_1, 
              test_data_stage_2 = test_data_stage_2,
              test_data_stage_3 = test_data_stage_3, 
              train_data_stage_1 = train_data_stage_1, 
              train_data_stage_2 = train_data_stage_2, 
              train_data_stage_3 = train_data_stage_3, 
              test_two_stage = test_two_stage, 
              train_two_stage = train_two_stage, 
              calibrate_two_stage = calibrate_two_stage, 
              calibrate_data_stage_1 = calibrate_data_stage_1,
              calibrate_data_stage_2 = calibrate_data_stage_2, 
              calibrate_data_stage_3 = calibrate_data_stage_3, 
              cm_data_comp = cm_data_1, 
              pgm_data_comp = pgm_data_1,
              all_pgm_data_train = all_pgm_data_train, 
              pgm_calibrate_1 = pgm_calibrate_1, cm_calibrate_1 = cm_calibrate_1, 
              do_nothing_data_1= do_nothing_data_1, 
              do_nothing_data_2= do_nothing_data_2, 
              do_nothing_data_3 = do_nothing_data_3))
}


########## CROSS VALIDATION FOR MODEL C3 FOR S = 2 ##########

s_values = 2
for(s in s_values) {
  
  if(s == 2){
    var.lag <- 5
  }else if(s == 3){
    var.lag <- 4
  }else if(s == 4){
    var.lag <- 3
  }else if(s == 5){
    var.lag <- 2
  }else{
    var.lag <- 1
  }
  
  cm_data <- data.table(cm_data)
  pgm_data <- data.table(pgm_data)
  
  cm_data[, ":="(gryrtotal_smallarms        = get(paste0("gryrtotal_smallarms_lag_", var.lag)),
                 gryrtotal_lightweapons     = get(paste0("gryrtotal_lightweapons_lag_", var.lag)),
                 gryrtotal_explosives       = get(paste0("gryrtotal_explosives_lag_", var.lag)),
                 gryrtotal_mcw              = get(paste0("gryrtotal_mcw_lag_", var.lag)),
                 gryrtotal_other_undefinied = get(paste0("gryrtotal_other_undefinied_lag_", var.lag)),
                 
                 overlapping =get(paste0("overlapping_lag_", var.lag)),                      
                 change_year =get(paste0("change_year_lag_", var.lag)),
                 
                 status_excluded =get(paste0("status_excluded_lag_", var.lag)),                      
                 reg_aut =get(paste0("reg_aut_lag_", var.lag)),
                 recruitment =get(paste0("recruitment_lag_", var.lag)),              
                 NL_mean =get(paste0("NL_mean_lag_", var.lag)),
                 log_population =get(paste0("log_population_lag_", var.lag)),
                 precip_anomaly =get(paste0("precip_anomaly_lag_", var.lag)),
                 temp_anomaly =get(paste0("temp_anomaly_lag_", var.lag))
  )]
  pgm_data[, ":="(neighbor1_fatality_bin_high = get(paste0("neighbor1_fatality_bin_high_lag_", var.lag)),
                  neighbor2_appear_bin_low  =get(paste0("neighbor2_appear_bin_low_lag_", var.lag)),                   
                  neighbor2_transborder_bin_low =get(paste0("neighbor2_transborder_bin_low_lag_", var.lag)),             
                  overlapping =get(paste0("overlapping_lag_", var.lag)),                            
                  nearest_dist =get(paste0("nearest_dist_lag_", var.lag)),
                  
                  status_excluded =get(paste0("status_excluded_lag_", var.lag)),                      
                  reg_aut =get(paste0("reg_aut_lag_", var.lag)),
                  recruitment =get(paste0("recruitment_lag_", var.lag)),              
                  NL_mean =get(paste0("NL_mean_lag_", var.lag)),
                  log_population =get(paste0("log_population_lag_", var.lag)),
                  precip_anomaly =get(paste0("precip_anomaly_lag_", var.lag)),
                  temp_anomaly =get(paste0("temp_anomaly_lag_", var.lag))
  )]
  
  cm_data[, noofgroups := get(paste0("noofgroups_lag_", var.lag))]
  
  all_data = data_prep(cm_data = cm_data, pgm_data = pgm_data, S = s)
  
  # Set up the formula
  formula <- future_fat_dummy_sb ~ month_id +
    factor(month) +
    time_since_fat_dummy_os+
    time_since_fat_dummy_ns+
    time_since_fat_dummy_sb+
    pgd_imr_mean +
    log1p(fat_best_sb) +
    log1p(fat_best_os) +
    log1p(fat_best_ns) +
    log(fvp_gdp200)  +
    polity +
    log(milit_exp) +
    pgd_capdist*log1p(mcw_receiver_rolling)+
    pgd_capdist*log1p(mcw_receiver_acute) +
    long+ lat+
    gryrtotal_smallarms +
    gryrtotal_lightweapons + 
    gryrtotal_explosives + 
    gryrtotal_mcw + 
    gryrtotal_other_undefinied +
    gryrtotal_mcw*log1p(mcw_receiver_rolling)+
    gryrtotal_mcw*log1p(mcw_receiver_acute)+
    gryrtotal_mcw*pgd_capdist+
    noofgroups+
    neighbor1_fatality_bin_high+
    neighbor2_appear_bin_low+                 
    neighbor2_transborder_bin_low+       
    overlapping+
    log10(nearest_dist)+
    status_excluded+
    reg_aut+
    recruitment+
    log_population+
    NL_mean+
    temp_anomaly+
    precip_anomaly
  
  # Set up the number of folds for cross-validation
  k <- 5
  
  # Split the data into k folds
  set.seed(123)  # For reproducibility
  folds <- cut(seq_len(nrow(all_data$stage_2)), breaks = k, labels = FALSE)
  
  # Initialize a vector to store the cross-validated performance metrics
  cv_metrics <- numeric(k)
  
  # Perform k-fold cross-validation
  for (i in 1:k) {
    # Subset the data into training and validation sets
    train_data <- all_data$stage_2[folds != i, ]
    val_data <- all_data$stage_2[folds == i, ]
    
    # Fit the Gradient Boosting model on the training data
    model <- gbm(formula, data = train_data, distribution = "bernoulli")
    
    # Make predictions on the validation data
    predictions <- predict(model, newdata = val_data, type = "response")
    val_data$predictions <- predictions
    
    # Don't forget about the first two stages of the model!
    merged_val_data <- merge(all_data$stage_2, val_data[ , c("predictions", "lat", "long", "year", "month")],by = c("lat", "long", "year", "month"), all.x = TRUE)
    subset <- subset(merged_val_data, select = c(future_fat_dummy_sb, predictions))
    rm(merged_val_data)
    subset[is.na(subset)] <- 0
    subset <- lapply(subset, function(x) {
      ifelse(is.infinite(x), 0, x)
    })
    
    # Evaluate the model performance (using MSE)
    mse = mean((log1p(subset$predictions) - log1p(subset$future_fat_dummy_sb))^2)
    cv_metrics[i] <- mse
  }
  
  # Calculate the average performance metric across the folds
  mean_cv_metric <- mean(cv_metrics)
  print(mean_cv_metric)
  
  gc()
  
}

load("pgm_model_data_final.RData")
pgm_all <- merge(all_data$stage_2, pgm_data_markov, by = c("pg_id", "month", "year"))
mse = mean((log1p(pgm_all$simulated_ged_dummy_sb) - log1p(pgm_all$future_fat_dummy_sb))^2)


