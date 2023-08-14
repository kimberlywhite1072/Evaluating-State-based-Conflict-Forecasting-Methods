########### MASTER THESIS CODE 3 ##########
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

### Set working directory
setwd("C:/Users/ra46bel/Downloads")

### Clear environment
rm(list = ls())

### Objective: Calculate MSE cross validation value for Models A1, A2, and A3


########## IMPORTING / TEEING-UP DATASETS FOR SUBSEQUENT CODE ##########

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


########## DATA PREPARATION FOR MODEL A1 / MODEL A2 ##########

prepare_model_cm = function(cm_data,S){
  setDT(cm_data)
  cm_data = cm_data[order(cm_data$country_id, cm_data$month_id), ]
  month_max =  max(cm_data$month_id)
  cm_data[, future_target:=c(ged_dummy_sb[-(1:S)],rep(NA, S)), by=country_id]
  cm_data[, date_target:=c(date[-(1:S)],rep(NA, S)), by=country_id]
  cm_data_lag = cm_data[!is.na(future_target)]
  cm_data_lag$ged_dummy_sb = cm_data_lag$future_target
  return(cm_data_lag)
}

prepare_model_pgm = function(pgm_data,S){
  pgm_data <- pgm_data[order(pgm_data$pg_id, pgm_data$month_id), ]
  month_max =  max(pgm_data$month_id)
  setDT(pgm_data)
  pgm_data[, future_ged_dummy_sb:=c(ged_dummy_sb[-(1:S)],rep(NA, S)), by=pg_id]
  pgm_data[, future_ged_best_sb:=c(ged_best_sb[-(1:S)],rep(NA, S)), by=pg_id]
  pgm_data[, date_target:=c(date[-(1:S)],rep(NA, S)), by=pg_id]
  pgm_data_lag = pgm_data[!is.na(future_ged_dummy_sb)]
  return(pgm_data_lag)
}

data_prep = function(cm_data,pgm_data, S) {
  cm_data_1 = prepare_model_cm(cm_data =cm_data,S = S) 
  data_stage_1 = cm_data_1
  tmp_data = cm_data_1[ged_dummy_sb == 1, c("country_id", "key_cm")]
  pgm_data_1 = prepare_model_pgm(pgm_data,S = S)
  data_stage_2 = pgm_data_1[key_cm %in% tmp_data$key_cm]
  data_stage_3 = data_stage_2[future_ged_dummy_sb>0]
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


########## CROSS VALIDATION FOR MODEL A1 ##########

s_values = 2:7
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
    

  var.k = 100
  
  cm_data <- data.table(cm_data)
  pgm_data <- data.table(pgm_data)
  
  cm_data[, ":="(gryrtotal_smallarms        = get(paste0("gryrtotal_smallarms_lag_", var.lag)),
                 gryrtotal_lightweapons     = get(paste0("gryrtotal_lightweapons_lag_", var.lag)),
                 gryrtotal_explosives       = get(paste0("gryrtotal_explosives_lag_", var.lag)),
                 gryrtotal_mcw              = get(paste0("gryrtotal_mcw_lag_", var.lag)),
                 gryrtotal_other_undefinied = get(paste0("gryrtotal_other_undefinied_lag_", var.lag)),
                 
                 overlapping =get(paste0("overlapping_lag_", var.lag)),                      
                 change_year =get(paste0("change_year_lag_", var.lag))
  )]
  pgm_data[, ":="(neighbor1_fatality_bin_high = get(paste0("neighbor1_fatality_bin_high_lag_", var.lag)),
                  neighbor2_appear_bin_low  =get(paste0("neighbor2_appear_bin_low_lag_", var.lag)),                   
                  neighbor2_transborder_bin_low =get(paste0("neighbor2_transborder_bin_low_lag_", var.lag)),             
                  overlapping =get(paste0("overlapping_lag_", var.lag)),                            
                  nearest_dist =get(paste0("nearest_dist_lag_", var.lag))
  )]
  
  cm_data[, noofgroups := get(paste0("noofgroups_lag_", var.lag))]
  
  all_data = data_prep(cm_data = cm_data, pgm_data = pgm_data, S = s)
  
  # Set up the formula for the GAMM
  formula <- future_ged_best_sb ~ s(month_id, bs="gp") +
                   factor(month) +
                   s(time_since_ged_dummy_os.x, bs="ps")+
                   s(time_since_ged_dummy_ns.x, bs="ps")+
                   s(time_since_ged_dummy_sb.x, bs="ps")+
                   pgd_imr_mean +
                   s(log1p(ged_best_sb), bs="ps") +
                   log1p(ged_best_os) +
                   log1p(ged_best_ns) +
                   log(fvp_gdp200)  +
                   polity +
                   log(milit_exp) +
                   pgd_capdist*log1p(mcw_receiver_rolling)+
                   pgd_capdist*log1p(mcw_receiver_acute) +
                   te(long, lat)+
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
                   log10(nearest_dist)
  
  # Set up the number of folds for cross-validation
  k <- 5
  
  # Split the data into k folds
  set.seed(123)  # For reproducibility
  folds <- cut(seq_len(nrow(all_data$stage_3)), breaks = k, labels = FALSE)
  
  # Initialize a vector to store the cross-validated performance metrics
  cv_metrics <- numeric(k)
  
  # Perform k-fold cross-validation
  for (i in 1:k) {
    # Subset the data into training and validation sets
    train_data <- all_data$stage_3[folds != i, ]
    val_data <- all_data$stage_3[folds == i, ]
    
    # Fit the GAMM model on the training data
    model <- bam(formula, data = train_data, family = ztpoisson(), discrete = T, nthreads = 20, use.chol = T)
    
    # Make predictions on the validation data
    predictions <- predict(model, newdata = val_data, type = "response")
    val_data$predictions <- predictions
    
    # Don't forget about the first two stages of the model!
    merged_val_data <- merge(all_data$stage_2, val_data[ , c("predictions", "lat", "long", "year", "month")],by = c("lat", "long", "year", "month"), all.x = TRUE)
    subset <- subset(merged_val_data, select = c(future_ged_best_sb, predictions))
    rm(merged_val_data)
    subset[is.na(subset)] <- 0
    
    # Evaluate the model performance (using MSE)
    mse = mean((log1p(subset$predictions) - log1p(subset$future_ged_best_sb))^2)
    cv_metrics[i] <- mse
  }
  
  # Calculate the average performance metric across the folds
  mean_cv_metric <- mean(cv_metrics)
  print(mean_cv_metric)

  gc()
  
}


########## CROSS VALIDATION FOR MODEL A2 ##########

s_values = 2:5
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
  
  
  var.k = 100
  
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
  
  # Set up the formula for the GAMM
  formula <- future_ged_best_sb ~ s(month_id, bs="gp") +
    factor(month) +
    s(time_since_ged_dummy_os.x, bs="ps")+
    s(time_since_ged_dummy_ns.x, bs="ps")+
    s(time_since_ged_dummy_sb.x, bs="ps")+
    pgd_imr_mean +
    s(log1p(ged_best_sb), bs="ps") +
    log1p(ged_best_os) +
    log1p(ged_best_ns) +
    log(fvp_gdp200)  +
    polity +
    log(milit_exp) +
    pgd_capdist*log1p(mcw_receiver_rolling)+
    pgd_capdist*log1p(mcw_receiver_acute) +
    te(long, lat)+
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
    log_population+
    reg_aut+
    recruitment+
    NL_mean+
    temp_anomaly+
    precip_anomaly
  
  # Set up the number of folds for cross-validation
  k <- 5
  
  # Split the data into k folds
  set.seed(123)  # For reproducibility
  folds <- cut(seq_len(nrow(all_data$stage_3)), breaks = k, labels = FALSE)
  
  # Initialize a vector to store the cross-validated performance metrics
  cv_metrics <- numeric(k)
  
  # Perform k-fold cross-validation
  for (i in 1:k) {
    # Subset the data into training and validation sets
    train_data <- all_data$stage_3[folds != i, ]
    val_data <- all_data$stage_3[folds == i, ]
    
    # Fit the GAMM model on the training data
    model <- bam(formula, data = train_data, family = ztpoisson(), discrete = T, nthreads = 20, use.chol = T)
    
    # Make predictions on the validation data
    predictions <- predict(model, newdata = val_data, type = "response")
    val_data$predictions <- predictions
    
    # Don't forget about the first two stages of the model!
    merged_val_data <- merge(all_data$stage_2, val_data[ , c("predictions", "lat", "long", "year", "month")],by = c("lat", "long", "year", "month"), all.x = TRUE)
    subset <- subset(merged_val_data, select = c(future_ged_best_sb, predictions))
    rm(merged_val_data)
    subset[is.na(subset)] <- 0
    
    # Evaluate the model performance (using MSE)
    mse = mean((log1p(subset$predictions) - log1p(subset$future_ged_best_sb))^2)
    cv_metrics[i] <- mse
  }
  
  # Calculate the average performance metric across the folds
  mean_cv_metric <- mean(cv_metrics)
  print(mean_cv_metric)
  
  gc()
  
}


########## DATA PREPARATION FOR MODEL A3 ##########

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



########## CROSS VALIDATION FOR MODEL A3 ##########

s_values = 2:7
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
  
  # Set up the formula for the GAMM
  formula <- future_fat_best_sb ~ s(month_id, bs="gp") +
    factor(month) +
    s(time_since_fat_dummy_os, bs="ps")+
    s(time_since_fat_dummy_ns, bs="ps")+
    s(time_since_fat_dummy_sb, bs="ps")+
    pgd_imr_mean +
    s(log1p(fat_best_sb), bs="ps") +
    log1p(fat_best_os) +
    log1p(fat_best_ns) +
    log(fvp_gdp200)  +
    polity +
    log(milit_exp) +
    pgd_capdist*log1p(mcw_receiver_rolling)+
    pgd_capdist*log1p(mcw_receiver_acute) +
    te(long, lat)+
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
  folds <- cut(seq_len(nrow(all_data$stage_3)), breaks = k, labels = FALSE)
  
  # Initialize a vector to store the cross-validated performance metrics
  cv_metrics <- numeric(k)
  
  # Perform k-fold cross-validation
  for (i in 1:k) {
    # Subset the data into training and validation sets
    train_data <- all_data$stage_3[folds != i, ]
    val_data <- all_data$stage_3[folds == i, ]
    
    # Fit the GAMM model on the training data
    model <- bam(formula, data = train_data, family = ztpoisson(), discrete = T, nthreads = 20, use.chol = T)
    
    # Make predictions on the validation data
    predictions <- predict(model, newdata = val_data, type = "response")
    val_data$predictions <- predictions
    
    # Don't forget about the first two stages of the model!
    merged_val_data <- merge(all_data$stage_2, val_data[ , c("predictions", "lat", "long", "year", "month")],by = c("lat", "long", "year", "month"), all.x = TRUE)
    subset <- subset(merged_val_data, select = c(future_fat_best_sb, predictions))
    rm(merged_val_data)
    subset[is.na(subset)] <- 0
    subset <- lapply(subset, function(x) {
      ifelse(is.infinite(x), 0, x)
    })
    
    # Evaluate the model performance (using MSE)
    mse = mean((log1p(subset$predictions) - log1p(subset$future_fat_best_sb))^2)
    cv_metrics[i] <- mse
  }
  
  # Calculate the average performance metric across the folds
  mean_cv_metric <- mean(cv_metrics)
  print(mean_cv_metric)
  
  gc()
  
}


