########## MASTER THESIS CODE 2 ##########
########## Kimberly A. White #############


########## STAGING AREA & OBJECTIVE ##########

### Load packages
library(dplyr)
library(data.table)

### Objective: Incorporate the newly-created PGM-level variables
### into the CM-level dataset

### Clear environment
rm(list = ls())

### Set working directory
setwd("C:/Users/ra46bel/Downloads")

### (1) The following country-level data is the final and most previous dataset
load("cm_data.RData")
cm_model_data <- cm_data

### (2) Load dataset saved from previous code
load("pgm_data_all_variables.RData")

# Subset for only relevant variables
pgm_data_cm <- subset(pgm_model_data, select=c(year, country_name, month,
                    lat, long, reg_aut, status_alone, status_share, status_excluded, 
                    status_irrelevant, claim, recruitment, support, 
                    population, NL_sum, NL_mean, NL_max, mean_temp_celsius,
                    temp_anomaly, mean_precip, precip_anomaly, acled_count_sb,
                    acled_count_ns, acled_count_os, acled_dummy_sb, acled_dummy_ns,
                    acled_dummy_os))


########## STATUS_ALONE ##########

### In a given country-month, if there is at least one occurrence of 'status_alone' == 1 
### in the pgm_data_cm dataset, then set new variable 'status_alone' in cm_data == 1.
### Otherwise, set 'status_alone' in cm_data == 0

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
status_alone_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(status_alone = max(status_alone, na.rm = TRUE)) %>%
  ungroup()

# Merge status_alone_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, status_alone_agg, by = c("country_name", "month", "year"), all.x = TRUE)
cm_data$status_alone[is.infinite(cm_data$status_alone) & cm_data$status_alone < 0] <- 0


########## STATUS_SHARE ##########

### In a given country-month, if there is at least one occurrence of 'status_share' == 1 
### in the pgm_data_cm dataset, then set new variable 'status_share' in cm_data == 1.
### Otherwise, set 'status_share' in cm_data == 0

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
status_share_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(status_share = max(status_share, na.rm = TRUE)) %>%
  ungroup()

# Merge status_share_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, status_share_agg, by = c("country_name", "month", "year"), all.x = TRUE)
cm_data$status_share[is.infinite(cm_data$status_share) & cm_data$status_share < 0] <- 0


########## STATUS_EXCLUDED ##########

### In a given country-month, if there is at least one occurrence of 'status_excluded' == 1 
### in the pgm_data_cm dataset, then set new variable 'status_excluded' in cm_data == 1.
### Otherwise, set 'status_excluded' in cm_data == 0

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
status_excluded_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(status_excluded = max(status_excluded, na.rm = TRUE)) %>%
  ungroup()

# Merge status_excluded_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, status_excluded_agg, by = c("country_name", "month", "year"), all.x = TRUE)
cm_data$status_excluded[is.infinite(cm_data$status_excluded) & cm_data$status_excluded < 0] <- 0


########## STATUS_IRRELEVANT ##########

### In a given country-month, if there is at least one occurrence of 'status_irrelevant' == 1 
### in the pgm_data_cm dataset, then set new variable 'status_irrelevant' in cm_data == 1.
### Otherwise, set 'status_irrelevant' in cm_data == 0

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
status_irrelevant_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(status_irrelevant = max(status_irrelevant, na.rm = TRUE)) %>%
  ungroup()

# Merge status_irrelevant_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, status_irrelevant_agg, by = c("country_name", "month", "year"), all.x = TRUE)
cm_data$status_irrelevant[is.infinite(cm_data$status_irrelevant) & cm_data$status_irrelevant < 0] <- 0


########## RECRUITMENT ##########

### In a given country-month, if there is at least one occurrence of 'recruitment' == 1 
### in the pgm_data_cm dataset, then set new variable 'recruitment' in cm_data == 1.
### Otherwise, set 'recruitment' in cm_data == 0

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
recruitment_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(recruitment = max(recruitment, na.rm = TRUE)) %>%
  ungroup()

# Merge recruitment_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, recruitment_agg, by = c("country_name", "month", "year"), all.x = TRUE)
cm_data$recruitment[is.infinite(cm_data$recruitment) & cm_data$recruitment < 0] <- 0


########## SUPPORT ##########

### In a given country-month, if there is at least one occurrence of 'support' == 1 
### in the pgm_data_cm dataset, then set new variable 'support' in cm_data == 1.
### Otherwise, set 'support' in cm_data == 0

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
support_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(support = max(support, na.rm = TRUE)) %>%
  ungroup()

# Merge support_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, support_agg, by = c("country_name", "month", "year"), all.x = TRUE)
cm_data$support[is.infinite(cm_data$support) & cm_data$support < 0] <- 0


########## CLAIM ##########

### In a given country-month, if there is at least one occurrence of 'claim' == 1 
### in the pgm_data_cm dataset, then set new variable 'claim' in cm_data == 1.
### Otherwise, set 'claim' in cm_data == 0

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
claim_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(claim = max(claim, na.rm = TRUE)) %>%
  ungroup()

# Merge claim_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, claim_agg, by = c("country_name", "month", "year"), all.x = TRUE)
cm_data$claim[is.infinite(cm_data$claim) & cm_data$claim < 0] <- 0


########## REG_AUT ########## 

### In a given country-month, take the average regional autonomy level in 
### a country, year, month

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
reg_aut_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarise(reg_aut = mean(reg_aut, na.rm = TRUE)) %>%
  ungroup()

reg_aut_agg$reg_aut[is.nan(reg_aut_agg$reg_aut)] <- NA

# Merge appear_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, reg_aut_agg, by = c("country_name", "month", "year"), all.x = TRUE)


########## POPULATION ##########

### In a given country-month, aggregate population in 
### a country, year, month

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
population_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(population = sum(population)) %>%
  ungroup()

# Merge appear_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, population_agg, by = c("country_name", "month", "year"), all.x = TRUE)
cm_data$log_population <- ifelse(is.na(cm_data$population), NA, log10(cm_data$population))


########## NL_SUM ##########

### In a given country-month, aggregate nighttime light sum in 
### a country, year, month

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
NL_sum_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(NL_sum = sum(NL_sum)) %>%
  ungroup()

# Merge NL_sum_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, NL_sum_agg, by = c("country_name", "month", "year"), all.x = TRUE)


########## NL_MEAN ##########

### In a given country-month, take mean nighttime light in 
### a country, year, month

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
NL_mean_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(NL_mean = mean(NL_mean)) %>%
  ungroup()

# Merge NL_mean_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, NL_mean_agg, by = c("country_name", "month", "year"), all.x = TRUE)


########## NL_MAX ##########

### In a given country-month, take max nighttime light in 
### a country, year, month

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
NL_max_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(NL_max= max(NL_max)) %>%
  ungroup()

# Merge NL_max_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, NL_max_agg, by = c("country_name", "month", "year"), all.x = TRUE)


########## TEMP_ANOMALY ##########

### In a given country-month, aggregate temperature anomalies in 
### a country, year, month

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
temp_anomaly_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(temp_anomaly = sum(temp_anomaly)) %>%
  ungroup()

# Merge temp_anomaly_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, temp_anomaly_agg, by = c("country_name", "month", "year"), all.x = TRUE)


########## PRECIP_ANOMALY ##########

### In a given country-month, aggregate precipitation anomalies in 
### a country, year, month

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
precip_anomaly_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(precip_anomaly = sum(precip_anomaly)) %>%
  ungroup()

# Merge precip_anomaly_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, precip_anomaly_agg, by = c("country_name", "month", "year"), all.x = TRUE)


########## MEAN_TEMP_CELSIUS ##########

### In a given country-month, take mean temperature celsius in 
### a country, year, month

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
mean_temp_celsius_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(mean_temp_celsius = mean(mean_temp_celsius)) %>%
  ungroup()

# Merge mean_temp_celsius_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, mean_temp_celsius_agg, by = c("country_name", "month", "year"), all.x = TRUE)


########## MEAN_PRECIP ##########

### In a given country-month, take mean precipitation in 
### a country, year, month

# Aggregate pgm_data_cm by 'country_name', 'month', 'year' 
mean_precip_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(mean_precip = mean(mean_precip)) %>%
  ungroup()

# Merge mean_precip_agg with cm_data based on 'country_name', 'month', and 'year'
cm_data <- merge(cm_data, mean_precip_agg, by = c("country_name", "month", "year"), all.x = TRUE)


### Save final dataset
save(cm_data, file = "cm_data_all_variables.RData")



########## SELECT MODEL VARIABLES ##########

### The following variables have been selected into model ethnic settlement CM dataset:
# reg_aut
# status_excluded
# claim
# recruitment
# support
# log_population
# NL_mean, NL_sum, NL_max
# temp_anomaly
# precip_anomaly

cm_model_data <- subset(cm_data, select = -c(mean_temp_celsius, population, mean_precip, status_share, status_irrelevant, status_alone))


########## CREATE LAGS OF SELECTED VARIABLES ##########

f.max <- function(x){
  if(all(is.na(x))){
    max <- 0
  }else{
    max <- max(x, na.rm = TRUE)
  }
  return(max)
}
f.agg.time <- function(.data, .n, .vec.col, .fun, .group){
  name.col <- paste0(.vec.col, "_lag_", .n)
  for(i in 1:length(.vec.col)){
    .data[, new := rollapply(get(.vec.col[i]), 
                             width = .n, 
                             FUN = .fun, 
                             align='right',
                             partial = TRUE) 
          , by = get(.group)]
    setnames(.data, "new", name.col[i])
  }
  
  return(.data)
}

vec2   <- c("reg_aut", "status_excluded", "recruitment", "claim", "support", 
            "temp_anomaly", "precip_anomaly", "log_population", "NL_sum", 
            "NL_max", "NL_mean")

data.cm.ged.unique.lag <- f.agg.time(.data = cm_model_data,
                                     .n = 2,
                                     .vec.col = vec2,
                                     .fun = f.max,
                                     .group = "country_name")

data.cm.ged.unique.lag <- f.agg.time(.data = data.cm.ged.unique.lag,
                                     .n = 3,
                                     .vec.col = vec2,
                                     .fun = f.max,
                                     .group = "country_name")

data.cm.ged.unique.lag <- f.agg.time(.data = data.cm.ged.unique.lag,
                                     .n = 4,
                                     .vec.col = vec2,
                                     .fun = f.max,
                                     .group = "country_name")

data.cm.ged.unique.lag <- f.agg.time(.data = data.cm.ged.unique.lag,
                                     .n = 5,
                                     .vec.col = vec2,
                                     .fun = f.max,
                                     .group = "country_name")

data.cm.ged.unique.lag <- f.agg.time(.data = data.cm.ged.unique.lag,
                                     .n = 6,
                                     .vec.col = vec2,
                                     .fun = f.max,
                                     .group = "country_name")
cm_model_data <- data.cm.ged.unique.lag


########## INCORPORATE ACLED DATA ##########

### In a given country-month, aggregate the ACLED dummy variables in 
### a country, year, month

# Aggregate 'acled_dummy_sb' in a country, year, month
acled_dummy_sb_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(acled_dummy_sb = max(acled_dummy_sb)) %>%
  ungroup()

# Merge acled_dummy_sb_agg with cm_data based on 'country_name', 'month', and 'year'
cm_model_data <- merge(cm_model_data, acled_dummy_sb_agg, by = c("country_name", "month", "year"), all.x = TRUE)


# Aggregate 'acled_dummy_ns' in a country, year, month
acled_dummy_ns_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(acled_dummy_ns = max(acled_dummy_ns)) %>%
  ungroup()

# Merge acled_dummy_ns_agg with cm_data based on 'country_name', 'month', and 'year'
cm_model_data <- merge(cm_model_data, acled_dummy_ns_agg, by = c("country_name", "month", "year"), all.x = TRUE)


# Aggregate 'acled_dummy_os' in a country, year, month
acled_dummy_os_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(acled_dummy_os = max(acled_dummy_os)) %>%
  ungroup()

# Merge acled_dummy_os_agg with cm_data based on 'country_name', 'month', and 'year'
cm_model_data <- merge(cm_model_data, acled_dummy_os_agg, by = c("country_name", "month", "year"), all.x = TRUE)


### In a given country-month, aggregate the ACLED count variables in 
### a country, year, month

# Aggregate 'acled_count_sb' in a country, year, month
acled_count_sb_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(acled_count_sb = sum(acled_count_sb)) %>%
  ungroup()

# Merge acled_count_sb_agg with cm_data based on 'country_name', 'month', and 'year'
cm_model_data <- merge(cm_model_data, acled_count_sb_agg, by = c("country_name", "month", "year"), all.x = TRUE)


# Aggregate 'acled_count_ns' in a country, year, month
acled_count_ns_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(acled_count_ns = sum(acled_count_ns)) %>%
  ungroup()

# Merge acled_count_ns_agg with cm_data based on 'country_name', 'month', and 'year'
cm_model_data <- merge(cm_model_data, acled_count_ns_agg, by = c("country_name", "month", "year"), all.x = TRUE)


# Aggregate 'acled_count_os' in a country, year, month
acled_count_os_agg <- pgm_data_cm %>%
  group_by(country_name, month, year) %>%
  summarize(acled_count_os = sum(acled_count_os)) %>%
  ungroup()

# Merge acled_count_os_agg with cm_data based on 'country_name', 'month', and 'year'
cm_model_data <- merge(cm_model_data, acled_count_os_agg, by = c("country_name", "month", "year"), all.x = TRUE)


# Creating new variables that are combinations of GED and ACLED:
# (1) 'fat_dummy_sb': At least either 'ged_dummy_sb' or 'acled_dummy_sb' are equal to 1
# (2) 'fat_best_sb': If either 'ged_dummy_sb' or 'acled_dummy_sb' are 0, then 
#                     take the count value from the variable that is equal to 1.
#                     But, if both 'ged_dummy_sb' or 'acled_dummy_sb' are equal to 1,
#                     then take the average of 'acled_count_sb' and 'ged_best_sb'
# same holds for os / ns

# (1) sb
cm_model_data$fat_dummy_sb <- ifelse(
  cm_model_data$ged_dummy_sb == 0 & cm_model_data$acled_dummy_sb == 0,
  cm_model_data$ged_dummy_sb,
  ifelse(
    cm_model_data$ged_dummy_sb == 1 & cm_model_data$acled_dummy_sb == 0,
    cm_model_data$ged_dummy_sb,
    ifelse(
      cm_model_data$ged_dummy_sb == 0 & cm_model_data$acled_dummy_sb == 1,
      cm_model_data$acled_dummy_sb,
      cm_model_data$ged_dummy_sb
    )
  )
)

# (2) sb
cm_model_data$fat_best_sb <- ifelse(
  cm_model_data$fat_dummy_sb == 0,
  0,
  ifelse(
    cm_model_data$fat_dummy_sb == 1,
    ceiling((cm_model_data$ged_best_sb + cm_model_data$acled_count_sb) / 2),
    0
  )
)

# (1) ns
cm_model_data$fat_dummy_ns <- ifelse(
  cm_model_data$ged_dummy_ns == 0 & cm_model_data$acled_dummy_ns == 0,
  cm_model_data$ged_dummy_ns,
  ifelse(
    cm_model_data$ged_dummy_ns == 1 & cm_model_data$acled_dummy_ns == 0,
    cm_model_data$ged_dummy_ns,
    ifelse(
      cm_model_data$ged_dummy_ns == 0 & cm_model_data$acled_dummy_ns == 1,
      cm_model_data$acled_dummy_ns,
      cm_model_data$ged_dummy_ns
    )
  )
)

# (2) ns
cm_model_data$fat_best_ns <- ifelse(
  cm_model_data$fat_dummy_ns == 0,
  0,
  ifelse(
    cm_model_data$fat_dummy_ns == 1,
    ceiling((cm_model_data$ged_best_ns + cm_model_data$acled_count_ns) / 2),
    0
  )
)

# (1) os
cm_model_data$fat_dummy_os <- ifelse(
  cm_model_data$ged_dummy_os == 0 & cm_model_data$acled_dummy_os == 0,
  cm_model_data$ged_dummy_os,
  ifelse(
    cm_model_data$ged_dummy_os == 1 & cm_model_data$acled_dummy_os == 0,
    cm_model_data$ged_dummy_os,
    ifelse(
      cm_model_data$ged_dummy_os == 0 & cm_model_data$acled_dummy_os == 1,
      cm_model_data$acled_dummy_os,
      cm_model_data$ged_dummy_os
    )
  )
)

# (2) os
cm_model_data$fat_best_os <- ifelse(
  cm_model_data$fat_dummy_os == 0,
  0,
  ifelse(
    cm_model_data$fat_dummy_os == 1,
    ceiling((cm_model_data$ged_best_os + cm_model_data$acled_count_os) / 2),
    0
  )
)

cm_model_data$fat_dummy_sb <- as.logical(cm_model_data$fat_dummy_sb)
cm_model_data$fat_best_sb <- as.integer(cm_model_data$fat_best_sb)
cm_model_data$time_since_fat_dummy_sb <- as.integer(cm_model_data$time_since_fat_dummy_sb)

cm_model_data$fat_dummy_ns <- as.logical(cm_model_data$fat_dummy_ns)
cm_model_data$fat_best_ns <- as.integer(cm_model_data$fat_best_ns)
cm_model_data$time_since_fat_dummy_ns <- as.integer(cm_model_data$time_since_fat_dummy_ns)

cm_model_data$fat_dummy_os <- as.logical(cm_model_data$fat_dummy_os)
cm_model_data$fat_best_os <- as.integer(cm_model_data$fat_best_os)
cm_model_data$time_since_fat_dummy_os <- as.integer(cm_model_data$time_since_fat_dummy_os)


### Calculate number of months since last fatality

# sb
cm_model_data <- cm_model_data %>%
  arrange(country_name, year, month) %>%
  group_by(country_name) %>%
  mutate(time_since_fat_dummy_sb = ifelse(fat_dummy_sb == 1 | (month == 1 & year == 1990 & fat_dummy_sb == 0), 1, 0))

cm_model_data <- cm_model_data %>%
  arrange(country_name, year, month) %>%
  group_by(country_name) %>%
  mutate(
    time_since_fat_dummy_sb = ifelse(fat_dummy_sb == 1 | (month == 1 & year == 1990 & fat_dummy_sb == 0), 1, 0),
    time_since_fat_dummy_sb = ave(time_since_fat_dummy_sb, cumsum(time_since_fat_dummy_sb == 1), FUN = seq_along)
  )

# ns
cm_model_data <- cm_model_data %>%
  arrange(country_name, year, month) %>%
  group_by(country_name) %>%
  mutate(time_since_fat_dummy_ns = ifelse(fat_dummy_ns == 1 | (month == 1 & year == 1990 & fat_dummy_ns == 0), 1, 0))

cm_model_data <- cm_model_data %>%
  arrange(country_name, year, month) %>%
  group_by(country_name) %>%
  mutate(
    time_since_fat_dummy_ns = ifelse(fat_dummy_ns == 1 | (month == 1 & year == 1990 & fat_dummy_ns == 0), 1, 0),
    time_since_fat_dummy_ns = ave(time_since_fat_dummy_ns, cumsum(time_since_fat_dummy_ns == 1), FUN = seq_along)
  )

# os
cm_model_data <- cm_model_data %>%
  arrange(country_name, year, month) %>%
  group_by(country_name) %>%
  mutate(time_since_fat_dummy_os = ifelse(fat_dummy_os == 1 | (month == 1 & year == 1990 & fat_dummy_os == 0), 1, 0))

cm_model_data <- cm_model_data %>%
  arrange(country_name, year, month) %>%
  group_by(country_name) %>%
  mutate(
    time_since_fat_dummy_os = ifelse(fat_dummy_os == 1 | (month == 1 & year == 1990 & fat_dummy_os == 0), 1, 0),
    time_since_fat_dummy_os = ave(time_since_fat_dummy_os, cumsum(time_since_fat_dummy_os == 1), FUN = seq_along)
  )

save(cm_model_data, file = "cm_model_data_final.RData")


