########## MASTER THESIS CODE 12 ##########
########## Kimberly A. White #############


########## STAGING AREA & OBJECTIVE ##########

### Load packages
library(dplyr)
library(tidyr)
library(pacman)
library(data.table)
library(RColorBrewer)
library(cshapes)
library(sf)
library(ggplot2)
library(leaflet)
library(colourlovers)
library(gganimate)
library(ggpubr)
library(lubridate)
library(transformr)
library(zoo)
library(xts)
library(gifski)
library(readr)
library(scales)
library(gridExtra)
library(scales)

### Clear environment
rm(list = ls())

### Set working directory
setwd("C:/Users/ra46bel/Downloads")

### Objective: Create various descriptive plots


########## IMPORTING / TEEING-UP DATASETS FOR SUBSEQUENT CODE ##########

# (1) Read GeoEPR - Geo-referencing Ethnic Power Relations Dataset
geo <- read.csv("GeoEPR-2021.csv")

# Create a binary var 'is_in_Africa' to identify non-African countries
geo$is_in_Africa <- ifelse(geo$statename == "Guinea-Bissau" | 
                             geo$statename == "Equatorial Guinea" |
                             geo$statename == "Gambia" |
                             geo$statename == "Mali" |
                             geo$statename == "Senegal" |
                             geo$statename == "Benin" |
                             geo$statename == "Mauritania" |
                             geo$statename == "Niger" |
                             geo$statename == "Cote D'Ivoire" |
                             geo$statename == "Guinea" |
                             geo$statename == "Burkina Faso (Upper Volta)" |
                             geo$statename == "Liberia" |
                             geo$statename == "Sierra Leone" |
                             geo$statename == "Ghana" |
                             geo$statename == "Togo" |
                             geo$statename == "Cameroon" |
                             geo$statename == "Nigeria" |
                             geo$statename == "Gabon" |
                             geo$statename == "Central African Republic" |
                             geo$statename == "Chad" |
                             geo$statename == "Congo" |
                             geo$statename == "Congo, Democratic Republic of (Zaire)" |
                             geo$statename == "Uganda" |
                             geo$statename == "Kenya" |
                             geo$statename == "Tanzania (Tanganyika)" |
                             geo$statename == "Zanzibar" |
                             geo$statename == "Burundi" |
                             geo$statename == "Rwanda" |
                             geo$statename == "Somalia" |
                             geo$statename == "Djibouti" |
                             geo$statename == "Ethiopia" |
                             geo$statename == "Eritrea" |
                             geo$statename == "Angola" |
                             geo$statename == "Mozambique" |
                             geo$statename == "Zambia" |
                             geo$statename == "Zimbabwe (Rhodesia)" |
                             geo$statename == "Malawi" |
                             geo$statename == "South Africa" |
                             geo$statename == "Lesotho" |
                             geo$statename == "Namibia" |
                             geo$statename == "Botswana" |
                             geo$statename == "Swaziland (Eswatini)" |
                             geo$statename == "Madagascar (Malagasy)" |
                             geo$statename == "Comoros" |
                             geo$statename == "Mauritius" |
                             geo$statename == "Morocco" |
                             geo$statename == "Algeria" |
                             geo$statename == "Tunisia" | 
                             geo$statename == "Libya" | 
                             geo$statename == "Sudan" | 
                             geo$statename == "Cape Verde" |
                             geo$statename == "Egypt" | 
                             geo$statename == "Western Sahara" |
                             geo$statename == "Ivory Coast" |
                             geo$statename == "South Sudan", 1, 0)

# Remove all non-African countries from the dataset
geo <- subset(geo, is_in_Africa == "1")

# Create the dataframe group_years that expands the var 'to' and var 'from' 
# years from GeoEPR_2021 for each 'groupid' within a unique 'statename'
group_years <- geo %>% 
  rowwise() %>% 
  do(data.frame(from= seq(.$from, .$to),
                groupid= rep(.$groupid, .$to-.$from+1),
                gwid= rep(.$gwid, .$to-.$from+1)))

# Merge the dataframe group_years with GeoEPR_2021 using 'all', which specifies
# keep all rows from the left and the right side, resulting in the 
# GeoEPR_2021_expanded dataframe
geo_expanded <- merge(geo,group_years, by= c("gwid", "groupid", "from"), all=TRUE)

# Delete 'to' column
geo_expanded <- geo_expanded[, -c(5)]

# Change 'from' column to 'year'
names(geo_expanded)[3] <- 'year'

# Fill down values in GeoEPR_2021_expanded
geo_expanded <- geo_expanded %>% fill(names(geo_expanded), .direction = "down")

# Change 'the_geom' column to 'geometry'
names(geo_expanded)[10] <- 'geometry'

# Clean columns
geo_expanded <- geo_expanded[, -c(11)]

# Save dataset for later use
save(geo_expanded, file = "geo_expanded.RData")

# Transform the 'geometry' column to a sf object 
geo1 <- st_as_sf(geo_expanded, wkt="geometry")

# Create a binary var 'for_a_year' to identify indicated year
geo1$for_a_year <- ifelse(geo1$year == "2016", 1, 0)

# Remove entries not within the indicated year from the dataset
geo2 <- subset(geo1, for_a_year == "1")

# Clean columns
geo1 <- geo1[, -c(11)]

# Expand 'geo1' to realize all months within a respective year
total_year <- unique(geo1$year)

df = as.data.frame(matrix(nrow = 0, ncol = 12))

length(total_year)

for (i in 1:length(total_year)){
  geo1$for_a_year <- ifelse(geo1$year == total_year[i], 1, 0)  
  geo3 <- subset(geo1, for_a_year == "1")
  group_years <- geo3 %>% 
    rowwise() %>% 
    do(data.frame(month = seq(1,12),
                  gwgroupid = rep(.$gwgroupid,12)
    ))
  geo3_expanded <- merge(geo3, group_years, by= c("gwgroupid"), all=TRUE)
  year_month <- paste(geo3_expanded$year, geo3_expanded$month, sep = '-')
  year_month <- ym(year_month)
  geo3_expanded$year_month <- year_month
  df <- rbind(df, geo3_expanded)
}

# Only keep observations from 1990 until present
df <- subset(df, year >= 1990)


# (2) Import the world map and store as an sf object
data.world <- cshp(date=as.Date("2000-1-01"), useGW=TRUE)
data.world <- st_as_sf(data.world)

# Create a binary var 'is_in_Africa' to identify non-African countries
data.world$is_in_Africa <- ifelse(data.world$country_name == "Guinea-Bissau" | 
                                    data.world$country_name == "Equatorial Guinea" |
                                    data.world$country_name == "Gambia" |
                                    data.world$country_name == "Mali" |
                                    data.world$country_name == "Senegal" |
                                    data.world$country_name == "Benin" |
                                    data.world$country_name == "Mauritania" |
                                    data.world$country_name == "Niger" |
                                    data.world$country_name == "Cote D'Ivoire" |
                                    data.world$country_name == "Guinea" |
                                    data.world$country_name == "Burkina Faso (Upper Volta)" |
                                    data.world$country_name == "Liberia" |
                                    data.world$country_name == "Sierra Leone" |
                                    data.world$country_name == "Ghana" |
                                    data.world$country_name == "Togo" |
                                    data.world$country_name == "Cameroon" |
                                    data.world$country_name == "Nigeria" |
                                    data.world$country_name == "Gabon" |
                                    data.world$country_name == "Central African Republic" |
                                    data.world$country_name == "Chad" |
                                    data.world$country_name == "Congo" |
                                    data.world$country_name == "Congo, Democratic Republic of (Zaire)" |
                                    data.world$country_name == "Uganda" |
                                    data.world$country_name == "Kenya" |
                                    data.world$country_name == "Tanzania (Tanganyika)" |
                                    data.world$country_name == "Zanzibar" |
                                    data.world$country_name == "Burundi" |
                                    data.world$country_name == "Rwanda" |
                                    data.world$country_name == "Somalia" |
                                    data.world$country_name == "Djibouti" |
                                    data.world$country_name == "Ethiopia" |
                                    data.world$country_name == "Eritrea" |
                                    data.world$country_name == "Angola" |
                                    data.world$country_name == "Mozambique" |
                                    data.world$country_name == "Zambia" |
                                    data.world$country_name == "Zimbabwe (Rhodesia)" |
                                    data.world$country_name == "Malawi" |
                                    data.world$country_name == "South Africa" |
                                    data.world$country_name == "Lesotho" |
                                    data.world$country_name == "Namibia" |
                                    data.world$country_name == "Botswana" |
                                    data.world$country_name == "Swaziland (Eswatini)" |
                                    data.world$country_name == "Madagascar (Malagasy)" |
                                    data.world$country_name == "Comoros" |
                                    data.world$country_name == "Mauritius" |
                                    data.world$country_name == "Morocco" |
                                    data.world$country_name == "Algeria" |
                                    data.world$country_name == "Tunisia" | 
                                    data.world$country_name == "Libya" | 
                                    data.world$country_name == "Sudan" | 
                                    data.world$country_name == "Cape Verde" |
                                    data.world$country_name == "Egypt" | 
                                    data.world$country_name == "Western Sahara" |
                                    data.world$country_name == "Ivory Coast" |
                                    data.world$country_name == "South Sudan", 1, 0)

# Remove all non-African countries from the dataset
data.africa <- subset(data.world, is_in_Africa == "1")

# Save dataset for later use
save(data.africa, file = "data.africa.RData")


# (3) Load final PGM dataset
load("pgm_model_data_final.RData")
pgm_data <-  pgm_model_data 
rm(pgm_model_data)

# Subset for relevant variables
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, lat, long, year, month, country_name)

# Derive combined 'year_month' variable
year_month <- paste(pgm_data_plot$year, pgm_data_plot$month, sep = '-')
year_month <- ym(year_month)
pgm_data_plot$year_month <- year_month

# Only keep observations from 1990 until present
pgm_data_plot <- subset(pgm_data_plot, year >= 1990)

# Subset for one year
pgm_data_plot1 <- subset(pgm_data_plot, year == 2016)

# Countries with the most fatalities since 1990
sort(table(pgm_data_plot$country_name),decreasing=TRUE)[1:15]

# The top 15 countries with the most fatalities since 1990: Algeria, Somalia,
# Nigeria, Congo, DRC, Sudan, Uganda, Mali, Burundi, Angola, Sierra Leone,
# Cameroon, Libya, South Sudan, Egypt, and Ethiopia


# (4) Read list of world countries
data.countrylist <- read.csv("country_list.csv")  %>%
  as.data.table()

# Filter for only African countries
data.country.list.africa <- data.countrylist %>%
  filter(continent == "Africa")

vec.countries.africa <- unique(data.country.list.africa$COW)


# (5) ACD2EPR
ACD2EPR_2021 <- read_csv("ACD2EPR-2021.csv")

# Create a binary var 'is_in_Africa' to identify non-African countries
ACD2EPR_2021$is_in_Africa <- ifelse(ACD2EPR_2021$statename == "Guinea-Bissau" | 
                                      ACD2EPR_2021$statename == "Equatorial Guinea" |
                                      ACD2EPR_2021$statename == "Gambia" |
                                      ACD2EPR_2021$statename == "Mali" |
                                      ACD2EPR_2021$statename == "Senegal" |
                                      ACD2EPR_2021$statename == "Benin" |
                                      ACD2EPR_2021$statename == "Mauritania" |
                                      ACD2EPR_2021$statename == "Niger" |
                                      ACD2EPR_2021$statename == "Cote D'Ivoire" |
                                      ACD2EPR_2021$statename == "Guinea" |
                                      ACD2EPR_2021$statename == "Burkina Faso (Upper Volta)" |
                                      ACD2EPR_2021$statename == "Liberia" |
                                      ACD2EPR_2021$statename == "Sierra Leone" |
                                      ACD2EPR_2021$statename == "Ghana" |
                                      ACD2EPR_2021$statename == "Togo" |
                                      ACD2EPR_2021$statename == "Cameroon" |
                                      ACD2EPR_2021$statename == "Nigeria" |
                                      ACD2EPR_2021$statename == "Gabon" |
                                      ACD2EPR_2021$statename == "Central African Republic" |
                                      ACD2EPR_2021$statename == "Chad" |
                                      ACD2EPR_2021$statename == "Congo" |
                                      ACD2EPR_2021$statename == "Congo, Democratic Republic of (Zaire)" |
                                      ACD2EPR_2021$statename == "Uganda" |
                                      ACD2EPR_2021$statename == "Kenya" |
                                      ACD2EPR_2021$statename == "Tanzania (Tanganyika)" |
                                      ACD2EPR_2021$statename == "Zanzibar" |
                                      ACD2EPR_2021$statename == "Burundi" |
                                      ACD2EPR_2021$statename == "Rwanda" |
                                      ACD2EPR_2021$statename == "Somalia" |
                                      ACD2EPR_2021$statename == "Djibouti" |
                                      ACD2EPR_2021$statename == "Ethiopia" |
                                      ACD2EPR_2021$statename == "Eritrea" |
                                      ACD2EPR_2021$statename == "Angola" |
                                      ACD2EPR_2021$statename == "Mozambique" |
                                      ACD2EPR_2021$statename == "Zambia" |
                                      ACD2EPR_2021$statename == "Zimbabwe (Rhodesia)" |
                                      ACD2EPR_2021$statename == "Malawi" |
                                      ACD2EPR_2021$statename == "South Africa" |
                                      ACD2EPR_2021$statename == "Lesotho" |
                                      ACD2EPR_2021$statename == "Namibia" |
                                      ACD2EPR_2021$statename == "Botswana" |
                                      ACD2EPR_2021$statename == "Swaziland (Eswatini)" |
                                      ACD2EPR_2021$statename == "Madagascar (Malagasy)" |
                                      ACD2EPR_2021$statename == "Comoros" |
                                      ACD2EPR_2021$statename == "Mauritius" |
                                      ACD2EPR_2021$statename == "Morocco" |
                                      ACD2EPR_2021$statename == "Algeria" |
                                      ACD2EPR_2021$statename == "Tunisia" | 
                                      ACD2EPR_2021$statename == "Libya" | 
                                      ACD2EPR_2021$statename == "Sudan" |
                                      ACD2EPR_2021$statename == "Cape Verde" |
                                      ACD2EPR_2021$statename == "Egypt" |
                                      ACD2EPR_2021$statename == "Western Sahara" |
                                      ACD2EPR_2021$statename == "Ivory Coast" |
                                      ACD2EPR_2021$statename == "South Sudan", 1, 0)

# Remove all non-African countries from the dataset
ACD2EPR_2021 <- subset(ACD2EPR_2021, is_in_Africa == "1")



########### A. Static Ethnic Settlement Plot / year ##############

# Create a function to plot any given year with a color scale and two layers:
# (1) World map and (2) Ethnic groups
f.plot.year <- function(.year = "1990"){
  if(.year %in% c("2019","2020")){
    var.world.year <- "2018"
  }else{
    var.world.year <- .year
  }
  
  data.world <- cshp(date=as.Date(paste0(var.world.year, "-1-01")), useGW=TRUE) %>%
    st_as_sf()
  
  data.plot.cm <- data.world[data.world$gwcode %in% vec.countries.africa,]

  # Plot ggplot
  plot.eth <- ggplot()+
    geom_sf(data = data.plot.cm, col = "black", fill = "white") +
    geom_sf(data = geo2, aes(fill = as.factor(group)), color = "#32481B",alpha=0.3) +
    theme_void() +
    theme(legend.position = "None")
    theme(plot.title = element_text(hjust = 0.5))
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank()) #+

  plot.eth
}

plot.list <- Map(f.plot.year, 2010:2017)

plot.2016 = ggplotGrob(f.plot.year(2016))
ggarrange(plot.2016)



########### B. Interactive Leaflet for Ethnic Groups / year ##############

# Assign unique ID for each 'group' (ethnic groups)
geo2 <- transform(geo2, category = as.numeric(factor(group)))

# Generate number of unique 'group's (ethnic groups)
n_distinct(geo2$category)

# Define factpal as a function of colorFactor
# User may define suitable color palette for ethnic groups
# display.brewer.all()
factpal <- colorFactor("Pastel1", geo2$category)

# Create a function to plot any given year given a color scale and two layers:
# (1) World map and (2) Ethnic groups
f.plot.year.leaflet <- function(.year){
  
  # Plot interactive leaflet
  leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
    addPolygons(data = data.africa, fillOpacity = 0.7,weight = 1.2,color = "grey") %>%
    addPolygons(data = geo2, fillOpacity = 0.7,weight = 1.2,color = ~factpal(category),label = ~geo2$group)
}

# Plot a given year
f.plot.year.leaflet(2016)



########### C. Recruitment of Ethnic Groups and Fatalities at Country level ##############

# Subset for relevant variables
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, recruitment, lat, long, year, month, country_name)
pgm_data_plot <- pgm_data_plot %>% filter(year == 2015)

# Keep only 'TRUE' ged_dummy_sb values
pgm_data_fatality_plot <- subset(pgm_data_plot, ged_dummy_sb == "TRUE" )
pgm_data_2015 <- pgm_data_fatality_plot %>% filter(year == 2015)

# Identify recruitment
recruitment_plot <- ACD2EPR_2021 %>%
  group_by(statename) %>%
  mutate(recruitment_flag = ifelse(recruitment %in% c(1, 2), 1, 0)) %>%
  distinct(statename, .keep_all = TRUE) %>%
  ungroup() %>%
  dplyr::select(statename, recruitment_flag, gwgroupid)

recruitment_plot <- recruitment_plot %>%
  rename(country_name = statename)

recruitment_plot <- distinct(recruitment_plot)

recruitment_plot <- recruitment_plot %>%
  group_by(country_name) %>%
  summarize(max_recruitment_flag = max(recruitment_flag))

# Rename country names for subsequent merge
recruitment_plot <- recruitment_plot %>%
  mutate(country_name = if_else(country_name == "Zimbabwe", "Zimbabwe (Rhodesia)", country_name))
recruitment_plot <- recruitment_plot %>%
  mutate(country_name = if_else(country_name == "Tanzania", "Tanzania (Tanganyika)", country_name))
recruitment_plot <- recruitment_plot %>%
  mutate(country_name = if_else(country_name == "Swaziland", "Swaziland (Eswatini)", country_name))
recruitment_plot <- recruitment_plot %>%
  mutate(country_name = if_else(country_name == "Madagascar", "Madagascar (Malagasy)", country_name))
recruitment_plot <- recruitment_plot %>%
  mutate(country_name = if_else(country_name == "Congo, DRC", "Congo, Democratic Republic of (Zaire)", country_name))
recruitment_plot <- recruitment_plot %>%
  mutate(country_name = if_else(country_name == "The Gambia", "Gambia", country_name)) 
recruitment_plot <- recruitment_plot %>%
  mutate(country_name = if_else(country_name == "Cote d'Ivoire", "Cote D'Ivoire", country_name)) 
recruitment_plot <- recruitment_plot %>%
  mutate(country_name = if_else(country_name == "Burkina Faso",  "Burkina Faso (Upper Volta)", country_name))

# Perform the merge
data.africa <- merge(data.africa, recruitment_plot, by = "country_name", all.x = TRUE)

# Filter data.africa for countries with at least one recruitment == 1
green_countries <- data.africa %>%
  filter(max_recruitment_flag == 1)

# Plot
static_ethnic_fat_plot <- ggplot() +
  geom_sf(data = data.africa) +
  geom_sf(data = green_countries, fill = "light green") +
  geom_tile(data = pgm_data_2015, aes(x = long, y = lat, width = 0.5, height = 0.5), fill = "red") +
  theme_void()

static_ethnic_fat_plot + theme(legend.position = "none")



########### D. Recruitment of Ethnic Groups and Fatalities at Group level ##############

# Subset for relevant variables
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, recruitment, lat, long, year, month, country_name)
pgm_data_plot <- pgm_data_plot %>% filter(year == 1995)

# Keep only 'TRUE' ged_dummy_sb values
pgm_data_fatality_plot <- subset(pgm_data_plot, ged_dummy_sb == "TRUE" )
pgm_data_1995 <- pgm_data_fatality_plot %>% filter(year == 1995)

# Transform the 'geometry' column to a sf object 
geo1 <- st_as_sf(geo_expanded, wkt="geometry")

# Create a binary var 'for_a_year' to identify indicated year
geo1$for_a_year <- ifelse(geo1$year == "1995", 1, 0)

# Remove entries not within the indicated year from the dataset
geo2 <- subset(geo1, for_a_year == "1")

ACD2EPR_filtered <- ACD2EPR_2021 %>%
  filter(!is.na(gwgroupid))

recruitment_plot <- ACD2EPR_filtered %>%
  group_by(gwgroupid) %>%
  mutate(recruitment_flag = ifelse(recruitment %in% c(1, 2), 1, 0)) %>%
  distinct(statename, .keep_all = TRUE) %>%
  ungroup() %>%
  dplyr::select(statename, recruitment_flag, gwgroupid)

geo_ACD2EPR <- merge(geo2, recruitment_plot, by = "gwgroupid", all.x = TRUE)
geo_ACD2EPR_2015 <- geo_ACD2EPR %>% filter(year == 1995)

green_groups <- geo_ACD2EPR %>%
  filter(recruitment_flag == 1)

# Plot
static_ethnic_fat_plot <- ggplot() +
  geom_sf(data = data.africa) +
  geom_sf(data = geo2, fill = "light blue") +
  geom_sf(data = green_groups, fill = "yellow") +
  geom_tile(data = pgm_data_1995, aes(x = long, y = lat, width = 0.5, height = 0.5), fill = "black") +
  theme_void()

static_ethnic_fat_plot + theme(legend.position = "none")



########### E. Understanding Fatality Trends ##############

pgm_data_plot <- dplyr::select(pgm_data, ged_count_sb, acled_count_sb, lat, long, year, month, country_name)
grouped_data <- pgm_data_plot %>%
  group_by(year) %>%
  summarize(acled_count_sum = sum(acled_count_sb, na.rm = TRUE),
            ged_count_sum = sum(ged_count_sb, na.rm = TRUE))

trend <- ggplot(grouped_data, aes(x = year)) +
  geom_line(aes(y = ged_count_sum, color = "UCDP Fatality Data"), size = 1) +
  geom_line(aes(y = acled_count_sum, color = "ACLED Fatality Data"), size = 1) +
  labs(x = "Year",
       y = "Number of Fatalities",
       color = NULL) +  # Set legend.title to NULL to remove the legend title
  scale_color_manual(values = c("UCDP Fatality Data" = "blue", "ACLED Fatality Data" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, margin = margin(r = -100)),  # Adjust the margin for y-axis title
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_blank())

print(trend)



# Group by country and year, then summarize to count the number of non-zero 'ged_count_sb' and 'acled_count_sb' values
countries_with_nonzero_counts <- pgm_data_plot %>%
  group_by(country_name, year) %>%
  summarize(count_nonzero_ged = sum(ged_count_sb != 0),
            count_nonzero_acled = sum(acled_count_sb != 0))

# Group by year to calculate the total number of countries with non-zero 'ged_count_sb' and 'acled_count_sb' values for each year
countries_with_nonzero_counts_by_year <- countries_with_nonzero_counts %>%
  group_by(year) %>%
  summarize(total_countries_with_nonzero_ged = sum(count_nonzero_ged > 0),
            total_countries_with_nonzero_acled = sum(count_nonzero_acled > 0))

trend_2 <- ggplot(countries_with_nonzero_counts_by_year, aes(x = year)) +
  geom_line(aes(y = total_countries_with_nonzero_ged, color = "UCDP Fatality Data"), size = 1) +
  geom_line(aes(y = total_countries_with_nonzero_acled, color = "ACLED Fatality Data"), size = 1) +
  labs(x = "Year",
       y = "Number of Countries",
       color = NULL) +  # Set legend.title to NULL to remove the legend title
  scale_color_manual(values = c("UCDP Fatality Data" = "blue", "ACLED Fatality Data" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, margin = margin(r = -100)),  # Adjust the margin for y-axis title
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_blank())

print(trend_2)



pgm_data_plot <- dplyr::select(pgm_data, ged_count_sb, acled_count_sb, lat, long, year, month, country_name)

pgm_data_plot <- subset(pgm_data_plot, year == 2019)

grouped_data <- pgm_data_plot %>%
  group_by(country_name, year) %>%
  summarize(acled_count_sum = sum(acled_count_sb, na.rm = TRUE),
            ged_count_sum = sum(ged_count_sb, na.rm = TRUE)) %>%
  filter(acled_count_sum > 50 & ged_count_sum > 50)

grouped_data$year <- factor(grouped_data$year)

# Plot the bar graph for ACLED count with the year 2019
bar_plot <- ggplot(grouped_data, aes(fill = country_name, y = acled_count_sum, x = year)) + 
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = acled_count_sum), position = position_stack(vjust = 0.5), size = 3, color = "black") + # Add y-axis labels on top of bars
  labs(title = "ACLED Fatality Data",
       x = "Year",
       y = "Number of Fatalities") +
  theme_minimal() +
  ylim(0, 3500) +
  theme(axis.text.y = element_blank()) +
  scale_fill_discrete(name = "Country Name")

# Plot
bar_plot_ged <- ggplot(grouped_data, aes(fill = country_name, y = ged_count_sum, x = year)) + 
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = ged_count_sum), position = position_stack(vjust = 0.5), size = 3, color = "black") + # Add y-axis labels on top of bars
  labs(title = "UCDP Fatality Data",
       x = "Year",
       y = "") +
  theme_minimal() + 
  ylim(0, 3500) +
  theme(axis.text.y = element_blank(),legend.title.align = 0.5)+
  scale_fill_discrete(name = "Country Name")

grid.arrange(bar_plot, bar_plot_ged, ncol = 2)



pgm_data_plot <- dplyr::select(pgm_data, ged_count_sb, acled_count_sb, log_population, lat, long, year, month, country_name)
pgm_data_plot$log_population <- exp(pgm_data_plot$log_population)

grouped_data <- pgm_data_plot %>%
  group_by(year) %>%
  summarize(population_sum = sum(log_population, na.rm = TRUE),
            acled_count_sum = sum(acled_count_sb, na.rm = TRUE),
            ged_count_sum = sum(ged_count_sb, na.rm = TRUE))  %>%
  filter(population_sum > 0)

grouped_data$pro_acled <- grouped_data$acled_count_sum / grouped_data$population_sum
grouped_data$pro_ged <- grouped_data$ged_count_sum / grouped_data$population_sum
grouped_data$pro_acled <- percent(grouped_data$pro_acled)
grouped_data$pro_ged <- percent(grouped_data$pro_ged)
grouped_data$pro_ged <- parse_number(grouped_data$pro_ged)
grouped_data$pro_acled <- parse_number(grouped_data$pro_acled)


# Plot
trend_3 <- ggplot(grouped_data, aes(x = year)) +
  geom_line(aes(y = pro_ged, color = "UCDP Fatality Data"), size = 1) +
  geom_line(aes(y = pro_acled, color = "ACLED Fatality Data"), size = 1) +
  labs(x = "Year",
       y = "Fatalities as a Share of Africa's Population",
       color = NULL) +
  scale_color_manual(values = c("UCDP Fatality Data" = "blue", "ACLED Fatality Data" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 14, margin = margin(r = -100)),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_blank())

print(trend_3)


########### F. Population Density ##############

# Subset for relevant variables
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, log_population, lat, long, year, month, country_name)
pgm_data_plot <- pgm_data_plot %>% filter(year == 2019)

# Keep only 'TRUE' ged_dummy_sb values
pgm_data_fatality_plot <- subset(pgm_data_plot, ged_dummy_sb == "TRUE" )
pgm_data_2019 <- pgm_data_fatality_plot %>% filter(year == 2019)

# Plot
static_pop_plot <- ggplot() +
  geom_sf(data = data.africa) +
  geom_tile(data = pgm_data_plot, aes(x = long, y = lat, fill = log_population)) +
  scale_fill_viridis_c(option = "C") + # You can change the color scale as needed
  geom_tile(data = pgm_data_2019, aes(x = long, y = lat, width = 0.5, height = 0.5), fill = "red") +
  theme_void()

print(static_pop_plot)


# Subset for relevant variables
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, log_population, lat, long, year, month, country_name)
pgm_data_plot <- pgm_data_plot %>% filter(year == 2019)
pgm_data_plot <- pgm_data_plot %>% filter(country_name == "Mali")

# Keep only 'TRUE' ged_dummy_sb values
pgm_data_fatality_plot <- subset(pgm_data_plot, ged_dummy_sb == "TRUE" )
pgm_data_2019 <- pgm_data_fatality_plot %>% filter(year == 2019)
pgm_data_2019 <- pgm_data_fatality_plot %>% filter(country_name == "Mali")

data.africa.Mali <- data.africa %>% filter(country_name == "Mali")

# Plot
static_pop_plot <- ggplot() +
  geom_sf(data = data.africa.Mali) +
  geom_tile(data = pgm_data_plot, aes(x = long, y = lat, fill = log_population)) +
  scale_fill_viridis_c(option = "C") + # You can change the color scale as needed
  theme_void()

print(static_pop_plot)


# Subset for relevant variables
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, log_population, lat, long, year, month, country_name)
pgm_data_plot <- pgm_data_plot %>% filter(year == 2019)
pgm_data_plot <- pgm_data_plot %>% filter(country_name == "Mali")

# Keep only 'TRUE' ged_dummy_sb values
pgm_data_fatality_plot <- subset(pgm_data_plot, ged_dummy_sb == "TRUE" )
pgm_data_2019 <- pgm_data_fatality_plot %>% filter(year == 2019)
pgm_data_2019 <- pgm_data_fatality_plot %>% filter(country_name == "Mali")

data.africa.Burkinafaso <- data.africa %>% filter(country_name == "Mali")

# Plot
static_pop_plot <- ggplot() +
  geom_sf(data = data.africa.Mali) +
  geom_tile(data = pgm_data_plot, aes(x = long, y = lat, fill = log_population)) +
  scale_fill_viridis_c(option = "C") + # You can change the color scale as needed
  geom_tile(data = pgm_data_2019, aes(x = long, y = lat, width = 0.5, height = 0.5), fill = "red") +
  theme_void()

print(static_pop_plot)



########### G. Nighttime Light Emissions ##############

# Subset for relevant variables
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, NL_mean, lat, long, year, month, country_name)
pgm_data_plot <- pgm_data_plot %>% filter(year == 1996, month == 6, NL_mean > 0.25)
pgm_data_plot_1996 <- pgm_data_plot %>% filter(country_name == "Congo, DRC")

data.africa.Congo <- data.africa %>% filter(country_name == "Congo, Democratic Republic of (Zaire)")

# Plot
static_nl_plot <- ggplot() +
  geom_sf(data = data.africa.Congo) +
  geom_tile(data = pgm_data_plot_1996, aes(x = long, y = lat, fill = NL_mean)) +
  scale_fill_viridis_c(option = "C") + # You can change the color scale as needed
  theme_void() +
  guides(fill = "none")

print(static_nl_plot)

# Subset for relevant variables
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, NL_mean, lat, long, year, month, country_name)
pgm_data_plot <- pgm_data_plot %>% filter(year == 1997, month == 1, NL_mean > 0.25)
pgm_data_plot_1997 <- pgm_data_plot %>% filter(country_name == "Congo, DRC")

data.africa.Congo <- data.africa %>% filter(country_name == "Congo, Democratic Republic of (Zaire)")

# Plot
static_nl_plot <- ggplot() +
  geom_sf(data = data.africa.Congo) +
  geom_tile(data = pgm_data_plot_1997, aes(x = long, y = lat, fill = NL_mean)) +
  scale_fill_viridis_c(option = "C") + # You can change the color scale as needed
  theme_void() +
  guides(fill = "none")

print(static_nl_plot)

mean(pgm_data_plot_1996$NL_mean)
mean(pgm_data_plot_1997$NL_mean)



########### H. Precipitation and Temperature Anomalies ##############

# Subset for relevant variables
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, precip_anomaly, lat, long, year, month, country_name)
pgm_data_plot_2019 <- pgm_data_plot %>% filter(year == 2020, month == 1)
pgm_data_plot_2019 <- pgm_data_plot_2019 %>% filter(country_name == "Somalia" | country_name == "Ethiopia" | country_name == "Eritrea" | country_name == "Djibouti")

data.africa.horn <- data.africa %>% filter(country_name == "Somalia" | country_name == "Ethiopia" | country_name == "Eritrea" | country_name == "Djibouti")

# Keep only 'TRUE' ged_dummy_sb values
pgm_data_fatality_plot <- subset(pgm_data_plot, ged_dummy_sb == "TRUE" )
pgm_data_2019 <- pgm_data_fatality_plot %>% filter(year == 2020, month == 1)
pgm_data_2019 <- pgm_data_2019 %>% filter(country_name == "Somalia" | country_name == "Ethiopia" | country_name == "Eritrea" | country_name == "Djibouti")

# Plot
static_pa_plot <- ggplot() +
  geom_sf(data = data.africa.horn) +
  geom_tile(data = pgm_data_plot_2019, aes(x = long, y = lat, fill = precip_anomaly)) +
  geom_tile(data = pgm_data_2019, aes(x = long, y = lat, width = 0.5, height = 0.5), fill = "red") +
  scale_fill_viridis_c(option = "C") + # You can change the color scale as needed
  theme_void()+
  guides(fill = "none")

print(static_pa_plot)



########### I. Fatalities (UCDP vs. ACLED) ##############

# Keep only 'TRUE' ged_dummy_sb values
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, lat, long, year, month, country_name)
pgm_data_fatality_plot <- subset(pgm_data_plot, ged_dummy_sb == "TRUE" )
pgm_data_2019 <- pgm_data_fatality_plot %>% filter(year == 2016)

# Keep only 'TRUE' acled_dummy_sb values
pgm_data_plot_acled <- subset(pgm_data, select = c(acled_dummy_sb, lat, long, year, month, country_name))
pgm_data_fatality_plot_acled <- subset(pgm_data_plot_acled, acled_dummy_sb == 1)
pgm_data_2019_acled <- pgm_data_fatality_plot_acled %>% filter(year == 2016)

# Plot
static_pa_plot <- ggplot() +
  geom_sf(data = data.africa) +
  geom_tile(data = pgm_data_2019_acled, aes(x = long, y = lat, width = 0.5, height = 0.5, fill = "ACLED Fatality Data")) +
  geom_tile(data = pgm_data_2019, aes(x = long, y = lat, width = 0.5, height = 0.5, fill = "UCDP Fatality Data")) +
  scale_fill_manual(values = c("ACLED Fatality Data" = "red", "UCDP Fatality Data" = "blue")) +
  theme_void() +
  guides(fill = guide_legend(title = ""))  # Add a legend title

print(static_pa_plot)


# Keep only 'TRUE' ged_dummy_sb values
pgm_data_plot <- dplyr::select(pgm_data, ged_dummy_sb, lat, long, year, month, country_name)
pgm_data_fatality_plot <- subset(pgm_data_plot, ged_dummy_sb == "TRUE" )
pgm_data_2019 <- pgm_data_fatality_plot %>% filter(year == 2016)

# Keep only 'TRUE' acled_dummy_sb values
pgm_data_plot_acled <- subset(pgm_data, select = c(acled_dummy_sb, lat, long, year, month, country_name))
pgm_data_fatality_plot_acled <- subset(pgm_data_plot_acled, acled_dummy_sb == 1)
pgm_data_2019_acled <- pgm_data_fatality_plot_acled %>% filter(year == 2016)

# Plot
static_pa_plot <- ggplot() +
  geom_sf(data = data.africa) +
  geom_tile(data = pgm_data_2019, aes(x = long, y = lat, width = 0.5, height = 0.5), fill = "blue") +
  scale_fill_viridis_c(option = "C") + # You can change the color scale as needed
  theme_void()+
  guides(fill = "none")

print(static_pa_plot)


