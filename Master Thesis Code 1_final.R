########## MASTER THESIS CODE 1 ##########
########## Kimberly A. White #############


########## STAGING AREA & OBJECTIVE ##########

### Load packages
library(sf)
library(haven)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(raster)
library(sf)
library(cshapes)
library(MODIStsp)
library(ncdf4)
library(R.utils)
library(purrr)
library(lubridate)
library(roll)
library(zoo)

### Set working directory
setwd("C:/Users/ra46bel/Downloads")

### Clear environment
rm(list = ls())

### Objective: Incorporate additional information on (1) Ethnic Power Relations, 
### (2) Nighttime Lights, (3) Population Distribution, and (4) Environmental Stress 
### to an existing dataset of Africa at the PRIO-Grid level. Then, add additional 
### information on violence from the Armed Conflict Location & Event Data Project (ACLED)



########## IMPORTING / TEEING-UP DATASETS FOR SUBSEQUENT CODE ##########

### (1) Import global PRIO-Grid shapefile from PRIO-GRID

# The global PRIO-Grid includes the cell geometry of the PRIO-Grid data
# structure for all land and sea cells (for entire globe, 259200 cells)
# Source: https://grid.prio.org/#/extensions
prio_grid <- st_read("priogrid_cell.shp")

# Convert 'prio_grid' into an sf object for subsequent spatial operations
prio_grid <- st_as_sf(prio_grid)

# Set the CRS of 'prio_grid' to WGS 84 (EPSG:4326)
st_crs(prio_grid) <- 4326

# Create a unique 'polygon_id' identifier of each PRIO-Grid cell
prio_grid$polygon_id <- 1:259200


### (2) Import PRIO-Grid data

# The following PRIO-Grid data is the final and most previous dataset
load("pgm_model_data.RData")
pgm_data <- pgm_model_data

# Extract each PRIO-Grid centroid as an sf object -- 'point' geometry
pgm_sf = st_as_sf(pgm_data, coords = c("long", "lat"), 
                 crs = 4326)

# Select only the 'geometry' column and remove rows with duplicated geometries
pgm_sf_points <- pgm_sf[, c("geometry")] 
pgm_sf_points <- pgm_sf_points[!duplicated(pgm_sf_points$geometry),] 
save(pgm_sf_points, file = "pgm_sf_points.RData")

# Subset for only relevant variables and drop duplicate rows
pgm_subset <- pgm_data %>% dplyr::select(lat, long, country_name)
pgm_subset <- unique(pgm_subset)
pgm_subset$pg_id <- seq(1:nrow(pgm_subset))


### (3) Import the world map and store as an sf object

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


### (4) Import PRIO-Grid ethnic settlement data

# The following PRIO-Grid ethnic settlement data includes id's indicating which 
# ethnic settlements are located within the given PRIO-Grid
load("prio_grid_ethnic.RData")


### (5) Import EPR Core dataset from the Ethnic Power Relations Family 2021

# The EPR Core dataset identifies all politically relevant ethnic groups and 
# their access to state power in every country of Africa from 1946 to 2021
# It codes the degree to which their representatives held executive-level power - 
# from total control of the government to overt political discrimination
# Source: https://icr.ethz.ch/data/epr/
EPR_2021 <- read_csv("EPR-2021.csv")

# Create a binary var 'is_in_Africa' to identify non-African countries
EPR_2021$is_in_Africa <- ifelse(EPR_2021$statename == "Guinea-Bissau" | 
                                  EPR_2021$statename == "Equatorial Guinea" |
                                  EPR_2021$statename == "Gambia" |
                                  EPR_2021$statename == "Mali" |
                                  EPR_2021$statename == "Senegal" |
                                  EPR_2021$statename == "Benin" |
                                  EPR_2021$statename == "Mauritania" |
                                  EPR_2021$statename == "Niger" |
                                  EPR_2021$statename == "Cote D'Ivoire" |
                                  EPR_2021$statename == "Guinea" |
                                  EPR_2021$statename == "Burkina Faso (Upper Volta)" |
                                  EPR_2021$statename == "Liberia" |
                                  EPR_2021$statename == "Sierra Leone" |
                                  EPR_2021$statename == "Ghana" |
                                  EPR_2021$statename == "Togo" |
                                  EPR_2021$statename == "Cameroon" |
                                  EPR_2021$statename == "Nigeria" |
                                  EPR_2021$statename == "Gabon" |
                                  EPR_2021$statename == "Central African Republic" |
                                  EPR_2021$statename == "Chad" |
                                  EPR_2021$statename == "Congo" |
                                  EPR_2021$statename == "Congo, Democratic Republic of (Zaire)" |
                                  EPR_2021$statename == "Uganda" |
                                  EPR_2021$statename == "Kenya" |
                                  EPR_2021$statename == "Tanzania (Tanganyika)" |
                                  EPR_2021$statename == "Zanzibar" |
                                  EPR_2021$statename == "Burundi" |
                                  EPR_2021$statename == "Rwanda" |
                                  EPR_2021$statename == "Somalia" |
                                  EPR_2021$statename == "Djibouti" |
                                  EPR_2021$statename == "Ethiopia" |
                                  EPR_2021$statename == "Eritrea" |
                                  EPR_2021$statename == "Angola" |
                                  EPR_2021$statename == "Mozambique" |
                                  EPR_2021$statename == "Zambia" |
                                  EPR_2021$statename == "Zimbabwe (Rhodesia)" |
                                  EPR_2021$statename == "Malawi" |
                                  EPR_2021$statename == "South Africa" |
                                  EPR_2021$statename == "Lesotho" |
                                  EPR_2021$statename == "Namibia" |
                                  EPR_2021$statename == "Botswana" |
                                  EPR_2021$statename == "Swaziland (Eswatini)" |
                                  EPR_2021$statename == "Madagascar (Malagasy)" |
                                  EPR_2021$statename == "Comoros" |
                                  EPR_2021$statename == "Mauritius" |
                                  EPR_2021$statename == "Morocco" |
                                  EPR_2021$statename == "Algeria" |
                                  EPR_2021$statename == "Tunisia" | 
                                  EPR_2021$statename == "Libya" | 
                                  EPR_2021$statename == "Sudan" |
                                  EPR_2021$statename == "Cape Verde" |
                                  EPR_2021$statename == "Egypt" |
                                  EPR_2021$statename == "Western Sahara" |
                                  EPR_2021$statename == "Ivory Coast" |
                                  EPR_2021$statename == "South Sudan", 1, 0)

# Remove all non-African countries from the dataset
EPR_2021 <- subset(EPR_2021, is_in_Africa == "1")

# Create the dataframe group_years that expands the var 'to' and var 'from' 
# years from EPR_2021 for each 'groupid' within a unique 'statename'
group_years <- EPR_2021 %>% 
  rowwise() %>% 
  do(data.frame(from= seq(.$from, .$to),
                groupid= rep(.$groupid, .$to-.$from+1),
                gwid= rep(.$gwid, .$to-.$from+1)))

# Merge the dataframe group_years with EPR_2021 using 'all', which specifies
# keep all rows from the left and the right side, resulting in the 
# EPR_2021_expanded dataframe
EPR_2021_expanded <- merge(group_years, EPR_2021, by= c("gwid", "groupid", "from"), all=TRUE)

# Delete 'to' column
EPR_2021_expanded <- EPR_2021_expanded[, -c(5)]

# Change 'from' column to 'year
names(EPR_2021_expanded)[3] <- 'year'

# Fill down values in EPR_2021_expanded
EPR_2021_expanded <- EPR_2021_expanded %>% tidyr::fill(names(EPR_2021_expanded), .direction = "down")


### (6) Import ACD2EPR dataset from the Ethnic Power Relations Family 2021

# The ACD2EPR dataset links conflicts inventoried in UCDP / PRIO Armed Conflict
# Dataset to politically relevant ethnic groups 
# Source: https://icr.ethz.ch/data/epr/
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

# Subset for only relevant columns and remove all duplicates and all NAs
ACD2EPR_2021 <- subset(ACD2EPR_2021, select = c(gwgroupid, statename, claim, recruitment, support))
ACD2EPR_2021 <- distinct(ACD2EPR_2021)
ACD2EPR_2021 <- subset(ACD2EPR_2021, gwgroupid != "")
ACD2EPR_2021 <- na.omit(ACD2EPR_2021)

# Group by 'gwgroupid' and 'statename', calculate the sum of the 'claim',
# 'recruitment', and 'support', and arrange in descending order
ACD2EPR_2021_sum <- ACD2EPR_2021 %>%
  group_by(gwgroupid, statename, claim, recruitment, support) %>%
  summarize(sum_row = sum(claim, recruitment, support)) %>%
  arrange(desc(sum_row))

# For each unique 'gwgroupid' and 'statename' combination, take only the row
# with the highest 'sum_row' value
ACD2EPR_2021 <- ACD2EPR_2021_sum %>%
  group_by(gwgroupid, statename) %>%
  slice_max(sum_row)

# Manually choose the remaining duplicates when grouping by 'gwgroupid' and
# 'statename'
ACD2EPR_2021 <- ACD2EPR_2021 %>%
  filter(!(gwgroupid == 43202000 & statename == "Mali" & claim == 0) &
           !(gwgroupid == 48302100 & statename == "Chad" & claim == 0) &
           !(gwgroupid == 48302200 & statename == "Chad" & claim == 0) &
           !(gwgroupid == 49011000 & statename == "Congo, Democratic Republic of (Zaire)" & support == 0) &
           !(gwgroupid == 50009110 & statename == "Uganda" & support == 0) &
           !(gwgroupid == 53008000 & statename == "Ethiopia" & support == 0))

# Expand each row of ACD2EPR_2021 to be recognized for each year from 1990 to 2021
ACD2EPR_2021_expanded <- ACD2EPR_2021[rep(seq_len(nrow(ACD2EPR_2021)), each = 32), ]
years <- rep(seq(1990, 2021))
years <- rep(years, times = 89)
ACD2EPR_2021_expanded$year <- years[1:nrow(ACD2EPR_2021_expanded)]

### (7) Import nighttime light dataset

# The dataset contains (1) temporally calibrated DMSP-OLS NTL time series data 
# from 1992-2013; and (2) converted NTL time series from the VIIRS data (2014-2018)
# Source: Li, X., Zhou, Y., Zhao, M. et al. A harmonized global nighttime light dataset 1992–2018. Sci Data 7, 168 (2020)

# Import nighttime light data as a raster object, which is gridded
# data saved in pixels
nighttime_lights_1992 <- raster("Harmonized_DN_NTL_1992_calDMSP.tif")
nighttime_lights_1993 <- raster("Harmonized_DN_NTL_1993_calDMSP.tif")
nighttime_lights_1994 <- raster("Harmonized_DN_NTL_1994_calDMSP.tif")
nighttime_lights_1995 <- raster("Harmonized_DN_NTL_1995_calDMSP.tif")
nighttime_lights_1996 <- raster("Harmonized_DN_NTL_1996_calDMSP.tif")
nighttime_lights_1997 <- raster("Harmonized_DN_NTL_1997_calDMSP.tif")
nighttime_lights_1998 <- raster("Harmonized_DN_NTL_1998_calDMSP.tif")
nighttime_lights_1999 <- raster("Harmonized_DN_NTL_1999_calDMSP.tif")
nighttime_lights_2000 <- raster("Harmonized_DN_NTL_2000_calDMSP.tif")
nighttime_lights_2001 <- raster("Harmonized_DN_NTL_2001_calDMSP.tif")
nighttime_lights_2002 <- raster("Harmonized_DN_NTL_2002_calDMSP.tif")
nighttime_lights_2003 <- raster("Harmonized_DN_NTL_2003_calDMSP.tif")
nighttime_lights_2004 <- raster("Harmonized_DN_NTL_2004_calDMSP.tif")
nighttime_lights_2005 <- raster("Harmonized_DN_NTL_2005_calDMSP.tif")
nighttime_lights_2006 <- raster("Harmonized_DN_NTL_2006_calDMSP.tif")
nighttime_lights_2007 <- raster("Harmonized_DN_NTL_2007_calDMSP.tif")
nighttime_lights_2008 <- raster("Harmonized_DN_NTL_2008_calDMSP.tif")
nighttime_lights_2009 <- raster("Harmonized_DN_NTL_2009_calDMSP.tif")
nighttime_lights_2010 <- raster("Harmonized_DN_NTL_2010_calDMSP.tif")
nighttime_lights_2011 <- raster("Harmonized_DN_NTL_2011_calDMSP.tif")
nighttime_lights_2012 <- raster("Harmonized_DN_NTL_2012_calDMSP.tif")
nighttime_lights_2013 <- raster("Harmonized_DN_NTL_2013_calDMSP.tif")
nighttime_lights_2014 <- raster("Harmonized_DN_NTL_2014_simVIIRS.tif")
nighttime_lights_2015 <- raster("Harmonized_DN_NTL_2015_simVIIRS.tif")
nighttime_lights_2016 <- raster("Harmonized_DN_NTL_2016_simVIIRS.tif")
nighttime_lights_2017 <- raster("Harmonized_DN_NTL_2017_simVIIRS.tif")
nighttime_lights_2018 <- raster("Harmonized_DN_NTL_2018_simVIIRS.tif")

# Assess the extent of the raster objects for subsequent stack
# The extent of a raster object describes the minimum and maximum x and y
# coordinates, as well as the number of rows and columns in the raster object
extent(nighttime_lights_1992)
extent(nighttime_lights_1993)
extent(nighttime_lights_1994)
extent(nighttime_lights_1995)
extent(nighttime_lights_1996)
extent(nighttime_lights_1997)
extent(nighttime_lights_1998)
extent(nighttime_lights_1999)
extent(nighttime_lights_2000)
extent(nighttime_lights_2001)
extent(nighttime_lights_2002)
extent(nighttime_lights_2003)
extent(nighttime_lights_2004)
extent(nighttime_lights_2005)
extent(nighttime_lights_2006)
extent(nighttime_lights_2007)
extent(nighttime_lights_2008)
extent(nighttime_lights_2009)
extent(nighttime_lights_2010)
extent(nighttime_lights_2011)
extent(nighttime_lights_2012)
extent(nighttime_lights_2013)
extent(nighttime_lights_2014)
extent(nighttime_lights_2015)
extent(nighttime_lights_2016)
extent(nighttime_lights_2017)
extent(nighttime_lights_2018)

# (1) For 'nighttime_lights_1992' through to 'nighttime_lights_2008', the raster
# objects have the following extent:
# class      : Extent 
# xmin       : -180.0042 
# xmax       : 180.0042 
# ymin       : -65.00417 
# ymax       : 75.00417 

# (2) For 'nighttime_lights_2009' through to 'nighttime_lights_2011', the raster
# objects have the following extent:
# class      : Extent 
# xmin       : -180 
# xmax       : 180.0083 
# ymin       : -65.00833 
# ymax       : 75 

# (3) For 'nighttime_lights_2012' through to 'nighttime_lights_2018', the raster
# objects have the following extent:
# class      : Extent 
# xmin       : -180.0042 
# xmax       : 180.0042 
# ymin       : -65.00417 
# ymax       : 75.00417 

# Note that since the second category of extents is different from the first
# and third category of extents, stacking all the raster objects into a single
# stack is not possible and will result in the following error -- 
# Error in compareRaster(x) : different extent

# Stack the raster objects from 1992 to 2008, combining the multiple layers
# into a single object. Each raster layer has the same extent (spatial coverage)
# and resolution (cell size) as the others in the stack
nighttime_lights_1992_2008 <- stack(nighttime_lights_1992, nighttime_lights_1993, nighttime_lights_1994, 
                                    nighttime_lights_1995, nighttime_lights_1996, nighttime_lights_1997, 
                                    nighttime_lights_1998, nighttime_lights_1999, nighttime_lights_2000, 
                                    nighttime_lights_2001, nighttime_lights_2002, nighttime_lights_2003, 
                                    nighttime_lights_2004, nighttime_lights_2005, nighttime_lights_2006,
                                    nighttime_lights_2007, nighttime_lights_2008)

# Assign a time stamp to each layer in the 'nighttime_lights_1992_2008' stack
# using the setZ function, which sets the Z dimension of a raster object
nighttime_lights_1992_2008 <- setZ(nighttime_lights_1992_2008,
                                   seq(as.Date("1992/1/1"), 
                                       by = "year",
                                       length.out = 17))

# Stack the raster objects from 2009 to 2011, combining the multiple layers
# into a single object. Each raster layer has the same extent (spatial coverage)
# and resolution (cell size) as the others in the stack
nighttime_lights_2009_2011 <- stack(nighttime_lights_2009, nighttime_lights_2010, nighttime_lights_2011)

# Assign a time stamp to each layer in the 'nighttime_lights_1992_2008' stack
# using the setZ function, which sets the Z dimension of a raster object
nighttime_lights_2009_2011 <- setZ(nighttime_lights_2009_2011,
                                   seq(as.Date("2009/1/1"), 
                                       by = "year",
                                       length.out = 3))

# Stack the raster objects from 2012 to 2018, combining the multiple layers
# into a single object. Each raster layer has the same extent (spatial coverage)
# and resolution (cell size) as the others in the stack
nighttime_lights_2012_2018 <- stack(nighttime_lights_2012, nighttime_lights_2013, nighttime_lights_2014, 
                                    nighttime_lights_2015, nighttime_lights_2016, nighttime_lights_2017, 
                                    nighttime_lights_2018)

# Assign a time stamp to each layer in the 'nighttime_lights_1992_2008' stack
# using the setZ function, which sets the Z dimension of a raster object
nighttime_lights_2012_2018 <- setZ(nighttime_lights_2012_2018,
                                   seq(as.Date("2012/1/1"), 
                                       by = "year",
                                       length.out = 7))


### (8) Import Population dataset from WorldPop

# Mosaiced 1km resolution global datasets using 100m resolution population
# count datasets
# Source: https://hub.worldpop.org

pop_2000 <- raster("ppp_2000_1km_Aggregated.tif")
pop_2001 <- raster("ppp_2001_1km_Aggregated.tif")
pop_2002 <- raster("ppp_2002_1km_Aggregated.tif")
pop_2003 <- raster("ppp_2003_1km_Aggregated.tif")
pop_2004 <- raster("ppp_2004_1km_Aggregated.tif")
pop_2005 <- raster("ppp_2005_1km_Aggregated.tif")
pop_2006 <- raster("ppp_2006_1km_Aggregated.tif")
pop_2007 <- raster("ppp_2007_1km_Aggregated.tif")
pop_2008 <- raster("ppp_2008_1km_Aggregated.tif")
pop_2009 <- raster("ppp_2009_1km_Aggregated.tif")
pop_2010 <- raster("ppp_2010_1km_Aggregated.tif")
pop_2011 <- raster("ppp_2011_1km_Aggregated.tif")
pop_2012 <- raster("ppp_2012_1km_Aggregated.tif")
pop_2013 <- raster("ppp_2013_1km_Aggregated.tif")
pop_2014 <- raster("ppp_2014_1km_Aggregated.tif")
pop_2015 <- raster("ppp_2015_1km_Aggregated.tif")
pop_2016 <- raster("ppp_2016_1km_Aggregated.tif")
pop_2017 <- raster("ppp_2017_1km_Aggregated.tif")
pop_2018 <- raster("ppp_2018_1km_Aggregated.tif")
pop_2019 <- raster("ppp_2019_1km_Aggregated.tif")
pop_2020 <- raster("ppp_2020_1km_Aggregated.tif")

# Assess the extent of the raster objects for subsequent stack
# The extent of a raster object describes the minimum and maximum x and y
# coordinates, as well as the number of rows and columns in the raster object
extent(pop_2000)
extent(pop_2001)
extent(pop_2002)
extent(pop_2003)
extent(pop_2004)
extent(pop_2005)
extent(pop_2006)
extent(pop_2007)
extent(pop_2008)
extent(pop_2009)
extent(pop_2010)
extent(pop_2011)
extent(pop_2012)
extent(pop_2013)
extent(pop_2014)
extent(pop_2015)
extent(pop_2015)
extent(pop_2016)
extent(pop_2017)
extent(pop_2018)
extent(pop_2019)
extent(pop_2020)

# (1) For 'pop_2000', the raster object has the following extent
# class      : Extent 
# xmin       : -180.0012 
# xmax       : 179.9987 
# ymin       : -71.99208 
# ymax       : 84.00792

# (2) For 'pop_2001' through to 'pop_2009' and for 'pop_2011' through to 'pop_2020',
# the raster objects have the following extent:
# class      : Extent 
# xmin       : -180.0012 
# xmax       : 179.9987 
# ymin       : -72.00042 
# ymax       : 83.99958 

# (1) For 'pop_2010', the raster object has the following extent
# class      : Extent 
# xmin       : -180.0012 
# xmax       : 179.9987 
# ymin       : -71.99208 
# ymax       : 84.00792 

# Select the part of the 'pop_2000' raster that overlaps with the extent of the
# 'pop_2001' raster and then for 'pop_2001' through to 'pop_2020', select the part
# of those respective rasters that overlap with the extent of 'pop_2000'
pop_2000 <- terra::crop(pop_2000,extent(pop_2001))
pop_2001 <- terra::crop(pop_2001,extent(pop_2000))
pop_2002 <- terra::crop(pop_2002,extent(pop_2000))
pop_2003 <- terra::crop(pop_2003,extent(pop_2000))
pop_2004 <- terra::crop(pop_2004,extent(pop_2000))
pop_2005 <- terra::crop(pop_2005,extent(pop_2000))
pop_2006 <- terra::crop(pop_2006,extent(pop_2000))
pop_2007 <- terra::crop(pop_2007,extent(pop_2000))
pop_2008 <- terra::crop(pop_2008,extent(pop_2000))
pop_2009 <- terra::crop(pop_2009,extent(pop_2000))
pop_2010 <- terra::crop(pop_2010,extent(pop_2000))
pop_2011 <- terra::crop(pop_2011,extent(pop_2000))
pop_2012 <- terra::crop(pop_2012,extent(pop_2000))
pop_2013 <- terra::crop(pop_2013,extent(pop_2000))
pop_2014 <- terra::crop(pop_2014,extent(pop_2000))
pop_2015 <- terra::crop(pop_2015,extent(pop_2000))
pop_2016 <- terra::crop(pop_2016,extent(pop_2000))
pop_2017 <- terra::crop(pop_2017,extent(pop_2000))
pop_2018 <- terra::crop(pop_2018,extent(pop_2000))
pop_2019 <- terra::crop(pop_2019,extent(pop_2000))
pop_2020 <- terra::crop(pop_2020,extent(pop_2000))

# Stack the raster objects from 2000 to 2020, combining the multiple layers
# into a single object. Each raster layer has the same extent (spatial coverage)
# and resolution (cell size) as the others in the stack
pop_2000_2020 <- stack(pop_2000, pop_2001, pop_2002, pop_2003,
                      pop_2004, pop_2005, pop_2006, pop_2007,
                      pop_2008, pop_2009, pop_2010, pop_2011,
                      pop_2012, pop_2013, pop_2014, pop_2015,
                      pop_2016, pop_2017, pop_2018, pop_2019, pop_2020)

# Assign a time stamp to each layer in the 'pop_2000_2020' stack
# using the setZ function, which sets the Z dimension of a raster object
pop_2000_2020 <- setZ(pop_2000_2020,
                     seq(as.Date("2000/1/1"),
                         by = "year", length.out = 21))


###  (9) Load Temperature and Precipitation Data

# Climatic Research Unit (CRU) Time-Series (TS) version 4.05 of high-resolution 
# gridded data of month-by-month variation in climate (Jan. 1901- Dec. 2020)
# Source: https://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.05/data/pre &
# https://data.ceda.ac.uk/badc/cru/data/cru_ts/cru_ts_4.05/data/temp
list_of_files <- list.files(path = "C:/Users/ra46bel/Documents",
    pattern = "\\.gz$",
    full.names = TRUE)

list_of_files %>% 
    walk(gunzip)

### Temperature ###

# Open the NetCDF file for temperature
temp_mean <- nc_open("cru_ts4.05.1901.2020.tmp.dat.nc")

# Extract 'lon', 'lat', 'time', and 'tmp' from the NetCDF file
lon <- ncvar_get(temp_mean, "lon")
lat <- ncvar_get(temp_mean, "lat", verbose = F)
t <- ncvar_get(temp_mean, "time")
temp_mean_array <- ncvar_get(temp_mean, "tmp")

# Check the dimensions of temp_mean_array
dim(temp_mean_array)

# Get the '_FillValue' attribute value associated with the variable 'tmp' in temp_mean
fillvalue <- ncatt_get(temp_mean, "tmp", "_FillValue")

# Close the opened NetCDF file
nc_close(temp_mean)

# Replace all values in temp_mean_array that are equal to the '_FillValue' attribute
# value with NA
temp_mean_array[temp_mean_array == fillvalue$value] <- NA

# Create a raster brick object, which is a collection of one or more raster objects
# that are share the same spatial extent and resolution, from temp_mean_array
r_brick <- brick(temp_mean_array, xmn=min(lat), xmx=max(lat), 
                 ymn=min(lon), ymx=max(lon), 
                 crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

# Flip the raster brick object along the y-axis
r_brick <- flip(t(r_brick), direction='y')

# Save as r_brick_temp
r_brick_temp <- r_brick

### Precipitation ###

# Open the NetCDF file for precipitation
precip_mean <- nc_open("cru_ts4.05.1901.2020.pre.dat.nc")

# Extract 'lon', 'lat', 'time', and 'pre' from the NetCDF file
lon <- ncvar_get(precip_mean, "lon")
lat <- ncvar_get(precip_mean, "lat", verbose = F)
t <- ncvar_get(precip_mean, "time")
precip_mean_array <- ncvar_get(precip_mean, "pre")

# Get the '_FillValue' attribute value associated with the variable 'pre' in precip_mean
fillvalue <- ncatt_get(precip_mean, "pre", "_FillValue")

# Close the opened NetCDF file
nc_close(precip_mean)

# Replace all values in precip_mean_array that are equal to the '_FillValue' attribute
# value with NA
precip_mean_array[precip_mean_array == fillvalue$value] <- NA

# Create a raster brick object, which is a collection of one or more raster objects
# that are share the same spatial extent and resolution, from precip_mean_array
r_brick <- brick(precip_mean_array, xmn=min(lat), xmx=max(lat), 
                 ymn=min(lon), ymx=max(lon), 
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

# Flip the raster brick object along the y-axis
r_brick_precip <- flip(t(r_brick), direction='y')


###  (10) Load Armed Conflict Location & Event Data Project (ACLED)

# Source: https://acleddata.com

acled_data <- read_csv("acled.data.csv", na="")



########## ASSIGNING PRIO-GRID CELL GEOMETRIES IN AFRICA ##########

# Initialize var 'polygon_id', 'indicator', 'contain', and 'store_date'
pgm_sf_points$polygon_id <- list(NA)
pgm_sf_points$indicator <- 1:10659
pgm_sf_points$contain <- 0
store_date <- pgm_sf_points$polygon_id

# Switch spherical geometry off
sf_use_s2(FALSE)

  # For each polygon in prio_grid, use the 'st_contains' function to 
  # identify which centroid points in pgm_sf_points are contained within the 
  # polygon
  for (i in 1:nrow(prio_grid)) {
    
    # Get the current polygon and centroid point
    polygon <- prio_grid$geometry[i]
    polygon_id = prio_grid$polygon_id[i]
    
    points <- pgm_sf_points$geometry
    
    # Employ 'st_contains'
    a <- unlist(st_contains(polygon, points))
    
    # If a centroid point is contained within the polygon, update the list
    # with the 'polygon_id' of each containing polygon
    if(length(a)!= 0){
      b <- pgm_sf_points$indicator[a]
      pgm_sf_points$contain[b] <- 1
      for (z in 1:length(b)){
        if(is.na(store_date[b[z]][[1]])[1]){
          store_date[b[z]][[1]] <- c(polygon_id)
        }
        else{
          store_date[b[z]][[1]] <- c(store_date[b[z]][[1]], polygon_id)
        }
      }
    }
  }

# At the end of the loop, the final 'pgm_sf_points$polygon_id' values are
# stored in 'store_date"
pgm_sf_points$polygon_id <- store_date

# Extract latitutde and longitude variables and remove 'geometry' for
# subsequent merge
pgm_sf_points$lat <- st_coordinates(pgm_sf_points)[, 2]
pgm_sf_points$long <- st_coordinates(pgm_sf_points)[, 1]
pgm_sf_points$geometry <- NULL

# Merge 'pgm_sf_points' and 'prio_grid' to realize PRIO-Grid cell geometries
pgm_data_geometry <- merge(prio_grid, pgm_sf_points, by = "polygon_id", all.x = TRUE)
pgm_data_geometry <- pgm_data_geometry[complete.cases(pgm_data_geometry$indicator), ]

# Save dataset
save(pgm_data_geometry, file="pgm_data_geometry.RData")



########## INCORPORATING DATASET ON ETHNIC DISCRIMINATION ##########

### Perform the merge between prio_grid_ethnic and EPR_2021_expanded on 'country_name'
### 'year', and 'gwgroupid'

# Perform the merge for gwgroupid1 and add a "1" to its respective columns
prio_grid_ethnic <- merge(prio_grid_ethnic, EPR_2021_expanded[, c("gwgroupid", "statename", "year", "size", "status", "reg_aut")], 
                by.x = c("gwgroupid1", "country_name", "year"),
                by.y = c("gwgroupid", "statename", "year"),
                all.x = TRUE)
col_names <- names(prio_grid_ethnic)[12:14]
new_col_names <- paste0(col_names, "1")
names(prio_grid_ethnic)[12:14] <- new_col_names

# Perform the merge for gwgroupid2 and add a "2" to its respective columns
prio_grid_ethnic <- merge(prio_grid_ethnic, EPR_2021_expanded[, c("gwgroupid", "statename", "year", "size", "status", "reg_aut")], 
                by.x = c("gwgroupid2", "country_name", "year"),
                by.y = c("gwgroupid", "statename", "year"),
                all.x = TRUE)
col_names <- names(prio_grid_ethnic)[15:17]
new_col_names <- paste0(col_names, "2")
names(prio_grid_ethnic)[15:17] <- new_col_names

# Perform the merge for gwgroupid3 and add a "3" to its respective columns
prio_grid_ethnic <- merge(prio_grid_ethnic, EPR_2021_expanded[, c("gwgroupid", "statename", "year", "size", "status", "reg_aut")], 
                by.x = c("gwgroupid3", "country_name", "year"),
                by.y = c("gwgroupid", "statename", "year"),
                all.x = TRUE)
col_names <- names(prio_grid_ethnic)[18:20]
new_col_names <- paste0(col_names, "3")
names(prio_grid_ethnic)[18:20] <- new_col_names

# Perform the merge for gwgroupid4 and add a "4" to its respective columns
prio_grid_ethnic <- merge(prio_grid_ethnic, EPR_2021_expanded[, c("gwgroupid", "statename", "year", "size", "status", "reg_aut")], 
                by.x = c("gwgroupid4", "country_name", "year"),
                by.y = c("gwgroupid", "statename", "year"),
                all.x = TRUE)
col_names <- names(prio_grid_ethnic)[21:23]
new_col_names <- paste0(col_names, "4")
names(prio_grid_ethnic)[21:23] <- new_col_names

# Perform the merge for gwgroupid6 and add a "6" to its respective columns
prio_grid_ethnic <- merge(prio_grid_ethnic, EPR_2021_expanded[, c("gwgroupid", "statename", "year", "size", "status", "reg_aut")], 
                by.x = c("gwgroupid6", "country_name", "year"),
                by.y = c("gwgroupid", "statename", "year"),
                all.x = TRUE)
col_names <- names(prio_grid_ethnic)[24:26]
new_col_names <- paste0(col_names, "6")
names(prio_grid_ethnic)[24:26] <- new_col_names

# Change 'reg_aut' == 1 if TRUE and 'reg_aut' == 0 if FALSE
prio_grid_ethnic$reg_aut1 <- ifelse(prio_grid_ethnic$reg_aut1 == TRUE, 1, 0)
prio_grid_ethnic$reg_aut2 <- ifelse(prio_grid_ethnic$reg_aut2 == TRUE, 1, 0)
prio_grid_ethnic$reg_aut3 <- ifelse(prio_grid_ethnic$reg_aut3 == TRUE, 1, 0)
prio_grid_ethnic$reg_aut4 <- ifelse(prio_grid_ethnic$reg_aut4 == TRUE, 1, 0)
prio_grid_ethnic$reg_aut6 <- ifelse(prio_grid_ethnic$reg_aut6 == TRUE, 1, 0)

### Convert 'status' variables to type ordinal

# The group rules alone
prio_grid_ethnic$status1[prio_grid_ethnic$status1 == "MONOPOLY"] <- 4
prio_grid_ethnic$status1[prio_grid_ethnic$status1 == "DOMINANT"] <- 4
# The group shares power
prio_grid_ethnic$status1[prio_grid_ethnic$status1 == "SENIOR PARTNER"] <- 3
prio_grid_ethnic$status1[prio_grid_ethnic$status1 == "JUNIOR PARTNER"] <- 3
# The group is excluded
prio_grid_ethnic$status1[prio_grid_ethnic$status1 == "POWERLESS"] <- 2
prio_grid_ethnic$status1[prio_grid_ethnic$status1 == "DISCRIMINATED"] <- 2
prio_grid_ethnic$status1[prio_grid_ethnic$status1 == "SELF-EXCLUSION"] <- 2
# The group is irrelevant
prio_grid_ethnic$status1[prio_grid_ethnic$status1 == "IRRELEVANT"] <- 1
prio_grid_ethnic$status1[prio_grid_ethnic$status1 == "STATE COLLAPSE"] <- 1

# The group rules alone
prio_grid_ethnic$status2[prio_grid_ethnic$status2 == "MONOPOLY"] <- 4
prio_grid_ethnic$status2[prio_grid_ethnic$status2 == "DOMINANT"] <- 4
# The group shares power
prio_grid_ethnic$status2[prio_grid_ethnic$status2 == "SENIOR PARTNER"] <- 3
prio_grid_ethnic$status2[prio_grid_ethnic$status2 == "JUNIOR PARTNER"] <- 3
# The group is excluded
prio_grid_ethnic$status2[prio_grid_ethnic$status2 == "POWERLESS"] <- 2
prio_grid_ethnic$status2[prio_grid_ethnic$status2 == "DISCRIMINATED"] <- 2
prio_grid_ethnic$status2[prio_grid_ethnic$status2 == "SELF-EXCLUSION"] <- 2
# The group is irrelevant
prio_grid_ethnic$status2[prio_grid_ethnic$status2 == "IRRELEVANT"] <- 1
prio_grid_ethnic$status2[prio_grid_ethnic$status2 == "STATE COLLAPSE"] <- 1

# The group rules alone
prio_grid_ethnic$status3[prio_grid_ethnic$status3 == "MONOPOLY"] <- 4
prio_grid_ethnic$status3[prio_grid_ethnic$status3 == "DOMINANT"] <- 4
# The group shares power
prio_grid_ethnic$status3[prio_grid_ethnic$status3 == "SENIOR PARTNER"] <- 3
prio_grid_ethnic$status3[prio_grid_ethnic$status3 == "JUNIOR PARTNER"] <- 3
# The group is excluded
prio_grid_ethnic$status3[prio_grid_ethnic$status3 == "POWERLESS"] <- 2
prio_grid_ethnic$status3[prio_grid_ethnic$status3 == "DISCRIMINATED"] <- 2
prio_grid_ethnic$status3[prio_grid_ethnic$status3 == "SELF-EXCLUSION"] <- 2
# The group is irrelevant
prio_grid_ethnic$status3[prio_grid_ethnic$status3 == "IRRELEVANT"] <- 1
prio_grid_ethnic$status3[prio_grid_ethnic$status3 == "STATE COLLAPSE"] <- 1

# The group rules alone
prio_grid_ethnic$status4[prio_grid_ethnic$status4 == "MONOPOLY"] <- 4
prio_grid_ethnic$status4[prio_grid_ethnic$status4 == "DOMINANT"] <- 4
# The group shares power
prio_grid_ethnic$status4[prio_grid_ethnic$status4 == "SENIOR PARTNER"] <- 3
prio_grid_ethnic$status4[prio_grid_ethnic$status4 == "JUNIOR PARTNER"] <- 3
# The group is excluded
prio_grid_ethnic$status4[prio_grid_ethnic$status4 == "POWERLESS"] <- 2
prio_grid_ethnic$status4[prio_grid_ethnic$status4 == "DISCRIMINATED"] <- 2
prio_grid_ethnic$status4[prio_grid_ethnic$status4 == "SELF-EXCLUSION"] <- 2
# The group is irrelevant
prio_grid_ethnic$status4[prio_grid_ethnic$status4 == "IRRELEVANT"] <- 1
prio_grid_ethnic$status4[prio_grid_ethnic$status4 == "STATE COLLAPSE"] <- 1

# The group rules alone
prio_grid_ethnic$status6[prio_grid_ethnic$status6 == "MONOPOLY"] <- 4
prio_grid_ethnic$status6[prio_grid_ethnic$status6 == "DOMINANT"] <- 4
# The group shares power
prio_grid_ethnic$status6[prio_grid_ethnic$status6 == "SENIOR PARTNER"] <- 3
prio_grid_ethnic$status6[prio_grid_ethnic$status6 == "JUNIOR PARTNER"] <- 3
# The group is excluded
prio_grid_ethnic$status6[prio_grid_ethnic$status6 == "POWERLESS"] <- 2
prio_grid_ethnic$status6[prio_grid_ethnic$status6 == "DISCRIMINATED"] <- 2
prio_grid_ethnic$status6[prio_grid_ethnic$status6 == "SELF-EXCLUSION"] <- 2
# The group is irrelevant
prio_grid_ethnic$status6[prio_grid_ethnic$status6 == "IRRELEVANT"] <- 1
prio_grid_ethnic$status6[prio_grid_ethnic$status6 == "STATE COLLAPSE"] <- 1

### Create new binary variable indicating if any ethnic settlement in a given
### PRIO-Grid rules alone (4), shares power (3), is excluded (2), or is
### irrelevant (1)

# Create binary variable for presence of an ethnic settlement in a PRIO-Grid that
# rules alone
prio_grid_ethnic$status_alone <- ifelse(
  (prio_grid_ethnic$overlapping == 1 & prio_grid_ethnic$status1 == 4) |
    (prio_grid_ethnic$overlapping == 2 & (prio_grid_ethnic$status1 == 4 | prio_grid_ethnic$status2 == 4)) |
    (prio_grid_ethnic$overlapping == 3 & (prio_grid_ethnic$status1 == 4 | prio_grid_ethnic$status2 == 4 | prio_grid_ethnic$status3 == 4)) |
    (prio_grid_ethnic$overlapping == 4 & (prio_grid_ethnic$status1 == 4 | prio_grid_ethnic$status2 == 4 | prio_grid_ethnic$status3 == 4 | prio_grid_ethnic$status4 == 4)) |
    (prio_grid_ethnic$overlapping == 6 & (prio_grid_ethnic$status1 == 4 | prio_grid_ethnic$status2 == 4 | prio_grid_ethnic$status3 == 4 | prio_grid_ethnic$status4 == 4 | prio_grid_ethnic$status6 == 4)),
  1, 0)

# Create binary variable for presence of an ethnic settlement in a PRIO-Grid that
# shares power
prio_grid_ethnic$status_share <- ifelse(
  (prio_grid_ethnic$overlapping == 1 & prio_grid_ethnic$status1 == 3) |
    (prio_grid_ethnic$overlapping == 2 & (prio_grid_ethnic$status1 == 3 | prio_grid_ethnic$status2 == 3)) |
    (prio_grid_ethnic$overlapping == 3 & (prio_grid_ethnic$status1 == 3 | prio_grid_ethnic$status2 == 3 | prio_grid_ethnic$status3 == 3)) |
    (prio_grid_ethnic$overlapping == 4 & (prio_grid_ethnic$status1 == 3 | prio_grid_ethnic$status2 == 3 | prio_grid_ethnic$status3 == 3 | prio_grid_ethnic$status4 == 3)) |
    (prio_grid_ethnic$overlapping == 6 & (prio_grid_ethnic$status1 == 3 | prio_grid_ethnic$status2 == 3 | prio_grid_ethnic$status3 == 3 | prio_grid_ethnic$status4 == 3 | prio_grid_ethnic$status6 == 3)),
  1, 0)

# Create binary variable for presence of an ethnic settlement in a PRIO-Grid that
# is excluded
prio_grid_ethnic$status_excluded <- ifelse(
  (prio_grid_ethnic$overlapping == 1 & prio_grid_ethnic$status1 == 2) |
    (prio_grid_ethnic$overlapping == 2 & (prio_grid_ethnic$status1 == 2 | prio_grid_ethnic$status2 == 2)) |
    (prio_grid_ethnic$overlapping == 3 & (prio_grid_ethnic$status1 == 2 | prio_grid_ethnic$status2 == 2 | prio_grid_ethnic$status3 == 2)) |
    (prio_grid_ethnic$overlapping == 4 & (prio_grid_ethnic$status1 == 2 | prio_grid_ethnic$status2 == 2 | prio_grid_ethnic$status3 == 2 | prio_grid_ethnic$status4 == 2)) |
    (prio_grid_ethnic$overlapping == 6 & (prio_grid_ethnic$status1 == 2 | prio_grid_ethnic$status2 == 2 | prio_grid_ethnic$status3 == 2 | prio_grid_ethnic$status4 == 2 | prio_grid_ethnic$status6 == 2)),
  1, 0)

# Create binary variable for presence of an ethnic settlement in a PRIO-Grid that
# is irrelevant
prio_grid_ethnic$status_irrelevant <- ifelse(
  (prio_grid_ethnic$overlapping == 1 & prio_grid_ethnic$status1 == 1) |
    (prio_grid_ethnic$overlapping == 2 & (prio_grid_ethnic$status1 == 1 | prio_grid_ethnic$status2 == 1)) |
    (prio_grid_ethnic$overlapping == 3 & (prio_grid_ethnic$status1 == 1 | prio_grid_ethnic$status2 == 1 | prio_grid_ethnic$status3 == 1)) |
    (prio_grid_ethnic$overlapping == 4 & (prio_grid_ethnic$status1 == 1 | prio_grid_ethnic$status2 == 1 | prio_grid_ethnic$status3 == 1 | prio_grid_ethnic$status4 == 1)) |
    (prio_grid_ethnic$overlapping == 6 & (prio_grid_ethnic$status1 == 1 | prio_grid_ethnic$status2 == 1 | prio_grid_ethnic$status3 == 1 | prio_grid_ethnic$status4 == 1 | prio_grid_ethnic$status6 == 1)),
  1, 0)
prio_grid_ethnic <- subset(prio_grid_ethnic, select = -c(status1, status2, status3, status4, status6))

### Weight average regional autonomy in a given PRIO-Grid

# Calculate weighted sum of 'reg_aut' columns
reg_aut_sum <- rowSums(prio_grid_ethnic[, c("reg_aut1", "reg_aut2", "reg_aut3", "reg_aut4", "reg_aut6")] * prio_grid_ethnic[, c("size1", "size2", "size3", "size4", "size6")], na.rm = TRUE)

# Calculate sum of weights
weight_sum <- rowSums(prio_grid_ethnic[, c("size1", "size2", "size3", "size4", "size6")], na.rm = TRUE)

# Calculate weighted average of 'reg_aut'
prio_grid_ethnic$reg_aut <- reg_aut_sum / weight_sum

prio_grid_ethnic <- subset(prio_grid_ethnic, select = -c(reg_aut1, reg_aut2, reg_aut3, reg_aut4, reg_aut6))



########## INCORPORATING DATASET ON CONFLICTS BETWEEN ETHNIC GROUPS ##########

### Perform the merge between prio_grid_ethnic and ACD2EPR_2021_expanded on 'country_name'
### 'year', and 'gwgroupid'

# Perform the merge for gwgroupid1 and add a "1" to its respective columns
prio_grid_ethnic <- merge(prio_grid_ethnic, ACD2EPR_2021_expanded[, c("gwgroupid", "statename", "year", "claim", "recruitment", "support")], 
                          by.x = c("gwgroupid1", "country_name", "year"),
                          by.y = c("gwgroupid", "statename", "year"),
                          all.x = TRUE)
col_names <- names(prio_grid_ethnic)[22:24]
new_col_names <- paste0(col_names, "1")
names(prio_grid_ethnic)[22:24] <- new_col_names

# Perform the merge for gwgroupid1 and add a "2" to its respective columns
prio_grid_ethnic <- merge(prio_grid_ethnic, ACD2EPR_2021_expanded[, c("gwgroupid", "statename", "year", "claim", "recruitment", "support")], 
                          by.x = c("gwgroupid2", "country_name", "year"),
                          by.y = c("gwgroupid", "statename", "year"),
                          all.x = TRUE)
col_names <- names(prio_grid_ethnic)[25:27]
new_col_names <- paste0(col_names, "2")
names(prio_grid_ethnic)[25:27] <- new_col_names

# Perform the merge for gwgroupid1 and add a "3" to its respective columns
prio_grid_ethnic <- merge(prio_grid_ethnic, ACD2EPR_2021_expanded[, c("gwgroupid", "statename", "year", "claim", "recruitment", "support")], 
                          by.x = c("gwgroupid2", "country_name", "year"),
                          by.y = c("gwgroupid", "statename", "year"),
                          all.x = TRUE)
col_names <- names(prio_grid_ethnic)[28:30]
new_col_names <- paste0(col_names, "3")
names(prio_grid_ethnic)[28:30] <- new_col_names

# Perform the merge for gwgroupid1 and add a "4" to its respective columns
prio_grid_ethnic <- merge(prio_grid_ethnic, ACD2EPR_2021_expanded[, c("gwgroupid", "statename", "year", "claim", "recruitment", "support")], 
                          by.x = c("gwgroupid2", "country_name", "year"),
                          by.y = c("gwgroupid", "statename", "year"),
                          all.x = TRUE)
col_names <- names(prio_grid_ethnic)[31:33]
new_col_names <- paste0(col_names, "4")
names(prio_grid_ethnic)[31:33] <- new_col_names

# Perform the merge for gwgroupid1 and add a "6" to its respective columns
prio_grid_ethnic <- merge(prio_grid_ethnic, ACD2EPR_2021_expanded[, c("gwgroupid", "statename", "year", "claim", "recruitment", "support")], 
                          by.x = c("gwgroupid2", "country_name", "year"),
                          by.y = c("gwgroupid", "statename", "year"),
                          all.x = TRUE)
col_names <- names(prio_grid_ethnic)[34:36]
new_col_names <- paste0(col_names, "6")
names(prio_grid_ethnic)[34:36] <- new_col_names

### Create new binary variable indicating if any ethnic settlement in a given
### PRIO-Grid has any evidence for claim, has any recruitment, or has large support

# Create binary variable for presence of an ethnic settlement in a PRIO-Grid that
# has a direct evidence for claim
prio_grid_ethnic$claim <- ifelse(
  (prio_grid_ethnic$overlapping == 1 & prio_grid_ethnic$claim1 == 1) |
    (prio_grid_ethnic$overlapping == 2 & (prio_grid_ethnic$claim1 == 1 | prio_grid_ethnic$claim2 == 1)) |
    (prio_grid_ethnic$overlapping == 3 & (prio_grid_ethnic$claim1 == 1 | prio_grid_ethnic$claim2 == 1 | prio_grid_ethnic$claim3 == 1)) |
    (prio_grid_ethnic$overlapping == 4 & (prio_grid_ethnic$claim1 == 1 | prio_grid_ethnic$claim2 == 1 | prio_grid_ethnic$claim3 == 1 | prio_grid_ethnic$claim4 == 1)) |
    (prio_grid_ethnic$overlapping == 6 & (prio_grid_ethnic$claim1 == 1 | prio_grid_ethnic$claim2 == 1 | prio_grid_ethnic$claim3 == 1 | prio_grid_ethnic$claim4 == 1 | prio_grid_ethnic$claim6 == 1)),
  1, 0)

# Create binary variable for presence of an ethnic settlement in a PRIO-Grid that
# has recruitment
prio_grid_ethnic$recruitment <- ifelse(
  (prio_grid_ethnic$overlapping == 1 & prio_grid_ethnic$recruitment1 == 1) |
    (prio_grid_ethnic$overlapping == 2 & (prio_grid_ethnic$recruitment1 == 1 | prio_grid_ethnic$recruitment2 == 1)) |
    (prio_grid_ethnic$overlapping == 3 & (prio_grid_ethnic$recruitment1 == 1 | prio_grid_ethnic$recruitment2 == 1 | prio_grid_ethnic$recruitment3 == 1)) |
    (prio_grid_ethnic$overlapping == 4 & (prio_grid_ethnic$recruitment1 == 1 | prio_grid_ethnic$recruitment2 == 1 | prio_grid_ethnic$recruitment3 == 1 | prio_grid_ethnic$recruitment4 == 1)) |
    (prio_grid_ethnic$overlapping == 6 & (prio_grid_ethnic$recruitment1 == 1 | prio_grid_ethnic$recruitment2 == 1 | prio_grid_ethnic$recruitment3 == 1 | prio_grid_ethnic$recruitment4 == 1 | prio_grid_ethnic$recruitment6 == 1)),
  1, 0)

# Create binary variable for presence of an ethnic settlement in a PRIO-Grid that
# has support
prio_grid_ethnic$support <- ifelse(
  (prio_grid_ethnic$overlapping == 1 & prio_grid_ethnic$support1 == 1) |
    (prio_grid_ethnic$overlapping == 2 & (prio_grid_ethnic$support1 == 1 | prio_grid_ethnic$support2 == 1)) |
    (prio_grid_ethnic$overlapping == 3 & (prio_grid_ethnic$support1 == 1 | prio_grid_ethnic$support2 == 1 | prio_grid_ethnic$support3 == 1)) |
    (prio_grid_ethnic$overlapping == 4 & (prio_grid_ethnic$support1 == 1 | prio_grid_ethnic$support2 == 1 | prio_grid_ethnic$support3 == 1 | prio_grid_ethnic$support4 == 1)) |
    (prio_grid_ethnic$overlapping == 6 & (prio_grid_ethnic$support1 == 1 | prio_grid_ethnic$support2 == 1 | prio_grid_ethnic$support3 == 1 | prio_grid_ethnic$support4 == 1 | prio_grid_ethnic$support6 == 1)),
  1, 0)

prio_grid_ethnic <- subset(prio_grid_ethnic, select = -c(claim1, claim2, claim3, claim4, claim6))
prio_grid_ethnic <- subset(prio_grid_ethnic, select = -c(recruitment1, recruitment2, recruitment3, recruitment4, recruitment6))
prio_grid_ethnic <- subset(prio_grid_ethnic, select = -c(support1, support2, support3, support4, support6))

save(prio_grid_ethnic, file = "prio_grid_ethnic_epr.RData")



########## INCORPORATING DATASET ON NIGHTTIME LIGHTS (SUM) ##########

# Extract bounding box from data.africa and apply it to nighttime_lights_1992_2008
# since nighttime_lights_1992_2008 applies to all continents
bbox <- st_bbox(data.africa)
nighttime_lights_1992_2008_cropped <- crop(nighttime_lights_1992_2008, bbox)

# Calcuate total sum of nighttime lights from 1992 to 2008 for each PRIO-Grid cell
nighttime_lights_1992_2008_sum <- MODIStsp_extract(nighttime_lights_1992_2008_cropped,
                                                   pgm_data_geometry,
                                                   as.Date("1992-01-01"),
                                                   as.Date("2008-01-01"), FUN = 'sum', 
                                                   na.rm = T)

# Transform nighttime_lights_1992_2008_sum to a dataframe
nighttime_lights_1992_2008_sum.df <- fortify(nighttime_lights_1992_2008_sum)

# Pivot dataframe longer, with columns representing year and all PRIO-Grid cells
nighttime_lights_1992_2008_sum.df <- nighttime_lights_1992_2008_sum.df %>%
  pivot_longer(cols=2:10660,names_to = "pg_id",values_to = "NL_sum")

# Subset pgm_data for only 'lat', 'long', 'year', and 'country_name' from
# 1992 to 2008 -- pgm_data_1992_2008
pgm_data_1992_2008 <- pgm_data %>% dplyr::select(lat, long, year, country_name)
pgm_data_1992_2008 <- subset(pgm_data_1992_2008, year >= 1992 & year <= 2008)
pgm_data_1992_2008 <- unique(pgm_data_1992_2008)

# Assign 'pg_id' to 'lat' and 'long' in pgm_data_1992_2008
pgm_data_1992_2008_pg_id <- merge(pgm_data_1992_2008, pgm_subset, by = c("lat", "long"), all.x = TRUE)

# Convert 'Index' in nighttime_lights_1992_2008_sum.df to character format
nighttime_lights_1992_2008_sum.df$Index <- as.character(nighttime_lights_1992_2008_sum.df$Index)

# Format 'Index' for only a year value
nighttime_lights_1992_2008_sum.df$Index <- format(as.Date(nighttime_lights_1992_2008_sum.df$Index), "%Y")
names(nighttime_lights_1992_2008_sum.df)[names(nighttime_lights_1992_2008_sum.df) == "Index"] <- "year"

# Conform column types for subsequent merge
nighttime_lights_1992_2008_sum.df$year <- as.integer(nighttime_lights_1992_2008_sum.df$year)
nighttime_lights_1992_2008_sum.df$pg_id <- as.integer(nighttime_lights_1992_2008_sum.df$pg_id)

# Perform the merge 
pgm_data_1992_2008_nighttime_light <- merge(pgm_data_1992_2008_pg_id, nighttime_lights_1992_2008_sum.df, by = c("year", "pg_id"), all.x = TRUE)
pgm_data_1992_2008_nighttime_light <- subset(pgm_data_1992_2008_nighttime_light, select = -country_name.y)
names(pgm_data_1992_2008_nighttime_light)[names(pgm_data_1992_2008_nighttime_light) == "country_name.x"] <- "country_name"


# Extract bounding box from data.africa and apply it to nighttime_lights_2009_2011
# since nighttime_lights_2009_2011 applies to all continents
bbox <- st_bbox(data.africa)
nighttime_lights_2009_2011_cropped <- crop(nighttime_lights_2009_2011, bbox)

# Calcuate total sum of nighttime lights from 2009 to 2011 for each PRIO-Grid cell
nighttime_lights_2009_2011_sum <- MODIStsp_extract(nighttime_lights_2009_2011_cropped,
                                                   pgm_data_geometry,
                                                   as.Date("2009-01-01"),
                                                   as.Date("2011-01-01"), FUN = 'sum', 
                                                   na.rm = T)

# Transform nighttime_lights_2009_2011_sum to a dataframe
nighttime_lights_2009_2011_sum.df <- fortify(nighttime_lights_2009_2011_sum)

### Pivot dataframe longer, with columns representing year and all PRIO-Grid cells
nighttime_lights_2009_2011_sum.df <- nighttime_lights_2009_2011_sum.df %>%
  pivot_longer(cols=2:10660,names_to = "pg_id",values_to = "NL_sum")

# Subset pgm_data for only 'lat', 'long', 'year', and 'country_name' from
# 2009 to 2011 -- pgm_data_2009_2011
pgm_data_2009_2011 <- pgm_data %>% dplyr::select(lat, long, year, country_name)
pgm_data_2009_2011 <- subset(pgm_data_2009_2011, year >= 2009 & year <= 2011)
pgm_data_2009_2011 <- unique(pgm_data_2009_2011)

# Assign 'pg_id' to 'lat' and 'long' in pgm_data_2009_2011
pgm_data_2009_2011_pg_id <- merge(pgm_data_2009_2011, pgm_subset, by = c("lat", "long"), all.x = TRUE)

# Convert 'Index' in nighttime_lights_2009_2011_sum.df to character format
nighttime_lights_2009_2011_sum.df$Index <- as.character(nighttime_lights_2009_2011_sum.df$Index)

# Format 'Index' for only a year value
nighttime_lights_2009_2011_sum.df$Index <- format(as.Date(nighttime_lights_2009_2011_sum.df$Index), "%Y")
names(nighttime_lights_2009_2011_sum.df)[names(nighttime_lights_2009_2011_sum.df) == "Index"] <- "year"

# Conform column types for subsequent merge
nighttime_lights_2009_2011_sum.df$year <- as.integer(nighttime_lights_2009_2011_sum.df$year)
nighttime_lights_2009_2011_sum.df$pg_id <- as.integer(nighttime_lights_2009_2011_sum.df$pg_id)

### Perform the merge 
pgm_data_2009_2011_nighttime_light <- merge(pgm_data_2009_2011_pg_id, nighttime_lights_2009_2011_sum.df, by = c("year", "pg_id"), all.x = TRUE)
pgm_data_2009_2011_nighttime_light <- subset(pgm_data_2009_2011_nighttime_light, select = -country_name.y)
names(pgm_data_2009_2011_nighttime_light)[names(pgm_data_2009_2011_nighttime_light) == "country_name.x"] <- "country_name"


# Extract bounding box from data.africa and apply it to nighttime_lights_2012_2018
# since nighttime_lights_2012_2018 applies to all continents
bbox <- st_bbox(data.africa)
nighttime_lights_2012_2018_cropped <- crop(nighttime_lights_2012_2018, bbox)

# Calcuate total sum of nighttime lights from 2012 to 2018 for each PRIO-Grid cell
nighttime_lights_2012_2018_sum <- MODIStsp_extract(nighttime_lights_2012_2018_cropped,
                                                   pgm_data_geometry,
                                                   as.Date("2012-01-01"),
                                                   as.Date("2018-01-01"), FUN = 'sum', 
                                                   na.rm = T)

# Transform nighttime_lights_2012_2018_sum to a dataframe
nighttime_lights_2012_2018_sum.df <- fortify(nighttime_lights_2012_2018_sum)

# Pivot dataframe longer, with columns representing year and all PRIO-Grid cells
nighttime_lights_2012_2018_sum.df <- nighttime_lights_2012_2018_sum.df %>%
  pivot_longer(cols=2:10660,names_to = "pg_id",values_to = "NL_sum")

# Subset pgm_data for only 'lat', 'long', 'year', and 'country_name' from
# 2012 to 2018 -- pgm_data_2012_2018
pgm_data_2012_2018 <- pgm_data %>% dplyr::select(lat, long, year, country_name)
pgm_data_2012_2018 <- subset(pgm_data_2012_2018, year >= 2012 & year <= 2018)
pgm_data_2012_2018 <- unique(pgm_data_2012_2018)

# Assign 'pg_id' to 'lat' and 'long' in pgm_data_2012_2018
pgm_data_2012_2018_pg_id <- merge(pgm_data_2012_2018, pgm_subset, by = c("lat", "long"), all.x = TRUE)

# Convert 'Index' in nighttime_lights_2012_2018_sum.df to character format
nighttime_lights_2012_2018_sum.df$Index <- as.character(nighttime_lights_2012_2018_sum.df$Index)

# Format 'Index' for only a year value
nighttime_lights_2012_2018_sum.df$Index <- format(as.Date(nighttime_lights_2012_2018_sum.df$Index), "%Y")
names(nighttime_lights_2012_2018_sum.df)[names(nighttime_lights_2012_2018_sum.df) == "Index"] <- "year"

# Conform column types for subsequent merge
nighttime_lights_2012_2018_sum.df$year <- as.integer(nighttime_lights_2012_2018_sum.df$year)
nighttime_lights_2012_2018_sum.df$pg_id <- as.integer(nighttime_lights_2012_2018_sum.df$pg_id)

# Perform the merge 
pgm_data_2012_2018_nighttime_light <- merge(pgm_data_2012_2018_pg_id, nighttime_lights_2012_2018_sum.df, by = c("year", "pg_id"), all.x = TRUE)
pgm_data_2012_2018_nighttime_light <- subset(pgm_data_2012_2018_nighttime_light, select = -country_name.y)
names(pgm_data_2012_2018_nighttime_light)[names(pgm_data_2012_2018_nighttime_light) == "country_name.x"] <- "country_name"


# Rbind the 1992-2008, 2009-2011, and 2012-2018 dataframes for final
# 'pgm_data_nighttime_light_sum' dataframe
pgm_data_nighttime_light_sum <- rbind(pgm_data_1992_2008_nighttime_light, pgm_data_2009_2011_nighttime_light, pgm_data_2012_2018_nighttime_light)
save(pgm_data_nighttime_light_sum, file = "pgm_data_nighttime_light_sum.RData")



########## INCORPORATING DATASET ON NIGHTTIME LIGHTS (MEAN) ##########

# Extract bounding box from data.africa and apply it to nighttime_lights_1992_2008
# since nighttime_lights_1992_2008 applies to all continents
bbox <- st_bbox(data.africa)
nighttime_lights_1992_2008_cropped <- crop(nighttime_lights_1992_2008, bbox)

# Calcuate mean of nighttime lights from 1992 to 2008 for each PRIO-Grid cell
nighttime_lights_1992_2008_mean <- MODIStsp_extract(nighttime_lights_1992_2008_cropped,
                                                   pgm_data_geometry,
                                                   as.Date("1992-01-01"),
                                                   as.Date("2008-01-01"), FUN = 'mean', 
                                                   na.rm = T)

# Transform nighttime_lights_1992_2008_mean to a dataframe
nighttime_lights_1992_2008_mean.df <- fortify(nighttime_lights_1992_2008_mean)

# Pivot dataframe longer, with columns representing year and all PRIO-Grid cells
nighttime_lights_1992_2008_mean.df <- nighttime_lights_1992_2008_mean.df %>%
  pivot_longer(cols=2:10660,names_to = "pg_id",values_to = "NL_mean")

# Subset pgm_data for only 'lat', 'long', 'year', and 'country_name' from
# 1992 to 2008 -- pgm_data_1992_2008
pgm_data_1992_2008 <- pgm_data %>% dplyr::select(lat, long, year, country_name)
pgm_data_1992_2008 <- subset(pgm_data_1992_2008, year >= 1992 & year <= 2008)
pgm_data_1992_2008 <- unique(pgm_data_1992_2008)

# Assign 'pg_id' to 'lat' and 'long' in pgm_data_1992_2008
pgm_data_1992_2008_pg_id <- merge(pgm_data_1992_2008, pgm_subset, by = c("lat", "long"), all.x = TRUE)

# Convert 'Index' in nighttime_lights_1992_2008_mean.df to character format
nighttime_lights_1992_2008_mean.df$Index <- as.character(nighttime_lights_1992_2008_mean.df$Index)

# Format 'Index' for only a year value
nighttime_lights_1992_2008_mean.df$Index <- format(as.Date(nighttime_lights_1992_2008_mean.df$Index), "%Y")
names(nighttime_lights_1992_2008_mean.df)[names(nighttime_lights_1992_2008_mean.df) == "Index"] <- "year"

# Conform column types for subsequent merge
nighttime_lights_1992_2008_mean.df$year <- as.integer(nighttime_lights_1992_2008_mean.df$year)
nighttime_lights_1992_2008_mean.df$pg_id <- as.integer(nighttime_lights_1992_2008_mean.df$pg_id)

# Perform the merge 
pgm_data_1992_2008_nighttime_light <- merge(pgm_data_1992_2008_pg_id, nighttime_lights_1992_2008_mean.df, by = c("year", "pg_id"), all.x = TRUE)
pgm_data_1992_2008_nighttime_light <- subset(pgm_data_1992_2008_nighttime_light, select = -country_name.y)
names(pgm_data_1992_2008_nighttime_light)[names(pgm_data_1992_2008_nighttime_light) == "country_name.x"] <- "country_name"


# Extract bounding box from data.africa and apply it to nighttime_lights_2009_2011
# since nighttime_lights_2009_2011 applies to all continents
bbox <- st_bbox(data.africa)
nighttime_lights_2009_2011_cropped <- crop(nighttime_lights_2009_2011, bbox)

# Calcuate mean of nighttime lights from 2009 to 2011 for each PRIO-Grid cell
nighttime_lights_2009_2011_mean <- MODIStsp_extract(nighttime_lights_2009_2011_cropped,
                                                   pgm_data_geometry,
                                                   as.Date("2009-01-01"),
                                                   as.Date("2011-01-01"), FUN = 'mean', 
                                                   na.rm = T)

# Transform nighttime_lights_2009_2011_mean to a dataframe
nighttime_lights_2009_2011_mean.df <- fortify(nighttime_lights_2009_2011_mean)

### Pivot dataframe longer, with columns representing year and all PRIO-Grid cells
nighttime_lights_2009_2011_mean.df <- nighttime_lights_2009_2011_mean.df %>%
  pivot_longer(cols=2:10660,names_to = "pg_id",values_to = "NL_mean")

# Subset pgm_data for only 'lat', 'long', 'year', and 'country_name' from
# 2009 to 2011 -- pgm_data_2009_2011
pgm_data_2009_2011 <- pgm_data %>% dplyr::select(lat, long, year, country_name)
pgm_data_2009_2011 <- subset(pgm_data_2009_2011, year >= 2009 & year <= 2011)
pgm_data_2009_2011 <- unique(pgm_data_2009_2011)

# Assign 'pg_id' to 'lat' and 'long' in pgm_data_2009_2011
pgm_data_2009_2011_pg_id <- merge(pgm_data_2009_2011, pgm_subset, by = c("lat", "long"), all.x = TRUE)

# Convert 'Index' in nighttime_lights_2009_2011_mean.df to character format
nighttime_lights_2009_2011_mean.df$Index <- as.character(nighttime_lights_2009_2011_mean.df$Index)

# Format 'Index' for only a year value
nighttime_lights_2009_2011_mean.df$Index <- format(as.Date(nighttime_lights_2009_2011_mean.df$Index), "%Y")
names(nighttime_lights_2009_2011_mean.df)[names(nighttime_lights_2009_2011_mean.df) == "Index"] <- "year"

# Conform column types for subsequent merge
nighttime_lights_2009_2011_mean.df$year <- as.integer(nighttime_lights_2009_2011_mean.df$year)
nighttime_lights_2009_2011_mean.df$pg_id <- as.integer(nighttime_lights_2009_2011_mean.df$pg_id)

### Perform the merge 
pgm_data_2009_2011_nighttime_light <- merge(pgm_data_2009_2011_pg_id, nighttime_lights_2009_2011_mean.df, by = c("year", "pg_id"), all.x = TRUE)
pgm_data_2009_2011_nighttime_light <- subset(pgm_data_2009_2011_nighttime_light, select = -country_name.y)
names(pgm_data_2009_2011_nighttime_light)[names(pgm_data_2009_2011_nighttime_light) == "country_name.x"] <- "country_name"


# Extract bounding box from data.africa and apply it to nighttime_lights_2012_2018
# since nighttime_lights_2012_2018 applies to all continents
bbox <- st_bbox(data.africa)
nighttime_lights_2012_2018_cropped <- crop(nighttime_lights_2012_2018, bbox)

# Calcuate mean of nighttime lights from 2012 to 2018 for each PRIO-Grid cell
nighttime_lights_2012_2018_mean <- MODIStsp_extract(nighttime_lights_2012_2018_cropped,
                                                   pgm_data_geometry,
                                                   as.Date("2012-01-01"),
                                                   as.Date("2018-01-01"), FUN = 'mean', 
                                                   na.rm = T)

# Transform nighttime_lights_2012_2018_mean to a dataframe
nighttime_lights_2012_2018_mean.df <- fortify(nighttime_lights_2012_2018_mean)

# Pivot dataframe longer, with columns representing year and all PRIO-Grid cells
nighttime_lights_2012_2018_mean.df <- nighttime_lights_2012_2018_mean.df %>%
  pivot_longer(cols=2:10660,names_to = "pg_id",values_to = "NL_mean")

# Subset pgm_data for only 'lat', 'long', 'year', and 'country_name' from
# 2012 to 2018 -- pgm_data_2012_2018
pgm_data_2012_2018 <- pgm_data %>% dplyr::select(lat, long, year, country_name)
pgm_data_2012_2018 <- subset(pgm_data_2012_2018, year >= 2012 & year <= 2018)
pgm_data_2012_2018 <- unique(pgm_data_2012_2018)

# Assign 'pg_id' to 'lat' and 'long' in pgm_data_2012_2018
pgm_data_2012_2018_pg_id <- merge(pgm_data_2012_2018, pgm_subset, by = c("lat", "long"), all.x = TRUE)

# Convert 'Index' in nighttime_lights_2012_2018_mean.df to character format
nighttime_lights_2012_2018_mean.df$Index <- as.character(nighttime_lights_2012_2018_mean.df$Index)

# Format 'Index' for only a year value
nighttime_lights_2012_2018_mean.df$Index <- format(as.Date(nighttime_lights_2012_2018_mean.df$Index), "%Y")
names(nighttime_lights_2012_2018_mean.df)[names(nighttime_lights_2012_2018_mean.df) == "Index"] <- "year"

# Conform column types for subsequent merge
nighttime_lights_2012_2018_mean.df$year <- as.integer(nighttime_lights_2012_2018_mean.df$year)
nighttime_lights_2012_2018_mean.df$pg_id <- as.integer(nighttime_lights_2012_2018_mean.df$pg_id)

# Perform the merge 
pgm_data_2012_2018_nighttime_light <- merge(pgm_data_2012_2018_pg_id, nighttime_lights_2012_2018_mean.df, by = c("year", "pg_id"), all.x = TRUE)
pgm_data_2012_2018_nighttime_light <- subset(pgm_data_2012_2018_nighttime_light, select = -country_name.y)
names(pgm_data_2012_2018_nighttime_light)[names(pgm_data_2012_2018_nighttime_light) == "country_name.x"] <- "country_name"


# Rbind the 1992-2008, 2009-2011, and 2012-2018 dataframes for final
# 'pgm_data_nighttime_light_mean' dataframe
pgm_data_nighttime_light_mean <- rbind(pgm_data_1992_2008_nighttime_light, pgm_data_2009_2011_nighttime_light, pgm_data_2012_2018_nighttime_light)
save(pgm_data_nighttime_light_mean, file = "pgm_data_nighttime_light_mean.RData")



########## INCORPORATING DATASET ON NIGHTTIME LIGHTS (MAX) ##########

# Extract bounding box from data.africa and apply it to nighttime_lights_1992_2008
# since nighttime_lights_1992_2008 applies to all continents
bbox <- st_bbox(data.africa)
nighttime_lights_1992_2008_cropped <- crop(nighttime_lights_1992_2008, bbox)

# Calcuate max of nighttime lights from 1992 to 2008 for each PRIO-Grid cell
nighttime_lights_1992_2008_max <- MODIStsp_extract(nighttime_lights_1992_2008_cropped,
                                                    pgm_data_geometry,
                                                    as.Date("1992-01-01"),
                                                    as.Date("2008-01-01"), FUN = 'max', 
                                                    na.rm = T)

# Transform nighttime_lights_1992_2008_max to a dataframe
nighttime_lights_1992_2008_max.df <- fortify(nighttime_lights_1992_2008_max)

# Pivot dataframe longer, with columns representing year and all PRIO-Grid cells
nighttime_lights_1992_2008_max.df <- nighttime_lights_1992_2008_max.df %>%
  pivot_longer(cols=2:10660,names_to = "pg_id",values_to = "NL_max")

# Subset pgm_data for only 'lat', 'long', 'year', and 'country_name' from
# 1992 to 2008 -- pgm_data_1992_2008
pgm_data_1992_2008 <- pgm_data %>% dplyr::select(lat, long, year, country_name)
pgm_data_1992_2008 <- subset(pgm_data_1992_2008, year >= 1992 & year <= 2008)
pgm_data_1992_2008 <- unique(pgm_data_1992_2008)

# Assign 'pg_id' to 'lat' and 'long' in pgm_data_1992_2008
pgm_data_1992_2008_pg_id <- merge(pgm_data_1992_2008, pgm_subset, by = c("lat", "long"), all.x = TRUE)

# Convert 'Index' in nighttime_lights_1992_2008_max.df to character format
nighttime_lights_1992_2008_max.df$Index <- as.character(nighttime_lights_1992_2008_max.df$Index)

# Format 'Index' for only a year value
nighttime_lights_1992_2008_max.df$Index <- format(as.Date(nighttime_lights_1992_2008_max.df$Index), "%Y")
names(nighttime_lights_1992_2008_max.df)[names(nighttime_lights_1992_2008_max.df) == "Index"] <- "year"

# Conform column types for subsequent merge
nighttime_lights_1992_2008_max.df$year <- as.integer(nighttime_lights_1992_2008_max.df$year)
nighttime_lights_1992_2008_max.df$pg_id <- as.integer(nighttime_lights_1992_2008_max.df$pg_id)

# Perform the merge 
pgm_data_1992_2008_nighttime_light <- merge(pgm_data_1992_2008_pg_id, nighttime_lights_1992_2008_max.df, by = c("year", "pg_id"), all.x = TRUE)
pgm_data_1992_2008_nighttime_light <- subset(pgm_data_1992_2008_nighttime_light, select = -country_name.y)
names(pgm_data_1992_2008_nighttime_light)[names(pgm_data_1992_2008_nighttime_light) == "country_name.x"] <- "country_name"


# Extract bounding box from data.africa and apply it to nighttime_lights_2009_2011
# since nighttime_lights_2009_2011 applies to all continents
bbox <- st_bbox(data.africa)
nighttime_lights_2009_2011_cropped <- crop(nighttime_lights_2009_2011, bbox)

# Calcuate max of nighttime lights from 2009 to 2011 for each PRIO-Grid cell
nighttime_lights_2009_2011_max <- MODIStsp_extract(nighttime_lights_2009_2011_cropped,
                                                    pgm_data_geometry,
                                                    as.Date("2009-01-01"),
                                                    as.Date("2011-01-01"), FUN = 'max', 
                                                    na.rm = T)

# Transform nighttime_lights_2009_2011_mean to a dataframe
nighttime_lights_2009_2011_max.df <- fortify(nighttime_lights_2009_2011_max)

### Pivot dataframe longer, with columns representing year and all PRIO-Grid cells
nighttime_lights_2009_2011_max.df <- nighttime_lights_2009_2011_max.df %>%
  pivot_longer(cols=2:10660,names_to = "pg_id",values_to = "NL_max")

# Subset pgm_data for only 'lat', 'long', 'year', and 'country_name' from
# 2009 to 2011 -- pgm_data_2009_2011
pgm_data_2009_2011 <- pgm_data %>% dplyr::select(lat, long, year, country_name)
pgm_data_2009_2011 <- subset(pgm_data_2009_2011, year >= 2009 & year <= 2011)
pgm_data_2009_2011 <- unique(pgm_data_2009_2011)

# Assign 'pg_id' to 'lat' and 'long' in pgm_data_2009_2011
pgm_data_2009_2011_pg_id <- merge(pgm_data_2009_2011, pgm_subset, by = c("lat", "long"), all.x = TRUE)

# Convert 'Index' in nighttime_lights_2009_2011_max.df to character format
nighttime_lights_2009_2011_max.df$Index <- as.character(nighttime_lights_2009_2011_max.df$Index)

# Format 'Index' for only a year value
nighttime_lights_2009_2011_max.df$Index <- format(as.Date(nighttime_lights_2009_2011_max.df$Index), "%Y")
names(nighttime_lights_2009_2011_max.df)[names(nighttime_lights_2009_2011_max.df) == "Index"] <- "year"

# Conform column types for subsequent merge
nighttime_lights_2009_2011_max.df$year <- as.integer(nighttime_lights_2009_2011_max.df$year)
nighttime_lights_2009_2011_max.df$pg_id <- as.integer(nighttime_lights_2009_2011_max.df$pg_id)

### Perform the merge 
pgm_data_2009_2011_nighttime_light <- merge(pgm_data_2009_2011_pg_id, nighttime_lights_2009_2011_max.df, by = c("year", "pg_id"), all.x = TRUE)
pgm_data_2009_2011_nighttime_light <- subset(pgm_data_2009_2011_nighttime_light, select = -country_name.y)
names(pgm_data_2009_2011_nighttime_light)[names(pgm_data_2009_2011_nighttime_light) == "country_name.x"] <- "country_name"


# Extract bounding box from data.africa and apply it to nighttime_lights_2012_2018
# since nighttime_lights_2012_2018 applies to all continents
bbox <- st_bbox(data.africa)
nighttime_lights_2012_2018_cropped <- crop(nighttime_lights_2012_2018, bbox)

# Calcuate max of nighttime lights from 2012 to 2018 for each PRIO-Grid cell
nighttime_lights_2012_2018_max <- MODIStsp_extract(nighttime_lights_2012_2018_cropped,
                                                    pgm_data_geometry,
                                                    as.Date("2012-01-01"),
                                                    as.Date("2018-01-01"), FUN = 'max', 
                                                    na.rm = T)

# Transform nighttime_lights_2012_2018_max to a dataframe
nighttime_lights_2012_2018_max.df <- fortify(nighttime_lights_2012_2018_max)

# Pivot dataframe longer, with columns representing year and all PRIO-Grid cells
nighttime_lights_2012_2018_max.df <- nighttime_lights_2012_2018_max.df %>%
  pivot_longer(cols=2:10660,names_to = "pg_id",values_to = "NL_max")

# Subset pgm_data for only 'lat', 'long', 'year', and 'country_name' from
# 2012 to 2018 -- pgm_data_2012_2018
pgm_data_2012_2018 <- pgm_data %>% dplyr::select(lat, long, year, country_name)
pgm_data_2012_2018 <- subset(pgm_data_2012_2018, year >= 2012 & year <= 2018)
pgm_data_2012_2018 <- unique(pgm_data_2012_2018)

# Assign 'pg_id' to 'lat' and 'long' in pgm_data_2012_2018
pgm_data_2012_2018_pg_id <- merge(pgm_data_2012_2018, pgm_subset, by = c("lat", "long"), all.x = TRUE)

# Convert 'Index' in nighttime_lights_2012_2018_max.df to character format
nighttime_lights_2012_2018_max.df$Index <- as.character(nighttime_lights_2012_2018_max.df$Index)

# Format 'Index' for only a year value
nighttime_lights_2012_2018_max.df$Index <- format(as.Date(nighttime_lights_2012_2018_max.df$Index), "%Y")
names(nighttime_lights_2012_2018_max.df)[names(nighttime_lights_2012_2018_max.df) == "Index"] <- "year"

# Conform column types for subsequent merge
nighttime_lights_2012_2018_max.df$year <- as.integer(nighttime_lights_2012_2018_max.df$year)
nighttime_lights_2012_2018_max.df$pg_id <- as.integer(nighttime_lights_2012_2018_max.df$pg_id)

# Perform the merge 
pgm_data_2012_2018_nighttime_light <- merge(pgm_data_2012_2018_pg_id, nighttime_lights_2012_2018_max.df, by = c("year", "pg_id"), all.x = TRUE)
pgm_data_2012_2018_nighttime_light <- subset(pgm_data_2012_2018_nighttime_light, select = -country_name.y)
names(pgm_data_2012_2018_nighttime_light)[names(pgm_data_2012_2018_nighttime_light) == "country_name.x"] <- "country_name"


# Rbind the 1992-2008, 2009-2011, and 2012-2018 dataframes for final
# 'pgm_data_nighttime_light_max' dataframe
pgm_data_nighttime_light_max <- rbind(pgm_data_1992_2008_nighttime_light, pgm_data_2009_2011_nighttime_light, pgm_data_2012_2018_nighttime_light)
save(pgm_data_nighttime_light_max, file = "pgm_data_nighttime_light_max.RData")

# Cbind all nighttime light information into one dataframe
NL_mean <- data.frame(NL_mean = pgm_data_nighttime_light_mean$NL_mean)
pgm_data_nighttime_light <- cbind(pgm_data_nighttime_light_sum, NL_mean)
NL_max <- data.frame(NL_max = pgm_data_nighttime_light_max$NL_max)
pgm_data_nighttime_light <- cbind(pgm_data_nighttime_light, NL_max)
save(pgm_data_nighttime_light, file = "pgm_data_nighttime_light.RData")


########## INCORPORATING POPULATION DATA ##########

# Extract bounding box from data.africa and apply it to pop_2000_2020
# since pop_2000_2020 applies to all continents
bbox <- st_bbox(data.africa)
pop_2000_2020_cropped <- crop(pop_2000_2020, bbox)

# Calculate sum of population from 2000 to 2020 for each PRIO-Grid cell
pop_2000_2020_sum <- MODIStsp_extract(pop_2000_2020_cropped,
                                                   pgm_data_geometry,
                                                   as.Date("2000-01-01"),
                                                   as.Date("2020-01-01"), FUN = 'sum', 
                                                   na.rm = T)

# Transform pop_2000_2020_sum to a dataframe
pop_2000_2020_sum.df <- fortify(pop_2000_2020_sum)

# Pivot dataframe longer, with columns representing year and all PRIO-Grid cells
pop_2000_2020_sum.df <- pop_2000_2020_sum.df %>%
  pivot_longer(cols=2:10660,names_to = "pg_id",values_to = "pop_sum")

# Assign 'pg_id' to 'lat' and 'long' in pgm_data
pgm_data_pg_id <- merge(pgm_data, pgm_subset, by = c("lat", "long"), all.x = TRUE)

# Convert 'Index' in pop_2000_2020_sum.df to character format
pop_2000_2020_sum.df$Index <- as.character(pop_2000_2020_sum.df$Index)

# Format 'Index' for only a year value
pop_2000_2020_sum.df$Index <- format(as.Date(pop_2000_2020_sum.df$Index), "%Y")
names(pop_2000_2020_sum.df)[names(pop_2000_2020_sum.df) == "Index"] <- "year"

# Conform column types for subsequent merge
pop_2000_2020_sum.df$year <- as.integer(pop_2000_2020_sum.df$year)
pop_2000_2020_sum.df$pg_id <- as.integer(pop_2000_2020_sum.df$pg_id)

# Perform the merge 
pgm_data_pop_sum <- merge(pgm_data_pg_id, pop_2000_2020_sum.df.df, by = c("year", "pg_id"), all.x = TRUE)
pgm_data_pop_sum <- subset(pgm_data_pop_sum, select = -country_name.y)
names(pgm_data_pop_sum)[names(pgm_data_pop_sum) == "country_name.x"] <- "country_name"
save(pgm_data_pop_sum, file = "pgm_data_pop_sum.RData")



########## INCORPORATING TEMPERATURE AND PRECIPITATION DATA ##########

### Temperature ###

# Following code adapted from Schon and Koren
# Extract the values of the r_brick_temp raster brick object at the location of
# the pgm_data_geometry, which contains the geometries of the PRIO-Grid cells
points_t <- raster::extract(r_brick_temp,
                           pgm_data_geometry, na.rm=F, df=T)

# Transform points_t from wide to long and rename column names
points_t_long <- points_t %>%
  pivot_longer(layer.1:layer.1440,
               names_to = "yearmonth",
               values_to = "mean_temp_celsius")

# Extract characters in 'yearmonth' starting from the 7th character to create
# new 'yearmonth', which has a format of YYYY-MM
points_t_long$yearmonth <- as.numeric(str_sub(points_t_long$yearmonth,
                                             7,
                                             str_length(points_t_long$yearmonth)))

# Create months_df with a 'ym' column, which is a sequence of dates starting 
# from "1901-01-01" and has a length of 1440 -- the number of layers in r_brick,
# and a 'yearmonth' column, which is a sequence of integers from 1 to 1440
months_df <- data.frame(ym=seq(as.Date("1901/1/1"), 
                              by = "month", 
                              length.out = 1440),
                       yearmonth=1:1440)

# Merge points_t_long and months_df
points_t_long <- merge(points_t_long, months_df,
                      by="yearmonth")

# Generate 'year' and 'month' columns
points_t_long$year <- year(points_t_long$ym)
points_t_long$month <- month(points_t_long$ym) 

# Create a new variable, 'temp.anomaly', which calculates the rolling mean within
# a window size of 30 years centered around each observation
points_t_long <- points_t_long %>%
  group_by(month,ID) %>%
  mutate(
    temp_anomaly = roll_scale(mean_temp_celsius,width=30,center=T,
                              scale=T,min_obs=30)
  ) %>%
  ungroup()


### Precipitation ###

# Extract the values of the r_brick_precip raster brick object at the location of
# the pgm_data_geometry, which contains the geometries of the PRIO-Grid cells
points_p <- raster::extract(r_brick_precip,
                           pgm_data_geometry,na.rm=F,df=T)

# Transform points_p from wide to long and rename column names
points_p_long <- points_p %>%
  pivot_longer(layer.1:layer.1440,
               names_to = "yearmonth",
               values_to = "mean_precip")

# Extract characters in 'yearmonth' starting from the 7th character to create
# new 'yearmonth', which has a format of YYYY-MM
points_p_long$yearmonth <- as.numeric(str_sub(points_p_long$yearmonth,
                                             7,
                                             str_length(points_p_long$yearmonth)))

# Merge points_p_long and months_df
points_p_long <- merge(points_p_long, months_df,
                      by="yearmonth")

# Generate 'year' and 'month' columns
points_p_long$year <- year(points_p_long$ym)
points_p_long$month <- month(points_p_long$ym) 

# Create a new variable, 'precip.anomaly', which calculates the rolling mean within
# a window size of 30 years centered around each observation
points_p_long <- points_p_long %>%
  group_by(month,ID) %>%
  mutate(
    precip_anomaly = roll_scale(mean_precip,width=30,center=T,
                                scale=T,min_obs=30)
  ) %>%
  ungroup()

# Select only relevant variables
points_t_long <- points_t_long %>% 
  dplyr::select(ID, year, month, mean_temp_celsius, temp_anomaly)

points_p_long <- points_p_long %>%
  dplyr::select(ID, year, month, mean_precip, precip_anomaly)

# Merge points_t_long and points_p_long
points_t_p_long <- merge(points_t_long, points_p_long,
                        by=c("year","month","ID"))
save(points_t_p_long, file = "points_t_p_long.RData")

# Create 'ID' in pgm_data_geometry value for subsequent merge
pgm_data_geometry <- pgm_data_geometry  %>% 
  mutate(ID = row_number())

# Merge on 'ID' to get back 'lat' and 'long' values for subsequent merge
points_t_p_long_sf <- merge(pgm_data_geometry, points_t_p_long,
                           by = "ID", all.x = TRUE)

# Drop 'geometry' column in points_t_p_long_sf
points_t_p_long_sf <- st_drop_geometry(points_t_p_long_sf)

# Temperature and precipitation dataset at the PRIO-Grid cell level
pgm_data_temp_precip <- merge(pgm_data_pop_sum, points_t_p_long_sf, by = c("month", "year", "lat", "long"), all.x = TRUE)
save(pgm_data_temp_precip, file = "pgm_data_temp_precip.RData")



########## INCORPORATING ACLED DATA ##########

# Create an sf object from 'latitude' and 'longitude'
acled_data_sf <- st_as_sf(acled_data, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Keep only observations where a fatality occurs: 'fatalities' != 0
acled_data_sf <- subset(acled_data_sf, fatalities != 0)

# Initialize var 'polygon_id', 'indicator', 'contain', and 'store_date'
acled_data_sf$polygon_id <- list(NA)
acled_data_sf$indicator <- 1:75405
acled_data_sf$contain <- 0
store_date <- acled_data_sf$polygon_id

# Switch spherical geometry off
sf_use_s2(FALSE)

# For each polygon in pgm_data_geometry, use the 'st_contains' function to 
# identify which fatality points in acled_data_sf are contained within the 
# polygon
for (i in 1:nrow(pgm_data_geometry)) {
  
  # Get the current polygon and centroid point
  polygon <- pgm_data_geometry$geometry[i]
  polygon_id = pgm_data_geometry$polygon_id[i]
  
  points <- acled_data_sf$geometry
  
  # Employ 'st_contains'
  a <- unlist(st_contains(polygon, points))
  
  # If a centroid point is contained within the polygon, update the list
  # with the 'polygon_id' of each containing polygon
  if(length(a)!= 0){
    b <- acled_data_sf$indicator[a]
    acled_data_sf$contain[b] <- 1
    for (z in 1:length(b)){
      if(is.na(store_date[b[z]][[1]])[1]){
        store_date[b[z]][[1]] <- c(polygon_id)
      }
      else{
        store_date[b[z]][[1]] <- c(store_date[b[z]][[1]], polygon_id)
      }
    }
  }
}

# At the end of the loop, the final 'acled_data_sf$polygon_id' values are
# stored in 'store_date"
acled_data_sf$polygon_id <- store_date

# Merge acled_data_sf with pgm_data_geometry on 'polygon_id' to get 'lat' and 'long'
# centroid value
acled_data_sf <- merge(acled_data_sf, pgm_data_geometry, by = "polygon_id", all.x = TRUE)

# Create 'month' variable from 'event_date'
acled_data_sf$event_date <- dmy(acled_data_sf$event_date)
acled_data_sf$month <- month(acled_data_sf$event_date)

# Rename to 'acled_count_sb' 'acled_count_ns', 'acled_count_os'
acled_data_sf <- rename(acled_data_sf, acled_count_sb = fatalities_sb)
acled_data_sf <- rename(acled_data_sf, acled_count_ns = fatalities_ns)
acled_data_sf <- rename(acled_data_sf, acled_count_os = fatalities_os)

# Merge on 'year', 'month', 'lat', and 'long' to get on PRIO-Grid level
pgm_data_acled <- merge(pgm_data, acled_data_sf, by = c("lat", "long", "year", "month"), all.x = TRUE)

# Create dummy variable
pgm_data_acled$acled_dummy_sb <- ifelse(!is.na(pgm_data_acled$acled_count_sb) & pgm_data_acled$acled_count_sb != 0, 1, 0)
pgm_data_acled$acled_dummy_ns <- ifelse(!is.na(pgm_data_acled$acled_count_ns) & pgm_data_acled$acled_count_ns != 0, 1, 0)
pgm_data_acled$acled_dummy_os <- ifelse(!is.na(pgm_data_acled$acled_count_os) & pgm_data_acled$acled_count_os != 0, 1, 0)


# Creating new variables that are combinations of GED and ACLED:
# (1) 'fat_dummy_sb': At least either 'ged_dummy_sb' or 'acled_dummy_sb' are equal to 1
# (2) 'fat_best_sb': If either 'ged_dummy_sb' or 'acled_dummy_sb' are 0, then 
#                     take the count value from the variable that is equal to 1.
#                     But, if both 'ged_dummy_sb' or 'acled_dummy_sb' are equal to 1,
#                     then take the average of 'acled_count_sb' and ged_best_sb'
# same holds for os / ns

# (1) sb
pgm_data_acled$fat_dummy_sb <- ifelse(
  pgm_data_acled$ged_dummy_sb == 0 & pgm_data_acled$acled_dummy_sb == 0,
  pgm_data_acled$ged_dummy_sb,
  ifelse(
    pgm_data_acled$ged_dummy_sb == 1 & pgm_data_acled$acled_dummy_sb == 0,
    pgm_data_acled$ged_dummy_sb,
    ifelse(
      pgm_data_acled$ged_dummy_sb == 0 & pgm_data_acled$acled_dummy_sb == 1,
      pgm_data_acled$acled_dummy_sb,
      pgm_data_acled$ged_dummy_sb
    )
  )
)

# (2) sb
pgm_data_acled$fat_best_sb <- ifelse(
  pgm_data_acled$fat_dummy_sb == 0,
  0,
  ifelse(
    pgm_data_acled$fat_dummy_sb == 1,
    ceiling((pgm_data_acled$ged_best_sb + pgm_data_acled$acled_count_sb) / 2),
    0
  )
)

# (1) ns
pgm_data_acled$fat_dummy_ns <- ifelse(
  pgm_data_acled$ged_dummy_ns == 0 & pgm_data_acled$acled_dummy_ns == 0,
  pgm_data_acled$ged_dummy_ns,
  ifelse(
    pgm_data_acled$ged_dummy_ns == 1 & pgm_data_acled$acled_dummy_ns == 0,
    pgm_data_acled$ged_dummy_ns,
    ifelse(
      pgm_data_acled$ged_dummy_ns == 0 & pgm_data_acled$acled_dummy_ns == 1,
      pgm_data_acled$acled_dummy_ns,
      pgm_data_acled$ged_dummy_ns
    )
  )
)

# (2) ns
pgm_data_acled$fat_best_ns <- ifelse(
  pgm_data_acled$fat_dummy_ns == 0,
  0,
  ifelse(
    pgm_data_acled$fat_dummy_ns == 1,
    ceiling((pgm_data_acled$ged_best_ns + pgm_data_acled$acled_count_ns) / 2),
    0
  )
)

# (1) os
pgm_data_acled$fat_dummy_os <- ifelse(
  pgm_data_acled$ged_dummy_os == 0 & pgm_data_acled$acled_dummy_os == 0,
  pgm_data_acled$ged_dummy_os,
  ifelse(
    pgm_data_acled$ged_dummy_os == 1 & pgm_data_acled$acled_dummy_os == 0,
    pgm_data_acled$ged_dummy_os,
    ifelse(
      pgm_data_acled$ged_dummy_os == 0 & pgm_data_acled$acled_dummy_os == 1,
      pgm_data_acled$acled_dummy_os,
      pgm_data_acled$ged_dummy_os
    )
  )
)

# (2) os
pgm_data_acled$fat_best_os <- ifelse(
  pgm_data_acled$fat_dummy_os == 0,
  0,
  ifelse(
    pgm_data_acled$fat_dummy_os == 1,
    ceiling((pgm_data_acled$ged_best_os + pgm_data_acled$acled_count_os) / 2),
    0
  )
)


pgm_data_acled$fat_dummy_sb <- as.logical(pgm_data_acled$fat_dummy_sb)
pgm_data_acled$fat_best_sb <- as.integer(pgm_data_acled$fat_best_sb)
pgm_data_acled$time_since_fat_dummy_sb <- as.integer(pgm_data_acled$time_since_fat_dummy_sb)

pgm_data_acled$fat_dummy_ns <- as.logical(pgm_data_acled$fat_dummy_ns)
pgm_data_acled$fat_best_ns <- as.integer(pgm_data_acled$fat_best_ns)
pgm_data_acled$time_since_fat_dummy_ns <- as.integer(pgm_data_acled$time_since_fat_dummy_ns)

pgm_data_acled$fat_dummy_os <- as.logical(pgm_data_acled$fat_dummy_os)
pgm_data_acled$fat_best_os <- as.integer(pgm_data_acled$fat_best_os)
pgm_data_acled$time_since_fat_dummy_os <- as.integer(pgm_data_acled$time_since_fat_dummy_os)


### Calculate number of months since last fatality

# sb
pgm_data_acled <- pgm_data_acled %>%
  arrange(lat, long, year, month) %>%
  group_by(lat, long) %>%
  mutate(time_since_fat_dummy_sb = ifelse(fat_dummy_sb == 1 | (month == 1 & year == 1990 & fat_dummy_sb == 0), 1, 0))

pgm_data_acled <- pgm_data_acled %>%
  arrange(lat, long, year, month) %>%
  group_by(lat, long) %>%
  mutate(
    time_since_fat_dummy_sb = ifelse(fat_dummy_sb == 1 | (month == 1 & year == 1990 & fat_dummy_sb == 0), 1, 0),
    time_since_fat_dummy_sb = ave(time_since_fat_dummy_sb, cumsum(time_since_fat_dummy_sb == 1), FUN = seq_along)
  )

# ns
pgm_data_acled <- pgm_data_acled %>%
  arrange(lat, long, year, month) %>%
  group_by(lat, long) %>%
  mutate(time_since_fat_dummy_ns = ifelse(fat_dummy_ns == 1 | (month == 1 & year == 1990 & fat_dummy_ns == 0), 1, 0))

pgm_data_acled <- pgm_data_acled %>%
  arrange(lat, long, year, month) %>%
  group_by(lat, long) %>%
  mutate(
    time_since_fat_dummy_ns = ifelse(fat_dummy_ns == 1 | (month == 1 & year == 1990 & fat_dummy_ns == 0), 1, 0),
    time_since_fat_dummy_ns = ave(time_since_fat_dummy_ns, cumsum(time_since_fat_dummy_ns == 1), FUN = seq_along)
  )

# os
pgm_data_acled <- pgm_data_acled %>%
  arrange(lat, long, year, month) %>%
  group_by(lat, long) %>%
  mutate(time_since_fat_dummy_os = ifelse(fat_dummy_os == 1 | (month == 1 & year == 1990 & fat_dummy_os == 0), 1, 0))

pgm_data_acled <- pgm_data_acled %>%
  arrange(lat, long, year, month) %>%
  group_by(lat, long) %>%
  mutate(
    time_since_fat_dummy_os = ifelse(fat_dummy_os == 1 | (month == 1 & year == 1990 & fat_dummy_os == 0), 1, 0),
    time_since_fat_dummy_os = ave(time_since_fat_dummy_os, cumsum(time_since_fat_dummy_os == 1), FUN = seq_along)
  )
save(pgm_data_acled, file = "pgm_data_acled.RData")



########## REALIZE ALL VARIABLES IN PGM DATASET ##########

# EPR 
load("prio_grid_ethnic_epr.RData")
pgm_model_data <- merge(pgm_model_data, prio_grid_ethnic[, c("status_alone" , "status_share", "status_excluded", "status_irrelevant", "reg_aut", "claim", "recruitment", "support", "month", "year", "lat", "long")], by = c("month", "year", "lat", "long"), all.x = TRUE)

# Nighttime Light
load("pgm_data_nighttime_light.RData")
pgm_data_nighttime_light <- na.omit(pgm_data_nighttime_light)
pgm_data_nighttime_light <- subset(pgm_data_nighttime_light, select = -c(country_name))
pgm_data_nighttime_light <- distinct(pgm_data_nighttime_light)

# Expand to the month-level
months <- 1:12
pgm_data_nighttime_light$month <- 1
pgm_data_nighttime_light_monthly <- pgm_data_nighttime_light %>%
  group_by(long, lat, year) %>%
  complete(month = months) %>%
  ungroup()
pgm_data_nighttime_light_monthly <- pgm_data_nighttime_light_monthly %>%
  group_by(long, lat, year) %>%
  fill(NL_sum, NL_mean, NL_max) %>%
  ungroup()
pgm_model_data <- merge(pgm_model_data, pgm_data_nighttime_light_monthly[, c("NL_sum", "NL_mean", "NL_max", "year", "lat", "long", "month")], by = c("year", "lat", "long", "month"), all.x = TRUE)

# Population
load("pgm_data_pop_sum.RData")
pgm_model_data <- merge(pgm_model_data, pgm_data_pop_sum[, c("population", "lat", "long", "year", "month")], by = c("month", "year", "lat", "long"), all.x = TRUE)
pgm_model_data$log_population <- ifelse(is.na(pgm_model_data$population), NA, log10(pgm_model_data$population))
pgm_model_data$log_population[is.infinite(pgm_model_data$log_population) & pgm_model_data$log_population < 0] <- 0

# Temperature and Precipitation
load("pgm_data_temp_precip.RData")
pgm_model_data <- merge(pgm_model_data, pgm_data_temp_precip[, c("mean_temp_celsius", "temp_anomaly", "mean_precip", "precip_anomaly", "month", "year", "lat", "long")], by = c("month", "year", "lat", "long"), all.x = TRUE)

# ACLED
load("pgm_data_acled.RData")
pgm_model_data <- merge(pgm_model_data, pgm_data_acledp[, c("acled_dummy_sb", "acled_count_sb", "fat_dummy_sb", "fat_count_sb", "month", "year", "lat", "long")], by = c("month", "year", "lat", "long"), all.x = TRUE)

save(pgm_model_data, file = "pgm_data_all_variables.RData")



########## CREATE LAGS OF SELECTED VARIABLES ##########

pgm_model_data <- setDT(pgm_model_data)

f.agg.time <- function(.data, .n, .vec.col, .fun, .group){
  name.col <- paste0(.vec.col, "_lag_", .n)
  time_start <- Sys.time()
  for(i in 1:length(.vec.col)){
    .data[, new := rollapply(get(.vec.col[i]),
                             width = .n,
                             FUN = .fun,
                             align = 'right',
                             partial = TRUE)
          , by = get(.group)]
    setnames(.data, "new", name.col[i])
  }
  time_end <- Sys.time()
  print((time_end- time_start))
  print(nrow(.data))
  return(.data)
}

f.max <- function(x){
  if(all(is.na(x))){
    max <- 0
  }else{
    max <- max(x, na.rm = TRUE)
  }
  return(max)
}

vec1<- c("log_population", "mean_temp_celsius", "temp_anomaly",        
         "mean_precip", "precip_anomaly", "status_excluded",
         "reg_aut", "claim", "recruitment", "support")

data.pgm.ged.unique.lag <- f.agg.time(.data = pgm_model_data,
                                      .n = 2,
                                      .vec.col = vec1,
                                      
                                      .fun = f.max,
                                      .group = "pg_id")

data.pgm.ged.unique.lag <- f.agg.time(.data = data.pgm.ged.unique.lag,
                                      .n = 3,
                                      .vec.col = vec1,
                                      
                                      .fun = f.max,
                                      .group = "pg_id")

data.pgm.ged.unique.lag <- f.agg.time(.data = data.pgm.ged.unique.lag,
                                      .n = 4,
                                      .vec.col =vec1,
                                      
                                      .fun = f.max,
                                      .group = "pg_id")

data.pgm.ged.unique.lag <- f.agg.time(.data = data.pgm.ged.unique.lag,
                                      .n = 5,
                                      .vec.col = vec1,
                                      
                                      .fun = f.max,
                                      .group = "pg_id")

data.pgm.ged.unique.lag <- f.agg.time(.data = data.pgm.ged.unique.lag,
                                      .n = 6,
                                      .vec.col =vec1,
                                      
                                      .fun = f.max,
                                      .group = "pg_id")

pgm_model_data$date <- as.Date(paste(pgm_model_data$year, pgm_model_data$month, "01", sep = "-"))

save(pgm_model_data, file = "pgm_model_data_final.RData")


