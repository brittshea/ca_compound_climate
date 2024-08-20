###Title: Wildfire Disasters in CA 2018-2019
###Author: Brittany Shea
###Date: 08-16-2024

#-------------------------
#load libraries

library(viridis)
library(tidyverse)
library(lubridate)
library(sf)
library(rgeos)
library(rgdal)
library(ggplot2)
library(raster)
library(sp)
library(plyr)
library(graticule)
library(zoo)
library(dplyr)
library(conflicted)
library(padr)

#remove conflicts when working with dplyr & spatial data

conflict_prefer("filter", "dplyr")

#-------------------------
#load data

load("data/final_binded_ca_disaster_fires_2018_2019.rdata")

#load shapefile of entire United States by county

shapefile = readOGR(dsn = paste0("data/cb_2015_us_county_500k.shp"),layer = "cb_2015_us_county_500k")

#-------------------------
#reproject shapefile

shapefile = spTransform(shapefile, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

#remove non-mainland territories (for entire mainland US)

ca_shapefile = shapefile[shapefile$STATEFP %in% c("06"),]

#change from spatial polygon data frame into sf object

ca_sf = st_as_sf(ca_shapefile)

ca_sf <- ca_sf %>%
  dplyr::mutate(fips = as.integer(as.character(GEOID)))

#select variables 

ca_sf <- ca_sf %>% 
  dplyr::select(COUNTYFP, NAME)

#CRS of wildfire spatial data: NAD83 / California Albers

#check CRS of dataframes

crs(ca_sf)
crs(final_binded_ca_disaster_fires_2018_2019)

#match CRS of county shapefile with wildfire spatial data

ca_sf <- st_transform(ca_sf, "EPSG:3310")
final_binded_ca_disaster_fires_2018_2019 <- st_transform(final_binded_ca_disaster_fires_2018_2019, "EPSG:3310")

#turn off spherical geometry (prevents errors)

sf::sf_use_s2(FALSE)

#spatial intersection of wildfire gis data with county shapefile

ca_wd_2018_2019 <- 
  st_intersection(st_make_valid(final_binded_ca_disaster_fires_2018_2019), st_make_valid(ca_sf))

#-------------------------
#rename COUNTYFP to fips; create column to determine whether exposed

ca_wd_2018_2019 <- ca_wd_2018_2019 %>% 
  dplyr::rename(fips = COUNTYFP) %>% 
  dplyr::rename(county = NAME) %>% 
  dplyr::filter(density_criteria_met == "Yes") %>% 
  dplyr::mutate(exposed = ifelse(civilian_fatality_y_n == "Y" | structures_destroyed_y_n == "Y" | fm_declaration_y_n == "Y", 1,0))
  
#expand dates between ignition and containment & drop geometry

sub_ca_wd_2018_2019 <- ca_wd_2018_2019 %>% 
  dplyr::select(year, state, fips, incident_name, ignition_date, containment_date)

sub_ca_wd_2018_2019 <- sub_ca_wd_2018_2019 %>% 
  pivot_longer(cols = c('ignition_date', 'containment_date'), names_to = 'times') %>% 
  st_drop_geometry() %>% 
  filter(!is.na(incident_name))

sub_ca_wd_2018_2019 <- sub_ca_wd_2018_2019 %>%
  group_by(incident_name, year, state, fips)

sub_ca_wd_2018_2019 <- sub_ca_wd_2018_2019 %>% 
  do(pad(., interval = "1 days")) %>% 
  ungroup() %>% 
  tidyr::fill(year, state, fips, incident_name)

#add 6 to fips; remove duplicate row (2019, CA, 065, 46th, containment_date, 2019-10-31)

sub_ca_wd_2018_2019$fips <- sub("^","6",sub_ca_wd_2018_2019$fips)
sub_ca_wd_2018_2019 <- unique( sub_ca_wd_2018_2019[ , c('year', 'state', 'fips','incident_name','value') ] )

#-------------------------
#create list of duplicates_wd to show counties with 2 wd in the same day

duplicates_wd <- sub_ca_wd_2018_2019

ncol = paste(duplicates_wd$fips,duplicates_wd$value, sep = "")

duplicates_wd$fipdate <- ncol

duplicates_wd = duplicates_wd[duplicated(duplicates_wd$fipdate ) | duplicated(duplicates_wd$fipdate , fromLast = TRUE),]

write_csv(duplicates_wd, "output/duplicates_wd.csv")

#-------------------------
#create list of all wd including duplicates: join sub_ca_wd_2018_2019 df to ca_wd_2018_2019 df

dup_exp_ca_wd_2018_2019 = left_join(sub_ca_wd_2018_2019, ca_wd_2018_2019, by = c("year" = "year", "state" = "state", "fips" = "fips", 
                                                         "incident_name" = "incident_name"))

write_csv(dup_exp_ca_wd_2018_2019, "output/dup_exp_ca_wd_2018_2019.csv")

#-------------------------
#create new data frame without duplicates to join to po and aw

no_duplicates_wd <- sub_ca_wd_2018_2019

no_duplicates_wd$incident_name = NULL

ncol = paste(no_duplicates_wd$fips,no_duplicates_wd$value, sep = "")

no_duplicates_wd$fipdate <- ncol

no_duplicates_wd = no_duplicates_wd[!duplicated(no_duplicates_wd), ]

no_duplicates_wd = no_duplicates_wd[ ,c("fipdate", "fips", "value")]

no_duplicates_wd$exposed_wd <- 1

colnames(no_duplicates_wd) = c("fipdate", "fips_wd", "date_wd", "exposed_wd")

write_csv(no_duplicates_wd, "output/wd2join.csv")

