###title: "Power Outage Dataset 2018-2019"
###author: Brittany Shea
###power outage data analysis completed by: Vivian Do, Heather McBrien, Milo Gordon

#------------------------------
#load libraries

library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(viridis)
library(stringr)
library(rgdal)
library(ggplot2)
library(raster)
library(sp)
library(plyr)
library(graticule)
library(zoo)
library(conflicted)

conflict_prefer("filter", "dplyr")

#------------------------------
#load data

po_data = read_rds("data/days_exposed_unexposed_expansion_w_all_fips.RDS")
svi = read.csv("data/svi.csv")

#------------------------------
#count number of individual fips in po_data for 2018-2019 in CA

po_data %>% 
  filter(year == 2018 | year == 2019) %>% 
  filter(clean_state_name == "california") %>% 
  dplyr::mutate(fips = substring(fips,2)) %>%
  group_by(fips) %>% 
  dplyr::count() 

#count number of fips with reliable data

po_fips_count_reliable = po_data %>% 
  filter(n_reliable_years_available == 2 | n_reliable_years_available == 3) %>% 
  filter(year == 2018 | year == 2019) %>%  
  filter(clean_state_name == "california") %>% 
  dplyr::mutate(fips = substring(fips,2)) %>% 
  group_by(fips) %>%
  dplyr::count()

#count number of fips that did NOT have reliable data

po_fips_count_notreliable = po_data %>% 
  filter(n_reliable_years_available == 0 | n_reliable_years_available == 1) %>% 
  filter(year == 2018 | year == 2019) %>%  
  filter(clean_state_name == "california") %>% 
  dplyr::mutate(fips = substring(fips,2)) %>% 
  group_by(fips) %>%
  dplyr::count()

#------------------------------
#find the average svi among counties that had versus did NOT have reliable power outage data

#merge reliable data with svi

svi_reliable = merge(po_fips_count_reliable, svi, by = "fips", all = TRUE)

svi_reliable = svi_reliable %>% 
  filter(!is.na(n))

#find the average svi in svi_reliable

svi_reliable %>%
  dplyr::summarize(mean_svi = mean(svi))

#merge not reliable data with svi

svi_notreliable = merge(po_fips_count_notreliable, svi, by = "fips", all = TRUE)

svi_notreliable = svi_notreliable %>% 
  filter(!is.na(n))

#get average svi in svi_notreliable

svi_notreliable %>%
  dplyr::summarize(mean_svi = mean(svi))

#------------------------------
#filter data to create po data for 2018-2019 in CA 

clean_po_data = po_data %>% 
  filter(n_reliable_years_available == 2 | n_reliable_years_available == 3) %>% 
  filter(pc > 0.5) %>% #outage affected > 0.5% of county customers 
  filter(year == 2018 | year == 2019) %>% 
  filter(exposed == 1) %>% 
  filter(clean_state_name == "california") %>% 
  dplyr::mutate(fips = substring(fips,2))

#------------------------------
#create new data frame to join po data to wd and aw data

po2join <- clean_po_data

ncol = paste(clean_po_data$fips,clean_po_data$day, sep = "")

po2join$fipdate <- ncol

po2join2 = po2join[ ,c("fipdate","day","fips","exposed")]

colnames(po2join2) = c("fipdate", "date_po",  "fips_po", "exposed_po")

write_csv(po2join2, "output/po2join.csv")

#------------------------------
#find number of po days by county over time

county_count_po <- clean_po_data %>% 
  group_by(fips) %>%
  dplyr::summarize(total_po = sum(exposed))

write_csv(county_count_po, "output/po_data.csv")
