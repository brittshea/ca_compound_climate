#Co-occurring climate events analysis for CA, 2018-2019
#Author: Brittany Shea
#Date: 08-16-2024

#------------------------- 
#load libraries

library(tidyverse)
library(knitr)
library(dplyr)
library(patchwork)
library(ggplot2)
library(viridis)

#-------------------------
#read files

aw = read.csv("output/aw2join.csv")
po = read.csv("output/po2join.csv")
wd = read.csv("output/wd2join.csv")
svi = read.csv("data/svi.csv")

#-------------------------
#list of only duplicate wildfires: counties that had more than 1 wildfire on the same day

wd_dups = read.csv("output/duplicates_wd.csv")

#list of wildfires including duplicates

wd_with_dups = read.csv("output/dup_exp_ca_wd_2018_2019.csv")

#-------------------------
#merge files

aw_po_merge = merge(aw, po, all = TRUE)

aw_po_wd_merge = merge(aw_po_merge, wd, all = TRUE)

aw_po_wd_merge = aw_po_wd_merge %>% 
  dplyr::select(fipdate, exposed_aw, exposed_po, exposed_wd)

aw_po_wd_merge[is.na(aw_po_wd_merge)] <- 0

#-------------------------
#find numbers to make barplot

wd_num <- aw_po_wd_merge %>% 
  filter(exposed_wd == 1) %>% 
  nrow()
po_num <- aw_po_wd_merge %>% 
  filter(exposed_po == 1) %>% 
  nrow()
aw_num <- aw_po_wd_merge %>% 
  filter(exposed_aw == 1) %>% 
  nrow()
wd_po_num <- aw_po_wd_merge %>% 
  filter(exposed_po == 1 & exposed_wd == 1) %>% 
  nrow()
wd_aw_num <- aw_po_wd_merge %>% 
  filter(exposed_aw == 1 & exposed_wd == 1) %>% 
  nrow()
po_aw_num <- aw_po_wd_merge %>% 
  filter(exposed_po == 1 & exposed_aw == 1) %>% 
  nrow()
po_aw_wd_num <- aw_po_wd_merge %>% 
  filter(exposed_po == 1 & exposed_aw == 1 & exposed_wd == 1) %>% 
  nrow()

po_aw_wd_barplot <- c(po_aw_wd_num, po_aw_num, wd_po_num, wd_aw_num, po_num, wd_num, aw_num)

#-------------------------
#create the barplot and save the bar numbers

#increase the size of the margins

par(mar = c(8, 18, 4, 2) + 0.1)

countydays_barplot <- barplot(po_aw_wd_barplot, 
                              names.arg = c("AW + WD + PO", "AW + PO", "WD + PO", "AW + WD", "Power Outage (PO)", "Wildfire Disaster (WD)", "Anomalously Warm (AW)"),
                              las = 2,
                              col = "tomato2",
                              horiz = TRUE,
                              xlim = c(0, 2500),
                              ylim = c(0, length(po_aw_wd_barplot) + 1),  
                              cex.names = 1.75,
                              cex.axis = 1.75,
                              cex.lab = 1.75)

#add numbers to the end of the bars with increased size

text(x = po_aw_wd_barplot, y = countydays_barplot, labels = po_aw_wd_barplot, pos = 4, cex = 1.75) 

#calculate % of total county-days of each event type; total county-days in period = 42,340

aw <- round((aw_num / 42340 * 100), 2)
wd <- round((wd_num / 42340 * 100), 2)
po <- round((po_num / 42340 * 100), 2)
aw_wd <- round((wd_aw_num / 42340 * 100), 2)
po_wd <- round((wd_po_num / 42340 * 100), 2)
po_aw <- round((po_aw_num / 42340 * 100), 2)
po_aw_wd <- round((po_aw_wd_num / 42340 * 100), 2)

percent_countydays <- c(po_aw_wd, po_aw, po_wd, aw_wd, po, wd, aw)
labels <- paste0("(", percent_countydays, "%)")

#add the labels with increased size

text(x = po_aw_wd_barplot, y = countydays_barplot, labels = labels, pos = 4, offset = 4, cex = 1.75)

mtext("County-Days Exposure", side = 1, line = 5, cex = 1.75) 

#-------------------------
#create date and fips column from fipdate

aw_po_wd_merge$date <- substring(aw_po_wd_merge$fipdate, 5) #sub-string starting from the 5th element of the string
aw_po_wd_merge$fips <- substring(aw_po_wd_merge$fipdate, 0, 4) #sub-string starting from the 0 to the 4th element of the string

#write merged file

write_csv(aw_po_wd_merge, file = "output/countyday_aw_po_wd.csv")
aw_po_wd_merge = read.csv("output/countyday_aw_po_wd.csv")

#-------------------------
#aggregate to find number of po aw wd days each fips has experienced

aw_days = aggregate(aw_po_wd_merge$exposed_aw, by = list(fips = aw_po_wd_merge$fips), FUN = sum)
colnames(aw_days)[2] = "aw_days"

po_days = aggregate(aw_po_wd_merge$exposed_po, by = list(fips = aw_po_wd_merge$fips), FUN = sum)
colnames(po_days)[2] = "po_days"

wd_days = aggregate(aw_po_wd_merge$exposed_wd, by = list(fips = aw_po_wd_merge$fips), FUN = sum)
colnames(wd_days)[2] = "wd_days"

days_single_events = merge(aw_days,wd_days, by = "fips", all = TRUE)
days_single_events = merge(days_single_events, po_days, by = "fips", all = TRUE)

write.csv(days_single_events, "output/days_single_events.csv") 

#-------------------------
#select co-occurring events and extract unique fips

#WD & AW
wd_aw_comp <- aw_po_wd_merge %>% 
  filter(exposed_aw == 1 & exposed_wd == 1)
wd_aw_fips = as.matrix(unique(wd_aw_comp$fips))

wd_aw_comp_count = aggregate(wd_aw_comp$exposed_aw, by = list(fips = wd_aw_comp$fips), FUN = sum)
colnames(wd_aw_comp_count)[2] = "wd_aw"

#WD & PO
wd_po_comp <- aw_po_wd_merge %>% 
  filter(exposed_po == 1 & exposed_wd == 1)
wd_po_fips = as.matrix(unique(wd_po_comp$fips))

wd_po_comp_count = aggregate(wd_po_comp$exposed_po, by = list(fips = wd_po_comp$fips), FUN = sum)
colnames(wd_po_comp_count)[2] = "wd_po"

#PO & AW 
po_aw_comp <- aw_po_wd_merge %>% 
  filter(exposed_po == 1 & exposed_aw == 1)
po_aw_fips = as.matrix(unique(po_aw_comp$fips))

po_aw_comp_count = aggregate(po_aw_comp$exposed_po, by = list(fips = po_aw_comp$fips), FUN = sum)
colnames(po_aw_comp_count)[2] = "po_aw"

#WD & AW & PO 
wd_aw_po_comp <- aw_po_wd_merge %>% 
  filter(exposed_wd == 1 & exposed_aw == 1 & exposed_po == 1)
wd_aw_po_fips = as.matrix(unique(wd_aw_po_comp$fips))

wd_aw_po_comp_count = aggregate(wd_aw_po_comp$exposed_aw, by = list(fips = wd_aw_po_comp$fips), FUN = sum)
colnames(wd_aw_po_comp_count)[2] = "wd_aw_po"

#-------------------------
#merge dfs with co-occurring events & unique events per fips ("wd_aw_comp_count", "wd_po_comp_count", & "po_aw_comp_count")

comp_count_fips = merge(wd_aw_comp_count,wd_po_comp_count, by = "fips", all = TRUE)
comp_count_fips = merge(comp_count_fips,po_aw_comp_count, by = "fips", all = TRUE)
comp_count_fips = merge(comp_count_fips,wd_aw_po_comp_count, by = "fips", all = TRUE)

final_count_fips = merge(comp_count_fips,days_single_events, by = "fips", all = TRUE)
final_count_fips[is.na(final_count_fips)] <- 0

#-------------------------
#merge with svi & county names

final_count_fips = merge(final_count_fips,svi, by = "fips", all = TRUE)
final_count_fips[is.na(final_count_fips)] <- 0

write.csv(final_count_fips, "output/final_count_fips.csv") 

median(final_count_fips$svi)

sum(final_count_fips$wd_po)

#-------------------------
#create table of svi values and all event types

svi_table = final_count_fips %>% 
  select(name, svi, aw_days, wd_days, po_days, wd_aw, wd_po, po_aw, wd_aw_po) %>% 
  rename(aw_wd = wd_aw) %>%
  rename(aw_po = po_aw) %>%
  rename(aw_wd_po = wd_aw_po) %>%
  rename(county_name = name) %>% 
  arrange(desc(svi))

write.csv(svi_table, "output/svi_events_table.csv") 

#-------------------------
#look at distribution of co-occurring climate events (by count of different types of co-occurring events and total count of co-occurring events)

#count total co-occurring events per row

distribution_comp <- svi_table %>%
  mutate(total_events = rowSums(select(., aw_wd:aw_wd_po), na.rm = TRUE))

#count number of types of co-occurring events per row

distribution_comp <- distribution_comp %>%
  mutate(types_of_events = rowSums(select(., aw_wd:aw_wd_po) != 0, na.rm = TRUE))

#order rows based on total co-occurring events and number of types of co-occurring events

distribution_comp <- distribution_comp %>%
  arrange(desc(total_events), desc(types_of_events))

#-------------------------
#calculate the 3rd quartile of the SVI scores

third_quantile_svi <- quantile(final_count_fips$svi, 0.75)

#find the rows that are in the 3rd quartile

top_quarter_svi <- final_count_fips[final_count_fips$svi > third_quantile_svi, ]

#print the data in the 3rd quartile

print(top_quarter_svi)

#find the total number of co-occurring days in top quarter

sum(top_quarter_svi$aw_days)

#calculate the bottom quartile of the SVI scores

first_quartile_svi <- quantile(final_count_fips$svi, 0.25)

#find the rows that are in the 1st quartile

bottom_quarter_svi <- final_count_fips[final_count_fips$svi < first_quartile_svi, ]

#print the data in the 1st quartile

print(bottom_quarter_svi)

#find the total number of co-occurring days in bottom quarter

sum(bottom_quarter_svi$po_aw)

#-------------------------
#plots of events by svi

#WD & AW, SVI

#wd_aw != 0

comp_wd_aw = final_count_fips[final_count_fips$wd_aw != 0,]

#lollipop plot with best fit line

plot1 <- ggplot(comp_wd_aw, aes(x = svi, y = wd_aw)) + 
  geom_segment(aes(xend = svi, yend = 0)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  scale_color_viridis_c() +
  theme_minimal() + 
  labs(y = "Days", x = "SVI") +
  theme(axis.title.y = element_text(size = 23),
        axis.title.x = element_text(size = 23),
        axis.text = element_text(size = 16),
        title = element_text(size = 16)) + 
  ggtitle("(A) Co-occurring Anomalously Warm & Wildfire Disaster Days by CA County, 2018-2019 (n = 144 county-days)")

#WD & PO, SVI

#wd_po != 0

comp_wd_po = final_count_fips[final_count_fips$wd_po != 0,]

#lollipop plot with best fit line

plot2 <- ggplot(comp_wd_po, aes(x = svi, y = wd_po)) + 
  geom_segment(aes(xend = svi, yend = 0)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_viridis_c() +
  theme(axis.text.x = element_blank()) + 
  theme_minimal() + 
  theme(axis.title.y = element_text(size = 23),
        axis.title.x = element_text(size = 23),
        axis.text = element_text(size = 16),
        title = element_text(size = 16)) + 
  labs(y = "Days", x = "SVI") +
  ggtitle("(B) Co-occurring Wildfire Disaster & Long Power Outage Days by CA County, 2018-2019 (n = 29 county-days)")

#PO & AW, SVI

#po_aw != 0

comp_po_aw = final_count_fips[final_count_fips$po_aw != 0,]

#lollipop plot with best fit line

plot3 <- ggplot(comp_po_aw, aes(x = svi, y = po_aw)) + 
  geom_segment(aes(xend = svi, yend = 0)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_color_viridis_c() +
  theme_minimal() + 
  labs(y = "Days", x = "SVI") +
  theme(axis.title.y = element_text(size = 23),
        axis.title.x = element_text(size = 23),
        axis.text = element_text(size = 16),
        title = element_text(size = 16)) + 
  ggtitle("(C) Co-occurring Anomalously Warm & Long Power Outage Days by CA County, 2018-2019 (n = 10 county-days)")

#AW, SVI

# AW != 0

plot_aw_days = final_count_fips[final_count_fips$aw_days != 0,]

#lollipop plot with best fit line

plot4 <- ggplot(plot_aw_days, aes(x = svi, y = aw_days)) + 
  geom_segment(aes(xend = svi, yend = 0)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  scale_color_viridis_c() +
  theme_minimal() + 
  labs(y = "Days", x = "SVI") +
  theme(axis.title.y = element_text(size = 23),
        axis.title.x = element_text(size = 23),
        axis.text = element_text(size = 16),
        title = element_text(size = 16)) +
  ggtitle("(D) Anomalously Warm Days by CA County, 2018-2019 (n = 2004 county-days)")

#WD, SVI

#wd_days != 0

plot_wd_days = final_count_fips[final_count_fips$wd_days != 0,]

#lollipop plot with best fit line

plot5 <- ggplot(plot_wd_days, aes(x = svi, y = wd_days)) + 
  geom_segment(aes(xend = svi, yend = 0)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  scale_color_viridis_c() +
  theme_minimal() + 
  labs(y = "Days", x = "SVI") +
  theme(axis.title.y = element_text(size = 23),
        axis.title.x = element_text(size = 23),
        axis.text = element_text(size = 16),
        title = element_text(size = 16)) + 
  ggtitle("(E) Wildfire Disaster Days by CA County, 2018-2019 (n = 1131 county-days)")

#PO, SVI

#po != 0

plot_po_days = final_count_fips[final_count_fips$po_days != 0,]

#lollipop plot with best fit line

plot6 <- ggplot(plot_po_days, aes(x = svi, y = po_days)) + 
  geom_segment(aes(xend = svi, yend = 0)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  scale_color_viridis_c() +
  theme_minimal() + 
  labs(y = "Days", x = "SVI") +
  theme(axis.title.y = element_text(size = 23),
        axis.title.x = element_text(size = 23),
        axis.text = element_text(size = 16),
        title = element_text(size = 16)) +
  ggtitle("(F) Long Power Outage Days by CA County, 2018-2019 (n = 597 county-days)")

#-------------------------
#combine the co-occurring events into lollipop plots

combined_plot1 <- plot1 / plot_spacer() / plot2 / plot_spacer() / plot3 /
  plot_layout(heights = c(1, 0.01, 1, 0.05, 1))

#print the combined plot

print(combined_plot1)

#combine the co-occurring events into lollipop plots

combined_plot2 <- plot4 / plot_spacer() / plot5 / plot_spacer() / plot6 +
  plot_layout(heights = c(1, 0.01, 1, 0.05, 1))

#print the combined plot

print(combined_plot2)

#-------------------------
#spearman correlation SVI and event days 

comp_wd_aw %>% 
  summarise(spearman = cor(x = svi, y = wd_aw, method = c("spearman")))

comp_wd_po %>% 
  summarise(spearman = cor(x = svi, y = wd_po, method = c("spearman")))

comp_po_aw %>% 
  summarise(spearman = cor(x = svi, y = po_aw, method = c("spearman")))

plot_wd_days %>% 
  summarise(spearman = cor(x = svi, y = wd_days, method = c("spearman")))

plot_aw_days %>% 
  summarise(spearman = cor(x = svi, y = aw_days, method = c("spearman")))

plot_po_days %>% 
  summarise(spearman = cor(x = svi, y = po_days, method = c("spearman")))
