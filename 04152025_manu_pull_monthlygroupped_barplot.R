# grouped barplot 
# monthly precipitation

## uses merged_dataset from 04032025 pulling additional points file 

library(dplyr)
library(tidyr)
library(gridExtra)

manu_pull_outliers <- Reduce(function(x, y) merge(x, y, by = "time", all = TRUE), manu_pulls)

manu_pull_outliers$time <- as.Date(manu_pull_outliers$time)
manu_pull_outliers$yearmonth <- format(manu_pull_outliers$time, "%Y-%m")
manu_pull_outliers$month <- format(manu_pull_outliers$time,"%m")

dataset_cols <- grep("_file_\\d{2,}_\\d{2,3}$", names(manu_pull_outliers), value = TRUE)

manu_long_df <- manu_pull_outliers %>%
  pivot_longer(cols = all_of(dataset_cols),
               names_to = "Source",
               values_to = "Precip") %>% 
  mutate(
    Dataset = sub("_file_.*", "", Source),
    Site_ID = sub(".*_(\\d{2,3}_\\d{2,3})$", "\\1", Source),
    yearmonth = format(time, "%Y-%m"),
    month = format(time, "%m")
  )

# totprcp per month 
monthly_agg <- manu_long_df %>%
  group_by(Site_ID,  month, Dataset) %>%
  summarise(totprcp = sum(Precip, na.rm = TRUE) / 33, .groups = 'drop') # do monthly climatology

# days of rain per month based on 0.25 
manu_day_rain <- manu_long_df %>%
  filter(!is.na(Precip)) %>%
  mutate(rainy_day = Precip >= 0.25) %>%
  group_by(Site_ID, month, Dataset) %>%
  summarise(totdaysrx = sum(rainy_day, na.rm = TRUE) / 33, .groups = "drop")

manu_day_rain_1 <- manu_long_df %>%
  filter(!is.na(Precip)) %>%
  mutate(rainy_day = Precip >= 1) %>%
  group_by(Site_ID, month, Dataset) %>%
  summarise(totdaysrx = sum(rainy_day, na.rm = TRUE) / 33, .groups = "drop")

manu_day_rain_5 <- manu_long_df %>%
  filter(!is.na(Precip)) %>%
  mutate(rainy_day = Precip >= 5) %>%
  group_by(Site_ID, month, Dataset) %>%
  summarise(totdaysrx = sum(rainy_day, na.rm = TRUE) / 33, .groups = "drop")

manu_day_rain_10 <- manu_long_df %>%
  filter(!is.na(Precip)) %>%
  mutate(rainy_day = Precip >= 10) %>%
  group_by(Site_ID, month, Dataset) %>%
  summarise(totdaysrx = sum(rainy_day, na.rm = TRUE) / 33, .groups = "drop")

# dry days 
manu_day_rain_le_0.25 <- manu_long_df %>%
  filter(!is.na(Precip)) %>%
  mutate(rainy_day = Precip <= 0.25) %>%
  group_by(Site_ID, month, Dataset) %>%
  summarise(totdaysrx = sum(rainy_day, na.rm = TRUE) / 33, .groups = "drop")

# add a percentile >= 0.95 
manu_day_rain_95 <- manu_long_df %>%
  filter(!is.na(Precip)) %>%
  mutate(rainy_day = Precip >= quantile(Precip, 0.95)) %>%
  group_by(Site_ID, month, Dataset) %>%
  summarise(totdaysrx = sum(rainy_day, na.rm = TRUE) / 33, .groups = "drop")

# add a percentile >= 0.99
manu_day_rain_99 <- manu_long_df %>%
  filter(!is.na(Precip)) %>%
  mutate(rainy_day = Precip >= quantile(Precip, 0.99)) %>%
  group_by(Site_ID, month, Dataset) %>%
  summarise(totdaysrx = sum(rainy_day, na.rm = TRUE) / 33, .groups = "drop")

# add a percentile <= 0.05
manu_day_rain_005 <- manu_long_df %>%
  filter(!is.na(Precip)) %>%
  mutate(rainy_day = Precip <= quantile(Precip, 0.05)) %>%
  group_by(Site_ID, month, Dataset) %>%
  summarise(totdaysrx = sum(rainy_day, na.rm = TRUE) / 33, .groups = "drop")

# add a percentile <= 0.01
manu_day_rain_001 <- manu_long_df %>%
  filter(!is.na(Precip)) %>%
  mutate(rainy_day = Precip <= quantile(Precip, 0.01)) %>%
  group_by(Site_ID, month, Dataset) %>%
  summarise(totdaysrx = sum(rainy_day, na.rm = TRUE) / 33, .groups = "drop")

# add a percentile = 50
manu_day_rain_50 <- manu_long_df %>%
  filter(!is.na(Precip)) %>%
  mutate(rainy_day = Precip <= quantile(Precip, 0.5)) %>%
  group_by(Site_ID, month, Dataset) %>%
  summarise(totdaysrx = sum(rainy_day, na.rm = TRUE) / 33, .groups = "drop")

# create a loop to create a unique monthly grouped barplot by unique site 

unique_manu_sites <- unique(monthly_agg$Site_ID)

for (site in unique_manu_sites){
  
  site_data <- filter(monthly_agg, Site_ID == site)
  site_data_rainy_days <- filter(manu_day_rain, Site_ID == site)
  site_data_rainy_days_1 <- filter(manu_day_rain_1, Site_ID == site)
  site_data_rainy_days_5 <- filter(manu_day_rain_5, Site_ID == site)
  site_data_rainy_days_10 <- filter(manu_day_rain_10, Site_ID == site)
  site_data_rainy_days_0.25 <- filter(manu_day_rain_le_0.25, Site_ID == site)
  #site_data_rainy_days_95 <- filter(manu_day_rain_95, Site_ID == site)
  #site_data_rainy_days_005 <- filter(manu_day_rain_005, Site_ID == site)
  
  lat <- unique(manu_pull_df$lat[manu_pull_df$clean_id ==site])
  lon <- unique(manu_pull_df$lon[manu_pull_df$clean_id ==site])
  
  # add lat lon information 
  
  p1 <- ggplot(site_data, aes(x = month, y = totprcp, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Monthly Precipitation (mm)", x = "Month", y = "Precipitation",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none")
  
  #print(p)
  
  p2 <- ggplot(site_data_rainy_days, aes(x = month, y = totdaysrx, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Days of Rain >= 0.25 (days)", x = "Month", y = "Days",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none")
  
  p3 <- ggplot(site_data_rainy_days_1, aes(x = month, y = totdaysrx, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Days of Rain >= 1 (days)", x = "Month", y = "Days",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none")
  
  p4 <- ggplot(site_data_rainy_days_5, aes(x = month, y = totdaysrx, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Days of Rain >= 5 (days)", x = "Month", y = "Days",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none")
  
  p5 <- ggplot(site_data_rainy_days_10, aes(x = month, y = totdaysrx, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Days of Rain >= 10 (days)", x = "Month", y = "Days",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none")
  
  p6 <- ggplot(site_data_rainy_days_0.25, aes(x = month, y = totdaysrx, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Days of Rain <= 0.25 (days)", x = "Month", y = "Days",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none") 
  
  # p7 <- ggplot(site_data_rainy_days_95, aes(x = month, y = totdaysrx, fill = Dataset)) + 
  #   geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  #   labs(title = "Days of Rain >= 95th Percentile (days)", x = "Month", y = "Days",
  #        subtitle = paste("Lon:", lon, "Lat:", lat)) + 
  #   scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
  #   theme(legend.position="none") 
  # 
  # p8 <- ggplot(site_data_rainy_days_005, aes(x = month, y = totdaysrx, fill = Dataset)) + 
  #   geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  #   labs(title = "Days of Rain <= 5th Percentile (days)", x = "Month", y = "Days",
  #        subtitle = paste("Lon:", lon, "Lat:", lat)) + 
  #   scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
  #   theme(legend.position="none") 
  
  #print(p2)
  
  grid.arrange(p6, p1, p2, p3, p4, p5)
}

# seperate percentile based days and accumulation days 
for (site in unique_manu_sites){
  
  site_data <- filter(monthly_agg, Site_ID == site)
  site_data_rainy_days_95 <- filter(manu_day_rain_95, Site_ID == site)
  site_data_rainy_days_99 <- filter(manu_day_rain_99, Site_ID == site)
  site_data_rainy_days_005 <- filter(manu_day_rain_005, Site_ID == site)
  site_data_rainy_days_001 <- filter(manu_day_rain_001, Site_ID == site)
  site_data_rainy_days_50 <- filter(manu_day_rain_50, Site_ID == site)
  
  lat <- unique(manu_pull_df$lat[manu_pull_df$clean_id ==site])
  lon <- unique(manu_pull_df$lon[manu_pull_df$clean_id ==site])
  
  # add lat lon information 
  
  p1 <- ggplot(site_data, aes(x = month, y = totprcp, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Monthly Precipitation (mm)", x = "Month", y = "Precipitation",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none")
  
  p7 <- ggplot(site_data_rainy_days_95, aes(x = month, y = totdaysrx, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Days of Rain >= 95th Percentile (days)", x = "Month", y = "Days",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none") 
  
  p8 <- ggplot(site_data_rainy_days_005, aes(x = month, y = totdaysrx, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Days of Rain <= 5th Percentile (days)", x = "Month", y = "Days",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none") 
  
  p9 <- ggplot(site_data_rainy_days_50, aes(x = month, y = totdaysrx, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Days of Rain 50 Percentile (days)", x = "Month", y = "Days",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none") 
  
  p10 <- ggplot(site_data_rainy_days_99, aes(x = month, y = totdaysrx, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Days of Rain >= 99 Percentile (days)", x = "Month", y = "Days",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none") 
  
  p11 <- ggplot(site_data_rainy_days_001, aes(x = month, y = totdaysrx, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "Days of Rain <= 1 Percentile (days)", x = "Month", y = "Days",
         subtitle = paste("Lon:", lon, "Lat:", lat)) + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) + 
    theme(legend.position="none") 
  
  grid.arrange(p1, p10, p7, p9, p8, p11)
}


#days_greater_equal = c(0.25, 1, 5, 10, 20, 30, 50)
