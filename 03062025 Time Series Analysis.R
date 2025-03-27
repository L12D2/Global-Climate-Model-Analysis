library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# this script requires the 03042025 time series extractor and plot to run. 
# store copies of previous dataframes in a new variable 
duplicate_outliers <- rep_final_extract_df 
unique_outliers <- unique_final_extract_df

duplicate_outliers$time <- as.Date(duplicate_outliers$time)
unique_outliers$time <- as.Date(unique_outliers$time)

#################################### find what days are 0 
zero_finder_duplicate <- duplicate_outliers # creating a copy of dataframe to handle 
zero_finder_unique <- unique_outliers

zero_finder_duplicate[, 2:ncol(zero_finder_duplicate)] <- lapply(zero_finder_duplicate[, 2:ncol(zero_finder_duplicate)], function(x) ifelse(x > 0, NA, x))
print(zero_finder_duplicate)

zero_finder_unique[, 2:ncol(zero_finder_unique)] <- lapply(zero_finder_unique[, 2:ncol(zero_finder_unique)], function(x) ifelse(x > 0, NA, x))
print(zero_finder_unique)

zero_finder_duplicate$year <- format(zero_finder_duplicate$time, "%Y")
zero_finder_unique$year <- format(zero_finder_unique$time, "%Y")

#monthly_zero_dup_count <- colSums(zero_finder_duplicate[,-1] == 0, na.rm = TRUE)
yearly_zero_dup_count <- aggregate(zero_finder_duplicate[,-1], by = list(zero_finder_duplicate$year), FUN = function(x) sum(x == 0, na.rm = TRUE))
yearly_zero_uni_count <- aggregate(zero_finder_unique[,-1], by = list(zero_finder_unique$year), FUN = function(x) sum(x == 0, na.rm = TRUE))

# # of dry days in duplicate outlier
plot(yearly_zero_dup_count$Group.1, yearly_zero_dup_count$daymet_file_83_374, type = "l", col = "red")
plot(yearly_zero_dup_count$Group.1, yearly_zero_dup_count$prism_file_83_374, type = "l", col = "blue")
plot(yearly_zero_dup_count$Group.1, yearly_zero_dup_count$livneh_file_83_374, type = "l", col = "black")
plot(yearly_zero_dup_count$Group.1, yearly_zero_dup_count$nclim_file_83_374, type = "l", col = "green")
plot(yearly_zero_dup_count$Group.1, yearly_zero_dup_count$metadata_file_83_374, type = "l", col = "pink")

# # of dry days in unique outlier 
plot(yearly_zero_uni_count$Group.1, yearly_zero_uni_count$nclim_file_83_376, type = "l", col = "green")
plot(yearly_zero_uni_count$Group.1, yearly_zero_uni_count$metadata_file_83_376, type = "l", col = "pink")

#################################### find what days >= 0.25 
zero_25_finder_duplicate <- duplicate_outliers # creating a copy of dataframe to handle 
zero_25_finder_unique <- unique_outliers

zero_25_finder_duplicate[, 2:ncol(zero_25_finder_duplicate)] <- lapply(zero_25_finder_duplicate[, 2:ncol(zero_25_finder_duplicate)], function(x) ifelse(x == 0, NA, x))
print(zero_25_finder_duplicate)

zero_25_finder_unique[, 2:ncol(zero_25_finder_unique)] <- lapply(zero_25_finder_unique[, 2:ncol(zero_25_finder_unique)], function(x) ifelse(x == 0, NA, x))
print(zero_25_finder_unique)

zero_25_finder_duplicate$year <- format(zero_finder_duplicate$time, "%Y")
zero_25_finder_unique$year <- format(zero_finder_unique$time, "%Y")

yearly_zero_25_dup_count <- aggregate(zero_25_finder_duplicate[,-1], by = list(zero_25_finder_duplicate$year), FUN = function(x) sum(x >= 0.25, na.rm = TRUE))
yearly_zero_25_uni_count <- aggregate(zero_25_finder_unique[,-1], by = list(zero_25_finder_unique$year), FUN = function(x) sum(x >= 0.25, na.rm = TRUE))

plot(yearly_zero_25_dup_count$Group.1, yearly_zero_25_dup_count$daymet_file_83_374, type = "l", col = "red")
plot(yearly_zero_25_dup_count$Group.1, yearly_zero_25_dup_count$prism_file_83_374, type = "l", col = "blue")
plot(yearly_zero_25_dup_count$Group.1, yearly_zero_25_dup_count$livneh_file_83_374, type = "l", col = "black")
plot(yearly_zero_25_dup_count$Group.1, yearly_zero_25_dup_count$nclim_file_83_374, type = "l", col = "green")
plot(yearly_zero_25_dup_count$Group.1, yearly_zero_25_dup_count$metadata_file_83_374, type = "l", col = "pink")

# # of dry days in unique outlier 
plot(yearly_zero_25_uni_count$Group.1, yearly_zero_25_uni_count$nclim_file_83_376, type = "l", col = "green")
plot(yearly_zero_25_uni_count$Group.1, yearly_zero_25_uni_count$metadata_file_83_376, type = "l", col = "pink")

# cor_matrix <- cor(zero_finder_duplicate[,-1], use = "complete.obs")
# corrplot(cor_matrix, method = "circle")

# remove non numeric time column 
duplicate_data_numeric <- duplicate_outliers[, sapply(duplicate_outliers, is.numeric)]
unique_data_numeric <- unique_outliers[, sapply(unique_outliers, is.numeric)]

precip_greater_equal = c(1, 5, 10, 20, 30, 50) # 0.25, 1, 5, 10, 20, 30 these were the original script calculations
# 0.25 to 1 havent been counted

count_exact_0 = colSums(duplicate_data_numeric == 0, na.rm = TRUE)
count_exact_0.25 = colSums(duplicate_data_numeric >= 0.25, na.rm = TRUE)

count_exact_0_uni = colSums(unique_data_numeric == 0, na.rm = TRUE)
count_exact_0.25_uni = colSums(unique_data_numeric >= 0.25, na.rm = TRUE)

####################################### sums for ratios 
# loop ratio for all values

count_exact_uni <- list()
count_exact_dup <- list()
ratio_uni <- list()
ratio_dup <- list()

results_df_dup <- data.frame(
  Threshold = numeric(),
  Source = character(),
  Ratio_Dup = numeric(),
  stringsAsFactors = FALSE
)

results_df_uni <- data.frame(
  Threshold = numeric(),
  Source = character(),
  Ratio_Uni = numeric(),
  stringsAsFactors = FALSE
)

for (threshold in precip_greater_equal){
  count_exact_uni[[as.character(threshold)]] = colSums(unique_data_numeric >= threshold, na.rm = TRUE)
  count_exact_dup[[as.character(threshold)]] = colSums(duplicate_data_numeric >= threshold, na.rm = TRUE)
  
  ratio_uni[[as.character(threshold)]] = (count_exact_uni[[as.character(threshold)]] / count_exact_0.25_uni) * 100
  ratio_dup[[as.character(threshold)]] = (count_exact_dup[[as.character(threshold)]] / count_exact_0.25) * 100
  
  #print(paste("Threshold: \n", threshold, "\n"))
  #print(ratio_uni[[as.character(threshold)]])
  #print(ratio_dup[[as.character(threshold)]])
  
  for (source in names(ratio_uni[[as.character(threshold)]])){
    results_df_uni <- rbind(
      results_df_uni,
      data.frame(
        Threshold = threshold,
        Source = source,
        Ratio_Uni = ratio_uni[[as.character(threshold)]][source]
      )
    )
  }
  
  for (source in names(ratio_dup[[as.character(threshold)]])){
    results_df_dup <- rbind(
      results_df_dup,
      data.frame(
        Threshold = threshold,
        Source = source,
        Ratio_Dup = ratio_dup[[as.character(threshold)]][source]
        )
      )
  }
}

total_count_exact_0.25 = colSums(duplicate_data_numeric >= 0.25, na.rm = TRUE)
total_count_exact_0 = colSums(duplicate_data_numeric == 0, na.rm = TRUE)

total_count_exact_0.25_uni = colSums(unique_data_numeric >= 0.25, na.rm = TRUE)
total_count_exact_0_uni = colSums(unique_data_numeric == 0, na.rm = TRUE)

total_count_exact_40 = colSums(duplicate_data_numeric >= 40, na.rm = TRUE)
total_count_exact_40_uni = colSums(unique_data_numeric >= 40, na.rm = TRUE)

ratio_dup = (total_count_exact_40 / total_count_exact_0.25)*100
ratio_dup =data.frame(ratio_dup)

ratio_uni = (total_count_exact_40_uni / total_count_exact_0.25_uni)*100
ratio_uni =data.frame(ratio_uni)

dry_days_dup = (total_count_exact_0 / total_count_exact_0.25)*100
dry_days_uni = (total_count_exact_0_uni / total_count_exact_0.25_uni)*100

ratio_dup$identifier <- str_extract(rownames(ratio_dup), "_\\d+_\\d+$")
ratio_dup$source <- sub("_\\d+_\\d+$", "", rownames(ratio_dup))
unique_ids <- unique(ratio_dup$identifier)

for (id in unique_ids){
  plot_data <- subset(ratio_dup, identifier == id)
  
  p <- ggplot(plot_data, aes(x = source, y = ratio_dup)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = paste("Identifier:", id), x = "Gridded Dataset", y = "(Days >= 40 / Days >= 0.25) %")
  
  print(p)
}

# counts for days > set threshold
counts_duplicate_outliers <- sapply(precip_greater_equal, function(thresh) colSums(duplicate_data_numeric >= thresh, na.rm = TRUE))
counts_unique_outliers <- sapply(precip_greater_equal, function(thresh) colSums(unique_data_numeric >= thresh, na.rm = TRUE))

duplicate_counted_days <- data.frame(
  gridded_set = c("daymet", "prism", "livneh", "nclim", "metadata"), 
  days_0 = count_exact_0 / 33,
  days_0.25 = count_exact_0.25 / 33,
  counts_duplicate_outliers / 33
)

unique_counted_days <- data.frame(
  gridded_set = c("daymet", "prism", "livneh", "nclim", "metadata"), 
  days_0 = count_exact_0_uni / 33,
  days_0.25 = count_exact_0.25_uni / 33,
  counts_unique_outliers / 33
)

colnames(duplicate_counted_days)[-1] <- c("days_0", "days_025", paste0("days_", precip_greater_equal))
colnames(unique_counted_days)[-1] <- c("days_0", "days_025", paste0("days_", precip_greater_equal))

print(duplicate_counted_days)
print(unique_counted_days)

duplicate_counted_days$identifier <- sub(".*_(\\d{2}_\\d{3})$", "\\1", rownames(duplicate_counted_days))

duplicate_counted_days_long <- duplicate_counted_days %>%
  gather(key = "days", value = "value", days_0, days_025, days_1, days_5, days_10, days_20) %>%
  separate(gridded_set, into = c("gridded_set", "file_id"), sep = "_file") %>%
  mutate(days = factor(days, levels = c("days_0", "days_025", "days_1", "days_5", "days_10", "days_20")))

ggplot(duplicate_counted_days_long, aes(x = days, y = value, color = gridded_set, group = interaction(identifier, gridded_set))) +
  geom_line() + 
  geom_point() + 
  facet_wrap(~ identifier) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Comparison of Days by Gridded Set",
       x = "Day Thresholds",
       y = "Count",
       color = "Gridded Set")

#################################### plot simple time series 
plot(duplicate_outliers$time, duplicate_outliers$daymet_file_83_374, type = "s", col = "red", ylim = c(0, 225))
lines(duplicate_outliers$time, duplicate_outliers$prism_file_83_374, col = "blue")
lines(duplicate_outliers$time, duplicate_outliers$livneh_file_83_374, col = "black")
lines(duplicate_outliers$time, duplicate_outliers$nclim_file_83_374, col = "green")
lines(duplicate_outliers$time, duplicate_outliers$metadata_file_83_374, col = "pink")

plot(duplicate_outliers$time, rollmean(duplicate_outliers$daymet_file_83_374, k = 250, fill = NA), type = "s", col = "red", ylim = c(0, 13))
lines(duplicate_outliers$time, rollmean(duplicate_outliers$prism_file_83_374, k = 250, fill = NA), col = "blue")
lines(duplicate_outliers$time, rollmean(duplicate_outliers$livneh_file_83_374, k = 250, fill = NA), col = "black")
lines(duplicate_outliers$time, rollmean(duplicate_outliers$nclim_file_83_374, k = 250, fill = NA), col = "green")
lines(duplicate_outliers$time, rollmean(duplicate_outliers$metadata_file_83_374, k = 250, fill = NA), col = "pink")
legend("topright", legend = c("Daymet", "PRISM", "Livneh", "NClim", "Metadata"), 
        col = c("red", "blue", "green", "black", "pink"), lty = 1, cex = 0.8)

#### Time series decomp
# duplicate_outliers_ts <- ts(duplicate_outliers[,-1], frequency = 12, start = c(1981, 1, 1))
# duplicate_outliers_ts <- as.zoo(duplicate_outliers_ts)
# plot(diff(duplicate_outliers_ts$daymet_file_83_374, differences = 2))

# grouped barplot 
# monthly precipitation

duplicate_outliers$time <- as.Date(duplicate_outliers$time)
duplicate_outliers$yearmonth <- format(duplicate_outliers$time, "%Y-%m")
duplicate_outliers$month <- format(duplicate_outliers$time, "%m")

monthly_agg <- duplicate_outliers %>% 
  group_by(yearmonth) %>%
  summarise(
    month = first(month),
    daymet = sum(daymet_file_83_374, na.rm = TRUE),
    prism = sum(prism_file_83_374, na.rm = TRUE),
    livneh = sum(livneh_file_83_374, na.rm = TRUE),
    nclim = sum(nclim_file_83_374, na.rm = TRUE),
    metadata = sum(metadata_file_83_374, na.rm = TRUE)
  ) %>%
  ungroup()

monthly_avg <- monthly_agg %>%
  group_by(month) %>%
  summarise(
    daymet = sum(daymet, na.rm = TRUE) / 33,
    prism = sum(prism, na.rm = TRUE) / 33,
    livneh = sum(livneh, na.rm = TRUE) / 33,
    nclim = sum(nclim, na.rm = TRUE) / 33,
    metadata = sum(metadata, na.rm = TRUE) / 33
    ) %>%
  ungroup()

monthly_long <- monthly_avg %>%
  pivot_longer(cols = -month, names_to = "Dataset", values_to = "Precipitation")

ggplot(monthly_long, aes(x = month, y = Precipitation, fill = Dataset)) + 
  geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Monthly Precipitation Comparison", x = "Month", y = "Precipitation") + 
  scale_fill_manual(values = c("red", "blue", "black", "green", "pink")) 

# days of rain per month 
duplicate_outliers$time <- as.Date(duplicate_outliers$time)
duplicate_outliers$yearmonth <- format(duplicate_outliers$time, "%Y-%m")
duplicate_outliers$month <- format(duplicate_outliers$time, "%m")

days_greater_equal = c(0.25, 1, 5, 10, 20, 30, 50)

for (threshold in days_greater_equal) {
  threshold_data <- duplicate_outliers %>%
    mutate(
      daymet = daymet_file_83_374 >= threshold,
      prism = prism_file_83_374 >= threshold,
      livneh = livneh_file_83_374 >= threshold,
      nclim = nclim_file_83_374 >= threshold,
      metadata = metadata_file_83_374 >= threshold
    ) %>%
    group_by(yearmonth) %>%
    summarise(
      month = first(month),
      daymet = sum(daymet, na.rm = TRUE),
      prism = sum(prism, na.rm = TRUE),
      livneh = sum(livneh, na.rm = TRUE),
      nclim = sum(nclim, na.rm = TRUE),
      metadata = sum(metadata, na.rm = TRUE)
    ) %>%
    ungroup()
  
  days_monthly_avg <- threshold_data %>%
    group_by(month) %>%
    summarise(
      daymet = mean(daymet, na.rm = TRUE),
      prism = mean(prism, na.rm = TRUE),
      livneh = mean(livneh, na.rm = TRUE),
      nclim = mean(nclim, na.rm = TRUE),
      metadata = mean(metadata, na.rm = TRUE)
    ) %>%
    ungroup()
  
  days_monthly_long <- days_monthly_avg %>% 
    pivot_longer(cols = -month, names_to = "Dataset", values_to = "Days of Rain")
  
  print(ggplot(days_monthly_long, aes(x = month, y = `Days of Rain`, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "dodge") + theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = paste("Monthly Days of Rain (\u2265", threshold, "mm)", sep = ""), x = "Month", y = "Days of Rain") + 
    scale_fill_manual(values = c("red", "blue", "black", "green", "pink")))
}







