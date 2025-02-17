
###################################################
# This function relies on the previous mapping tools to get all aspects to work. Needs to be debugged 
# / remove the reliance...
###################################################

# quick calculations
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# Possible file to pull from 
train_data <- list(
)

# file info finder 
infofinder <- function(filename, varname){
  nc = nc_open(filename)
  units_time = nc$var[[varname]]$dim[[3]]$units
  calendar = nc$var[[varname]]$dim[[3]]$calendar
  varunits = nc$var[[varname]]$units
  times = ncvar_get(nc, varid = "time")
  lon = ncvar_get(nc, varid = "lon")
  lat = ncvar_get(nc, varid = "lat")
  startdate1 = do.call("c", strsplit(units_time, " "))
  origindate = as.PCICt(startdate1[3], cal = calendar)
  filedate = origindate + times*86400
  years = unique(as.numeric(substr(filedate, 1, 4)))
  
  nc_close(nc)
  
  infofinder_list = list(varunits = varunits, units_time = units_time, times = times, 
                         calendar = calendar, filedate = filedate, years = years, 
                         lat = lat, lon = lon)
  return(infofinder_list)
}

duplicate_indices <- map_df[duplicated(map_df[, c("lat", "lon")]) | duplicated(map_df[, c("lat", "lon")], fromLast = TRUE), ]
unique_indices <- map_df[!(duplicated(map_df[, c("lat", "lon")]) | duplicated(map_df[, c("lat", "lon")], fromLast = TRUE)), ]

extract_time_series <- function(row, col, file_pull, varname){
  # grab dates & relevant time info
  info <- infofinder(file_pull, varname)
  filedate <- info$filedate
  units_time <- info$units_time
  calendar <- info$calendar
  times <- info$filedate
  times <- as.POSIXct(times, format = "%Y-%m-%d")
  
  nc_file <- nc_open(file_pull)
  time_non_con <- ncvar_get(nc_file, "time")
  
  startdate <- do.call("c", strsplit(units_time, " "))
  
  time_series <- ncvar_get(nc_file, varname, start = c(row, col, 1), count = c(1, 1, -1))  
  time_series <- time_series * 86400
  
  df <- data.frame(
    time = times,  
    precip = as.vector(time_series)  
  )
  
  nc_close(nc_file)
  return(df)
}

final_extract_df <- NULL

for (i in 1:nrow(duplicate_indices)){ # will just need to change duplicate indces / unique indices.
  row <- duplicate_indices[i, "row"]
  col <- duplicate_indices[i, "col"]
  dataset_name <- duplicate_indices[i, "dataset"]
  
  file_pull <- train_data[[dataset_name]]
  
  extract <- extract_time_series(row, col, file_pull, "pr")
  extract$time <- as.POSIXct(extract$time, format = "%Y-%m-%d")
  col_name <- paste0(dataset_name, "_", row, "_", col)
  colnames(extract)[2] <- col_name
  
  if(is.null(final_extract_df)){
    final_extract_df <- extract
  } else{
    final_extract_df <- merge(final_extract_df, extract, by = "time", all = TRUE)
    colnames(final_extract_df)[ncol(final_extract_df)] <- col_name
  }
}
# NOTE DIFFERENT YEAR STARTS.... 
final_extract_df <- final_extract_df[format(final_extract_df$time, "%Y") != "1980", ]

head(final_extract_df)

# get rid of 0s
dropped_0 <- final_extract_df
dropped_0[dropped_0 == 0] <- NA
dropped_0 <- dropped_0[complete.cases(dropped_0), ]

# reformat the data 
dropped_0_long <- dropped_0 %>%
  pivot_longer(cols = -time, names_to = "variable", values_to = "value")  

dropped_0_long <- dropped_0_long %>% 
  mutate(suffix = str_extract(variable, "[0-9]+_[0-9]+$"))

unique_suffixes <- unique(dropped_0_long$suffix)

for (sufx in unique_suffixes){
  filtered_data <- dropped_0_long %>% filter(suffix == sufx)
  
  plot <- ggplot(filtered_data, aes(x = value, color = variable)) + 
    geom_density() + 
    labs(title = 'Denisty Plots of Unique Indices', x = "Precip value [mm]", y = "Density") +
    theme_minimal() 
  print(plot)
}

