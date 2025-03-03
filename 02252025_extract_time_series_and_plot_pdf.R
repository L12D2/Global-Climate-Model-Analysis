
###################################################
# This function relies on the previous mapping tools to get all aspects to work. Needs to be debugged 
# / remove the reliance...
###################################################

# quick calculations
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ncdf4)
library(lubridate)

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
# in dup uindices make sure you know which one is an outlier... 

extract_time_series <- function(row, col, file_pull, varname){
  # grab dates & relevant time info
  info <- infofinder(file_pull, varname)
  filedate <- info$filedate
  calendar <- info$calendar
  units_time <- info$units_time
  
  nc_file <- nc_open(file_pull)
  time_series <- ncvar_get(nc_file, varname, start = c(row, col, 1), count = c(1, 1, -1))
  times <- filedate
  times <- substr(times, 1, 10) 
  
  nc_close(nc_file)
  
  df <- data.frame(
    time = times,  
    precip = as.vector(time_series)*86400  
  )
  return(df)
}

final_extract_df <- NULL
unique_final_extract_df <- NULL

for (i in 1:nrow(duplicate_indices)){ # will just need to change duplicate indces / unique indices.
  row <- duplicate_indices[i, "row"]
  col <- duplicate_indices[i, "col"]
  
  extracted_datasets <- list()
  
  for (dataset in names(train_data)){
    print(paste0("proc. dataset, row, & col:", dataset, row, col))
    file_pull <- train_data[[dataset]]
    extract <- extract_time_series(row, col, file_pull, "pr")
    extract$time <- as.POSIXct(extract$time, format = "%Y-%m-%d")
    col_name <- paste0(dataset, "_", row, "_", col)
    colnames(extract)[2] <- col_name
    
    extracted_datasets[[dataset]] <- extract 
  }
  
  for (dataset in names(extracted_datasets)){
    print(paste0("extracting and merging I think"))
    if (is.null(final_extract_df)){
      final_extract_df <- extracted_datasets[[dataset]]
    } else{
      final_extract_df <- merge(
        final_extract_df,
        extracted_datasets[[dataset]],
        by = "time", all = TRUE,
        suffixes = c("", "")
        #suffixes = c("", paste0("_dup", row, col)) # needs to be here for the duplicate indices because there are indices that "duplicate"
      )
    }
  }  
}

########################################################

for (i in 1:nrow(unique_indices)){ # will just need to change duplicate indces / unique indices.
  row <- unique_indices[i, "row"]
  col <- unique_indices[i, "col"]
  
  extracted_datasets <- list()
  
  for (dataset in names(train_data)){
    print(paste0("proc. dataset, row, & col:", dataset, row, col))
    file_pull <- train_data[[dataset]]
    extract <- extract_time_series(row, col, file_pull, "pr")
    extract$time <- as.POSIXct(extract$time, format = "%Y-%m-%d")
    col_name <- paste0(dataset, "_", row, "_", col)
    colnames(extract)[2] <- col_name
    
    extracted_datasets[[dataset]] <- extract 
  }
  
  for (dataset in names(extracted_datasets)){
    print(paste0("extracting and merging I think"))
    if (is.null(unique_final_extract_df)){
      unique_final_extract_df <- extracted_datasets[[dataset]]
    } else{
      unique_final_extract_df <- merge(
        unique_final_extract_df,
        extracted_datasets[[dataset]],
        by = "time", all = TRUE
      )
    }
  }
}

gc()

# NOTE DIFFERENT YEAR STARTS.... 
final_extract_df <- final_extract_df[format(final_extract_df$time, "%Y") != "1980", ]
unique_final_extract_df <- unique_final_extract_df[format(unique_final_extract_df$time, "%Y") != "1980", ]

head(final_extract_df)

# remove duplicate column nams in final extract df 
rep_final_extract_df <- final_extract_df
duplicates <- duplicated(names(rep_final_extract_df))
rep_final_extract_df <- rep_final_extract_df[, !(duplicates & names(rep_final_extract_df) != "time")]
#colnames(rep_final_extract_df)

head(rep_final_extract_df) # note the copy of the dataframe
head(unique_final_extract_df)

############################# Duplicate Indices Plotting 
output_folder <- "~/02132025 Statistical Analysis/Duplicate Indice PDF "
output_folder2 <- "~/02132025 Statistical Analysis/Duplicate Indice Histogram"
output_folder3 <- "~/02132025 Statistical Analysis/Duplicate Indice CDF"

# get rid of 0s
dropped_0_dup <- rep_final_extract_df
dropped_0_dup[dropped_0_dup == 0] <- NA
dropped_0_dup <- dropped_0_dup %>%
  mutate_all(~ ifelse(. == 0 | is.nan(.), NA, .)) # need to remove NaN 

dropped_0_uni <- unique_final_extract_df
dropped_0_uni[dropped_0_uni == 0] <- NA
dropped_0_uni <- dropped_0_uni %>%
  mutate_all(~ ifelse(. == 0 | is.nan(.), NA, .))

# reformat the data 
# Duplicate indices (e.g. where datasets shared outliers)
dropped_0_long <- dropped_0_dup %>%
  pivot_longer(cols = -time, names_to = "variable", values_to = "value")  

dropped_0_long <- dropped_0_long %>% 
  mutate(suffix = str_extract(variable, "[0-9]+_[0-9]+$"))

unique_suffixes <- unique(dropped_0_long$suffix)

# marking lat / lon & which indices was the "outlier" 
mark_duplicate_indices <- duplicate_indices # note that i made a copy of the duplicate indices variable

for (sufx in unique_suffixes){
  sufx_parts <- strsplit(sufx, "_")[[1]]
  row <- as.integer(sufx_parts[1])
  col <- as.integer(sufx_parts[2])
  
  filtered_data <- dropped_0_long %>% filter(suffix == sufx)
  
  # marking the original outliers 
  matching_indices <- mark_duplicate_indices[mark_duplicate_indices$row == row & mark_duplicate_indices$col == col, ]
  for (i in 1:nrow(matching_indices)) {
    duplicate_legend_labels <- c(legend_labels, paste0(matching_indices$dataset[i], "_", row, "_", col, "*"))
  }
  
  # plot title 
  plot_title <- paste0("Density Plot of Duplicate Indices",
                       "\nLat: ", matching_indices$lat, "Lon:", matching_indices$lon)
  
  plot <- ggplot(filtered_data, aes(x = value, color = variable)) + 
    geom_density(na.rm = TRUE) + 
    labs(title = plot_title, 
         x = "Precip value [mm]", y = "Density") +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    guides(color = guide_legend(title = "Datasets:")) 
  
  plot2 <- ggplot(filtered_data, aes(x = value, color = variable)) + 
    geom_histogram(binwidth = 0.75, na.rm = TRUE) + 
    labs(title = plot_title, 
         x = "Precip value [mm]", y = "Density") +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    guides(color = guide_legend(title = "Datasets:")) 
  
  plot3 <- ggplot(filtered_data, aes(x = value, color = variable)) + 
    stat_ecdf(geom = "step", na.rm = TRUE) + 
    labs(title = plot_title, 
         x = "Precip value [mm]", y = "Density") +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    guides(color = guide_legend(title = "Datasets:")) 
  
  file_name <- paste0(output_folder, "/", sufx, "_PDF.png")
  file_name2 <- paste0(output_folder2, "/", sufx, "_HIST.png")
  file_name3 <- paste0(output_folder3, "/", sufx, "_CDF.png")
  
  # save output 
  # png
  # print()
  # dev . off() 
  # required to save plot. 
  
  png(file_name, width = 800, height = 600)
  print(plot)
  dev.off() 
  
  png(file_name2, width = 800, height = 600)
  print(plot2)
  dev.off()
  
  png(file_name3, width = 800, height = 600)
  print(plot3)
  dev.off()
  
  #print(plot)
  
  print(plot)
  print(plot2)
  print(plot3)
  
  rm(plot, plot2, plot3, filtered_data)
  gc()
}

duplicate_legend_labels

############################# Unique Indices Plotting 

unique_output_folder <- "~/02132025 Statistical Analysis/Unique Indice PDF"
unique_output_folder2 <- "~/02132025 Statistical Analysis/Unique Indice Histogram"
unique_output_folder3 <- "~/02132025 Statistical Analysis/Unique Indice CDF"

# get rid of 0s
# done above ^^^^ 

# reformat the data 
# Duplicate indices (e.g. where datasets shared outliers)
dropped_0_long_uni <- dropped_0_uni %>%
  pivot_longer(cols = -time, names_to = "variable", values_to = "value")  

dropped_0_long_uni <- dropped_0_long_uni %>% 
  mutate(suffix = str_extract(variable, "[0-9]+_[0-9]+$"))

unique_suffixes_uni <- unique(dropped_0_long_uni$suffix)

# marking lat / lon & which indices was the "outlier" 
mark_unique_indices <- unique_indices # note that i made a copy of the unique indices variable

for (sufx in unique_suffixes_uni){
  sufx_parts <- strsplit(sufx, "_")[[1]]
  row <- as.integer(sufx_parts[1])
  col <- as.integer(sufx_parts[2])
  
  filtered_data <- dropped_0_long_uni %>% filter(suffix == sufx)
  
  # plot title 
  plot_title <- paste0("Density Plot of Unique Indices",
                       "\nLat: ", matching_indices_uni$lat, "Lon:", matching_indices_uni$lon)
  
  plot <- ggplot(filtered_data, aes(x = value, color = variable)) + 
    geom_density(na.rm = TRUE) + 
    labs(title = plot_title, 
         x = "Precip value [mm]", y = "Density") +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    guides(color = guide_legend(title = "Datasets:"))
  
  plot2 <- ggplot(filtered_data, aes(x = value, color = variable)) + 
    geom_histogram(binwidth = 0.75, na.rm = TRUE) + 
    labs(title = plot_title, 
         x = "Precip value [mm]", y = "Density") +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    guides(color = guide_legend(title = "Datasets:")) 
  
  plot3 <- ggplot(filtered_data, aes(x = value, color = variable)) + 
    stat_ecdf(geom = "step", na.rm = TRUE) + 
    labs(title = plot_title, 
         x = "Precip value [mm]", y = "Density") +
    theme_minimal() + 
    theme(legend.position = "bottom") +
    guides(color = guide_legend(title = "Datasets:")) 
  
  # save output 
  file_name <- paste0(unique_output_folder, "/", sufx, "_PDF.png")
  file_name2 <- paste0(unique_output_folder2, "/", sufx, "_HIST.png")
  file_name3 <- paste0(unique_output_folder3, "/", sufx, "_CDF.png")
  
  png(file_name, width = 800, height = 600)
  print(plot)
  dev.off()
  
  png(file_name2, width = 800, height = 600)
  print(plot2)
  dev.off()
  
  png(file_name3, width = 800, height = 600)
  print(plot3)
  dev.off()
  
  print(plot)
  print(plot2)
  print(plot3)
  
  rm(plot, plot2, plot3, filtered_data)
  gc()
}










