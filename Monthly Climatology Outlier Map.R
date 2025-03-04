library(dplyr)
library(ncdf4)

# directories 
train_data <- list(
)

# calculate outliers 
gridded_data <- list(
  "daymet_file",
  "prism_file",
  "livneh_file",
  "nclim_file",
  "metadata_file"
)

outlier_indices_list <- list()
location_outliers <- list()
map_df <- data.frame()

# Monthly outliers 
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for (dataset in gridded_data){
  for (month in months){
    climatology_output <- final_monthly_climatology[[dataset]][[month]]
    if (dataset == "daymet_file"){
      climatology_output <- ifelse(mask == 1, final_monthly_climatology[["daymet_file"]][[month]], NA)
    }
    #climatology_output[climatology_output==0] <- NA
    
    cat("dataset:", dataset, "\nMonth:", month)
    #print(climatology_output)
    
    q1 <- quantile(climatology_output, 0.25, na.rm = TRUE)
    q3 <- quantile(climatology_output, 0.75, na.rm = TRUE)
    iqr <- q3 - q1 # IQR function does same thing. 
    
    lower_bound <- q1 - 1.5*iqr
    upper_bound <- q3 + 1.5*iqr
    
    outlier_indices <- matrix(NA, nrow = nrow(climatology_output), ncol = ncol(climatology_output))
    
    outlier_indices <- which(climatology_output < lower_bound | climatology_output > upper_bound, arr.ind = TRUE)
    outlier_indices_list[outlier_indices] <- climatology_output[outlier_indices]
    
    outlier_indices_list[[dataset]] <- outlier_indices
    
    if (length(outlier_indices) == 0) next
    
    print(paste("Outliers for:", dataset))
    #print(outlier_indices)
    
    # time to pair indices to lat lons
    # structure is alr in a loop, so no need to create another one.
    ncFile <- train_data[[dataset]]
    
    fileinfo <- infofinder(ncFile, varname = "pr")
    
    lat <- fileinfo$lat
    lon <- fileinfo$lon
    df_outlier_lat_lon <- data.frame(
      dataset = dataset,
      month = month,
      lat = lat[outlier_indices[,2]],
      lon = lon[outlier_indices[,1]]
    )
    
    #print(df_outlier_lat_lon)
    
    #location_outliers[[dataset]][[season]] <- df_outlier_lat_lon
    
    #print(paste("Dataset with outlier:", dataset))
    #print(df_outlier_lat_lon)
    
    map_df <- bind_rows(map_df, df_outlier_lat_lon)
    print(map_df)
    #sum(map_df$season == "DJF")
    #print(map_df[map_df$season == "SON", ]) calculations show that this checks out
  }
}

# Factor class variable the month  
map_df$month <- factor(map_df$month, levels = months)

conus <- map_data("state")
sc_casc <- subset(conus, region %in% c("utah", "arizona", "new mexico", "colorado",
                                       "kansas", "nebraska", "iowa", "wyoming", "oklahoma","idaho",
                                       "texas", "missouri", "arkansas", "louisiana"))

map <- ggplot() + geom_polygon(data = sc_casc, aes(x = long, y = lat, group = group),fill = "white", color = "black") + coord_fixed(1.3)

# map + geom_point(data = map_df, aes(x = lon, y = lat, color = dataset), size = 0.5) + theme_minimal() +
#   labs(x = "Longitude", y = "Latitude", title = "Outliers from Gridded Datasets", color = "Outliers in:")+
#   facet_wrap(~dataset, ncol = 4)

map + geom_point(data = map_df, aes(x = lon, y = lat, color = dataset), size = 0.5) + theme_minimal()

unique_map <- unique(map_df$dataset)
output_folder <- "~/03042025_mask Statistical Analysis/maskOutlierMaps/Monthly Maps"

for (dataset_map in unique_map){
  map_subset <- map_df %>% filter(dataset == dataset_map)
  plot <- map + 
    geom_point(data = map_subset, aes(x = lon, y = lat, color = dataset), size = 0.5) + 
    labs(x = "Longitude", y = "Latitude", title = paste("Outliers in", dataset_map), color = "Outliers in:") +
    facet_wrap(~month, ncol = 4)
  
  file_name <- paste0(output_folder, "/", dataset_map, "_map.png")
  png(file_name, width = 800, height = 600)
  
  print(plot)
  
  dev.off()
  rm()
  gc()
}

gc()
