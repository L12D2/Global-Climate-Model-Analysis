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

# Seasonal outliers 
seasons <- c("DJF", "MAM", "JJA", "SON")

for (dataset in gridded_data){
  for (season in seasons){
    climatology_output <- final_seasonal_climatology[[dataset]][[season]]
    if (dataset == "daymet_file"){
      climatology_output <- ifelse(mask == 1, final_seasonal_climatology[["daymet_file"]][[season]], NA)
    }
    climatology_output[climatology_output==0] <- NA
    
    cat("dataset:", dataset, "\nSeason:", season)
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
      season = season,
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

# add a factor class for the season
output_folder <- "~/03042025_mask Statistical Analysis/maskOutlierMaps/Seasonal Maps"

conus <- map_data("state")
sc_casc <- subset(conus, region %in% c("utah", "arizona", "new mexico", "colorado",
                                       "kansas", "nebraska", "iowa", "wyoming", "oklahoma","idaho",
                                       "texas", "missouri", "arkansas", "louisiana"))

map <- ggplot() + geom_polygon(data = sc_casc, aes(x = long, y = lat, group = group),fill = "white", color = "black") + coord_fixed(1.3)

file_name <- paste0(output_folder, "/", "seasonal", "_map.png")
png(file_name, width = 800, height = 600)

map + geom_point(data = map_df, aes(x = lon, y = lat, color = dataset), size = 0.5) + theme_minimal() +
  labs(x = "Longitude", y = "Latitude", title = "Outliers from Gridded Datasets", color = "Outliers in:")+
  facet_wrap(~season + dataset)

dev.off()

seasonal_outliers <- list()

for (season in unique(map_df$season)){
  season_df <- map_df[map_df$season == season, ]
  
  duplicate_indices <- season_df[duplicated(season_df[, c("lat", "lon")]) | duplicated(season_df[, c("lat", "lon")], fromLast = TRUE), ]
  unique_indices <- season_df[!(duplicated(season_df[, c("lat", "lon")]) | duplicated(season_df[, c("lat", "lon")], fromLast = TRUE)), ]
  
  seasonal_outliers[[season]] <- list(duplicate_indices = duplicate_indices, unique_indices = unique_indices)
}
gc()


