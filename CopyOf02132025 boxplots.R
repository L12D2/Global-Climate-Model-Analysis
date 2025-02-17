library(ggridges)
library(ggplot2)

############### full climatology #################

layout(matrix(1:1, ncol = 1))
par(oma=c(0,0,0,0), mar=c(2.5, 5.5, 4, 1.5))

data <- list(
  daymet = final_climatology$daymet_file,
  prism = final_climatology$prism_file,
  livneh = final_climatology$livneh_file,
  nclim = final_climatology$nclim_file,
  metadata = final_climatology$metadata_file
)

datasets <- list("daymet_file", "prism_file", "livneh_file", "nclim_file", "metadata_file")

boxplot(data, 
        main = "Gridded Data Comparison (1981-2014)",
        # ylim = c(0), 
        names = c("Daymet", "Prism", "Livneh", "Nclim", "Training"), 
        ylab = "Climatological Precip")

means <- sapply(data, mean, na.rm = TRUE)  
points(1:length(means), means, col = "red", pch = 19, cex = 1.2)

############### monthly climatology #################

months <- c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

data_monthly <- list(
  daymet = final_monthly_climatology$daymet_file,
  prism = final_monthly_climatology$prism_file,
  livneh = final_monthly_climatology$livneh_file,
  nclim = final_monthly_climatology$nclim_file,
  metadata = final_monthly_climatology$metadata_file
)

dataset_name <- c("daymet", "prism", "livneh", "nclim", "metadata")

#par(mfrow=c(3,4), mar=c(3, 3, 2, 0.5))

for (month in months){
  file_name <- paste0(output_folder, "/", month, "_boxplot.png")
  png(file_name, width = 800, height = 600)
  monthly_data <- lapply(dataset_name, function(dataset) data_monthly[[dataset]][[month]])
  names(monthly_data) <- dataset_name
  boxplot(monthly_data, main=paste(month), names = dataset_name)
  means <- sapply(monthly_data, function(data) mean(data, na.rm = TRUE))  
  points(1:length(means), means, col = "red", pch = 19, cex = 1.2)
  
  dev.off()
}

############### seasonal climatology #################

seasons <- c("DJF", "MAM", "JJA", "SON")

data_seasonal <- list(
  daymet = final_seasonal_climatology$daymet_file,
  prism = final_seasonal_climatology$prism_file,
  livneh = final_seasonal_climatology$livneh_file,
  nclim = final_seasonal_climatology$nclim_file,
  metadata = final_seasonal_climatology$metadata_file
)

for (season in seasons){
  seasonal_data <- lapply(dataset_name, function(dataset){
    data <- data_seasonal[[dataset]][[season]]
    
    data[data==0] <- NA
    return(data)
  }) 
  
  file_name <- paste0(output_folder, "/", season, "_boxplot.png")
  png(file_name, width = 800, height = 600)
  
  boxplot(seasonal_data, main = paste(season), names = dataset_name)
  means <- sapply(seasonal_data, function(data) mean(data, na.rm = TRUE))
  points(1:length(means), means, col = "red", pch = 19, cex = 1.2)
  
  dev.off()
}

############### rainy day climatology #################

load("~/daily_climatology_results.RData")

thresholds <- list("0.25", "1", "5", "10", "20", "30")

for (dataset in datasets){
  for (threshold in thresholds){
    rain_1mm_daily <- lapply(datasets, function(dataset) daily_combined[[threshold]][["annual"]][[dataset]])
    
    file_name <- paste0(output_folder, "/", threshold, "_boxplot.png")
    png(file_name, width = 800, height = 600)
    
    boxplot(rain_1mm_daily, main = paste(threshold,":","Days of Rain / yr (1981-2014)"), names = dataset_name)
    means <- sapply(rain_1mm_daily, function(data) mean(data, na.rm = TRUE))
    points(1:length(means), means, col = "red", pch = 19, cex = 1.2)
    dev.off()
  }
  #print(rain_1mm_daily)
}

############### seasonal rainy climatology #################
load("~/daily_climatology_results.RData")
seasons <- c("DJF", "MAM", "JJA", "SON")
thresholds <- list("0.25", "1", "5", "10", "20", "30")

for (threshold in thresholds){
  for (season in seasons){
    daily_seasonal_data <- list()
    
    for (dataset in datasets){
      data <- daily_combined[[threshold]][["seasonal"]][[dataset]][[season]]
      data[data == 0] <- NA
      daily_seasonal_data[[dataset]] <- data
    }
    
    file_name <- paste0(output_folder, "/", season, threshold, "_boxplot.png")
    png(file_name, width = 800, height = 600)
  
    boxplot(daily_seasonal_data, main = paste(season, threshold,":","Days of Rain / yr (1981-2014)"), names = dataset_name)
    means <- sapply(daily_seasonal_data, function(data) mean(data, na.rm = TRUE))
    points(1:length(means), means, col = "red", pch = 19, cex = 1.2)
    
    dev.off()
  }
}

############### seasonal rainy climatology #################
load("~/daily_climatology_results.RData")
months <- c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
thresholds <- list("0.25", "1", "5", "10", "20", "30")

for (threshold in thresholds){
  for (month in months){
    daily_seasonal_monthly_data <- list()
    
    for (dataset in datasets){
      data <- daily_combined[[threshold]][["monthly"]][[dataset]][[month]]
      data[data == 0] <- NA
      daily_seasonal_monthly_data[[dataset]] <- data
    }
    
    file_name <- paste0(output_folder, "/", month, threshold, "_boxplot.png")
    png(file_name, width = 800, height = 600)
    
    boxplot(daily_seasonal_monthly_data, main = paste(month, threshold,":","Days of Rain / month (1981-2014)"), names = dataset_name)
    means <- sapply(daily_seasonal_monthly_data, function(data) mean(data, na.rm = TRUE))
    points(1:length(means), means, col = "red", pch = 19, cex = 1.2)
    
    dev.off()
  }
}


