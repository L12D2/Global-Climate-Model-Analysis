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
  monthly_data <- lapply(dataset_name, function(dataset) data_monthly[[dataset]][[month]])
  names(monthly_data) <- dataset_name
  boxplot(monthly_data, main=paste(month), names = dataset_name)
  means <- sapply(monthly_data, function(data) mean(data, na.rm = TRUE))  
  points(1:length(means), means, col = "red", pch = 19, cex = 1.2)
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
  
  boxplot(seasonal_data, main = paste(season), names = dataset_name)
  means <- sapply(seasonal_data, function(data) mean(data, na.rm = TRUE))
  points(1:length(means), means, col = "red", pch = 19, cex = 1.2)
}

# priors scripts that handle rainy day calculations need further debugging
############### rainy day climatology #################

load("~/daily_climatology_results.RData")

for (dataset in datasets){
  rain_1mm_daily <- lapply(datasets, function(dataset) final_climatology[[dataset]])
  print(rain_1mm_daily)
  boxplot(rain_1mm_daily, main = paste("Days of Rain / yr (1981-2014)"), names = dataset_name)
  means <- sapply(rain_1mm_daily, function(data) mean(data, na.rm = TRUE))
  points(1:length(means), means, col = "red", pch = 19, cex = 1.2)
}

############### seasonal rainy climatology #################

for (season in seasons){
  daily_seasonal_data <- lapply(datasets, function(dataset) rain_1mm_seasonal_climate_processed$results[[dataset]][[season]])
  boxplot(daily_seasonal_data, main = paste(season), names = dataset_name)
  means <- sapply(rain_1mm_seasonal_climate_processed, function(data) mean(data, na.rm = TRUE))
  points(1:length(means), means, col = "red", pch = 19, cex = 1.2)
}
