library(ggridges)
library(ggplot2)

############### full climatology #################

layout(matrix(1:1, ncol = 1))
par(oma=c(0,0,0,0), mar=c(2.5, 5.5, 4, 1.5))

mask_nc = nc_open("")
mask = ncvar_get(mask_nc, "mask")

masked_daymet <- ifelse(mask == 1, final_climatology$daymet_file, NA)

data <- list(
  daymet = masked_daymet,
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

output_folder <- "~/03042025_mask Statistical Analysis/maskMonthly Precip Boxplots"
months <- c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

masked_monthly_daymet <- list()

for (month in months){
  masked_monthly_daymet[[month]] <- ifelse(mask == 1, final_monthly_climatology[["daymet_file"]][[month]], NA)  
}

data_monthly <- list(
  daymet = masked_monthly_daymet,
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
output_folder <- "~/03042025_mask Statistical Analysis/maskSeasonal Precip Boxplots"

masked_seasonal_daymet <- list()

for (season in seasons){
  masked_seasonal_daymet[[season]] <- ifelse(mask == 1, final_seasonal_climatology[["daymet_file"]][[season]], NA)
}

data_seasonal <- list(
  daymet = masked_seasonal_daymet,
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

# priors scripts that handle rainy day calculations need further debugging
############### rainy day climatology #################

load("~/daily_climatology_results.RData")
output_folder <- "~/03042025_mask Statistical Analysis/maskDays of Rain Boxplots"

thresholds <- list("0.25", "1", "5", "10", "20", "30")

for (dataset in datasets){
  for (threshold in thresholds){
    rain_1mm_daily <- lapply(datasets, function(dataset) {
      if (dataset == "daymet_file"){
        return(ifelse(mask == 1, daily_combined[[threshold]][["annual"]][[dataset]], NA))
      } else {
        return(daily_combined[[threshold]][["annual"]][[dataset]])
      }
    }) 
    
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
output_folder <- "~/03042025_mask Statistical Analysis/maskDays of Rain Seasonal Boxplots"

for (threshold in thresholds){
  for (season in seasons){
    daily_seasonal_data <- list()
    
    for (dataset in datasets){
      data <- daily_combined[[threshold]][["seasonal"]][[dataset]][[season]]
      data[data == 0] <- NA
      
      if (dataset == "daymet_file"){
        data <- ifelse(mask == 1, data, NA)
      }
      
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

############### Monthly rainy climatology #################
load("~/daily_climatology_results.RData")
months <- c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
thresholds <- list("0.25", "1", "5", "10", "20", "30")
output_folder <- "~/03042025_mask Statistical Analysis/maskDays of Rain Monthly Boxplots"

for (threshold in thresholds){
  for (month in months){
    daily_seasonal_monthly_data <- list()
    
    for (dataset in datasets){
      data <- daily_combined[[threshold]][["monthly"]][[dataset]][[month]]
      data[data == 0] <- NA
      
      if (dataset == "daymet_file"){
        data <- ifelse(mask == 1, data, NA)
      }
      
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


