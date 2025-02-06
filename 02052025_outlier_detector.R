library(dplyr)
library(ncdf4)

# directories 
train_data <- list(
  daymet_file = "/ourdisk/hpc/southcentralcasc/auto_archive_notyet/tape_2copies/tmp/cdstore04/data/DS_proj/CPREP2/input/DAYMETv4_pr_1980-2014_CPREP2v2_5kmgridv2.nc",
  prism_file = "/ourdisk/hpc/southcentralcasc/auto_archive_notyet/tape_2copies/tmp/cdstore04/data/DS_proj/CPREP2/input/PRISM_pr_1980-2014_CPREP2v2_5kmgridv2.nc",
  livneh_file = "/ourdisk/hpc/southcentralcasc/auto_archive_notyet/tape_2copies/tmp/cdstore04/data/DS_proj/CPREP2/input/LIVNEHv2_pr_1980-2014_CPREP2v2_5kmgridv2.nc",
  nclim_file = "/ourdisk/hpc/southcentralcasc/auto_archive_notyet/tape_2copies/tmp/cdstore04/data/DS_proj/CPREP2/input/nClimGrid_pr_1980-2014_CPREP2v2_5kmgridv2.nc",
  metadata_file = "/ourdisk/hpc/southcentralcasc/auto_archive_notyet/tape_2copies/tmp/cdstore04/data/DS_proj/CPREP2/input/METDATA_pr_1980-2014_CPREP2v2_5kmgridv2.nc"
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

for (dataset in gridded_data){
  climatolology_output <- final_climatology[[dataset]]
  #print(climatolology_output)
  
  q1 <- quantile(climatolology_output, 0.25, na.rm = TRUE)
  q3 <- quantile(climatolology_output, 0.75, na.rm = TRUE)
  iqr <- q3 - q1 # IQR function does same thing. 
  
  lower_bound <- q1 - 1.5*iqr
  upper_bound <- q3 + 1.5*iqr
  
  outliers <- climatolology_output[climatolology_output < lower_bound | climatolology_output > upper_bound]
  outlier_indices <- which(climatolology_output < lower_bound | climatolology_output > upper_bound) # grabs indices of the outliers
  
  outlier_indices_list[[dataset]] <- outlier_indices
  print(paste("Outliers for:", dataset))
  print(outlier_indices)
  
  # time to pair indices to lat lons 
  for (ncFile in train_data){
    fileinfo <- infofinder(ncFile, varname = "pr")
    #print(fileinfo)
    
    
  }
}



#lat_lon_exp$lon[locidx[,1]]
#lat_lon_exp$lat[locidx[,2]]
#locidx = which(daymet_climate > 1800, arr.ind = TRUE)
