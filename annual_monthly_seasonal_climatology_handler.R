library(ncdf4)
library(dplyr)
library(PCICt)
library(abind)

# yearly stats necessary for climatology 
yearly_calc <- function(filename, varname, yearstart = NA, yearend = NA){
  # grab necessary details 
  print("Opening Netcdf....")
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
  
  if(is.na(yearstart) == FALSE & is.na(yearend)==FALSE){
    yearlyarray = array(NA,dim=c(length(lon),length(lat),length(yearstart:yearend)))
    yearsin = yearstart:yearend
  } else {
    yearlyarray = array(NA,dim=c(length(lon),length(lat),length(years)))
    yearsin = years
  }
  
  yearlyarray = array(NA, dim = c(length(lon), length(lat), length(yearsin)))
  
  for (y in 1:length(yearsin)){
    print(paste("Proc. year:", yearsin[y]))
    idxin = which(as.numeric(substr(filedate, 1, 4)) == yearsin[y])
    
    nc = nc_open(filename)
    obsdata = ncvar_get(nc, varname, start = c(1, 1, idxin[1]), count = c(-1, -1, length(idxin)))
    nc_close(nc)
    
    if (varname == "pr"){
      obsdata = obsdata * 86400
    }
    
    yearlyarray[,,y] = apply(obsdata, c(1,2), sum, na.rm = TRUE)
    yearlyarray[,,y] = ifelse(is.na(obsdata[,,1]) == FALSE, yearlyarray[,,y], NA)
    rm(obsdata)
    gc()
  }
  print("yearly complete!")
  return(yearlyarray)
}  

seasonal_calc <- function(filename, varname, yearstart = NA, yearend = NA){
  
  # grab necessary details 
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
  years = as.numeric(substr(filedate, 1, 4))
  months = as.numeric(substr(filedate, 6, 7))
  
  nc_close(nc)
  
  if (!is.na(yearstart) & !is.na(yearend)) {
    yearsin = yearstart:yearend
  } else {
    yearsin = unique(years)
  }
  
  # meteorological season
  seasons <- list(
    djf = c(12, 1, 2),
    mam = c(3, 4, 5),
    jja = c(6, 7, 8),
    son = c(9, 10, 11)
  )
  
  seasonal_array <- array(NA, dim = c(length(lon), length(lat), length(seasons), length(yearsin)))
  
  for (y in 1:length(yearsin)){
    print(paste("Proc. year:", yearsin[y]))
    for (s in names(seasons)){
      print(paste("Proc. season:", s))
      
      # indices for the season 
      idxin = which(years == yearsin[y] & months %in% seasons[[s]])
      
      if (length(idxin) > 0){
        nc = nc_open(filename)
        obsdata = ncvar_get(nc, varname, start = c(1,1, idxin[1]), count = c(-1, -1, length(idxin)))
        nc_close(nc)
        
        if (varname == "pr"){
          obsdata = obsdata * 86400
        }
        
        # sum over the season
        seasonal_array[, , which(names(seasons) == s), y] = apply(obsdata, c(1,2), sum, na.rm = TRUE)
        
        # mask the missing values (gulf, mexico, etc.)
        seasonal_array[, , which(names(seasons) == s), y] == ifelse(is.na(obsdata[,,1]) == FALSE, seasonal_array[, , which(names(seasons) == s), y], NA)

        rm(obsdata)
        gc()
      }
    }
  }
  print("Seasonal complete!")
  return(seasonal_array)
}

monthly_calc <- function(filename, varname, yearstart = NA, yearend = NA) {
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
  years = as.numeric(substr(filedate, 1, 4))
  months = as.numeric(substr(filedate, 6, 7))
  
  nc_close(nc)
  
  if (!is.na(yearstart) & !is.na(yearend)) {
    yearsin = yearstart:yearend
  } else {
    yearsin = unique(years)
  }
  
  #months 
  months <- c(
    jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6,
    jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12
  )
  
  monthly_array <- array(NA, dim = c(length(lon), length(lat), length(months), length(yearsin)))
  
  for (y in 1:length(yearsin)) { #use seqalong?
    print(paste("Proc. year:", yearsin[y]))
    for (s in names(months)) {
      print(paste("Proc. month:", s))
      
      idxin = which(years == yearsin[y] & months == months[s])
      
      if (length(idxin) > 0) {
        nc = nc_open(filename)
        obsdata = ncvar_get(nc, varname, start = c(1, 1, idxin[1]), count = c(-1, -1, length(idxin)))
        nc_close(nc)
        
        if (varname == "pr") {
          obsdata = obsdata * 86400 
        }
        
        monthly_array[, , which(names(months) == s), y] = apply(obsdata, c(1,2), sum, na.rm = TRUE)
        monthly_array[, , which(names(months) == s), y] = ifelse(is.na(obsdata[,,1]) == FALSE, monthly_array[, , which(names(months) == s), y], NA)
        rm(obsdata)
        gc()
      }
    }
  }  
  print("month complete!")
  return(monthly_array)
}

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

# calculate climatology from 1981 to 2014
climatology <- function(yearlyarray){
  climoarray = apply(yearlyarray, c(1,2), mean, na.rm = TRUE)
  climoarray = ifelse(is.na(yearlyarray[,,1]) == FALSE, climoarray, NA)
  print(dim(climoarray))
  return(climoarray)
}

seasonal_climatology <- function(seasonal_array){
  seasonal_climoarray = apply(seasonal_array, c(1,2,3), mean, na.rm = TRUE)
  #seasonal_climoarray = ifelse(is.na(seasonal_array[,,1]) == FALSE, seasonal_climoarray, NA)
  #seasonal_climoarray = array(seasonal_climoarray, dim = c(dim(seasonal_array)[1], dim(seasonal_array)[2], 1))
  #seasonal_climoarray <- ifelse(!is.na(seasonal_array[,,1,1]), seasonal_climoarray, NA)
  seasonal_climoarray[is.nan(seasonal_climoarray)] <- NA
  print(dim(seasonal_climoarray))
  return(seasonal_climoarray)
}

#print(dim(seasonal_climatology))
#a[,,1,]

monthly_climatology <- function(monthly_array){
  monthly_climoarray = apply(monthly_array, c(1,2,3), mean, na.rm = TRUE)
  #monthly_climoarray = ifelse(is.na(monthly_array[,,1]) == FALSE, monthly_climoarray, NA) not sure why this line causes the error. something to do with dims
  monthly_climoarray[is.nan(monthly_climoarray)] <- NA
  print(dim(monthly_climoarray))
  return(monthly_climoarray)
}

#rain_1mm_climatology <- function()
#rain_1mm_seasonal_climatology <- function()
#rain_1mm_monthly_climatology <- function()

# directories 
train_data <- list(
 variable = "directory"
)

combined_df_annual <- list()
combined_df_seasonal <- list()
combined_df_monthly <- list()

for (data_path in names(train_data)){
  path <- train_data[[data_path]]
  print(paste("Proc dataset:", data_path))
  fileinfo <- infofinder(path, varname = "pr")
  
  print("Calc yearly precip")
  yearly_array <- yearly_calc(path, varname = "pr", yearstart = 1981, yearend = 2014)
  print("Calc seasonal precip")
  seasonal_array <- seasonal_calc(path, varname = "pr", yearstart = 1981, yearend = 2014)
  print("Calc monthly precip")
  monthly_array <- monthly_calc(path, varname = "pr", yearstart = 1981, yearend = 2014)
  
  print("Calc all climatologies...")
  climo_calc <- climatology(yearly_array)
  seasonal_result <- seasonal_climatology(seasonal_array)
  monthly_result <- monthly_climatology(monthly_array)
  
  print("Storing")
  combined_df_annual[[data_path]] <- climo_calc
  combined_df_seasonal[[data_path]] <- seasonal_result
  combined_df_monthly[[data_path]] <- monthly_result
  #break
}

# combine and store all the results 
final_climatology <- list()
final_seasonal_climatology <- list()
final_monthly_climatology <- list()

for (dataset_name in names(combined_df_annual)) {
  final_climatology[[dataset_name]] <- combined_df_annual[[dataset_name]]
}

for (dataset_name in names(combined_df_seasonal)) {
  seasonal_data <- combined_df_seasonal[[dataset_name]]
  final_seasonal_climatology[[dataset_name]] <- list(
    DJF = seasonal_data[,,1],  
    MAM = seasonal_data[,,2],  
    JJA = seasonal_data[,,3],  
    SON = seasonal_data[,,4]  
  )
}

for (dataset_name in names(combined_df_monthly)) {
  monthly_data <- combined_df_monthly[[dataset_name]]
  final_monthly_climatology[[dataset_name]] <- list(
    Jan = monthly_data[,,1], Feb = monthly_data[,,2], Mar = monthly_data[,,3],
    Apr = monthly_data[,,4], May = monthly_data[,,5], Jun = monthly_data[,,6],
    Jul = monthly_data[,,7], Aug = monthly_data[,,8], Sep = monthly_data[,,9],
    Oct = monthly_data[,,10], Nov = monthly_data[,,11], Dec = monthly_data[,,12]
  )
}

print(dim(final_climatology$daymet_file))
print(dim(final_seasonal_climatology$daymet_file$DJF))
print(dim(final_monthly_climatology$daymet_file$Jan))

print("Complete!")

save(final_climatology, final_seasonal_climatology, final_monthly_climatology, 
     file = "climatology_results.RData")

print("Climatology calculations completed and saved!")


