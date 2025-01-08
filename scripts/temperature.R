##############################################################################
#California Temperature
#Zoë J. Kitchel
#Created 3 January 2024
#Modified 12 January 2024
##############################################################################
#There are multiple options for where to get temperature data:
#1) OISST (NOAA 1/4° Daily Optimum Interpolation Sea Surface Temperature (OISST); Less data intensive, lower resolution)
#2) 
#3) 

#We may be interested in average temperature over the past 5 years, or max, min, mean temperature of sampling year (April to April).
#Function will allow user to choose which data source to use using arguments

#source = c("OISST", ), mean = T, max = T, min = T, seas = T

#another idea could be to link to closest NOAA buoy

##############################################################################
#SETUP#
library(sf)
library(data.table)
library(ggplot2)
library(dplyr)
library(rerddap)
library(lubridate)
library(oce) #delete if we don't end up using
library(ocedata)
library(raster)
library(ncdf4)
library(terra)
library(sdmpredictors) #allows us to access 2002-2009 average from aquaMODIS quickly through BioOracle
library(rnoaa) #directly access NOAA buoy data, note this package may be deprecated soon
library(nngeo) #k-Nearest Neighbor Join for Spatial Data

source("Functions_pulling_vrg_data.R") #this function pulls most recent clean version of raw data

############################################################################
#Load up all VRG lat longs
############################################################################
events <- pull_VRG_data_files(event = T) #specifically we want raw event data

#flag some strange latitudes and longitudes
events[,flag_lat := ifelse(Latitude < 31, TRUE, FALSE)][,flag_long := ifelse(Latitude > 33.6 & Longitude > -117.5, TRUE, FALSE)]

VRG_lat_lon_date_all <- unique(events[,.(Latitude, Longitude,SurveyDepth, flag_lat, flag_long, SampleDate)])

VRG_lat_lon_date_all[,Longitude := ifelse(!(Longitude >= -150 & Longitude <= -50),NA,Longitude)]
VRG_lat_lon_date_all <- VRG_lat_lon_date_all[complete.cases(VRG_lat_lon_date_all[, .(Longitude, Latitude)])]

############################################################################
#Edit VRG sampling points
############################################################################

#delete unexpected points
VRG_lat_lon_date_all.r <- VRG_lat_lon_date_all[flag_lat == FALSE & flag_long == FALSE,]

#round down to nearest multiple of 0.25 offset by 0.125 for OISST grid matching
VRG_lat_lon_date_all.r[,Longitude_r := (round(Longitude / 0.25) - 0.5) * 0.25][,Latitude_r := (round(Latitude / 0.25) - 0.5) * 0.25]

#add sample year column (January-April goes back to previous year)
VRG_lat_lon_date_all.r[,Sample_Year := ifelse(month(dmy(SampleDate)) <= 4, year(dmy(SampleDate))-1, year(dmy(SampleDate)))]

###########################################################################
#1) OISST in R
#SST, Daily Optimum Interpolation (OI), AVHRR Only, Version 2.1, Final, Global
#Not fully satellite, combination of buoys and satellites etc.
#Optimum Interpolation of SST (so, smoothed between observations)
#See helpful guide and best practices here: https://climatedataguide.ucar.edu/climate-data/sst-data-noaa-optimal-interpolation-oi-sst-analysis-version-2-oisstv2-1x1

#0.25 degree resolution: 27.4 km at equator
#daily values

#Partially adapted from: https://cran.r-project.org/web/packages/heatwaveR/vignettes/OISST_preparation.html
############################################################################

# the information for the NOAA OISST data
rerddap_info <- rerddap::info(datasetid = "ncdcOisst21Agg_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

# this function downloads and prepares data based on user provided start and end dates (specified below)
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(datasetx = "ncdcOisst21Agg_LonPM180", #names
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/", 
                       time = c(time_df$start, time_df$end), 
                       zlev = c(0, 0),
                       latitude = c(min(VRG_lat_lon_date_all.r$Latitude)-0.1, max(VRG_lat_lon_date_all.r$Latitude)+0.1),
                       longitude = c(min(VRG_lat_lon_date_all.r$Longitude-0.1), max(VRG_lat_lon_date_all.r$Longitude)+0.1),
                       fields = "sst")$data %>% #only download sst variable
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
    dplyr::select(longitude, latitude, time, sst) %>%
    na.omit()
}

#ERDDAP server limits how much data we can pull, therefore, we will create a dataframe of start and end dates that will allow us to automate the entire download

#make dataframe based on starting sample year of data input (we need 3 years before then)

first_year <- min(VRG_lat_lon_date_all.r$Sample_Year)-2

#the most recent SST data available through OISST
last_date <- date(rerddap_info$alldata$NC_GLOBAL$value[44])

#using the April break point (April temps looped up with previous May-December), what's the latest year of data we need
samp_year_coverage <- ifelse(month(last_date) < 5 ,year(last_date) -1 ,year(last_date)) #limit will be April of this year

#date download range by start and end dates per year (pulls in chunks of 7 years to ensure they will pull from ERDDAP)
sequence_years_7 <- seq(first_year, samp_year_coverage, by = 7)

#create empty dataframe to guide time periods to query from erddap in 7 year chunks, ending with most recent day of data (last_date)
dl_years <- data.frame(date_index = seq(1,length(sequence_years_7)))
#loop to make dataframe to guide time periods to query from erddap
for (i in 1:length(sequence_years_7)){
  dl_years[i,"start"] <- paste0(sequence_years_7[i],"-01-01") 
  dl_years[i,"end"] <- paste0(sequence_years_7[i+1]-1,"-12-31") 
  
  if(i == length(sequence_years_7)){
    dl_years[i,"end"] <- paste0(last_date)
  }
}


# Download all of the data with one nested request
# The time this takes will vary greatly based on connection speed
system.time(
  OISST_data <- dl_years %>% 
    group_by(date_index) %>% 
    group_modify(~OISST_sub_dl(.x)) %>%  #apply function to each group (from dplyr; purr style function that can be used to iterate on grouped tibbles)
    ungroup() %>%
    dplyr::select(longitude, latitude, time, sst) #removes data_index column
) # 521.783 seconds, ~130 seconds per batch

#convert into data.table
OISST_data <- data.table(OISST_data)

#add sample year column (January-April goes back to previous year)
OISST_data[,Sample_Year := ifelse(month(time) <= 4, year(time)-1, year(time))]

#take yearly average, yearly min, yearly max, yearly seasonality per cell from daily temperatures
OISST_data[,sst_sampleyear_avg := round(mean(sst, na.rm = T),2),.(longitude,latitude,Sample_Year)]
OISST_data[,sst_sampleyear_max := max(sst, na.rm = T),.(longitude,latitude,Sample_Year)]
OISST_data[,sst_sampleyear_min := min(sst, na.rm = T),.(longitude,latitude,Sample_Year)]
OISST_data[,sst_sampleyear_seas := round(sst_sampleyear_max-sst_sampleyear_min,2),.(longitude,latitude,Sample_Year)]

#Summary values for years (starting in May of 2000, ending in April of 2023)
OISST_data.r <- OISST_data[!(Sample_Year %in% c(1999,2023)),] #reduce to 2000-2022 sample years

#Sample year averages
OISST_data.avg <- unique(OISST_data.r[,.(longitude, latitude, Sample_Year, sst_sampleyear_avg, sst_sampleyear_max, sst_sampleyear_min, sst_sampleyear_seas)])

#order by latitude and longitude to allow us to use rolling average function
setkey(OISST_data.avg, longitude, latitude, Sample_Year)

#mean over past 3 years (moving average)
#set rolling mean value to NA if in 2000, 2002 (only 1 or 2 years of data)
OISST_data.avg[,sst_3sampleyears_avg := ifelse(Sample_Year %in% c(2000,2001),NA,round(frollmean(sst_sampleyear_avg,3),2))]

#merge OISST_data.avg with VRG lat lon year coordinates
vrg_lat_lon_dat_OISST <- OISST_data.avg[VRG_lat_lon_date_all.r, on = c(longitude = "Longitude_r",latitude = "Latitude_r", Sample_Year = "Sample_Year")]

#Works great! Not give you temperature data for sampling day unless we have full 12 months from OISST (aka, has to be June XXXX to have temperature averages for sampling season XXXX-1)

############################################################################
##2) Use MUR (Multi-scale Ultra-high Resolution) is an analyzed SST product at 0.01-degree resolution going back to 2002 from RERDAP package
############################################################################

#jplMURSST41 = dataset ID
sstInfo <- info('jplMURSST41')

#sstInfo <- info('jplMURSST41')
# get latest daily sst
murSST <- griddap(sstInfo, latitude = c(min(VRG_lat_lon_date_all.r$Latitude)-0.1, max(VRG_lat_lon_date_all.r$Latitude)+0.1),
                  longitude = c(min(VRG_lat_lon_date_all.r$Longitude-0.1), max(VRG_lat_lon_date_all.r$Longitude)+0.1),
                  time = c('last','last'), fields = 'analysed_sst')

#when we want SST for past 5 years (just avg temperature values for each site, ignoring time)

VRG_lat_lon_all.r <- unique(VRG_lat_lon_date_all.r[,.(Longitude, Latitude)])

#a week ago (data a few days delayed to post)
last_week <- now()-weeks(1)

#five years ago
five_years <- last_week - years(5)

#list of dates
five_years_dates <- seq(date(five_years), date(last_week), by = "days")

#empty vector
full_SST_list <- c()

#new empty column in lat lon list
VRG_lat_lon_all.r[,five_yr_avg:= NA]

#build loop to populate values
for (i in 1:nrow(VRG_lat_lon_all.r)){
  for (j in 1:five_years_dates){
  SST_point <- suppressMessages(griddap(sstInfo, latitude = c(VRG_lat_lon_all.r[i,Latitude],VRG_lat_lon_all.r[i,Latitude]), 
          longitude = c(VRG_lat_lon_all.r[i,Longitude], VRG_lat_lon_all.r[i,Longitude]),
          time = c(paste0(five_years_dates[j]),paste0(five_years_dates[j])), fields = 'analysed_sst'))
  
  full_SST_list <- c(full_SST_list,  SST_point$data$analysed_sst)
  
  print(paste0(j," out of ",length(five_years_dates), " for coordinate ",i," out of ",nrow(VRG_lat_lon_all.r)))
  
  }
  #average daily values to single 5 year value
  avg_SST <- mean(full_SST_list)
  
  #put average SST into data table with lat lon values
  VRG_lat_lon_all.r[i,five_yr_avg:= avg_SST]
}



###########################################################################
#3) BioORACLE from sdm predictors package (temperature averages between 2002 and 2009, 
    #from MODIS, Feldman & McClain 2010, URL: http://oceancolor.gsfc.nasa.gov/)
    #Resolution = 5 arcmin (9.2 km)
############################################################################

BO_sst <- load_layers(layercodes = c("BO_sstmax", "BO_sstmean", "BO_sstmin", "BO_sstrange") ,
                       equalarea=FALSE, rasterstack=TRUE) 

#reduce to CA only
CA_sst <- crop(BO_sst, extent(min(VRG_lat_lon_date_all.r$Longitude)-0.1, max(VRG_lat_lon_date_all.r$Longitude)+0.1,
               min(VRG_lat_lon_date_all.r$Latitude)-0.1, max(VRG_lat_lon_date_all.r$Latitude)+0.1))

rm(BO_sst)

#create multifocal function to apply focal function to each layer of the raster brick

multiFocal <- function(x, w=matrix(1, nr=3, nc=3), ...) {
  
  if(is.character(x)) {
    x <- brick(x)
  }
  # The function to be applied to each individual layer
  fun <- function(ind, x, w, ...){
    focal(x[[ind]], w=w, ...)
  }
  
  n <- seq(nlayers(x))
  list <- lapply(X=n, FUN=fun, x=x, w=w, ...)
  
  out <- stack(list)
  return(out)
}


#fill in two missing cells with neighborhood averages
#important to note that this does also add erroneous cells with values on the edge, but this does not impact our extractions because these are points on land and no sampling points are on land
CA_sst.t <- multiFocal(CA_sst, w = matrix(1, 3, 3), fun = "mean", NAonly = TRUE, na.rm = T)

#make points into sf in same projection
VRG_lat_lon_only <- copy(VRG_lat_lon_date_all.r)

#we only need unique lat lon coordinates, so we exclude time and reduce to unique longitude and latitude values
VRG_lat_lon_only <- VRG_lat_lon_only[,.(Longitude, Latitude)]

#convert list of lat lons to a simple feature object
VRG_lat_lon_only.sf <- st_as_sf(VRG_lat_lon_only, coords = c("Longitude","Latitude"), crs = 4326)

#transform to crs of raster stack
VRG_lat_lon_only.t <- st_transform(VRG_lat_lon_only.sf, crs = st_crs(BO_sst))

#add new columns with extracted temperature data
#maximum
VRG_lat_lon_date_all.r[,BO_sstmax := raster::extract(CA_sst.t[[1]], VRG_lat_lon_only.t)]

#mean
VRG_lat_lon_date_all.r[,BO_sstmean := raster::extract(CA_sst.t[[2]], VRG_lat_lon_only.t)]

#minimum
VRG_lat_lon_date_all.r[,BO_sstmin := raster::extract(CA_sst.t[[3]], VRG_lat_lon_only.t)]

#range
VRG_lat_lon_date_all.r[,BO_sstseas := raster::extract(CA_sst.t[[4]], VRG_lat_lon_only.t)]

###########################################################################
#4) Aqua MODIS

#California merged satellite-derived 1-km dataset
#Mati Kahru, mkahru@ucsd.edu    Updated : 9/20/2021

#https://spg-satdata.ucsd.edu/ca1km/
############################################################################

# Specify the file path
file_path <- "/Users/kitchel/Downloads/Chla_fromInterp2x/C20212412021262_chl_comp.hdf"

# List subdatasets available in the HDF file
r <- raster::raster(file_path) #no subdatasets
print(sds)

# Read all subdatasets into a SpatRaster stack
r_stack <- lapply(sds, rast)
r_stack <- rast(file_path)

# Plot the raster stack (this will show the first layer by default)
plot(r_stack)

# Get information about the stack
print(r_stack)



###########################################################################
#5) Closest NOAA Buoy
############################################################################

#list all NOAA buoys
NOAA_buoys <- buoy_stations()

#buoy station, lat and lon only
NOAA_buoys <- NOAA_buoys %>% 
                data.table() %>% #convert to data table
                .[,.(station, lat, lon)] %>% #only need station, lat, lon columns
                  na.omit(.) #delete any rows missing any of these columns

#convert lat lon list into simple feature
VRG_lat_lon_only <- copy(VRG_lat_lon_date_all.r)

#we only need unique lat lon coordinates, so we exclude time and reduce to unique longitude and latitude values
VRG_lat_lon_only <- VRG_lat_lon_only[,.(Longitude, Latitude)]

#convert list of lat lons to a simple feature object
VRG_lat_lon_only.sf <- st_as_sf(VRG_lat_lon_only, coords = c("Longitude","Latitude"), crs = 4326)

#convert NOAA buoys to simple feature

#convert list of lat lons to a simple feature object
NOAA_buoys.sf <- st_as_sf(NOAA_buoys, coords = c("lon","lat"), crs = 4326)

# Find nearest point (buoy) per polygon point
match_buoy_VRG <- st_join(VRG_lat_lon_only.sf, NOAA_buoys.sf, join = nngeo::st_nn, k = 1, progress = FALSE)
matched

#Link dearest neighbor buoy name with VRG sampling site data table
VRG_lat_lon_only[,NOAA_buoy := match_buoy_VRG$station ]

