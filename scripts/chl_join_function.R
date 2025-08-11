#This function matches chlorophyll-a data from Scripps Oceanography (see ucsd_metadata.R) to a list of Latitudes and Longitudes in the Southern California Bight


#This function needs the following packages to work
#library(data.table)
#library(tidyverse)
#library(rvest)
#library(raster)
#library(sf)

#######Chlorophyll san diego 1km satellite
#Link: http://spg-satprojects.ucsd.edu
#Link: https://spg-satdata.ucsd.edu
#Metadata for HDF files

#To add to time series, add line to `ucsd_data_download_1km_chl_2016_2024.txt` file with new year of data

#Arguments
#annual = F (if F, exports average from 2016 until 2024, if T, gives value for each year)
#min_max = F (if F, exports mean annual chlorophyll only, if T, gives min and max)
#grouping_Var = "Site" ; for what scale do you want to calculate chl? default = "Site" (What's your other column namee in lat_lon?)
#If annual = T and min_max = T, it will give annual mean, min, and max values
#If annual = F and min_max = T, it will give average mean, min, and max values between 2016 and 2024

chl_join <- function(lat_lon, annual = F, min_max = F){
  
  #Check for required columns
  required_cols <- c("Latitude", "Longitude")
  missing_cols <- setdiff(required_cols, names(lat_lon))
  if (length(missing_cols) > 0) {
    stop(paste("Error: The following required columns are missing from the data frame:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  #Check for missing values
  if (any(is.na(lat_lon$Latitude) | is.na(lat_lon$Longitude))) {
    stop("Error: Your data contains missing values in the Latitude or Longitude columns. Please remove or fill these before proceeding.")
  }
  
  print("Your input looks good! Now get a snack, it will take some time to download these high-rez data.")
  
  #Scrape links to monthly chlorophyll values from UCSD website
  #first, navigate to year page
  folder_urls <- readLines("https://raw.githubusercontent.com/zoekitchel/CA_environmental_data/refs/heads/main/data/ucsd_sstchl/ucsd_data_download_1km_chl_2016_2024.txt")
  
  #empty data.table
  full_link.dt <- data.table()
  
  #make list of all links to download
  for(i in 1:length(folder_urls)){
    url <- folder_urls[i]
    
    #variable <- ifelse(str_detect(url,"chl"),"chl","sst")
    variable <- "chl"
    year <- as.numeric(substr(url,30,33))
    
    #individual month links using rvest
    monthly_link_list <- url %>%
      read_html() %>%
      html_nodes("table") %>% html_nodes("tr") %>% html_nodes("a") %>%
      html_attr("href") #identify all links on page
    
    monthly_link_list.r <- monthly_link_list |>
      keep(~ str_detect(.x, "comp.hdf"))
    
    #add full url to all links
    monthly_link_list.full <- paste0(url,monthly_link_list.r)
    
    #make datatable
    subset.dt <- data.table(variable = variable, year = year, month = seq(1,12),
                            file_name = monthly_link_list.r, link_long = monthly_link_list.full)
    
    full_link.dt <- rbind(full_link.dt, subset.dt)
    
  }
  
  #empty data table to fill with site, and month specific data
  lat_lon_variable_full <- data.table()
  
  #load up hdf key with lat lon (download if it doesn't exist locally already)
  
  get_hdf_stack <- function(file_name, download_url) {
    # Search for the file in working directory and all subfolders
    all_files <- list.files(path = ".", pattern = file_name, recursive = TRUE, full.names = TRUE)
    
    if (length(all_files) == 0) {
      message("File not found locally. Downloading...")
      download.file(download_url, destfile = file_name, mode = "wb")
      file_to_use <- file_name
    } else {
      file_to_use <- all_files[1]
      message("Using local file: ", file_to_use)
    }
    
    # Load the HDF file as a RasterStack
    hdf_stack <- raster::stack(file_to_use)
    
    return(hdf_stack)
  }
  
  hdf_key <- get_hdf_stack(file_name = "cal_aco_3840_Latitude_Longitude.hdf", download_url = "http://wimsoft.com/CAL/files/cal_aco_3840_Latitude_Longitude.hdf")
  
  
  #trim to study area
  crop_ext <- extent(1617,2069,1727,2204)
  
  hdf_key.c <- crop(hdf_key, crop_ext)
  
  #convert into data table
  hdf_key.xyz <- data.table(rasterToPoints(hdf_key.c))
  
  colnames(hdf_key.xyz) <- c("x","y","latitude","longitude")
  
  #download and process each kmz file and populate data table with chlorophyll data
  for(i in 1:nrow(full_link.dt)){
    temp <- tempdir()
    download.file(full_link.dt[i,link_long], file.path(temp, "temp.hdf"))
    hdf <- raster(file.path(temp, "temp.hdf"))
    
    #trim to study area
    crop_ext <- extent(1617,2069,1727,2204)
    
    hdf.c <- crop(hdf, crop_ext)
    
    #change 255 values to NA ()
    hdf.c.re <- reclassify(hdf.c, cbind(255, NA)) #"values of 0 and 255 are considered invalid"
    
    #convert into data table
    hdf.xyz <- data.table(rasterToPoints(hdf.c.re))
    
    colnames(hdf.xyz) <- c("x","y","value")
    
    #merge with key
    hdf_merge <- hdf.xyz[hdf_key.xyz, on = c("x","y")]
    
    #set up empty raster
    e <- extent(min(hdf_merge$longitude), max(hdf_merge$longitude), min(hdf_merge$latitude),max(hdf_merge$latitude))
    
    r <- raster(e,ncol = 452, nrow = 477)
    
    #then, rasterize xyz
    new_raster <- rasterize(x = hdf_merge[,.(longitude,latitude)], y = r, field = hdf_merge[,.(value)], fun = mean, na.rm = T)
    
    #set crs
    crs(new_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
    
    #Coordinates only
    lat_lon_coords <- lat_lon %>% ungroup() %>% dplyr::select(Longitude, Latitude)
    
    #Convert list of lat lon into sf
    lat_lon.sf <- st_as_sf(lat_lon_coords, coords = c("Longitude","Latitude"), crs = st_crs(4326))
    
    #match crs of lat lon to crs of raster
    lat_lon_site.t <- st_transform(lat_lon.sf, crs(new_raster))
    
    value <- raster::extract(new_raster,lat_lon_site.t)
    
    #If NA values exist, take neighborhood of these points
    if(any(is.na(value))){
      
      missing_idx <- which(is.nan(value))
      
      missing_rows <- lat_lon_site.t[missing_idx,] #identify missing points
      
      neighborhood_values <- raster::extract(new_raster,missing_rows, buffer = 10000, fun = mean, na.rm = TRUE)
      
      # Fill missing values with neighborhood means
      value[missing_idx] <- neighborhood_values
      
    }
   
    #copy input lat lon 
    lat_lon_variable <- copy(lat_lon)
    
    #fill new columns
    lat_lon_variable <- lat_lon_variable %>%
      mutate(
        year = full_link.dt$year[i],
        month = full_link.dt$month[i],
        chl = value)
    
    #add to full data table
    lat_lon_variable_full <- rbind(lat_lon_variable_full, lat_lon_variable)
    
    print(paste0(i," out of ",nrow(full_link.dt)))
    
  }
  
  #Note that values use 1 byte per pixel with standard scaling. Logarithmic scaling for Chl: https://spg-satdata.ucsd.edu/Readme.htm
  #so, conversion
  lat_lon_variable_full <- lat_lon_variable_full %>%
    mutate(chl_adj = 10^(0.015*chl-2.0))
  
  #If we just want a single overall average for each site (mean only)
  if(annual == F & min_max == F){
    lat_lon_variable_full_mean <- lat_lon_variable_full %>%
      group_by(AR_Complex, Site, year) %>%
      mutate(chl_avg_annual = mean(chl_adj, na.rm = T)) %>% ungroup()
    
    #Then take average of averages
    average_chlorophyll_site <- lat_lon_variable_full_mean %>% 
      group_by (AR_Complex, Site) %>% 
      summarise(mean_chl = mean(chl_avg_annual))
    
    return(average_chlorophyll_site)
  }
  
  #If we want overall average mean, minimum, and maximum for each site
  if(annual == F & min_max == T){
    lat_lon_variable_full_meanminmax <- lat_lon_variable_full %>%
      group_by(AR_Complex, Site, year) %>%
      mutate(chl_avg_annual = mean(chl_adj, na.rm = T),
             chl_max_annual = max(chl_adj, na.rm = T),
             chl_min_annual = min(chl_adj, na.rm = T)) %>% ungroup()
    
    #Then take average of averages
    average_chlorophyll_meanminmax <- lat_lon_variable_full_meanminmax %>% group_by (AR_Complex, Site) %>% summarise(mean_chl = mean(chl_avg_annual),
                                                                                                                     max_chl = max(chl_max_annual),
                                                                                                                     min_chl = min(chl_min_annual))
    
    return(average_chlorophyll_site_meanminmax)
    
  }
  
  #If we want annual values but only for mean
  if(annual == T & min_max == F){
    
    lat_lon_site_variable_full_mean <- lat_lon_site_variable_full %>%
      group_by(AR_complex, Site, year) %>%
      summarize(chl_avg_annual = mean(chl_adj, na.rm = T))
    
    return(lat_lon_site_variable_full_mean)
  }
  
  #If we want annual values for mean, max, and min
  if(annual == T & min_max == T){
    lat_lon_variable_full_meanminmax <- lat_lon_variable_full %>%
      group_by(AR_Complex, Site, year) %>%
      summarize(chl_avg_annual = mean(chl_adj, na.rm = T),
                chl_max_annual = max(chl_adj, na.rm = T),
                chl_min_annual = min(chl_adj, na.rm = T))
    
    return(lat_lon_variable_full_meanminmax)
  }
}


