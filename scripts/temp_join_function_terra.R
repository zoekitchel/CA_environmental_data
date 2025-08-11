#This function matches sea surface temperature data from Scripps Oceanography (see ucsd_metadata.R) to a list of Latitudes and Longitudes in the Southern California Bight


#This function needs the following packages to work
#library(data.table)
#library(dplyr) or library(tidyverse)
#library(rvest)
#library(terra)
#library(sf)

#######TEMP san diego 1km satellite
#Link: http://spg-satprojects.ucsd.edu
#Link: https://spg-satdata.ucsd.edu
#Metadata for HDF files

#To add to time series, add line to `ucsd_data_download_1km_sst_2016_2024.txt` file with new year of data

#Arguments
#annual = F (if F, exports average from 2016 until 2024, if T, gives value for each year)
#min_max = F (if F, exports mean annual temp only, if T, gives min and max)
#If annual = T and min_max = T, it will give annual mean, min, and max values
#If annual = F and min_max = T, it will give average mean, min, and max values between 2016 and 2024

temp_join <- function(lat_lon, grouping_var = "Site", annual = F, min_max = F){
  
  #Check for required columns
  required_cols <- c("Latitude", "Longitude")
  missing_cols <- setdiff(required_cols, names(lat_lon))
  
  lat_lon <- lat_lon %>%
    rename(Site := !!sym(grouping_var))
  
  
  if (length(missing_cols) > 0) {
    stop(paste("Error: The following required columns are missing from the data frame:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  #Check for missing values
  if (any(is.na(lat_lon$Latitude) | is.na(lat_lon$Longitude))) {
    stop("Error: Your data contains missing values in the Latitude or Longitude columns. Please remove or fill these before proceeding.")
  }
  
  print("Your input looks good! Now get a snack, it will take some time to download these high-rez data.")
  
  #Scrape links to monthly temp values from UCSD website
  #first, navigate to year page (need to source from GitHub)
  folder_urls <- readLines("https://raw.githubusercontent.com/zoekitchel/CA_environmental_data/refs/heads/main/data/ucsd_sstchl/ucsd_data_download_1km_sst_2016_2024.txt")
  
  #empty data.table
  full_link.dt <- data.table()
  
  #make list of all links to download
  for(i in 1:length(folder_urls)){
    url <- folder_urls[i]
    
    #variable <- ifelse(str_detect(url,"chl"),"chl","sst")
    variable <- "sst"
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
    hdf_stack <- #raster::stack(file_to_use) #old
      
      # Load the HDF file as a SpatRaster
      hdf_stack <- terra::rast(file_to_use)
    
    return(hdf_stack)
  }
  
  hdf_key <- get_hdf_stack(file_name = "cal_aco_3840_Latitude_Longitude.hdf",
                           download_url = "http://wimsoft.com/CAL/files/cal_aco_3840_Latitude_Longitude.hdf")
  
  
  #trim to study area
  crop_ext <- terra::ext(1617,2069,1727,2204)
  
  hdf_key.c <- crop(hdf_key, crop_ext)
  
  # Convert key raster to data.table
  hdf_key.xyz <- as.data.table(as.data.frame(hdf_key.c, xy = TRUE))
  setnames(hdf_key.xyz, c("x", "y", "latitude", "longitude"))
  
  # Predefine crop extent once
  crop_ext <- terra::ext(1617, 2069, 1727, 2204)
  
  #download and process each kmz file and populate data table with temp data
  for(i in 1:nrow(full_link.dt)){
    temp <- tempfile(fileext = ".hdf")
    download.file(full_link.dt$link_long[i], temp, quiet = T)
    hdf <- terra::rast(temp)
    
    hdf.c <- crop(hdf, crop_ext)
    
    # Replace 255 with NA
    hdf.c.re <- classify(hdf.c, matrix(c(255, NA), ncol = 2, byrow = TRUE))
    
    # Convert to data.table
    hdf.xyz <- as.data.table(as.data.frame(hdf.c.re, xy = TRUE))
    setnames(hdf.xyz, c("x", "y", "value"))
    
    # Merge with key coords
    hdf_merge <- merge(hdf.xyz, hdf_key.xyz, by = c("x", "y"), all.x = TRUE)
    
    # Create empty raster from extent
    e <- terra::ext(
      min(hdf_merge$longitude), max(hdf_merge$longitude),
      min(hdf_merge$latitude),  max(hdf_merge$latitude)
    )
    
    r <- terra::rast(e, ncol = 452, nrow = 477, crs = "EPSG:4326")
    
    # Make SpatVector with coords + value in same object
    pts <- terra::vect(
      hdf_merge[, .(longitude, latitude, value)],
      geom = c("longitude", "latitude"),
      crs = "EPSG:4326"
    )
    
    # Rasterize
    new_raster <- terra::rasterize(
      x = pts,
      y = r,
      field = "value",   # now refers to column inside pts
      fun = mean,
      na.rm = TRUE
    )
    
    # Prepare site points
    lat_lon_coords <- lat_lon %>% select(Longitude, Latitude)
    lat_lon.sf <- st_as_sf(lat_lon_coords, coords = c("Longitude", "Latitude"), crs = 4326)
    lat_lon.sf <- st_transform(lat_lon.sf, crs(new_raster))
    
    # Extract
    value <- terra::extract(new_raster, terra::vect(lat_lon.sf))[, 2]
    
    
    # Fill NAs with neighborhood mean
    if (anyNA(value)) {
      missing_idx <- which(is.na(value))
      missing_points <- lat_lon.sf[missing_idx, ]
      neighborhood_values <- terra::extract(
        new_raster, terra::vect(missing_points),
        buffer = 10000, fun = mean, na.rm = TRUE
      )[, 2]
      value[missing_idx] <- neighborhood_values
    }
    
    # Append metadata
    lat_lon_variable <- lat_lon %>%
      mutate(
        year = full_link.dt$year[i],
        month = full_link.dt$month[i],
        sst = value
      )
    
    lat_lon_variable_full <- rbind(lat_lon_variable_full, lat_lon_variable)
    
    message(i, " out of ", nrow(full_link.dt))
    
    
  }
  
    # Final SST adjustment
    lat_lon_variable_full <- lat_lon_variable_full %>%
      mutate(sst_adj = 0.15 * sst - 3.0)
    
    
    #If we just want a single overall average for each site (mean only)
    if (!annual && !min_max) {
      lat_lon_variable_full_mean <- lat_lon_variable_full %>%
        group_by(Site, year) %>%
        mutate(sst_avg_annual = mean(sst_adj, na.rm = TRUE)) %>%
        ungroup()
      
      average_temperature_site <- lat_lon_variable_full_mean %>%
        group_by(Site) %>%
        summarise(mean_sst = mean(sst_avg_annual), .groups = "drop") %>%
        rename(!!sym(grouping_var) := Site)
      
      return(average_temperature_site)
    }
    
    #If we want overall average mean, minimum, and maximum for each site
    if(annual == F & min_max == T){
      lat_lon_variable_full_meanminmax <- lat_lon_variable_full %>%
        group_by(Site, year) %>%
        mutate(sst_avg_annual = mean(sst_adj, na.rm = T),
               sst_max_annual = max(sst_adj, na.rm = T),
               sst_min_annual = min(sst_adj, na.rm = T)) %>% ungroup()
      
      #Then take average of averages
      average_temperature_meanminmax <- lat_lon_variable_full_meanminmax %>% 
        group_by (Site) %>%
        summarise(mean_sst = mean(sst_avg_annual),
                  max_sst = max(sst_max_annual),
                  min_sst = min(sst_min_annual))
      
      #If grouping var doesn't = Site, swap name back
      average_temperature_site <- average_temperature_site %>%
        rename(!!sym(grouping_var) := Site)
      
      return(average_temperature_site_meanminmax)
      
    }
    
    #If we want annual values but only for mean
    if(annual == T & min_max == F){
      
      lat_lon_site_variable_full_mean <- lat_lon_site_variable_full %>%
        group_by(Site, year) %>%
        summarize(sst_avg_annual = mean(sst_adj, na.rm = T))
      
      #If grouping var doesn't = Site, swap name back
      average_temperature_site <- average_temperature_site %>%
        rename(!!sym(grouping_var) := Site)
      
      return(lat_lon_site_variable_full_mean)
    }
    
    #If we want annual values for mean, max, and min
    if(annual == T & min_max == T){
      lat_lon_variable_full_meanminmax <- lat_lon_variable_full %>%
        group_by(Site, year) %>%
        summarize(sst_avg_annual = mean(sst_adj, na.rm = T),
                  sst_max_annual = max(sst_adj, na.rm = T),
                  sst_min_annual = min(sst_adj, na.rm = T))
      
      #If grouping var doesn't = Site, swap name back
      average_temperature_site <- average_temperature_site %>%
        rename(!!sym(grouping_var) := Site)
      
      return(lat_lon_variable_full_meanminmax)
    }
  }