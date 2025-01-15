##############################################################################
#Function to join California bathymetry data by linking to list of lat/longs

#Largely pulled from bathy.R

#Zoë J. Kitchel
#Created 13 December 2023
#Modified 15 January 2025
##############################################################################
#There are multiple options for where to get bathymetry data:
#method = "ETOPO": Marmap package (but our sites may be too close to shore for this to work, example below anyway though)
#method = "USGS_socal": USGS, SoCal only, "A Seamless, High-Resolution, Coastal Digital Elevation Model (DEM) for Southern California": https://pubs.usgs.gov/ds/487/ds487_text.pdf

#Depricated
  #method = "CDFW": Full state of California out to 200 eez lower rez: https://wildlife.ca.gov/Conservation/Marine/GIS/Downloads (Arc/Info Binary Grid (ADF) ), but we already have data
        #200mEEZ_BathyGrids.zip > bd200m_v2i

##############################################################################

#you will need the following packages loaded
 # library(sf)
 # library(data.table)
 # library(dplyr)
 # library(raster)
 # library(marmap) #to pull depth data

#lat_lon input must be data frame with lat and lon in Longitude and Latitude columns, okay if there are other columns

add_depth_columns <- function(lat_lon, ETOPO = F, USGS_socal=F, ETOPO_dist_200m = F){ 
  
  if(!("Longitude" %in% colnames(lat_lon)) | !("Latitude" %in% colnames(lat_lon))){
    if("longitude" %in% colnames(lat_lon)){
      data.table::setnames(lat_lon, c("latitude","longitude"),c("Latitude","Longitude"))
    }
    if(!("longitude" %in% colnames(lat_lon))){
      stop("Are you sure you have Longitude and Latitude columns in this dataframe?")
    }
  }
  
  lat_lon_only <- data.table(lat_lon)
  
  if(any(is.na(lat_lon_only))) {
      warning("Input contains NA values. Those rows will be deleted from output.")
    }
  
  lat_lon_only <- na.omit(unique(lat_lon_only[,.(Longitude, Latitude)]))
  
  if(ETOPO == T){

    #set square from which to extract bathy data from NOAA server
    marmap_bathy <- getNOAA.bathy(min(lat_lon_only$Longitude), max(lat_lon_only$Longitude), min(lat_lon_only$Latitude), max(lat_lon_only$Latitude), resolution = 0.000001) #bathymetry matrix
  
    #make new copy of lat lon only to add new column to so no repeats if we have T for multiple bathy types
    lat_lon_only.c <- copy(lat_lon_only)
    
    #get depth info for specified lat lon coordinates
    lat_lon_only.c[,ETOPODepth := get.depth(marmap_bathy, lat_lon_only.c, locator=F)$depth]
    
    #merge with main 'return' output
    lat_lon_bathy <- lat_lon[lat_lon_only.c, on = c("Longitude","Latitude"), all = T]
    
  }
  
  if(CDFW == T){ #this is now deprecated
    
    temp <- tempdir()
    
    #download file to temp directory
    download.file("https://filelib.wildlife.ca.gov/Public/R7_MR/BATHYMETRY/200mEEZ_BathyGrids.zip", file.path(temp,"200mEEZ_BathyGrids.zip")) #can no longer download from web
    
    #unzip file
    unzip(file.path(temp,"200mEEZ_BathyGrids.zip"), exdir = temp)
    
    #depth data
    bathy_200m <- raster::raster(file.path(temp,"bd200m_v2i", "w001001.adf"))
    bathy_200m.dt <- data.table(as.data.frame(values(bathy_200m))) #just depth values
    bathy_200m.xy <- data.table(as.data.frame(bathy_200m, xy = T)) #this should extract x/y and depth values correctly, but for some reason data are factored, so I have to do manually in next step
    
    bathy_200m.dt <- cbind(bathy_200m.xy[,.(x,y)],bathy_200m.dt[,.(`values(bathy_200m)`)]) #link x/y with depth values
    
    #coordinate system of raster
    crs_Raster <- sf::st_crs(raster::crs(bathy_200m))
    
    #convert points to sf with non-projected crs (must do first)
      lat_lon_only.sf <- st_as_sf(lat_lon_only, coords = c("Longitude", "Latitude"),
                                   crs = 4326) # WGS84 - a good default for unprojected coords)
    
      lat_lon_only.t <- st_transform(lat_lon_only.sf, crs = crs_Raster) #convert to raster projection for extraction
    
      #make new copy of lat lon only to add new column to so no repeats if we have T for multiple bathy types
      lat_lon_only.c <- copy(lat_lon_only)
      
    #match sites to depths
      lat_lon_only.c[, CDFW_200m:= extract(bathy_200m, lat_lon_only.t)]
      
      #link up (either to other bathy if it exists or just with lat lon coordinates)
        #merge with main 'return' output
      if(exists("lat_lon_bathy")){
      lat_lon_bathy <- lat_lon_bathy[lat_lon_only.c, on = c("Longitude","Latitude"), all = T]
      }else{
      lat_lon_bathy <- lat_lon[lat_lon_only.c, on = c("Longitude","Latitude"), all = T]
      }
  }
  
  if(ETOPO_dist_200m == T){
    
    site_dist_200m <- data.table(dist2isobath(marmap_bathy, lat_lon_only$Longitude, lat_lon_only$Latitude, isobath = 200))
    
    #make new copy of lat lon only to add new column to so no repeats if we have T for multiple bathy types
    lat_lon_only.c <- copy(lat_lon_only)
    
    #match sites to distances
    lat_lon_only.c[, dist_200m_bath:= site_dist_200m$distance]
    
    #link up (either to other bathy if it exists or just with lat lon coordinates)
      #merge with main 'return' output
    if(exists("lat_lon_bathy")){
      lat_lon_bathy <- lat_lon_bathy[lat_lon_only.c, on = c("Longitude","Latitude"), all = T]
    }else{
      lat_lon_bathy <- lat_lon[lat_lon_only.c, on = c("Longitude","Latitude"), all = T]
    }
    
    
  }
  
  if(USGS_socal == T){
    
    options(timeout=1000) #allow more time to download large rasters
    
      #download key (where is what ID)
      
      # Define the URL of the zip file
      zip_url <- "https://pubs.usgs.gov/ds/487/data/DEMCoverageAreas/DEMCoverageAreas.zip"
      
      # Create a temporary directory
      temp_dir <- tempdir()
      
      # Create a temporary file to save the downloaded zip file
      zip_file <- file.path(temp_dir, "temp_download.zip")
      
      # Download the zip file
      download.file(zip_url, destfile = zip_file, mode = "wb")
      
      # Extract the contents of the zip file to the temporary directory
      unzip(zip_file, exdir = temp_dir)
    
      #make simple feature 
      coverage_area <- read_sf(file.path(temp_dir,"DEMCoverageAreas.shp"))
      
      #transform lat lon to match CRS of reference shapefile
      lat_lon_only.sf <- st_as_sf(lat_lon_only, coords = c("Longitude", "Latitude"), crs = 4326) %>% 
        st_transform(crs = crs(coverage_area))
      
      #add other lat lon values from previous projection
      lat_lon_only.sf$Latitude <- lat_lon_only$Latitude
      lat_lon_only.sf$Longitude <- lat_lon_only$Longitude
      
      #which lat long points link to which relevant DEM boundaries (DEM_ID)
      
      lat_lon_DEM_link <- st_intersection(lat_lon_only.sf, coverage_area)
      
      #DEM_ID, put into single vector
      DEM_IDs <- levels(factor(lat_lon_DEM_link$DEM_ID))
      
      #convert to lower
      DEM_to_download <- tolower(levels(factor(lat_lon_DEM_link$DEM_ID)))
      
      #setup data table to take stats
      highres_socal <- data.table()
      
      #download DEMs one by one to current working directory (temporarily! these are large)
      for (i in 1:length(DEM_to_download)) {
        #relevant lat_lons
        lat_lon_DEM_link.subset <- lat_lon_DEM_link %>% filter(DEM_ID == DEM_IDs[i])
        
        file_url <- paste0("https://pubs.usgs.gov/ds/487/data/DEMs/",DEM_to_download[i],".zip")
        
        #download file to working directory
        download.file(file_url,paste0(DEM_to_download[i],".zip")) #this will vary in time, some up to ~200 MB!
        
        #unzip file
        unzip(paste0(DEM_to_download[i],".zip"))
        
        #remove zip file, only keep la10.txt
        file.remove(paste0(DEM_to_download[i],".zip"))
        
        #load up as raster
        DEM_single_raster <- raster(paste0(DEM_to_download[i],".txt"))
        
        #set CRS
        utm_crs <- "+proj=utm +zone=11 +datum=WGS84 +units=m +no_defs" #from technical documentation
        projection(DEM_single_raster) <- utm_crs
        
        #subset data table
        highres_socal.subset <- data.table(Longitude = lat_lon_DEM_link.subset$Longitude,
                                           Latitude = lat_lon_DEM_link.subset$Latitude,
                                           highrez_depth = raster::extract(DEM_single_raster,  lat_lon_DEM_link.subset))   #extract relevant lat lon values
        
        #remove raster file
        file.remove(paste0(DEM_to_download[i],".txt"))
        
        #set up datatable
        highres_socal <- rbind(highres_socal, highres_socal.subset)
      }
      
      #link up (either to other bathy if it exists or just with lat lon coordinates)
       #merge with main 'return' output
      if(exists("lat_lon_bathy")){
        lat_lon_bathy <- lat_lon_bathy[highres_socal, on = c("Longitude","Latitude"), all = T]
      }else{
        lat_lon_bathy <- lat_lon[highres_socal, on = c("Longitude","Latitude"), all = T]
      }
      
      
      
    }
    
  return(lat_lon_bathy)
}
