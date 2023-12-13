##############################################################################
#Function to join California bathymetry data by linking to list of lat/longs

#Largely pulled from bathy.R

#Zoë J. Kitchel
#Created 13 December 2023
#Modified 13 December 2023
##############################################################################
#There are multiple options for where to get bathymetry data:
#method = "ETOPO": Marmap package (but our sites may be too close to shore for this to work, example below anyway though)
#method = "CDFW": Full state of California out to 200 eez lower rez: https://wildlife.ca.gov/Conservation/Marine/GIS/Downloads (Arc/Info Binary Grid (ADF) )
#200mEEZ_BathyGrids.zip > bd200m_v2i
#method = "USGS_socal": USGS, SoCal only, "A Seamless, High-Resolution, Coastal Digital Elevation Model (DEM) for Southern California": https://pubs.usgs.gov/ds/487/ds487_text.pdf

##############################################################################

#you will need the following packages loaded
# library(sf)
# library(data.table)
# library(ggplot2)
# library(dplyr)
# library(maps)
# library(raster)
# library(rnaturalearth)
# library(viridis)
# library(RColorBrewer)
# library(marmap) #to pull depth data
# library(rasterVis)

#lat_lon input must be data frame with lat and lon in Longitude and Latitude columns, okay if there are other columns

get_depth <- function(lat_lon, ETOPO = T, CDFW = T, USGS_socal=T, dist_200m = F){ #default will give you all three added to your table
  
  lat_lon_only <- data.table(lat_lon)
  
  if(!("Longitude" %in% colnames(lat_lon)) | !("Latitude" %in% colnames(lat_lon))){
    stop("Are you sure you have Longitude and Latitude columns in this dataframe?")
  }
  
  lat_lon_only <- unique(lat_lon_only[,.(Longitude, Latitude)])
  
  if(ETOPO == T){

    #set square from which to extract bathy data from NOAA server
    marmap_bathy <- getNOAA.bathy(min(lat_lon_only$Longitude), max(lat_lon_only$Longitude), min(lat_lon_only$Latitude), max(lat_lon_only$Latitude), resolution = 0.000001) #bathymetry matrix
    lat_lon_only[,ETOPODepth := get.depth(marmap_bathy, lat_lon_only, locator=F)$depth]
    
    lat_lon_bathy <- lat_lon[lat_lon_only, on = c("Longitude","Latitude"), all = T]
    
  }
  
  if(CDFW == T){
    
    temp <- tempdir()
    
    #download file to temp directory
    download.file("https://filelib.wildlife.ca.gov/Public/R7_MR/BATHYMETRY/200mEEZ_BathyGrids.zip", file.path(temp,"200mEEZ_BathyGrids.zip")) 
    
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
    
    #match sites to depths
      lat_lon_only[, CDFW_200m:= extract(bathy_200m, lat_lon_only.t)]
      
      #link up
      lat_lon_bathy <- lat_lon[lat_lon_only, on = c("Longitude","Latitude"), all = T]
    
  }
  
  if(dist_200m == T){
    
    temp <- tempdir()
    
    #download file to temp directory
    download.file("https://filelib.wildlife.ca.gov/Public/R7_MR/BATHYMETRY/contours_5m.zip", file.path(temp,"contours_5m.zip")) #very large, takes a while
    
    #unzip file
    unzip(file.path(temp,"contours_5m.zip"), exdir = temp)
    
    CA_contours_5m <- read_sf(file.path("raw_data","contours_5m","contours_5m.shp"))
    
    CA_200m_contour <- CA_contours_5m %>% filter(CONTOUR == -200)
    CA_200m_contour.t <- st_transform(CA_200m_contour, crs = crs(VRG_lat_lon_all.sf)) #match crs
    CA_0m_contour <- CA_contours_5m %>% filter(CONTOUR == 0)
    
    #calculate distance of point to each of 37 spatial features in CA_200m_contour.t
    VRG_site_dist_200m <- data.table(st_distance(VRG_lat_lon_all.sf, CA_200m_contour.t))
    
    #we only want closest value (minimum distance)
    # Find the minimum distance for each row
    VRG_site_dist_200m_min <- apply(VRG_site_dist_200m, 1, min)
    
    #add back to point dt
    VRG_lat_lon_all.r[,dist_200m_bath := VRG_site_dist_200m_min]
    
  }
  
  if(USGS_socal == T){
    
    options(timeout=1000) #allow more time to download large rasters
    
    
    
    
    
    
    
    
  }
  
  return(lat_lon_bathy)
}