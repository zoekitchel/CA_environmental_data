##############################################################################
#California Bathymetry Data
#Zoë J. Kitchel
#Created 30 October 2023
#Modified 11 December 2023
##############################################################################
#There are multiple options for where to get bathymetry data:
  #1) Marmap package (but our sites may be too close to shore for this to work, example below anyway though)
  #2) Full state of California out to 200 eez lower rez: https://wildlife.ca.gov/Conservation/Marine/GIS/Downloads (Arc/Info Binary Grid (ADF) )
            #200mEEZ_BathyGrids.zip > bd200m_v2i
  #3) USGS, SoCal only, "A Seamless, High-Resolution, Coastal Digital Elevation Model (DEM) for Southern California": https://pubs.usgs.gov/ds/487/ds487_text.pdf
  #4) We have some (limited) site specific bathymetry. Talk to Chelsea. Not included here.

##############################################################################
#SETUP#
library(sf)
library(data.table)
library(ggplot2)
library(dplyr)
library(maps)
library(raster)
library(rnaturalearth)
library(viridis)
library(RColorBrewer)
library(marmap) #to pull depth data
library(rasterVis)
library(ggspatial)
library(ggmap) #background google map

source("Functions_pulling_vrg_data.R") #this function pulls most recent clean version of raw data

options(timeout=1000) #allow more time to download large rasters

############################################################################
#Load up all VRG lat longs
############################################################################
events <- pull_VRG_data_files(event = T) #specifically we want raw event data

#flag some strange latitudes and longitudes
events[,flag_lat := ifelse(Latitude < 31, TRUE, FALSE)][,flag_long := ifelse(Latitude > 33.6 & Longitude > -117.5, TRUE, FALSE)]

#label if ARM
events[,ARM := ifelse(DepthZone == "ARM",TRUE, FALSE)]

#extract a site reference that is just mainland/island
mainland_island <- 

VRG_lat_lon_all <- unique(events[,.(Latitude, Longitude,SurveyDepth, flag_lat, flag_long, ARM, SamplingOrganization)])

VRG_lat_lon_all[,Longitude := ifelse(!(Longitude >= -150 & Longitude <= -50),NA,Longitude)]
VRG_lat_lon_all <- VRG_lat_lon_all[complete.cases(VRG_lat_lon_all[, .(Longitude, Latitude, ARM)])]


#minimum and maximum lat and long
min_lat <- min(VRG_lat_lon_all$Latitude, na.rm = T)
min_long <- min(VRG_lat_lon_all$Longitude, na.rm = T)
max_lat <- max(VRG_lat_lon_all$Latitude, na.rm = T)
max_long <- max(VRG_lat_lon_all$Longitude, na.rm = T)

#ALTERNATIVELY, only pull in lat lon from priority sites (I use 2022 list here)
VRG_priority_lat_lon <- fread(file.path("VRG_sites","2022_DiveSitePriority_Coor_List.csv"))

#take average lat and lon across matching sites
VRG_priority_lat_lon[,Latitude:= mean(Lat),.(Site)][,Longitude := mean(Lon),.(Site)]

VRG_priority_lat_lon.r <- unique(VRG_priority_lat_lon[,.(Site,Latitude, Longitude, ARM)])

#Version with no island sites
VRG_priority_lat_lon.mainland <- VRG_priority_lat_lon.r[!grepl("SCLI|SCAI|SNI|SBI",VRG_priority_lat_lon.r$Site)]

############################################################################
#Load up all VRG & PISCO lat longs
############################################################################
MLPA_kelpforest_site_table_4 <- fread(file.path("raw_data","PISCO","MLPA_kelpforest_site_table.4.csv"))

#Limit to correct geography
MLPA_sites_socal <- MLPA_kelpforest_site_table_4[latitude < 34.509 & longitude > -120.5191 & longitude < -117.11189]

#Only methods that include FISH
MLPA_fish_socal <- MLPA_sites_socal[grepl("FISH",MLPA_sites_socal[,method]),]

#Island mainland key
island_mainland_key <- data.table(CA_MPA_Name_Short = 
                                    c("Abalone Cove SMCA",                  "Anacapa Island SMCA",                "Anacapa Island SMR",                 "Arrow Point to Lion Head Point SMCA",
                                      "Begg Rock SMR",                      "Blue Cavern Onshore SMCA",           "Cabrillo SMR",                       "Campus Point SMCA",                 
                                      "Carrington Point SMR",               "Cat Harbor SMCA",                    "Crystal Cove SMCA",                  "Dana Point SMCA",                   
                                       "Farnsworth Onshore SMCA",            "Gull Island SMR",                    "Harris Point SMR",                   "Laguna Beach SMR",                  
                                       "Long Point SMR",                     "Lover's Cove SMCA",                  "Matlahuayl SMR",                     "N/A",                               
                                       "Naples SMCA",                        "Painted Cave SMCA",                  "Point Conception SMR",               "Point Dume SMCA",                   
                                       "Point Dume SMR",                     "Point Vicente SMCA",                 "Santa Barbara Island SMR",           "Scorpion SMR",                      
                                       "South La Jolla SMR",                 "South Point SMR",                    "Swamis SMCA",                        "Vandenberg SMR"),
                                  island_mainland = c("mainland", "island","island","island",
                                    "island", "island","mainland","mainland",
                                    "island","island","mainland","mainland",
                                    "island","island","island","mainland",
                                    "island","island","mainland","island",
                                    "mainland", "island","mainland","mainland",
                                    "mainland","mainland","island","island",
                                    "mainland","island","mainland","mainland"))

#Add island mainland status back to sites
MLPA_fish_socal <- MLPA_fish_socal[island_mainland_key, on = "CA_MPA_Name_Short"]

#Limit to only mainland sites
MLPA_fish_socal_mainland <- MLPA_fish_socal[island_mainland == "mainland"]

#Average lat lon from all years of data
MLPA_fish_socal_mainland_lat_lon <- MLPA_fish_socal_mainland[,.(latitude = mean(latitude), longitude = mean(longitude)), .(site, CA_MPA_Name_Short, island_mainland)]

#Change to VRG site naming convention
MLPA_fish_socal_mainland_lat_lon[,Site:=(gsub("_"," ",site))][,Site:= stringr::str_to_title(Site)][,Site:=gsub(" W$"," West",Site)][,Site:=gsub(" E$"," East",Site)][,Site:=gsub(" N$"," North",Site)][,Site:=gsub(" S$"," South",Site)]
MLPA_fish_socal_mainland_lat_lon[,Site:=(gsub("Children's Pool","Childrens Pool",site))][,Site:=(gsub("Swami's","Swamis",site))]

#Merge VRG with PISCO
VRG_PISCO_Fish_Mainland <- merge(MLPA_fish_socal_mainland_lat_lon,VRG_priority_lat_lon.mainland, on = "Site", all = T)

#If a PISCO site, move lat lon over to Lat Lon for plotting
VRG_PISCO_Fish_Mainland[,Latitude := ifelse(is.na(Latitude),latitude,Latitude)][,Longitude:=ifelse(is.na(Longitude),longitude,Longitude)]

#If NA, then mainland site
VRG_PISCO_Fish_Mainland[,ARM := ifelse(is.na(ARM),FALSE,ARM)]

############################################################################
#Make a map with our survey points
############################################################################

#map of world at high-ish resolution
world <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")

ggplot() + 
  geom_sf(data = world, fill = "palegreen", color ="darkgrey") + 
  geom_point(data = VRG_lat_lon_all, aes(x = Longitude, y = Latitude, color = ARM), size = 0.5) +
  coord_sf(xlim = c(-121,-115), ylim = c(30, 35)) +
  theme_classic()

#delete unexpected points
VRG_lat_lon_all.r <- VRG_lat_lon_all[flag_lat == FALSE & flag_long == FALSE,]

#only include vrg as sampling organization
VRG_lat_lon_all.vrg <- VRG_lat_lon_all.r[SamplingOrganization == "VRG",]

############################################################################
#1) Use Etopo2 from marmap package
############################################################################

#set square from which to extract bathy data from NOAA server
bathy_VRG <- getNOAA.bathy(min_long-2, max_long+2, min_lat, max_lat+0.5, resolution = 0.000001) #bathymetry matrix
VRG_lat_lon_all.r[,ETOPODepth := get.depth(bathy_VRG, VRG_lat_lon_all.r[, .(Longitude, Latitude)], locator=F)$depth]
VRG_lat_lon_all.r[,diff_survey_ETOPO := abs(ETOPODepth-SurveyDepth)]

#Not as bad as you might expect, but not perfect

#map of bathymetry
autoplot.bathy(bathy_VRG, geom=c("tile"
                                # ,"contour" exclude contour
                                 )) +
  scale_fill_etopo(breaks = c(-12000,-6000, 0, 6000, 10000), labels = c(12, 6, 0, 6, 10), #from marmap, great way to visualize land and water instead of 'world' object
                   guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
  borders("world", colour = "black") +
  geom_point(data = VRG_lat_lon_all.r, aes(x = Longitude, y = Latitude), size = 0.2, color = "red") +
  labs(y = "Latitude", x = "Longitude", fill = "Elevation/Depth\n(1000s of m)") +
  scale_x_continuous(breaks = c(-121:-115), labels = c("121˚W" ,"120˚W" ,"119˚W" ,"118˚W" ,"117˚W" ,"116˚W" ,"115˚W")) +
  scale_y_continuous(breaks = c(32:35),labels = c("32˚N" ,"33˚N" ,"34˚N" ,"35˚N")) +
  coord_sf(xlim = c(-121,-115), ylim = c(32, 35), expand = T) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0.2, 0.45),
        legend.justification = c("right", "top"),
        legend.box.just = "right")

#Zoom in a bit on Santa Monica Bay, and label points as ARM or natural (use VRG_priority_lat_lon.r instead)
#map of bathymetry
autoplot.bathy(bathy_VRG, geom=c("tile"
                                 # ,"contour" exclude contour
)) +
  scale_fill_etopo(breaks = c(-6000, -3000, 0, 3000, 6000), labels = c(6,3, 0,3, 6), #from marmap, great way to visualize land and water instead of 'world' object
                   guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
 # borders("world", colour = "black") +
  geom_point(data = VRG_priority_lat_lon.r, aes(x = Longitude, y = Latitude, color = ARM, shape = ARM), size = 4, alpha = 0.5) +
  labs(y = "Latitude", x = "Longitude", fill = "Elevation/Depth\n(1000s of m)") +
  #scale_x_continuous(breaks = c(-119:-117), labels = c("119˚W" ,"118˚W" ,"117˚W")) +
  #scale_y_continuous(breaks = c(32:34),labels = c("32˚N" ,"33˚N" ,"34˚N")) +
  scale_color_manual(values = c("hotpink3", "lightseagreen")) +
  scale_shape_manual(values = c(1,8)) +
  coord_sf(xlim = c(-119.5,-118), ylim = c(33.6,34.1), expand = T) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0.2, 0.45),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.box.background = element_rect(colour = "black"))

###########################################################################
#2) California from CDFW
############################################################################

#depth data
bathy_200m <- raster::raster("raw_data/200mEEZ_BathyGrids/bd200m_v2i/w001001.adf")
bathy_200m.dt <- data.table(as.data.frame(values(bathy_200m))) #just depth values
bathy_200m.xy <- data.table(as.data.frame(bathy_200m, xy = T)) #this should extract x/y and depth values correctly, but for some reason data are factored, so I have to do manually in next step

bathy_200m.dt <- cbind(bathy_200m.xy[,.(x,y)],bathy_200m.dt[,.(`values(bathy_200m)`)])

#coordinate system of raster
crs_Raster <- sf::st_crs(raster::crs(bathy_200m))

#convert points to sf with non-projected crs (must do first)
VRG_lat_lon_all.sf <- st_as_sf(VRG_lat_lon_all.r, coords = c("Longitude", "Latitude"),
                               crs = 4326) # WGS84 - a good default for unprojected coords)

VRG_lat_lon_all.t <- st_transform(VRG_lat_lon_all.sf, crs = crs_Raster) #convert to raster projection for extraction

#match sites to depths
VRG_lat_lon_all.r$CDFW_200m <- extract(bathy_200m, VRG_lat_lon_all.t)

#difference
VRG_lat_lon_all.r[,diff_survey_CDFW := abs(CDFW_200m-SurveyDepth)]

# Create a ggplot using the raster data frame
ggplot() +
  geom_raster(data=bathy_200m.dt, aes(x = x, y = y, fill = `values(bathy_200m)`)) +
  geom_sf(data = VRG_lat_lon_all.t, color = "red", size = 0.5) +
  scale_fill_gradientn(colors = c("midnightblue","steelblue4","steelblue3","steelblue1","lightsteelblue3","lightsteelblue1","white","darkgreen"),
                       breaks = c(-4830,-500,-20,-10,-5,-2,-0.01,0), name = "Depth") +
  lims(x = c(-63900,307400), y = c(-707400,-300000)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0.3, 0.3),
        legend.justification = c("right", "top"),
        legend.box.just = "right")

#Plot just Santa Monica Bay
ggplot() +
  geom_raster(data=bathy_200m.dt, aes(x = x, y = y, fill = `values(bathy_200m)`)) +
  geom_sf(data = VRG_lat_lon_all.t, color = "red", size = 0.5) +
  scale_fill_gradientn(colors = c("midnightblue","steelblue4","steelblue3","steelblue1","lightsteelblue3","lightsteelblue1","white","darkgreen"),
                       breaks = c(-4830,-500,-20,-10,-5,-2,-0.01,0), name = "Depth") +
  lims(x = c(-63900,307400), y = c(-707400,-300000)) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0.3, 0.3),
        legend.justification = c("right", "top"),
        legend.box.just = "right")


############################################################################
#Link to 200m isobath (typically high Chl-a along the shelf break that closely follows the 200 m isobath)
#200m isobath defined by CDFW
############################################################################

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

#same map as above, but with 200m isobath and points colored by distance to isobath
bathy_vrg_xyz <- as.xyz(bathy_VRG) #set as xyz to extract contour

autoplot.bathy(bathy_VRG, geom=c("tile"
                                 # ,"contour" exclude contour
)) +
  scale_fill_etopo(breaks = c(-12000,-6000, 0, 6000, 10000), labels = c(12, 6, 0, 6, 10), #from marmap, great way to visualize land and water instead of 'world' object
                   guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) + 
  borders("world", colour = "black") +
  labs(y = "Latitude", x = "Longitude", fill = "Elevation/Depth\n(1000s of m)") +
  geom_contour(data = bathy_vrg_xyz, 
               aes(x = V1, y = V2, z = V3),
               breaks = -200, color = "white", linewidth = 0.5) +
  geom_point(data = VRG_lat_lon_all.r, aes(x = Longitude, y = Latitude, color = dist_200m_bath), size = 0.2) +
  scale_colour_gradientn(colors = c("red","gold","white"), name = "Distance to 200m\nIsobath") +
  scale_x_continuous(breaks = c(-121:-115), labels = c("121˚W" ,"120˚W" ,"119˚W" ,"118˚W" ,"117˚W" ,"116˚W" ,"115˚W")) +
  scale_y_continuous(breaks = c(32:35),labels = c("32˚N" ,"33˚N" ,"34˚N" ,"35˚N")) +
  coord_sf(xlim = c(-121,-115), ylim = c(32, 35), expand = T) +
  theme_classic() +
  theme(axis.title = element_blank(),
        legend.position = c(0.93, 1.1),
        legend.justification = c("right", "top"),
        legend.box.just = "right")



###########################################################################
#3) California from Barnard and Hoover 2010
#Barnard, P.L., and Hoover, D., 2010, A seamless, high-resolution coastal 
#digital elevation model (DEM) for southern California. U.S. Geological Survey Data Series 487, 8 p. and database

###Note this is for southern california only (map coverage_area to check extent
############################################################################

#only download files you need, and delete after
#check if you've done this already
if(!file.exists(file.path("raw_data", "Barnard_hoover_2010", "DEMCoverageAreas.shp"))) {
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

# List the files in the temporary directory (optional)
extracted_files <- list.files(temp_dir, full.names = TRUE)

#move these files to Barnard_hoover_2010 folder in repository

files_to_move <- grep("DEMCoverageAreas", extracted_files, value = TRUE)

# Replace 'target_directory' with the path to your desired target directory
target_directory <- "raw_data/Barnard_hoover_2010"

# Move files to the target directory
file.rename(files_to_move, file.path(target_directory, basename(files_to_move)))

}

#make simple feature 
coverage_area <- read_sf(file.path("raw_data","Barnard_hoover_2010","DEMCoverageAreas.shp"))

#reduce to lat long
VRG_lat_lon_all.latlong <- unique(VRG_lat_lon_all.r[,.(Latitude, Longitude)])

#transform lat lon to match CRS of reference shapefile
VRG_lat_lon_all.sf <- st_as_sf(VRG_lat_lon_all.latlong, coords = c("Longitude", "Latitude"), crs = 4326) %>% 
                                        st_transform(crs = crs(coverage_area))

#add other lat lon values from previous projection
VRG_lat_lon_all.sf$Latitude <- VRG_lat_lon_all.latlong$Latitude
VRG_lat_lon_all.sf$Longitude <- VRG_lat_lon_all.latlong$Longitude



#which lat long points link to which relevant DEM boundaries (DEM_ID)

VRG_lat_lon_DEM_link <- st_intersection(VRG_lat_lon_all.sf, coverage_area)


#DEM_ID, put into single vector
DEM_IDs <- levels(factor(VRG_lat_lon_DEM_link$DEM_ID))

#convert to lower
DEM_to_download <- tolower(levels(factor(VRG_lat_lon_DEM_link$DEM_ID)))

#setup data table to take stats
highres_socal <- data.table()

#download DEMs one by one to current working directory (temporarily! these are large)
for (i in 1:length(DEM_to_download)) {
  #relevant lat_lons
  VRG_lat_lon_DEM_link.subset <- VRG_lat_lon_DEM_link %>% filter(DEM_ID == DEM_IDs[i])
  
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
  highres_socal.subset <- data.table(Longitude = VRG_lat_lon_DEM_link.subset$Longitude,
                                     Latitude = VRG_lat_lon_DEM_link.subset$Latitude,
                                     highrez_depth = raster::extract(DEM_single_raster,  VRG_lat_lon_DEM_link.subset))   #extract relevant lat lon values
  
  #remove raster file
  file.remove(paste0(DEM_to_download[i],".txt"))
  
  #set up datatable
  highres_socal <- rbind(highres_socal, highres_socal.subset)
}

#Merge back with DT
VRG_lat_lon_all.test <- highres_socal[VRG_lat_lon_all.r, on = c("Longitude", "Latitude")] #8597 fall outside 

#Get a sense of which ones are missing (No islands! And missing some more inshore too!)
ggplot(data = VRG_lat_lon_all.test) +
  geom_point(aes(x = Longitude, y = Latitude, color = highrez_depth)) +
  theme_classic()

#how well correlated with SurveyDepth?
ggplot(data = VRG_lat_lon_all.test) +
  geom_point(aes(x = SurveyDepth, y = -highrez_depth)) +
  geom_smooth(aes(x = SurveyDepth, y = -highrez_depth),method = "lm") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_classic()

#how well correlated?

##########################
#Google map satellite background
##########################
#to run, you'll need google maps API key

# Plot just SMB and sites around there

# get the map info
SMB_map <- get_googlemap("Malibu, CA", zoom = 9, maptype = "satellite")

artificial_natural_reef_SMB <-
ggmap(SMB_map) + 
  geom_point(data = VRG_priority_lat_lon.r, aes(x = Longitude, y = Latitude, fill = ARM, shape = ARM), size = 2, color = "white", stroke = 0.2) +
  labs(y = "Latitude", x = "Longitude") +
  scale_fill_manual(values = c("hotpink3", "lightseagreen"), labels = c("Natural reef","Artificial reef")) +
  scale_shape_manual(values = c(21,24), labels = c("Natural reef","Artificial reef")) +
  scale_x_continuous(labels = c("119.5˚W" ,"119˚W" ,"118.5˚W" ,"118˚W"),
                     expand = c(0, 0), limits = c(-119.56,-117.8)) +
  scale_y_continuous(breaks = c(33.5,34), labels = c("33.5˚N" ,"34 ˚N"),
                     expand = c(0, 0), limits = c(33.3,34.25)) +
  theme_void() +
  theme(axis.text = element_text(), axis.ticks = element_line())

artificial_natural_reef_SMB

ggsave(artificial_natural_reef_SMB, path = file.path("figures","site_maps"), filename = "artificial_natural_reef_SMB.jpg", height = 5, width = 6)

# Plot california

# get the map info
CA_map <- get_googlemap("Santa Cruz, CA", zoom = 6, maptype = "hybrid")

full_CA_map <-
  ggmap(CA_map) + 
  theme_void()

full_CA_map

ggsave(full_CA_map, path = file.path("figures","site_maps"), filename = "full_CA_map.jpg", height = 5, width = 5)

##########################
#Google map satellite background with PISCO sites
##########################
#to run, you'll need google maps API key (you'll have to copy key from zoe.j.kitchel google maps account)

# Plot just SMB and sites around there

#Set bounds, and get map info
lats<-c(32.6,34.55)
lons<-c(-121,-116.8)
bb<-make_bbox(lon=lons,lat=lats,f=0.05)
full_SMB_map<-get_map(bb,maptype="satellite")

#map
artificial_natural_artificial_reef_SMB <-
  ggmap(full_SMB_map) + 
  geom_point(data = VRG_PISCO_Fish_Mainland, aes(x = Longitude, y = Latitude,color = ARM, shape = ARM), size = 2, fill = NA, stroke = 0.5) +
  scale_color_manual(values = c("yellow","red"),labels = c("Natural reef","Artificial reef")) +
  scale_shape_manual(values = c(1,5),labels = c("Natural reef","Artificial reef")) +
  labs(y = "Latitude", x = "Longitude", shape = "Reef type",color = "Reef type") +
  scale_x_continuous(breaks = c(-120,-119,-118,-117),labels = c("120 ˚W","119˚W" ,"118 ˚W","117 ˚W"),
                     expand = c(0, 0), limits = lons) +
  scale_y_continuous(breaks = c(33,33.5,34,34.5),labels = c("33 ˚N" ,"33.5˚N","34 ˚N","34.5˚N"),
                     expand = c(0, 0), limits = lats) +
  annotation_scale(text_col = "white", width_hint = 0.5) + 
  coord_sf(crs = 4326) +
  theme_void() +
  theme(axis.text = element_text(), axis.ticks = element_line(), legend.position = c(0.2,0.2))

artificial_natural_artificial_reef_SMB

ggsave(artificial_natural_artificial_reef_SMB, path = file.path("figures","site_maps"), filename = "artificial_natural_artificial_reef_SMB.jpg", height = 4, width = 6)

