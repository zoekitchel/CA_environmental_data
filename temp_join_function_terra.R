library(data.table)
library(dplyr)
library(rvest)
library(terra)
library(sf)
library(stringr)
library(purrr)

temp_join <- function(lat_lon, annual = FALSE, min_max = FALSE) {
  
  # 1 — Check for required columns
  required_cols <- c("Latitude", "Longitude")
  missing_cols <- setdiff(required_cols, names(lat_lon))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  if (any(is.na(lat_lon$Latitude) | is.na(lat_lon$Longitude))) {
    stop("Latitude/Longitude columns contain missing values.")
  }
  
  message("✅ Input validated — starting UCSD SST download and processing...")
  
  # 2 — Get list of HDF links
  folder_urls <- readLines(
    "https://raw.githubusercontent.com/zoekitchel/CA_environmental_data/refs/heads/main/data/ucsd_sstchl/ucsd_data_download_1km_sst_2016_2024.txt"
  )
  
  get_monthly_links <- function(url) {
    year <- as.numeric(substr(url, 30, 33))
    monthly <- url %>%
      read_html() %>%
      html_nodes("a") %>%
      html_attr("href") %>%
      keep(~ str_detect(.x, "comp\\.hdf"))
    
    data.table(
      variable = "sst",
      year = year,
      month = seq_len(length(monthly)), 
      file_name = monthly,
      link_long = paste0(url, monthly)
    )
  }
  
  full_link.dt <- rbindlist(lapply(folder_urls, get_monthly_links))
  
  # 3 — Helper: download or use local copy
  get_hdf <- function(file_name, url) {
    local_files <- list.files(".", pattern = file_name, recursive = TRUE, full.names = TRUE)
    if (length(local_files) == 0) {
      message("⬇️ Downloading ", file_name)
      download.file(url, file_name, mode = "wb")
      return(file_name)
    } else {
      message("📂 Using local file: ", local_files[1])
      return(local_files[1])
    }
  }
  
  # 4 — Load the lat/lon reference grid
  hdf_key_file <- get_hdf(
    "cal_aco_3840_Latitude_Longitude.hdf",
    "http://wimsoft.com/CAL/files/cal_aco_3840_Latitude_Longitude.hdf"
  )
  
  hdf_key <- rast(hdf_key_file)
  crop_ext <- ext(1617, 2069, 1727, 2204)
  hdf_key.c <- crop(hdf_key, crop_ext)
  hdf_key.xyz <- as.data.table(as.data.frame(hdf_key.c, xy = TRUE))
  colnames(hdf_key.xyz) <- c("x", "y", "latitude", "longitude")
  
  # 5 — Storage for results
  lat_lon_variable_full <- data.table()
  
  # 6 — Loop through each month
  for (i in seq_len(nrow(full_link.dt))) {
    tmp_file <- tempfile(fileext = ".hdf")
    download.file(full_link.dt[i, link_long], tmp_file, mode = "wb")
    
    hdf <- rast(tmp_file) %>%
      crop(crop_ext) %>%
      classify(matrix(c(255, NA), ncol = 2, byrow = TRUE))
    
    hdf.xyz <- as.data.table(as.data.frame(hdf, xy = TRUE))
    setnames(hdf.xyz, c("x", "y", "value"))
    
    hdf_merge <- merge(hdf.xyz, hdf_key.xyz, by = c("x", "y"))
    
    # Make a SpatRaster directly from merged coords
    r <- rast(
      xmin = min(hdf_merge$longitude),
      xmax = max(hdf_merge$longitude),
      ymin = min(hdf_merge$latitude),
      ymax = max(hdf_merge$latitude),
      ncol = 452, nrow = 477, crs = "EPSG:4326"
    )
    r <- rasterize(hdf_merge[, c("longitude", "latitude")], r, field = hdf_merge$value, fun = mean, na.rm = TRUE)
    
    # Convert site coords to sf & match CRS
    lat_lon.sf <- st_as_sf(lat_lon, coords = c("Longitude", "Latitude"), crs = 4326)
    values <- terra::extract(r, vect(lat_lon.sf))[, 2]
    
    # Fill missing with 10km buffer mean
    if (anyNA(values)) {
      missing_idx <- which(is.na(values))
      buffer_vals <- terra::extract(r, buffer(vect(lat_lon.sf[missing_idx, ]), width = 10000), fun = mean, na.rm = TRUE)[, 2]
      values[missing_idx] <- buffer_vals
    }
    
    lat_lon_variable <- copy(lat_lon)
    lat_lon_variable[, `:=`(
      year = full_link.dt$year[i],
      month = full_link.dt$month[i],
      sst = values
    )]
    
    lat_lon_variable_full <- rbind(lat_lon_variable_full, lat_lon_variable)
    message(i, "/", nrow(full_link.dt), " processed.")
  }
  
  # 7 — Adjust SST values
  lat_lon_variable_full[, sst_adj := 0.15 * sst - 3.0]
  
  # 8 — Summarize results based on arguments
  if (!annual && !min_max) {
    return(lat_lon_variable_full %>%
             group_by(AR_Complex, Site, year) %>%
             summarise(sst_avg_annual = mean(sst_adj, na.rm = TRUE), .groups = "drop") %>%
             group_by(AR_Complex, Site) %>%
             summarise(mean_sst = mean(sst_avg_annual), .groups = "drop"))
  }
  
  if (!annual && min_max) {
    return(lat_lon_variable_full %>%
             group_by(AR_Complex, Site, year) %>%
             summarise(
               sst_avg_annual = mean(sst_adj, na.rm = TRUE),
               sst_max_annual = max(sst_adj, na.rm = TRUE),
               sst_min_annual = min(sst_adj, na.rm = TRUE),
               .groups = "drop"
             ) %>%
             group_by(AR_Complex, Site) %>%
             summarise(
               mean_sst = mean(sst_avg_annual),
               max_sst = max(sst_max_annual),
               min_sst = min(sst_min_annual),
               .groups = "drop"
             ))
  }
  
  if (annual && !min_max) {
    return(lat_lon_variable_full %>%
             group_by(AR_Complex, Site, year) %>%
             summarise(sst_avg_annual = mean(sst_adj, na.rm = TRUE), .groups = "drop"))
  }
  
  if (annual && min_max) {
    return(lat_lon_variable_full %>%
             group_by(AR_Complex, Site, year) %>%
             summarise(
               sst_avg_annual = mean(sst_adj, na.rm = TRUE),
               sst_max_annual = max(sst_adj, na.rm = TRUE),
               sst_min_annual = min(sst_adj, na.rm = TRUE),
               .groups = "drop"
             ))
  }
}
