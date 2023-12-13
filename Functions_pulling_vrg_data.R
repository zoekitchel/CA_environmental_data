#note, you can only have one true at a time (only return one file)
#note, that all archived files MUST be in 'Old Versions' file for the following code to work

pull_VRG_data_files <- function(dropbox_path = NA, #you could manually set dropbox path if this isn't working
                                newest = T, #do you want newest version of data file (T) or list of all options (F)
                                event = F, #do you want raw event data?
                                fish = F, #do you want raw fish observation data?
                                swath = F, #do you want raw swath data?
                                UPC = F, #do you want raw UPC data?
                                UPC_format = NA, #do you want UPC data in pisco format or vrg format? c("pisco","vrg")
                                ISC = F, #do you want raw ISC data?
                                all_sites = F, #do you want all sites table?
                                all_fish = F, #do you want all fish table?
                                benthic_reef_spp = F #do you want benthic reef species table?
                                                      ){
  if(is.na(dropbox_path)){
    dropbox_path <- file.path(path.expand("~"),"Dropbox")
    
    #does this folder exist?
    if(!dir.exists(dropbox_path)){
      stop("The default script is not able to locate Dropbox on your computer. Please enter path manually in function argument dropbox_path = ")
      }
    }
    VRG_files <- "VRG Files"

    #full path name
    VRG_files_full_path <- file.path(dropbox_path, VRG_files)
  
  
  
  ############################################################################
  #Event data
  ############################################################################
  if(event == T){
    VRG_integrated_event_files <- list.files(path = VRG_files_full_path, recursive = T, pattern = "Integrated Event Data.*\\.csv")
    
    if(newest == T){
      VRG_integrated_event_files <- VRG_integrated_event_files[!grepl("Old Versions",VRG_integrated_event_files)]
      
      VRG_integrated_event <- fread(file.path(VRG_files_full_path,VRG_integrated_event_files))
      
      return(VRG_integrated_event)
    }else{return(VRG_integrated_event_files)}
    
  }
  ############################################################################
  #Fish data
  ############################################################################
  if (fish == T){
    VRG_integrated_fish_files <- list.files(path = VRG_files_full_path, recursive = T, pattern = "Integrated Fish Data .*\\.csv")
    
    if(newest == T){
      VRG_integrated_fish_files <- VRG_integrated_fish_files[!grepl("Old Versions",VRG_integrated_fish_files)]
      
      VRG_integrated_fish <- fread(file.path(VRG_files_full_path,VRG_integrated_fish_files))  
      return(VRG_integrated_fish)
    }else{return(VRG_integrated_fish_files)}
    
  }
  ############################################################################
  #UPC data
  ############################################################################
  if (UPC == T & is.na(UPC_format)){
    print("Please specify UPC_format as `pisco` or `vrg` in function arguments")
  }
  
  
  if (UPC == T & UPC_format %in% c("vrg", "VRG")){ #VRG format
    VRG_integrated_UPC_files <- list.files(path = VRG_files_full_path, recursive = T, pattern = "Integrated VRG UPC Data .*\\.csv")
    
    if(newest == T){
      VRG_integrated_UPC_files <- VRG_integrated_UPC_files[!grepl("Old Versions",VRG_integrated_UPC_files)]
      
      VRG_integrated_UPC <- fread(file.path(VRG_files_full_path,VRG_integrated_UPC_files))
      return(VRG_integrated_UPC)
    }else(return(VRG_integrated_UPC_files))
    
  }
  
  if (UPC == T & UPC_format %in% c("pisco", "PISCO")){ #PISCO format
    VRG_integrated_UPC_files <- list.files(path = VRG_files_full_path, recursive = T, pattern = "Integrated UPC PISCO Data .*\\.csv")
    
    if(newest == T){
      VRG_integrated_UPC_files <- VRG_integrated_UPC_files[!grepl("Old Versions",VRG_integrated_UPC_files)]
      
      VRG_integrated_UPC <- fread(file.path(VRG_files_full_path,VRG_integrated_UPC_files))
      return(VRG_integrated_UPC)
    }else(return(VRG_integrated_UPC_files))
    
  }
  ############################################################################
  #Swath data
  ############################################################################
  if (swath == T){
    VRG_integrated_swath_files <- list.files(path = VRG_files_full_path, recursive = T, pattern = "Integrated Swath Data .*\\.csv")
    
    if(newest == T){
      VRG_integrated_swath_files <- VRG_integrated_swath_files[!grepl("Old Versions",VRG_integrated_swath_files)]
      VRG_integrated_swath <- fread(file.path(VRG_files_full_path,VRG_integrated_swath_files))  
      return(VRG_integrated_swath)
    }else{return(VRG_integrated_swath_files)}
    
  }
  ############################################################################
  #ISC data
  ############################################################################
  if (event == T){
    VRG_integrated_ISC_files <- list.files(path = VRG_files_full_path, recursive = T, pattern = "Integrated ISC Data .*\\.csv")
    
    if(newest == T){
      
      VRG_integrated_ISC_files <- VRG_integrated_ISC_files[!grepl("Old Versions",VRG_integrated_ISC_files)]
      
      VRG_integrated_ISC <- fread(file.path(VRG_files_full_path,VRG_integrated_ISC_files))  
      return(VRG_integrated_ISC)
      
    }else{return(VRG_integrated_ISC_files)}
    
  }
  
  
  ###########################################################################
  #All sites (ALL_Sites_)
  ###########################################################################
  if (all_sites == T){
    VRG_all_sites_files <- list.files(path = VRG_files_full_path, recursive = T, pattern = "ALL_Sites_.*\\.csv")
    
    if(newest == T){
      
      VRG_all_sites_files <-VRG_all_sites_files[!grepl("Old Versions",VRG_all_sites_files)]
      
      VRG_all_sites <- fread(file.path(VRG_files_full_path,VRG_all_sites_files))
      return(VRG_all_sites)
      
    }else{return(VRG_all_sites_files)}
    
  }
  ############################################################################
  #All fish (Species.Length.Weight.Guild.)
  ############################################################################
  if (all_fish == T){
    VRG_all_fish_files <- list.files(path = VRG_files_full_path, recursive = T, pattern = "Species.Length.Weight.Guild.*\\.csv")
    
    if(newest == T){
      #exclude strings
      exclude_strings <- c("Old Versions", "Latitude Integration", "Platform Project")
      pattern <- paste0(exclude_strings, collapse = "|")
      
      VRG_all_fish_files <-VRG_all_fish_files[!grepl(pattern,VRG_all_fish_files)]
      
      VRG_all_fish <- fread(file.path(VRG_files_full_path,VRG_all_fish_files))
      return(VRG_all_fish)
      
    }else{return(VRG_all_fish_files)}
    
  }
  
  ############################################################################
  #Benthic reef species (BRS ONLY)
  ############################################################################
  if (benthic_reef_spp == T){
    VRG_BRS_files <- list.files(path = VRG_files_full_path, recursive = T, pattern = "BRS ONLY.*\\.csv")
    
    if(newest == T){
      
      VRG_BRS_files <-VRG_BRS_files[!grepl("Old Versions",VRG_BRS_files)]
      
      VRG_BRS <- fread(file.path(VRG_files_full_path,VRG_BRS_files))  
      return(VRG_BRS)
      
    }else{return(VRG_BRS_files)}
    
  }
}