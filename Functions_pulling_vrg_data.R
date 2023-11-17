#note, you can only have one true at a time (only return one file)
pull_VRG_data_files <- function(system = c("macos","windows","custom_path"), newest = T, event = F, fish = F, swath = F, UPC = F, ISC = F){
  
if(system == "macos"){  
  #Link to VRG Files dropbox

VRG_files <- file.path(path.expand("~"),"Dropbox","VRG Files")
VRG_files <- sub("/Documents", "", VRG_files)
}

if(system == "windows"){ #this needs to be checked on a windows
  VRG_files <- file.path(path.expand("~"),"Dropbox","VRG Files")
  VRG_files <- sub("/Documents", "", VRG_files)       
}
  
if(system == "custom_path"){
  x <- readline("What is your file path to dropbox? (i.e /Users/santaclaus/Dropbox/ or C::)")
}

############################################################################
#Event data
############################################################################
if(event == T){
  VRG_integrated_event_files <- list.files(path = VRG_files, recursive = T, pattern = "Integrated Event Data.*\\.csv")

if(newest == T){
VRG_integrated_event_files <- VRG_integrated_event_files[!grepl("Old Versions",VRG_integrated_event_files)]

VRG_integrated_event <- fread(file.path(VRG_files,VRG_integrated_event_files))

return(VRG_integrated_event)
}else{return(VRG_integrated_event_files)}

  }
############################################################################
#Fish data
############################################################################
if (fish == T){
  VRG_integrated_fish_files <- list.files(path = VRG_files, recursive = T, pattern = "Integrated Fish Data .*\\.csv")

if(newest == T){
  VRG_integrated_fish_files <- VRG_integrated_fish_files[!grepl("Old Versions",VRG_integrated_fish_files)]
  
  VRG_integrated_fish <- fread(file.path(VRG_files,VRG_integrated_fish_files))  
  return(VRG_integrated_fish)
}else{return(VRG_integrated_fish_files)}
  
  }
############################################################################
#UPC data
############################################################################
if (UPC == T){
  VRG_integrated_UPC_files <- list.files(path = VRG_files, recursive = T, pattern = "Integrated VRG UPC Data .*\\.csv")

if(newest == T){
  VRG_integrated_UPC_files <- VRG_integrated_UPC_files[!grepl("Old Versions",VRG_integrated_UPC_files)]
  
  VRG_integrated_UPC <- fread(file.path(VRG_files,VRG_integrated_UPC_files))
  return(VRG_integrated_UPC)
}else(return(VRG_integrated_UPC_files))

  }
############################################################################
#Swath data
############################################################################
if (swath == T){
  VRG_integrated_swath_files <- list.files(path = VRG_files, recursive = T, pattern = "Integrated Swath Data .*\\.csv")
  
  if(newest == T){
    VRG_integrated_swath_files <- VRG_integrated_swath_files[!grepl("Old Versions",VRG_integrated_swath_files)]
    VRG_integrated_swath <- fread(file.path(VRG_files,VRG_integrated_swath_files))  
    return(VRG_integrated_swath)
  }else{return(VRG_integrated_swath_files)}
  
  }
############################################################################
#ISC data
############################################################################
if (event == T){
  VRG_integrated_ISC_files <- list.files(path = VRG_files, recursive = T, pattern = "Integrated ISC Data .*\\.csv")
  
  if(newest == T){
    
    VRG_integrated_ISC_files <- VRG_integrated_ISC_files[!grepl("Old Versions",VRG_integrated_ISC_files)]
    
    VRG_integrated_ISC <- fread(file.path(VRG_files,VRG_integrated_ISC_files))  
    return(VRG_integrated_ISC)
    
  }else{return(VRG_integrated_ISC_files)}
  
  }
}
