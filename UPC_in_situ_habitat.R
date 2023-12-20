##############################################################################
#Functions to calculate in situ relief and substrate metrics from UPC (like Susie did in SK biomass project)

#Some elements of code pulled from VRG Files/R Code/SK Analyses/Biomass Habitat/Process Env Var for SK Data.R

# We will also add in a measure of diversity/richness/evenness (Simpson diversity index)

#Zoë J. Kitchel
#Created 14 December 2023
#Modified 20 December 2023

###########################################################################
#NB: this function requires data.table
############################################################################
#Vertical relief 
#Calculate transect mean (using quantitative representations of relief)
#Also calculate variability, measured as both SD and simpson diversity (includes both evenness and richness)
############################################################################
get_relief <- function(UPC_complete){

  UPC_complete <- data.table(UPC_complete)
  relief_only <- UPC_complete[Category == "UPC Relief",]

  relief_only[, Relief_quant := ifelse(Taxa == "0-.1m", 0.0, 
                                     ifelse(Taxa == ".1-1m", 0.0,
                                            ifelse(Taxa == "1-2m", 1.0, 
                                                   ifelse(Taxa == ">2m",2.0,NA))))] # scales off of lowest value
  
  #all relief less than 1 m has the same relief value, so we will simplify to this value instead of Taxa designation and remove taxa column
    #three rows per transect instead of 4
 
   #take sum of BRS_per_cov for each Relief_quant, and change to 0.XXX instead of XX.X%
  relief_only[, BRS_per_cov_decimal := sum(BRS_per_cov/100),.(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone, Category, Relief_quant)]
  
  #new data table with 3 rows per transect instead of 4
  relief_only.r <- unique(relief_only[,.(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone, Category, Relief_quant, BRS_per_cov_decimal)])
  
  #take dot product per transect (sum of each relief type * percent cover), %*% = dot product
  relief_only.r[, Relief_index := (BRS_per_cov_decimal) %*% Relief_quant, .(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone)] 
  
  #how many points for each relief type, given #meters sampled = 31
  relief_only.r[, relief_points := round(BRS_per_cov_decimal*31,0)]
  
  #SD of relief over space
  relief_only.r[, Relief_SD := sd(rep(Relief_quant,relief_points)), .(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone)]
  
  #Simpson's Diversity Index is a measure of diversity which takes into account the number of species present, 
        #as well as the relative abundance of each species.
  
        #equation = 1-(sum(n(n-1))/N(N-1)) where n = # of meters of each relief type and N = total # of meters in transect (always 31)
  #we will take simpson's diversity of relief
  #numerator (n(n-1)) and excluding all 0s
  
  relief_only.r[, Relief_simpson_numerator := ifelse(relief_points == 0, 0, sum(relief_points)*(sum(relief_points)-1)),.(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone, Relief_quant)]
  
  #calculate Simpson diversity index for each transect
  relief_only.r[, Relief_simpson := 1-sum(Relief_simpson_numerator)/(31*(31-1)), .(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone)]
  
  #reduce to unique transects
  relief_by_transect <- unique(relief_only.r[,.(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone, Relief_index, Relief_SD, Relief_simpson)])
  
  #return a site/year/transect table
  
          return(relief_by_transect)
      }

#model and plotting SD vs Simpson diversity index for visualization
# relief_simp_SD_lm <- lm(Relief_simpson~Relief_SD, data = relief_by_transect)

# #plot for visualization of SD vs. Simpson diversity
# ggplot(data = relief_by_transect, aes(x = Relief_SD, y = Relief_simpson)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   annotate(geom = "text", x = 0.2, y = 0.8, label = paste0("R-squared = ",round(summary(relief_simp_SD_lm)$r.squared,2))) +
#   theme_classic()

#Characteristics of relatively high SD and relatively low Simpson diversity
#i.e. SD = 0.9 and simpson = 0.45
#80% .1-1m (quantitative value = 0)
#20% 	>2m (quantitative value = 2)
#-->Simpson doesn't care about distance between two habitat types quantitatively

#In comparison, those that are quite close to the trend line
#i.e. SD = 0.34 and simpson = 0.23 (MODERATE)
#40% .1-1m
#50% 0-.1m
#10% 1-2m
#0% 2+m

#or
#i.e. SD = 0.34 and simpson = 0.23 (HIGH)
#25% .1-1m
#25% 0-.1m
#25% 1-2m
#25% 2+m

############################################################################
#Substrate
#Mean (what's the typical substrate on this transect)
#Also, variability (spatial heterogeneity)
############################################################################

get_substrate <- function(UPC_complete){
  
  UPC_complete <- data.table(UPC_complete)
  substrate_only <- UPC_complete[Category == "UPC Substrate",]
  
  substrate_only[, Substrate_quant := ifelse(Taxa == "sand", 1.0, 
                                       ifelse(Taxa == "cobble", 2.0,
                                              ifelse(Taxa == "boulder", 3.0, 
                                                     ifelse(Taxa == "bedrock",4.0,NA))))] # scales off of lowest value

  #take dot product per transect (percent cover in decimals * # corresponding with substrate type), %*% = dot product
  substrate_only[, Substrate_index := (BRS_per_cov/100) %*% Substrate_quant, .(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone)] 
  
  #how many points for each substrate type, given #meters sampled = 31
  substrate_only[, substrate_points := round((BRS_per_cov/100)*31,0)]
  
  #SD of substrate over space
  substrate_only[, Substrate_SD := sd(rep(Substrate_quant,substrate_points)), .(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone)]
  
  #Simpson's Diversity Index is a measure of diversity which takes into account the number of species present, 
  #as well as the relative abundance of each species.
  
  #equation = 1-(sum(n(n-1))/N(N-1)) where n = # of meters of each substrate type and N = total # of meters in transect (always 31)
  #we will take simpson's diversity of substrate
  #numerator (n(n-1)) and excluding all 0s
  
  substrate_only[, Substrate_simpson_numerator := ifelse(substrate_points == 0, 0, sum(substrate_points)*(sum(substrate_points)-1)),.(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone, Substrate_quant)]
  
  #calculate Simpson diversity index for each transect
  substrate_only[, Substrate_simpson := 1-sum(Substrate_simpson_numerator)/(31*(31-1)), .(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone)]
  
  #reduce to unique transects
  substrate_by_transect <- unique(substrate_only[,.(Project, Era, SampleYear, Site, SampleDate, DiveReplicate, DepthZone, Substrate_index, Substrate_SD, Substrate_simpson)])
  
  #return a site/year/transect table
  
  return(substrate_by_transect)
}

#model and plotting SD vs Simpson diversity index for visualization
# substrate_simp_SD_lm <- lm(Substrate_simpson~Substrate_SD, data = substrate_by_transect)
# 
# #plot for visualization of SD vs. Simpson diversity
# ggplot(data = substrate_by_transect, aes(x = Substrate_SD, y = Substrate_simpson)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   annotate(geom = "text", x = 0.2, y = 0.8, label = paste0("R-squared = ",round(summary(substrate_simp_SD_lm)$r.squared,2))) +
#   theme_classic()

#Characteristics of relatively high SD and relatively low Simpson diversity
#i.e. SD = 1.20 and simpson = 0.32
#80% bedrock (quantitative value = 4)
#20% 	sand (quantitative value = 1)
#-->Simpson doesn't care about distance between two substrate types quantitatively

#In comparison, those that are quite close to the trend line
#i.e. SD = 0.42 and simpson = 0.18 (LOW)
#90% bedrock
#6% boulder
#3% cobble
#0% sand

#or
#i.e. SD = 1.35 and simpson = 0.68 (HIGH)
#60% bedrock
#0% boulder
#3% cobble
#38% sand




