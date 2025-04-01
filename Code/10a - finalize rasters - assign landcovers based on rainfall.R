
# This script assigns new landcovers resulting from spreads and fires according
# to rainfall.

library(raster)




##### load data #####

# load rainfall raster
ras_rainfall <-
  raster(paste0("D:/Nextcloud/LandcareLab_Cloud/FireShed/Nate full re-run/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/",
                "HI_EVAP_mean_annual_rainfall__statewide_250m.tif"))

# load landcover raster
ras_landcover <-
  # raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
  #               "mhi_land_cover_names.tif"))
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
                "mhi_s0_baseline_noNames.tif"))




##### define pertinent landcovers #####

# define alien forest (wet, mesic, dry)
vec_forestAlien <- c(1900, 2000, 2100)

# define alien grassland (wet, mesic, dry)
vec_grassAlien <- c(3700, 3800, 3900)

# define native forest (closed ohia wet, mesic, dry)  ## IS THIS CORRECT??
vec_forestNative <- c(100, 600, 1100)

# define all landcovers by moisture zone
vec_moistZoneWet <- c(100, 200, 300, 400, 500, 1600, 1900, 2400, 2500, 3100,
                      3300, 3700)
vec_moistZoneMesic <- c(600, 700, 800, 900, 1000, 1500, 1700, 2000, 2600, 3200,
                        3400, 3800)
vec_moistZoneDry <- c(1100, 1200, 1300, 1400, 1800, 2100, 2200, 2700, 2800,
                      3500, 3900)





##### get rainfall summaries of pertinent landcovers #####

#resample rainfall raster
#ras_rainfall <- projectRaster(ras_rainfall, ras_landcover)
# writeRaster(ras_rainfall,
#             filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/rainfall_ann/",
#                               "HI_EVAP_mean_annual_rainfall__statewide_projectedToMHIlandcover.tif"),
#             overwrite = TRUE)
ras_rainfall <- raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/rainfall_ann/",
                              "HI_EVAP_mean_annual_rainfall__statewide_projectedToMHIlandcover.tif"))

# get rainfall values for each landcover type
dat_rainfallAlienForest <-
  list(rain_wetAlienForest = values(ras_rainfall)[values(ras_landcover) == 1900],
       rain_mesicAlienForest = values(ras_rainfall)[values(ras_landcover) == 2000],
       rain_dryAlienForest = values(ras_rainfall)[values(ras_landcover) == 2100])
dat_rainfallNativeForest <-
  list(rain_wetNativeForest = values(ras_rainfall)[values(ras_landcover) == 100],
       rain_mesicNativeForest = values(ras_rainfall)[values(ras_landcover) == 600],
       rain_dryNativeForest = values(ras_rainfall)[values(ras_landcover) == 1100])
dat_rainfallAlienGrass <-
  list(rain_wetAlienGrass = values(ras_rainfall)[values(ras_landcover) == 3700],
       rain_mesicAlienGrass = values(ras_rainfall)[values(ras_landcover) == 3800],
       rain_dryAlienGrass = values(ras_rainfall)[values(ras_landcover) == 3900])
gc()

# get median rainfall value for each landcover type, along with max island-wide rainfall for upper bound
vec_rainfallAlienForest_median <-
  sapply(dat_rainfallAlienForest, median, na.rm = TRUE)
vec_rainfallNativeForest_median <-
  sapply(dat_rainfallNativeForest, median, na.rm = TRUE)
vec_rainfallAlienGrass_median <-
  sapply(dat_rainfallAlienGrass, median, na.rm = TRUE)
vec_maxRainfallIslandwide <- ceiling(max(values(ras_rainfall), na.rm = TRUE))

# create rainfall breakpoints/bins (wet, mesic, dry)
vec_rainfallAlienForest_bins <-
  c(vec_maxRainfallIslandwide,
    round(vec_rainfallAlienForest_median[[1]] + vec_rainfallAlienForest_median[[2]]) / 2,
    round(vec_rainfallAlienForest_median[[2]] + vec_rainfallAlienForest_median[[3]]) / 2,
    0)
vec_rainfallNativeForest_bins <-
  c(vec_maxRainfallIslandwide,
    round(vec_rainfallNativeForest_median[[1]] + vec_rainfallNativeForest_median[[2]]) / 2,
    round(vec_rainfallNativeForest_median[[2]] + vec_rainfallNativeForest_median[[3]]) / 2,
    0)
vec_rainfallAlienGrass_bins <-
  c(vec_maxRainfallIslandwide,
    round(vec_rainfallAlienGrass_median[[1]] + vec_rainfallAlienGrass_median[[2]]) / 2,
    round(vec_rainfallAlienGrass_median[[2]] + vec_rainfallAlienGrass_median[[3]]) / 2,
    0)

rm(vec_rainfallAlienForest_median, vec_rainfallAlienGrass_median,
   vec_rainfallNativeForest_median, vec_maxRainfallIslandwide)
gc()




##### assign new landcover values based on bins #####

# load rasters to convert
list_finalRasters_names <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values',
             full.names = TRUE, pattern = '.tif')
list_finalRasters <-
  lapply(list_finalRasters_names, raster)

names(list_finalRasters) <- c('best case 2070', 'best case 2100',
                              'middle case 2070', 'middle case 2100',
                              'worst case 2070', 'worst case 2100')

# find burned pixels (forest -> alien grass) and find their rainfall values
pixelIDs_burned <-
  lapply(list_finalRasters,
         function(r){
           which(values(r) == -2)
         })

# find native forest converted to alien forest
pixelIDs_invadedForest <-
  lapply(list_finalRasters,
         function(r){
           which(values(r) == 0)
         })

# find restored native forest
pixelIDs_restoredForest <-
  lapply(list_finalRasters,
         function(r){
           which(values(r) == 1)
         })

# find forest to grass
pixelIDs_forestToGrass <-
  lapply(list_finalRasters,
         function(r){
           which(values(r) == -5)
         })

# use pixel IDs to extract rainfall value of pixel and assign it a rainfall bin/landcover
# NOTE: results are given DRY - MESIC - WET
pixelIDs_burned_rainfallBin <-
  lapply(pixelIDs_burned,
         function(v){
           if(length(v) > 0){
             cut(ras_rainfall[v], breaks = vec_rainfallAlienGrass_bins,
                 include.lowest = TRUE, labels = FALSE)
           } else {
             NA
           }
         })
pixelIDs_invadedForest_rainfallBin <-
  lapply(pixelIDs_invadedForest,
         function(v){
           if(length(v) > 0){
             cut(ras_rainfall[v], breaks = vec_rainfallAlienForest_bins,
                 include.lowest = TRUE, labels = FALSE)
           } else {
             NA
           }
           
         })
pixelIDs_restoredForest_rainfallBin <-
  lapply(pixelIDs_restoredForest,
         function(v){
           if(length(v) > 0){
             cut(ras_rainfall[v], breaks = vec_rainfallNativeForest_bins,
                 include.lowest = TRUE, labels = FALSE)
           } else {
             NA
           }
           
         })
pixelIDs_forestToGrass_rainfallBin <-
  lapply(pixelIDs_forestToGrass,
         function(v){
           if(length(v) > 0){
             cut(ras_rainfall[v], breaks = vec_rainfallAlienGrass_bins,
                 include.lowest = TRUE, labels = FALSE)
           } else {
             NA
           }
           
         })

gc()

# assign new landcover values based on rainfall bins
pixelIDs_burned_newLandcoverValue <-
  lapply(pixelIDs_burned_rainfallBin,
         function(v){
           if(length(v) != 1){
             rev(vec_grassAlien)[v]
           } else {
             NA
           }
         })
pixelIDs_invadedForest_newLandcoverValue <-
  lapply(pixelIDs_invadedForest_rainfallBin,
         function(v){
           if(length(v) != 1){
             rev(vec_forestAlien)[v]
           } else {
             NA
           }
         })
pixelIDs_restoredForest_newLandcoverValue <-
  lapply(pixelIDs_restoredForest_rainfallBin,
         function(v){
           if(length(v) != 1){
             rev(vec_forestNative)[v]
           } else {
             NA
           }
         })
pixelIDs_forestToGrass_newLandcoverValue <-
  lapply(pixelIDs_forestToGrass_rainfallBin,
         function(v){
           if(length(v) != 1){
             rev(vec_grassAlien)[v]
           } else {
             NA
           }
         })

# replace raster values according to newLandcoverValues
list_finalRasters_newLandcoverValues <- list()
for(i in 1:length(list_finalRasters)){
  
  # get raster i
  r <- list_finalRasters[[i]]
  
  # assign raster i's new landcover values, if applicable
  if(length(pixelIDs_burned_newLandcoverValue[[i]]) > 1){
    r[pixelIDs_burned[[i]]] <- pixelIDs_burned_newLandcoverValue[[i]]
  }
  if(length(pixelIDs_invadedForest_newLandcoverValue[[i]]) > 1){
    r[pixelIDs_invadedForest[[i]]] <- pixelIDs_invadedForest_newLandcoverValue[[i]]
  }
  if(length(pixelIDs_restoredForest_newLandcoverValue[[i]]) > 1){
    r[pixelIDs_restoredForest[[i]]] <- pixelIDs_restoredForest_newLandcoverValue[[i]]
  }
  if(length(pixelIDs_forestToGrass_newLandcoverValue[[i]]) > 1){
    r[pixelIDs_forestToGrass[[i]]] <- pixelIDs_forestToGrass_newLandcoverValue[[i]]
  }
  
  # return raster
  list_finalRasters_newLandcoverValues[[i]] <- r
  
  rm(r); gc(); print(paste0(i, '-', Sys.time()))
}
names(list_finalRasters_newLandcoverValues) <- names(list_finalRasters)




##### make moisture zone corrections if needed, based on original landcover moisture zone #####

# e.g., if original landcover was dry, make sure new landcover is dry

# load original landcover raster
ras_original <- raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
                              "mhi_s0_baseline_noNames.tif"))

# get raster values of original raster
vals_original <- values(ras_original); gc()

# for each case, replace landcover values to maintain consistent moisture zones
list_finalRasters_consistentMoistureZones <- list()
for(i in seq_along(list_finalRasters_newLandcoverValues)){
  
  # get values of raster i
  vec_vals <- values(list_finalRasters_newLandcoverValues[[i]]); gc()
  
  # replace ALIEN FOREST pixels if necessary, based on original moisture zone
  vec_vals[vec_vals %in% vec_forestAlien & vals_original %in% vec_moistZoneWet]   <- 1900  # wet
  vec_vals[vec_vals %in% vec_forestAlien & vals_original %in% vec_moistZoneMesic] <- 2000  # mesic
  vec_vals[vec_vals %in% vec_forestAlien & vals_original %in% vec_moistZoneDry]   <- 2100  # dry
  gc()
  
  # replace ALIEN GRASSLAND pixels if necessary, based on original moisture zone
  vec_vals[vec_vals %in% vec_grassAlien & vals_original %in% vec_moistZoneWet]   <- 3700  # wet
  vec_vals[vec_vals %in% vec_grassAlien & vals_original %in% vec_moistZoneMesic] <- 3800  # mesic
  vec_vals[vec_vals %in% vec_grassAlien & vals_original %in% vec_moistZoneDry]   <- 3900  # dry
  gc()
  
  # replace NATIVE FOREST pixels if necessary, based on original moisture zone
  vec_vals[vec_vals %in% vec_forestNative & vals_original %in% vec_moistZoneWet]   <-  100  # wet
  vec_vals[vec_vals %in% vec_forestNative & vals_original %in% vec_moistZoneMesic] <-  600  # mesic
  vec_vals[vec_vals %in% vec_forestNative & vals_original %in% vec_moistZoneDry]   <- 1100  # dry
  gc()
  
  # create new raster
  r <- list_finalRasters[[i]]
  values(r) <- vec_vals
  list_finalRasters_consistentMoistureZones[[i]] <- r
  gc(); print(paste0(i, '-', Sys.time()))
}
rm(i, r, vec_vals)
names(list_finalRasters_consistentMoistureZones) <-
  names(list_finalRasters_newLandcoverValues)

# for best case, any remaining dummy values (protected areas = -1) need to be returned to
# original land cover value
for(i in 1:length(list_finalRasters_newLandcoverValues)){
  r <- list_finalRasters_newLandcoverValues[[i]]
  r_consistent <- list_finalRasters_consistentMoistureZones[[i]]
  r[r == -1] <- ras_original[r == -1]
  r_consistent[r_consistent == -1] <- ras_original[r_consistent == -1]
  list_finalRasters_newLandcoverValues[[i]] <- r
  list_finalRasters_consistentMoistureZones[[i]] <- r_consistent
  rm(r, r_consistent); gc()
}
rm(i)




# # save rasters - janky moisture zones
# for(i in 1:length(list_finalRasters_newLandcoverValues)){
#   writeRaster(list_finalRasters_newLandcoverValues[[i]],
#               filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
#                                 names(list_finalRasters_newLandcoverValues)[[i]], '.tif'),
#               overwrite = TRUE)
#   print(paste0(i, '-', Sys.time()))
# }

# save rasters - corrected moisture zones
for(i in 1:length(list_finalRasters_consistentMoistureZones)){
  writeRaster(list_finalRasters_consistentMoistureZones[[i]],
              filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                                names(list_finalRasters_consistentMoistureZones)[[i]], ' - moisture zones consistent w original raster.tif'),
              overwrite = TRUE)
  print(paste0(i, '-', Sys.time()))
}

gc()
