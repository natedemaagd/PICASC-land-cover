
# This script fixes some miscellaneous cases where certain landcovers were adjusted
# for the initial spread simulation and still need to be switched back.

library(terra)




##### data #####

# list all rasters
list_rasters <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/',
                           full.names = TRUE, pattern = '.tif')
list_rasters <- list_rasters[!grepl('V2', list_rasters)]  # don't include the finalized rasters (created here) if they already exist

# list dummy rasters
list_rastersDummy <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values',
                                pattern = '.tif', full.names = TRUE)




##### adjustments #####

# list forest/grass types (note that these exclude the "new" values searched for in the loop below to streamline the code)
vec_alienForest <- c(1600, 1700, 11900, 12000)
vec_nativeForest <- c(100, 200, 300, 400, 500, 1200, 10100, 10200, 10300, 10400, 10500, 10600,
                      600, 700, 800, 1500, 10700, 10800, 10900, 11700)
vec_alienGrass <- c(3700, 3800, 3900)

# replace values in rasters if needed
for(i in 1:length(list_rasters)){
  
  # load original landcover raster (must load each time since we clear tmp files)
  ras_original <- rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
                              "mhi_s0_baseline_noNames.tif"))
  
  # get raster i
  r <- rast(list_rasters[[i]])
  
  # get dummy raster associated with scenario of raster i
  r_dummyName <- strsplit(list_rasters[[i]], '/')[[1]]
  r_dummyName <- r_dummyName[[length(r_dummyName)]]
  r_dummyName <- strsplit(r_dummyName, ' ')[[1]]
  r_dummyName <- paste(r_dummyName[1:3], collapse = ' ')
  r_dummyName <- grep(r_dummyName, list_rastersDummy)
  r_dummy <- rast(list_rastersDummy[[r_dummyName]])
  
  # replace values to be original if they weren't part of conversion
  for(j in 1:length(vec_alienForest)){
    r[ras_original == vec_alienForest[[j]] & r_dummy == 0] <- vec_alienForest[[j]]
    gc()
  }
  for(j in 1:length(vec_nativeForest)){
    r[ras_original == vec_nativeForest[[j]] & r_dummy %in% c(-1, 1)] <- vec_nativeForest[[j]]
    gc()
  }
  for(j in 1:length(vec_alienGrass)){
    r[ras_original == vec_alienGrass[[j]] & r_dummy %in% c(-2, -5)] <- vec_alienGrass[[j]]
  }
  
  # generate filenames
  name_r <- substr(list_rasters[[i]], 1, nchar(list_rasters[[i]])-4)
  name_r <- paste0(name_r, ' - finalizedV2.tif')
  
  # save raster
  writeRaster(r, filename = name_r, overwrite = TRUE)
  
  rm(r, name_r, r_dummyName, r_dummy)
  gc()
  tmpFiles(remove = TRUE)
  print(paste(i, '-', Sys.time()))
}
rm(i,j)
