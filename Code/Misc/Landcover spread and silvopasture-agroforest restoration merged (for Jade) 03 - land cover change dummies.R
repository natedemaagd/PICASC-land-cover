
# This script creates rasters with dummy variables for each land cover change type.

library(terra)




##### load rasters #####

# baseline landcover
rast_baseline <-
  rast(paste0(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
                     "mhi_s0_baseline_noNames.tif")))

# load simulation + agro-silvo results
list_rast_simulation_names <-
  list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
             pattern = '.tif',
             full.names = TRUE)
list_rast_simulation_names <-
  list_rast_simulation_names[-grep('_dummy', list_rast_simulation_names)]
list_rast_simulation <-
  lapply(list_rast_simulation_names, rast)
names(list_rast_simulation) <- list_rast_simulation_names
rm(list_rast_simulation_names); gc()

# remove existing raster names
for(r in 1:length(list_rast_simulation)){
  names(list_rast_simulation[[r]]) <- NULL
}
rm(r)




##### create dummy rasters #####

# define values used for alien forest spread
vec_forestAlien <- c(1900, 2000, 2100)
vec_forestAlien_susceptibles <- c(100, 200, 300, 400, 500, 10100, 10200, 10300, 10400, 10500, 10600,
                                  600, 700, 800, 1500, 10700, 10800, 10900, 11700)

# define values used for native forest
vec_forestNative <- c(100, 600, 1100)  # uses closed ohia
vec_forestNative_susceptibles <- c(1600, 1700, 1800, 1900, 2000, 2100,
                                   3700, 3800, 3900, 11800, 11900, 12000, 12100,
                                   13700, 13800, 13900, 14100)

# define values used for alien grasses (for identifying fire-driven forest loss)
vec_grassAlien <- c(3700, 3800, 3900)
vec_grassAlien_susceptibles <- c(1600, 1700, 1800, 1900, 2000, 2100,
                                 11800, 11900, 12000, 12100,
                                 13700, 13800, 13900, 14100,
                                 100, 200, 300, 400, 500, 10100, 10200, 10300, 10400, 10500, 10600,
                                 600, 700, 800, 1500, 10700, 10800, 10900, 11700)

# define agroforestry and silvopasture
vec_agroforestry_1 <- -10
vec_agroforestry_2 <- -20
vec_agroforestry_3 <- -30
vec_silvopasture   <- -100

# for each scenario, find pixels that changed
for(r in 1:length(list_rast_simulation)){
  
  # get raster for scenario r
  ras <- list_rast_simulation[[r]]
  
  # identify pixels that converted into alien forest
  ras[ras %in% vec_forestAlien & rast_baseline %in% vec_forestAlien_susceptibles] <- 5
  gc()
  
  # identify pixels that converted into native forest
  ras[ras %in% vec_forestNative & rast_baseline %in% vec_forestNative_susceptibles] <- 6
  gc()
  
  # identify pixels that converted from forest to alien grasses (burned)
  ras[ras %in% vec_grassAlien & rast_baseline %in% vec_grassAlien_susceptibles] <- 7
  gc()
  
  # identify pixels of restored agroforestry and silvopasture
  ras[ras %in% vec_agroforestry_1] <- 1
  ras[ras %in% vec_agroforestry_2] <- 2
  ras[ras %in% vec_agroforestry_3] <- 3
  ras[ras %in% vec_silvopasture]   <- 4
  
  # make all other pixels NA
  ras[!(ras %in% 1:7)] <- NA
  gc()
  
  # create scenario filename
  name <- strsplit(names(list_rast_simulation)[[r]], '/')[[1]][[10]]
  name <- substr(name, 1, nchar(name) - 4)
  name <- paste0(name, '_mask.tif')
  
  writeRaster(ras,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/Masks and underlying landcovers/',
                                name),
              overwrite = TRUE)
  
  rm(ras, name); gc()
}

rm(list=setdiff(ls(), 'rast_baseline'))
gc()






##### use masks to identify underlying affected baseline landcovers #####

# list mask rasters
list_rast_masks <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/Masks and underlying landcovers',
             pattern = '_mask',
             full.names = TRUE)

# keep list of names
list_rast_mask_names <- lapply(list_rast_masks, strsplit, '/')
list_rast_mask_names <- sapply(list_rast_mask_names, function(x){
  name <- x[[1]][[length(x[[1]])]]
  name <- substr(name, 1, nchar(name) - 9)
})

# mask baseline landcover raster under each scenario
for(r in 1:length(list_rast_masks)){
  
  # load mask raster for scenario r
  rast_mask <- rast(list_rast_masks[[r]])
  
  # mask baseline
  rast_baseline_masked <- mask(rast_baseline, rast_mask)
  
  # output raster
  writeRaster(rast_baseline_masked,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/Masks and underlying landcovers/',
                                list_rast_mask_names[[r]], '_landcover.tif'),
              overwrite = TRUE)
  
  rm(rast_mask, rast_baseline_masked)
  gc()
}
rm(r, list_rast_mask_names, list_rast_masks)
