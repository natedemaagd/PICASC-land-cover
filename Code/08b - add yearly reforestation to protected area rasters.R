
# This script adds the restored native forest to the protected areas from 08a
# by year. It adds it to both the masked rasters and the rasters with the
# original landcover values.

library(ggplot2)
library(raster)
library(viridis)
library(gganimate)




##### count pixels converted each year #####

# list non-native spread - protected areas masked
list_rastersNonnative_ungulateMasked <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07d ungulate protected/protected pixels set to -1',
             full.names = TRUE, pattern = '.tif')

# list non-native spread - protected areas original landcover values
list_rastersNonnative_ungulateActualValues <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07d ungulate protected/protected pixels set to original values',
             full.names = TRUE, pattern = '.tif')

# list reforestation rasters - masked values
list_rastersReforestationMasked <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08a - statewide landcover spread - native forest restoration in ungulate-protected areas - masked',
             full.names = TRUE, pattern = '.tif')

# reforestation took less than 100 years: repeat last raster to fill list of 100
list_rastersReforestationMasked_lastElement <-
  list_rastersReforestationMasked[[length(list_rastersReforestationMasked)]]
list_rastersReforestationMasked[length(list_rastersReforestationMasked):100] <-
  list_rastersReforestationMasked_lastElement



##### Each year, reforest appropriate pixels #####

# define mode function for use with raster aggregation below
Mode <- function(x, na.rm = TRUE) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# define year vector
vecYear <- 1:100
vecYear <- stringr::str_pad(vecYear, 3, side = 'left', pad = '0')

# replace values in each year
#for(i in 1:length(list_rastersReforestationMasked)){
for(i in 1:100){
  
  # NOTE:
  # - Protected areas in the non-native spread rasters are set to -1
  # - Non-native forest in all non-native spread rasters is set to 0
  # - So, reset reforested native pixels to be 1 before replacing
  
  # load year i rasters
  ras_nonnativeSpread_actual <- raster(list_rastersNonnative_ungulateActualValues[[i]])
  ras_nonnativeSpread_masked <- raster(list_rastersNonnative_ungulateMasked[[i]])
  ras_reforestation_masked <- raster(list_rastersReforestationMasked[[i]])
  
  # if landcover is native forest in `ras_reforestation_masked` (i.e. == 0), then
  # make it == 1 in the other rasters
  ras_nonnativeSpread_actual[ras_reforestation_masked == 0] <- 1
  ras_nonnativeSpread_masked[ras_reforestation_masked == 0] <- 1
  
  # save rasters in 30m and 250m resolution (250m for plotting timelapse)
  writeRaster(ras_nonnativeSpread_actual,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/30m rasters/unmasked/',
                                'year', vecYear[[i]], '.tif'),
              overwrite = TRUE)
  writeRaster(ras_nonnativeSpread_masked,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/30m rasters/masked/',
                                'year', vecYear[[i]], '.tif'),
              overwrite = TRUE)
  
  # aggregate to 250m then save
  ras_nonnativeSpread_actual <-
    aggregate(ras_nonnativeSpread_actual, fact = round(250/30), fun = Mode,
              expand = FALSE)
  ras_nonnativeSpread_masked <-
    aggregate(ras_nonnativeSpread_masked, fact = round(250/30), fun = Mode,
              expand = FALSE)
  writeRaster(ras_nonnativeSpread_actual,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/250m rasters/unmasked/',
                                'year', vecYear[[i]], '.tif'),
              overwrite = TRUE)
  writeRaster(ras_nonnativeSpread_masked,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/250m rasters/masked/',
                                'year', vecYear[[i]], '.tif'),
              overwrite = TRUE)
  
  removeTmpFiles(h = 1); gc()
  print(paste(i, '-', Sys.time()))
  
}

