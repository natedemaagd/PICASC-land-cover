
# This script generates the invasibility probability raster used in the
# spread of non-native forest simulation.

library(raster)

setwd("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/invasibility rasters Lucas Fortini/data_release_files")

# list only nested suitability rasters for species of interest
list_rasters <-
  list.files(pattern = 'nested')
list_rasters <-
  grep(pattern = 'suitability', x = list_rasters, value = TRUE)
list_rasters <-
  grep(pattern = '.tif.aux', x = list_rasters, value = TRUE, invert = TRUE)
list_rasters_forest <-
  grep(pattern = 'Leucaena|Miconia|Morella|Psidium|Schinus|Ulex',
       x = list_rasters, value = TRUE)
list_rasters_grass <- 
  grep(pattern = 'Cenchrus|Melinis|Panicum',
       x = list_rasters, value = TRUE)

# load invasibility rasters for desired species
list_rasters_forest <- lapply(list_rasters_forest, raster)
list_rasters_grass <- lapply(list_rasters_grass, raster)

# for values of -9999, replace with NA
list_rasters_forest <- lapply(list_rasters_forest,
                       function(r){
                         r[r == -9999] <- NA
                         r
                       })
list_rasters_grass <- lapply(list_rasters_grass,
                              function(r){
                                r[r == -9999] <- NA
                                r
                              })

# get max of invasibility values
stack_rasters_forest <- stack(list_rasters_forest)
stack_rasters_grass <- stack(list_rasters_grass)
raster_maxInvasibility_forest <-
  max(stack_rasters_forest, na.rm = TRUE)
raster_maxInvasibility_grass <-
  max(stack_rasters_grass, na.rm = TRUE)

# save raster
writeRaster(raster_maxInvasibility_forest,
            filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07a_max_forest_invasibility.tif',
            overwrite = TRUE)
writeRaster(raster_maxInvasibility_grass,
            filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07a_max_grass_invasibility.tif',
            overwrite = TRUE)
