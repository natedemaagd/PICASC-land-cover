
# This script generates the invasibility probability raster used in the
# spread of non-native forest simulation.

library(raster)

setwd("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/invasibility/baseline_pmw/")

# load invasibility rasters for desired species
list_rasters <- lapply(list("Leuc_Thresh.tif", "Mico_Thresh.tif", "More_Thresh.tif",
                            "Psid_Thresh.tif", "Schi_Thresh.tif", "Ulex_Thresh.tif"),
                       raster)

# for values of -9999, replace with NA
list_rasters <- lapply(list_rasters,
                       function(r){
                         r[r == -9999] <- NA
                         r
                       })

# get mean of invasibility values
stack_rasters <- stack(list_rasters)
raster_meanInvasibility <-
  mean(stack_rasters, na.rm = TRUE)

# save raster
writeRaster(raster_meanInvasibility,
            filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07a_mean_forest_invasibility.tif',
            overwrite = TRUE)
