
# This script ensures the inputs rasters for the landcover baseline raster do
# not result in clipping the extent when creating the mosaic layer.

library(terra)




##### load data #####

# define input directory
dir_inputs <- 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/input_baseline/'

# load input rasters
rast_landcover <- rast(paste0(dir_inputs, 'cah_luc_codes.tif'))
rast_pasture <- rast(paste0(dir_inputs, 'land_cover_pasture_luc_codes_w_agric_codes.tif'))
rast_lava <- rast(paste0(dir_inputs, 'lava.tif'))




##### format rasters #####

# extents match for landcover and lava; pasture needs to be extended
rast_pasture_extended <- extend(rast_pasture, rast_landcover)

# create mosaic w/ landcover as baseline, then pasture, then lava
# NOTE: lava and pasture overlap so lava is prioritized over pasture
rast_mosaic <- rast_landcover
rast_mosaic[!is.na(rast_pasture_extended)] <-
  rast_pasture_extended[!is.na(rast_pasture_extended)]
rast_mosaic[!is.na(rast_lava)] <-
  rast_lava[!is.na(rast_lava)]




##### export rasters #####

# define output directory
dir_outputs <- "H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/output_baseline/"

# write rasters
writeRaster(rast_pasture_extended,
            filename = paste0(dir_outputs,
                              'land_cover_pasture_luc_codes_w_agric_codes_expandedExtent.tif'),
            overwrite = TRUE)
writeRaster(rast_mosaic,
            filename = paste0(dir_outputs,
                              'lc_pasture_lava_mosaic.tif'),
            overwrite = TRUE)
