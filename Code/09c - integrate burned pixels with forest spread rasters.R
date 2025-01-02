
# This script adds the burned pixels to the forest spread rasters from 07 and 08.

library(raster)




##### combine rasters - worst case #####

### worst case: unrestricted non-native forest spread, no native forest restoration,
### unrestricted fire-driven forest loss.

# load year 2070 and year 2100 landcover rasters - nonnative forest spread and no native reforestation
ras_landcover2070 <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07b unprotected/',
                'year 054.tif'))
ras_landcover2100 <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07b unprotected/',
                'year 084.tif'))

# load fire-driven forest loss rasters - no reduction in risk/pixels burned
ras_burnedPixels2070 <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                '09b - ras_forestBurned2070.tif'))
ras_burnedPixels2100 <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                '09b - ras_forestBurned2100.tif'))

# combine rasters
ras_worstCase2070 <- ras_landcover2070
ras_worstCase2070[ras_burnedPixels2070 == -2] <- -2
ras_worstCase2100 <- ras_landcover2100
ras_worstCase2100[ras_burnedPixels2100 == -2] <- -2

# save rasters
writeRaster(ras_worstCase2070,
            filename = paste0(
              'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
              'worst case 2070.tif'
            ), overwrite = TRUE)
writeRaster(ras_worstCase2100,
            filename = paste0(
              'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
              'worst case 2100.tif'
            ), overwrite = TRUE)




##### combine rasters - middle case #####

### middle case: non-native forest spread restricted to areas w/o ungulate protection,
### no native forest restoration, 50% reduction in fire-driven forest loss.

# load year 2070 and year 2100 landcover rasters - nonnative forest spread and no native reforestation
ras_landcover2070 <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07d ungulate protected/protected pixels set to -1/',
                'year 054.tif'))
ras_landcover2100 <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07d ungulate protected/protected pixels set to -1/',
                'year 084.tif'))

# load fire-driven forest loss rasters - no reduction in risk/pixels burned
ras_burnedPixels2070 <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                '09b - ras_forestBurned2070_50pctReduction.tif'))
ras_burnedPixels2100 <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                '09b - ras_forestBurned2100_50pctReduction.tif'))

# combine rasters
ras_middleCase2070 <- ras_landcover2070
ras_middleCase2070[ras_burnedPixels2070 == -2] <- -2
ras_middleCase2100 <- ras_landcover2100
ras_middleCase2100[ras_burnedPixels2100 == -2] <- -2

# save rasters
writeRaster(ras_middleCase2070,
            filename = paste0(
              'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
              'middle case 2070.tif'
            ), overwrite = TRUE)
writeRaster(ras_middleCase2100,
            filename = paste0(
              'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
              'middle case 2100.tif'
            ), overwrite = TRUE)
