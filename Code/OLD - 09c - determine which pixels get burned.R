
# This script uses the number of pixels to burn from 09b and the model of fire
# risk in 09 to determine which forest pixels get burned (converted to grass,
# if any) within each watershed.

# SCENARIOS:
#   - Worst case: No ungulate protection for non-native forest spread, no reduction in fire risk in management areas.
#   - Middle ground: No spread of non-native forest in ungulate protected areas, 50% reduction in fire risk within management areas.
#   - Best case: Spread of native forest within ungulate protected areas, 100% reduction in fire risk within management areas.

library(raster)
library(sf)
library(rgdal)
library(ggplot2)
library(exactextractr)




##### load data #####

# load original landcover raster to compare extent, resolution, CRS, etc.
ras_landcover_original <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
                "mhi_land_cover_names.tif"))

# load year 100 landcover rasters (all scenarios)
ras_landcoverYear100_worstCase <-  # no protection from non-native spread
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07b unprotected/",
                "year 100.tif"))
ras_landcoverYear100_middleCase <-  # no non-native (or native) spread in protected areas
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07d ungulate protected/protected pixels set to original values/",
                "year 100.tif"))
ras_landcoverYear100_bestCase <-  # native restoration in protected areas, non-native spread elsewhere
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/30m rasters/unmasked/",
                "year100.tif"))

# load nearest grass pixel risk data
ras_closestGrassFireProb <-
  raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09d - raster_wood2016_fireRiskClosestGrassPixel.tif")
ras_closestGrassMeters <-
  raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09d - raster_wood2016_closestGrassPixelMeters.tif")

# load watershed dataset
dat_watershed <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                 '09b - sf_watershedData with forest loss.rds'))

# load woody cover dummy raster
ras_wood2016 <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
                "HI_FracLC_wood_2016.tif"))

# load GLM
glm_riskOfForestLoss <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                 '09 glm_riskOfForestLoss.rds'))




##### data calculations #####

# resample distance and risk rasters to match landcover raster
ras_closestGrassMeters <-
  projectRaster(ras_closestGrassMeters, ras_landcover_original)
ras_closestGrassFireProb <-
  projectRaster(ras_closestGrassFireProb, ras_landcover_original)

# resample woody cover raster
ras_wood2016_reprojected <-
  projectRaster(ras_wood2016, ras_landcover_original, method = 'ngb')

# calculate pixel-level risk of forest loss using GLM
dat_risk <-
  data.frame(distanceToGrass_meters =
               values(ras_closestGrassMeters),
             fireRiskClosestGrassPixel =
               values(ras_closestGrassFireProb)
             )
gc()
dat_risk$riskOfForestLoss <-
  predict(glm_riskOfForestLoss, newdata = dat_risk, type = 'response')
gc()

save.image(file = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling",
                         "09c pre-modeling data.Rdata"))
load(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling",
            "09c pre-modeling data.Rdata"))

# create a forest dummy raster using ras_landcover_original
ras_forestDummy <- ras_landcover_original
vec_forestTypes <- c(1600, 1700, 1800, 1900, 2000, 2100,  # non-native forests
                     3700, 3800, 3900, 11800, 11900, 12000, 12100,
                     13700, 13800, 13900, 14100,
                     100, 200, 300, 400, 500, 10100, 10200, 10300, 10400, 10500, 10600,  # native forests
                     600, 700, 800, 1500, 10700, 10800, 10900, 11700)
ras_forestDummy[ras_forestDummy %in% vec_forestTypes] <- 1
ras_forestDummy[ras_forestDummy != 1] <- NA
gc()

# create risk of forest loss raster, masked by landcover raster
ras_riskOfForestLoss <-
  ras_landcover_original
values(ras_riskOfForestLoss) <- dat_risk$riskOfForestLoss
ras_riskOfForestLoss_masked <-
  raster::mask(ras_riskOfForestLoss, ras_forestDummy)




##### for each watershed, determine how many pixels need to be burned #####

# determine number of fires between now and 2070 (midpoint of current and future risk)
dat_watershed$numFiresBy2070 <-
  (dat_watershed$numFiresBy2070_currentRisk +
     dat_watershed$numFiresBy2070_futureRisk) /
  2
dat_watershed$numFiresBy2070_protected50pct <-
  (dat_watershed$numFiresBy2070_currentRisk_protected50pct +
     dat_watershed$numFiresBy2070_futureRisk_protected50pct) /
  2
dat_watershed$numFiresBy2070_protected100pct <-
  (dat_watershed$numFiresBy2070_currentRisk_protected100pct +
     dat_watershed$numFiresBy2070_futureRisk_protected100pct) /
  2

# total number of pixels burned by 2070
dat_watershed$forestPixelsLostBy2070 <-
  with(dat_watershed,
       numFiresBy2070 *
         forestPixelsLostPerFire_mean_noProtection)
dat_watershed$forestPixelsLostBy2070_protected50pct <-
  with(dat_watershed,
       numFiresBy2070_protected50pct *
         dat_watershed$forestPixelsLostPerFire_mean_protection050pct)
dat_watershed$forestPixelsLostBy2070_protected100pct <-
  with(dat_watershed,
       numFiresBy2070_protected100pct *
         dat_watershed$forestPixelsLostPerFire_mean_protection100pct)




##### in 2070 landcover raster, convert required burned pixels to grass #####
# according to loss of forest risk values

# load watershed shapefile
sf_watershed <-
  read_sf(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii watersheds/",
                 "Watersheds.shp"))

# extract pixels by watershed
dat_riskByWatershed <-
  exact_extract(ras_riskOfForestLoss_masked,
                sf_watershed,
                include_cell = TRUE)

# sort pixels by risk within watersheds
dat_riskByWatershed <-
  lapply(dat_riskByWatershed,
         function(df){
           df[order(df$value, decreasing = TRUE),]
         })

# get indices of top n pixels within each watershed, where n is the number needed
# to burn from dat_watershed
list_pixelsToBurn_noProtection <- list()
list_pixelsToBurn_protection50pct <- list()
list_pixelsToBurn_protection100pct <- list()
for(i in 1:nrow(dat_watershed)){
  
  # pixels to burn - no protection
  if(dat_watershed$forestPixelsLostBy2070[[i]] == 0 |
     is.na(dat_watershed$forestPixelsLostBy2070[[i]])){
    list_pixelsToBurn_noProtection[i] <- NA
  } else {
    list_pixelsToBurn_noProtection[[i]] <-
      dat_riskByWatershed[[i]]$cell[1:round(dat_watershed$forestPixelsLostBy2070[[i]])]
  }
  
  # pixels to burn - 50% risk reduction in mgmt areas
  if(dat_watershed$forestPixelsLostBy2070_protected50pct[[i]] == 0 |
     is.na(dat_watershed$forestPixelsLostBy2070_protected50pct[[i]])){
    list_pixelsToBurn_protection50pct[i] <- NA
  } else {
    list_pixelsToBurn_protection50pct[[i]] <-
      dat_riskByWatershed[[i]]$cell[1:round(dat_watershed$forestPixelsLostBy2070_protected50pct[[i]])]
  }
  
  # pixels to burn - 100% risk reduction in mgmt areas
  if(dat_watershed$forestPixelsLostBy2070_protected100pct[[i]] == 0 |
     is.na(dat_watershed$forestPixelsLostBy2070_protected100pct[[i]])){
    list_pixelsToBurn_protection100pct[i] <- NA
  } else {
    list_pixelsToBurn_protection100pct[[i]] <-
      dat_riskByWatershed[[i]]$cell[1:round(dat_watershed$forestPixelsLostBy2070_protected100pct[[i]])]
  }
}

# consolidate pixels to burn by scenario and remove NAs
vec_pixelsToBurn_noProtection <- do.call(c, list_pixelsToBurn_noProtection)
vec_pixelsToBurn_noProtection <-
  vec_pixelsToBurn_noProtection[!is.na(vec_pixelsToBurn_noProtection) &
                                  !duplicated(vec_pixelsToBurn_noProtection)]
vec_pixelsToBurn_protection50pct <- do.call(c, list_pixelsToBurn_protection50pct)
vec_pixelsToBurn_protection50pct <-
  vec_pixelsToBurn_protection50pct[!is.na(vec_pixelsToBurn_protection50pct) &
                                  !duplicated(vec_pixelsToBurn_protection50pct)]
vec_pixelsToBurn_protection100pct <- do.call(c, list_pixelsToBurn_protection100pct)
vec_pixelsToBurn_protection100pct <-
  vec_pixelsToBurn_protection100pct[!is.na(vec_pixelsToBurn_protection100pct) &
                                  !duplicated(vec_pixelsToBurn_protection100pct)]
rm(list_pixelsToBurn_noProtection,
   list_pixelsToBurn_protection50pct,
   list_pixelsToBurn_protection100pct)

# load 2070 rasters
ras_2070worstCase <-  # no non-native spread protection, no fire reduction
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07b unprotected/",
                "year 070.tif"))
ras_2070midCase <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07d ungulate protected/protected pixels set to original values/",
                "year 070.tif"))
ras_2070bestCase <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/30m rasters/unmasked/",
                "year070.tif"))

# convert burned pixels to new dummy value
ras_2070bestCase[vec_pixelsToBurn_protection100pct] <- -2
ras_2070midCase[vec_pixelsToBurn_protection50pct] <- -2
ras_2070worstCase[vec_pixelsToBurn_noProtection] <- -2

# save rasters
writeRaster(ras_2070bestCase,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
                              'year 70 - best case.tif'), overwrite = TRUE)
writeRaster(ras_2070midCase,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
                              'year 70 - mid case.tif'), overwrite = TRUE)
writeRaster(ras_2070worstCase,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
                              'year 70 - worst case.tif'), overwrite = TRUE)
