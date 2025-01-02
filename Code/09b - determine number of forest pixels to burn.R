
# This script uses the rates of fire-driven forest loss determined in 09a
# to determine how many forest pixels to burn on each island.




library(raster)
library(sf)
#library(rgdal)
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

# load woody cover dummy raster
ras_wood2016 <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
                "HI_FracLC_wood_2016.tif"))

# load GLM
glm_riskOfForestLoss <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                 '09 glm_riskOfForestLoss.rds'))

# load fire-driven forest lost rates
load(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
            '09a - rates of fire-driven forest loss.Rdata'))

# load island coastlines
sf_coastlines <-
  read_sf(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii coastlines/",
                 "coast_n83.shp"))





##### create risk of forest loss raster #####

# # resample distance and risk rasters to match landcover raster
# ras_closestGrassMeters <-
#   projectRaster(ras_closestGrassMeters, ras_landcover_original)
# ras_closestGrassFireProb <-
#   projectRaster(ras_closestGrassFireProb, ras_landcover_original)
# 
# # resample woody cover raster
# ras_wood2016_reprojected <-
#   projectRaster(ras_wood2016, ras_landcover_original, method = 'ngb')
# 
# # calculate pixel-level risk of forest loss using GLM
# dat_risk <-
#   data.frame(distanceToGrass_meters =
#                values(ras_closestGrassMeters),
#              fireRiskClosestGrassPixel =
#                values(ras_closestGrassFireProb)
#   )
# gc()
# dat_risk$riskOfForestLoss <-
#   predict(glm_riskOfForestLoss, newdata = dat_risk, type = 'response')
# gc()
# 
# save.image(file = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling",
#                          "09c pre-modeling data.Rdata"))
load(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling",
            "09c pre-modeling data.Rdata"))

# create a forest dummy raster using ras_landcover_original
ras_forestDummy <- ras_landcover_original
vec_forestTypes <- c(1600, 1700, 1800, 1900, 2000, 2100,  # non-native forests
                     11800, 11900, 12000, 12100,
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

# clean up
rm(dat_forestLossSimResults_pixelsLostPerFire_noProtection,
   dat_forestLossSimResults_pixelsLostPerFire_protection050pct,
   dat_forestLossSimResults_pixelsLostPerFire_protection100pct,
   dat_risk, dat_watershed, dat_watershed2, gam_forestLoss, glm_riskOfForestLoss,
   list_forestLossSimResults_pixelsLostPerFire_noProtection,
   list_forestLossSimResults_pixelsLostPerFire_protection050pct,
   list_forestLossSimResults_pixelsLostPerFire_protection100pct,
   list_simulationDat_noProtection, list_simulationDat_protection050pct,
   list_simulationDat_protection100pct, ras_closestGrassFireProb,
   ras_closestGrassMeters, ras_fireRisk2070Dry_sta, ras_fireRiskCurrentDry,
   ras_riskOfForestLoss, m, vec_knots)
gc()




##### calculate how many pixels to burn by island #####

# rasterize island shapefile
sf_coastlines$island <- 1:nrow(sf_coastlines)
# ras_islands <- rasterize(sf_coastlines, ras_riskOfForestLoss_masked,
#                          field = 'island')
# writeRaster(ras_islands,
#             filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/",
#                               "09b ras_islands.tif"),
#             overwrite = TRUE)
ras_islands <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/",
                "09b ras_islands.tif"))

# define islands of ras_islands
vec_islandCodes_rasIslands <-
  c(1, 4, 8, 9, 10, 13)
names(vec_islandCodes_rasIslands) <-
  c('Kauai', 'Oahu', 'Molokai', 'Maui', 'Lanai', 'Big Island')

# number of pixels to burn, 2016 - 2070 and 2016 - 2100
vec_numPixelsToBurn2070                 <- vec_islandAnnualWoodPixelsLost * (2070 - 2016) * 1.00
vec_numPixelsToBurn2100                 <- vec_islandAnnualWoodPixelsLost * (2100 - 2016) * 1.00
vec_numPixelsToBurn2070_50pctReduction  <- vec_islandAnnualWoodPixelsLost * (2070 - 2016) * 0.50
vec_numPixelsToBurn2100_50pctReduction  <- vec_islandAnnualWoodPixelsLost * (2100 - 2016) * 0.50
vec_numPixelsToBurn2070_100pctReduction <- vec_islandAnnualWoodPixelsLost * (2070 - 2016) * 0.00
vec_numPixelsToBurn2100_100pctReduction <- vec_islandAnnualWoodPixelsLost * (2100 - 2016) * 0.00




##### burn required number of pixels #####

# convert required rasters into data.frame
dat_burn <- as.data.frame(ras_islands, xy = TRUE); gc()
colnames(dat_burn) <- c('x', 'y', 'islandCode')
dat_burn$riskOfForestLoss <- values(ras_riskOfForestLoss_masked); gc()

# create separate data.frame without NAs (less memory-intensive)
dat_burn2 <- dat_burn[!is.na(dat_burn$islandCode) &
                        !is.na(dat_burn$riskOfForestLoss),]; gc()

# split data by island
dat_burn2 <- split(dat_burn2, dat_burn2$islandCode); gc()

# keep only relevant islands
dat_burn2 <- dat_burn2[names(dat_burn2) %in% vec_islandCodes_rasIslands]; gc()

# sort each island's pixels according to risk of forest loss
dat_burn2 <-
  lapply(dat_burn2,
         function(df){
           df[order(-df$riskOfForestLoss),]
         }); gc()

# burn the top pixels for each time horizon - worst case (no reduction in risk)
dat_burn2 <-
  lapply(dat_burn2,
         function(df){
           df$pixelBurned2070 <- NA
           df$pixelBurned2100 <- NA
           df
         }); gc()
for(i in 1:length(dat_burn2)){
  dat_burn2[[i]]$pixelBurned2070[1:vec_numPixelsToBurn2070[[i]]] <- -2
  dat_burn2[[i]]$pixelBurned2100[1:vec_numPixelsToBurn2100[[i]]] <- -2
}; gc()

# recombine into one data.frame - worst case (no reduction in risk)
dat_burn2 <- do.call(rbind, dat_burn2); gc()
dat_burn2 <- dat_burn2[c('x', 'y', 'pixelBurned2070', 'pixelBurned2100')]; gc()
dat_burn <- dplyr::left_join(dat_burn, dat_burn2, c('x', 'y')); gc()
rm(dat_burn2); gc()

# burn the top pixels for each time horizon - middle case (50% reduction in risk)
dat_burn2 <- dat_burn[!is.na(dat_burn$islandCode) &
                        !is.na(dat_burn$riskOfForestLoss),]; gc()
dat_burn2 <- split(dat_burn2, dat_burn2$islandCode); gc()
dat_burn2 <- dat_burn2[names(dat_burn2) %in% vec_islandCodes_rasIslands]; gc()
dat_burn2 <-
  lapply(dat_burn2,
         function(df){
           df[order(-df$riskOfForestLoss),]
         }); gc()
dat_burn2 <-
  lapply(dat_burn2,
         function(df){
           df$pixelBurned2070_50pctReduction <- NA
           df$pixelBurned2100_50pctReduction <- NA
           df
         }); gc()
for(i in 1:length(dat_burn2)){
  dat_burn2[[i]]$pixelBurned2070_50pctReduction[1:vec_numPixelsToBurn2070_50pctReduction[[i]]] <- -2
  dat_burn2[[i]]$pixelBurned2100_50pctReduction[1:vec_numPixelsToBurn2100_50pctReduction[[i]]] <- -2
}; gc()

# recombine into one data.frame - middle case (50% reduction in risk)
dat_burn2 <- do.call(rbind, dat_burn2); gc()
dat_burn2 <- dat_burn2[c('x', 'y', 'pixelBurned2070_50pctReduction', 'pixelBurned2100_50pctReduction')]; gc()
dat_burn <- dplyr::left_join(dat_burn, dat_burn2, c('x', 'y')); gc()
rm(dat_burn2); gc()

# turn into rasters
ras_forestBurned2070 <-
  rasterFromXYZ(dat_burn[c('x', 'y', 'pixelBurned2070')],
                crs = crs(ras_wood2016_reprojected)); gc()
ras_forestBurned2100 <-
  rasterFromXYZ(dat_burn[c('x', 'y', 'pixelBurned2100')],
                crs = crs(ras_wood2016_reprojected)); gc()
ras_forestBurned2070_50pctReduction <-
  rasterFromXYZ(dat_burn[c('x', 'y', 'pixelBurned2070_50pctReduction')],
                crs = crs(ras_wood2016_reprojected)); gc()
ras_forestBurned2100_50pctReduction <-
  rasterFromXYZ(dat_burn[c('x', 'y', 'pixelBurned2100_50pctReduction')],
                crs = crs(ras_wood2016_reprojected)); gc()
rm(dat_burn); gc()

# save rasters
writeRaster(ras_forestBurned2070,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                              '09b - ras_forestBurned2070.tif'),
            overwrite = TRUE)
writeRaster(ras_forestBurned2100,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                              '09b - ras_forestBurned2100.tif'),
            overwrite = TRUE)
writeRaster(ras_forestBurned2070_50pctReduction,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                              '09b - ras_forestBurned2070_50pctReduction.tif'),
            overwrite = TRUE)
writeRaster(ras_forestBurned2100_50pctReduction,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                              '09b - ras_forestBurned2100_50pctReduction.tif'),
            overwrite = TRUE)
