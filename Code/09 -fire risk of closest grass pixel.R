

# This script converts forest pixels into grass pixels using the fire dataset
# generated in 09c.

library(sf)
library(raster)
library(exactextractr)
library(geosphere)
library(mgcv)
library(rms)
library(tidyverse)
library(ggplot2)
library(doParallel)
registerDoParallel(cores = 10)




##### load data #####

load(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
            '09c - initial raster formatting.Rdata'))
rm(ras_grassPixels2016_dummy, ras_woodPixels2016_dummy, ras_dummy,
   ras_fireRisk2070Dry_sta, ras_fireRiskCurrentDry)

# load rasters
raster_wood2016 <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                                 '09c - ras_woodPixels2016_dummy.tif'))
raster_grass2016 <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                                  '09c - ras_grassPixels2016_dummy.tif'))
raster_fireRiskCurrentDry <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                                           '09c - ras_fireRiskCurrentDry.tif'))



##### for each forest pixel, find closest grass pixel and its fire risk value #####

# scale grass and risk rasters to 250m
raster_grass2016_250m <-
  raster::aggregate(raster_grass2016,
                    fact = round(250/30), fun = max, na.rm = TRUE)
raster_fireRiskCurrentDry_250m <-
  raster::aggregate(raster_fireRiskCurrentDry,
                    fact = round(250/30), fun = max, na.rm = TRUE)
raster_wood2016_250m <-
  raster::aggregate(raster_wood2016,
                    fact = round(250/30), fun = max, na.rm = TRUE)

# get coordinates and fire risk values of grass pixels
dat_grass <-
  as.data.frame(raster_grass2016_250m, xy = TRUE)
dat_fireRisk <-
  as.data.frame(raster_fireRiskCurrentDry_250m, xy = TRUE)
gc()
dat_grass$fireRiskCurrent <- dat_fireRisk$layer
colnames(dat_grass)[1:3] <-
  c('x', 'y', 'isGrass')
dat_grass <- dat_grass[dat_grass$isGrass == 1 & !is.na(dat_grass$isGrass),]
dat_grass <- dat_grass[c('x', 'y', 'fireRiskCurrent')]
dat_grass <- dat_grass[!is.na(dat_grass$fireRiskCurrent),]
gc()
# saveRDS(dat_grass,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
#                       '09d - dat_grass.rds'))
saveRDS(dat_grass,
        file = paste0('D:/OneDrive - hawaii.edu/Documents/Projects/PICASC TEMP/Data/Intermediate/Water yield/09 forest loss modeling/',
                      '09d - dat_grass.rds'))
dat_grass <- readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/PICASC TEMP/Data/Intermediate/Water yield/09 forest loss modeling/09d - dat_grass.rds")
  # readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
  #                '09d - dat_grass.rds'))

# isolate forest pixels
dat_wood <-
  as.data.frame(raster_wood2016, xy = TRUE)
dat_wood <-
  dat_wood[!is.na(dat_wood$layer) & dat_wood$layer == 1,]
dat_wood <- dat_wood[c('x', 'y')]
gc()

# # for each forest pixel, find closest grass pixel's fire probability
# a <- Sys.time()
# dat_wood$closestGrassFireProb <- NA
# closestGrassFireProb <-
#   foreach(i = 1:nrow(dat_wood),
#           .packages = 'geosphere',
#           .errorhandling = 'pass',
#           .combine = 'rbind') %dopar% {
#   distVec <- distGeo(dat_wood[i, c('x', 'y')],
#                      dat_grass[c('x', 'y')])
#   gc()
#   return(c(min(distVec),
#            dat_grass$fireRiskCurrent[[which.min(distVec)]]
#            )
#          )
# 
#           }
# 
# # closestGrassFireProb <- c(closestGrassFireProb1[,2], NA, closestGrassFireProb2[,2], closestGrassFireProb3[,2], closestGrassFireProb4[,2])  # RAN CODE IN CHUNKS SO HAD TO COLLATE - IGNORE THESE TWO LINES
# # closestGrassFireProb_distanceMeters <- c(closestGrassFireProb1[,1], NA, closestGrassFireProb2[,1], closestGrassFireProb3[,1], closestGrassFireProb4[,1])
# 
# saveRDS(closestGrassFireProb,
#         file = "D:/OneDrive - hawaii.edu/Documents/Projects/PICASC TEMP/Data/Intermediate/Water yield/09 forest loss modeling/09d - closestGrassFireProb.rds")
# saveRDS(closestGrassFireProb_distanceMeters,
#         file = "D:/OneDrive - hawaii.edu/Documents/Projects/PICASC TEMP/Data/Intermediate/Water yield/09 forest loss modeling/09d - closestGrassFireProb_distanceMeters.rds")
# Sys.time()-a

# add distance to grass pixels and updated risk to dat_wood
dat_wood$fireRisk_closestGrassPixel <- closestGrassFireProb
dat_wood$distanceMeters_closestGrassPixel <- closestGrassFireProb_distanceMeters
rm(closestGrassFireProb, closestGrassFireProb_distanceMeters)

# create new fire risk raster using new probabilities, and a distance raster
# giving distance to closest grass pixel for each forest pixel
raster_wood2016_fireRiskClosestGrassPixel <-
  raster_wood2016
raster_wood2016_fireRiskClosestGrassPixel[raster_wood2016_fireRiskClosestGrassPixel == 1 & !is.na(raster_wood2016_fireRiskClosestGrassPixel)] <-
  dat_wood$fireRisk_closestGrassPixel
raster_wood2016_closestGrassPixelMeters <-
  raster_wood2016
raster_wood2016_closestGrassPixelMeters[raster_wood2016_closestGrassPixelMeters == 1 & !is.na(raster_wood2016_closestGrassPixelMeters)] <-
  dat_wood$distanceMeters_closestGrassPixel

# save rasters
raster::writeRaster(raster_wood2016_fireRiskClosestGrassPixel,
                    filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/",
                                      "09d - raster_wood2016_fireRiskClosestGrassPixel.tif")
                    )
raster::writeRaster(raster_wood2016_closestGrassPixelMeters,
                    filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/",
                                      "09d - raster_wood2016_closestGrassPixelMeters.tif")
)




##### run model of burned vs unburned pixels #####

# load rasters from last section (faster loading)
raster_wood2016_fireRiskClosestGrassPixel <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/",
                "09d - raster_wood2016_fireRiskClosestGrassPixel.tif"))
raster_wood2016_closestGrassPixelMeters <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/",
                "09d - raster_wood2016_closestGrassPixelMeters.tif"))

# load fire perimeters
sf_firePerimeters <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/2019_1999_Hawaii_Fire_Perimeters.shp")

# create rasters of burned and unburned areas for each covariate
raster_wood2016_closestGrassPixelMeters_burned <-
  raster::mask(raster_wood2016_closestGrassPixelMeters, sf_firePerimeters)
raster_wood2016_fireRiskClosestGrassPixel_burned <-
  raster::mask(raster_wood2016_fireRiskClosestGrassPixel, sf_firePerimeters)
raster_wood2016_closestGrassPixelMeters_unburned <-
  raster::mask(raster_wood2016_closestGrassPixelMeters, sf_firePerimeters,
               inverse = TRUE)
raster_wood2016_fireRiskClosestGrassPixel_unburned <-
  raster::mask(raster_wood2016_fireRiskClosestGrassPixel, sf_firePerimeters,
               inverse = TRUE)

# convert burned area into data.frame and keep only forest pixels (risk > 0)
dat_burnedForest <-
  data.frame(distanceToGrass_meters =
               values(raster_wood2016_closestGrassPixelMeters_burned),
             fireRiskClosestGrassPixel =
               values(raster_wood2016_fireRiskClosestGrassPixel_burned)
             )
dat_burnedForest <-
  dat_burnedForest[!is.na(dat_burnedForest$fireRiskClosestGrassPixel) &
                     dat_burnedForest$fireRiskClosestGrassPixel > 0,]
gc()

# convert unburned forest into data.frame and randomly select pixels to match
# number of pixels from `dat_burnedForest` for modeling
dat_unburnedForest <-
  data.frame(distanceToGrass_meters =
               values(raster_wood2016_closestGrassPixelMeters_unburned),
             fireRiskClosestGrassPixel =
               values(raster_wood2016_fireRiskClosestGrassPixel_unburned)
  )
dat_unburnedForest <-
  dat_unburnedForest[!is.na(dat_unburnedForest$fireRiskClosestGrassPixel) &
                     dat_unburnedForest$fireRiskClosestGrassPixel > 0,]
gc()
dat_unburnedForest <-
  dat_unburnedForest[sample(1:nrow(dat_unburnedForest),
                            size = nrow(dat_burnedForest)
                            ),]

# create burned dummy then combine data to run model
dat_burnedForest$burned = 1
dat_unburnedForest$burned = 0
dat_forest <- rbind(dat_burnedForest, dat_unburnedForest)
rm(dat_burnedForest, dat_unburnedForest); gc()

# run model
glm_riskOfForestLoss <-
  glm(burned ~
        distanceToGrass_meters + fireRiskClosestGrassPixel +
        distanceToGrass_meters*fireRiskClosestGrassPixel,
      data = dat_forest,
      family = 'binomial')

# save model
saveRDS(glm_riskOfForestLoss,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                      '09 glm_riskOfForestLoss.rds'))
