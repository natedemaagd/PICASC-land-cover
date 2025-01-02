
# This script uses the forest loss (forest fire) model to predict the extent of
# forest loss (grassland conversion).

library(sf)
library(raster)
library(exactextractr)
library(mgcv)
library(rms)
library(tidyverse)
library(ggplot2)
library(rgdal)





##### load data #####

# load landcover raster
ras_landcover <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
                "mhi_land_cover_names.tif"))

# load dummy raster
ras_dummy <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                'raster_dummy_master.tif'))

# load fire size model - created in
  # H:/My Drive/Projects/PICASC Land-to-sea/Hawaii-fire-risk/Code/16c - model validation - OOS predicitions - non-temporal model.R
gam_forestLoss <-
  readRDS(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/",
                 "16c - model validation - final GAM k2.rds")
          )

# load watershed shapefile
sf_watershed <-
  read_sf("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii watersheds/Watersheds.shp")

# load 2016 grass raster
ras_grassPixels2016 <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
                "HI_FracLC_herb_2016.tif")
  )

# load 2016 wood raster
ras_woodPixels2016 <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
                "HI_FracLC_wood_2016.tif")
  )

# load mean rainfall raster
ras_rainfall <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/",
                "HI_EVAP_mean_annual_rainfall__statewide_250m.tif")
  )

# load current dry season fire risk rasters and merge
ras_fireRiskCurrentDry <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/prediction rasters mean by season',
             pattern = 'Dry')
ras_fireRiskCurrentDry <-
  lapply(ras_fireRiskCurrentDry, function(r){
    raster(
      paste0(
        'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/prediction rasters mean by season/',
        r
      )
      )
  }
  )
ras_fireRiskCurrentDry <- do.call(merge, ras_fireRiskCurrentDry)

# load 2070 statistical dry season fire risk rasters and merge
ras_fireRisk2070Dry <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/prediction rasters future mean by season/',
             pattern = '2070')
ras_fireRisk2070Dry <-
  ras_fireRisk2070Dry[str_detect(ras_fireRisk2070Dry,
                                 pattern = "Dry")]
ras_fireRisk2070Dry_sta <-
  ras_fireRisk2070Dry[str_detect(ras_fireRisk2070Dry,
                                 pattern = "sta")]  # keep only statistical downscale
rm(ras_fireRisk2070Dry)
ras_fireRisk2070Dry_sta <-
  lapply(ras_fireRisk2070Dry_sta,
         function(r){
           raster(paste0(
             'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_future/prediction rasters future mean by season/',
             r
           ))
         })
ras_fireRisk2070Dry_sta <- do.call(merge, ras_fireRisk2070Dry_sta)
gc()




##### count pixels by watershed #####

# mean rainfall values by watershed
list_rainfallByWatershed <- exact_extract(ras_rainfall, sf_watershed)
sf_watershed$rainfall_mmMeanAnn <-
  sapply(list_rainfallByWatershed,
         function(df){
           mean(df$value, na.rm = TRUE)
         })
rm(list_rainfallByWatershed, ras_rainfall)

# number of grass pixels by watershed
ras_grassPixels2016_dummy <-
  ras_grassPixels2016
ras_grassPixels2016_dummy <-
  resample(ras_grassPixels2016_dummy,
           ras_dummy, method = 'ngb')
rm(ras_grassPixels2016); gc()
ras_grassPixels2016_dummy[ras_grassPixels2016_dummy > 0.40]  <- 1
ras_grassPixels2016_dummy[ras_grassPixels2016_dummy <= 0.40] <- 0
gc()
list_grassPixelsByWatershed <-
  exact_extract(ras_grassPixels2016_dummy,
                sf_watershed)
sf_watershed$numGrassPixels2016 <-
  sapply(list_grassPixelsByWatershed,
         function(df){
           sum(df$value, na.rm = TRUE)
         })
rm(list_grassPixelsByWatershed)
gc()

# number of forest pixels by watershed
ras_woodPixels2016_dummy <-
  ras_woodPixels2016
ras_woodPixels2016_dummy <-
  resample(ras_woodPixels2016_dummy,
           ras_dummy, method = 'ngb')
rm(ras_woodPixels2016); gc()
ras_woodPixels2016_dummy[ras_woodPixels2016_dummy > 0.40]  <- 1
ras_woodPixels2016_dummy[ras_woodPixels2016_dummy <= 0.40] <- 0
gc()
list_woodPixelsByWatershed <-
  exact_extract(ras_woodPixels2016_dummy,
                sf_watershed)
sf_watershed$numForestPixels2016 <-
  sapply(list_woodPixelsByWatershed,
         function(df){
           sum(df$value, na.rm = TRUE)
         })
rm(list_woodPixelsByWatershed, ras_woodPixels2016_dummy)




##### create risk-adjusted rasters using the protected area shapefile #####
  # middle ground: risk reduced by 50% if watershed has any protected area in it
  # best case: risk reduced by 100% if watershed has any protected area in it

# load protection area shapefile
sf_protectedAreas <-
  readOGR(dsn = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Fire/management area shapefiles/",
          layer = "Hawaii_Veg_Management_D_poly")
sf_protectedAreas <- spTransform(sf_protectedAreas,
                                 CRSobj = crs(ras_dummy))
sl_protectedAreas <-
  readOGR(dsn = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Fire/management area shapefiles/",
          layer = "Hawaii_Veg_Management_D_ln")
sl_protectedAreas <- spTransform(sl_protectedAreas,
                                 CRSobj = crs(ras_dummy))

# rasterize protected areas
ras_protectedAreasPoly <-
  rasterize(sf_protectedAreas, ras_dummy)
ras_protectedAreasLine <-
  rasterize(sl_protectedAreas, ras_dummy)

# combine lines and poly rasterizations into one raster
ras_protectedPixels <-
  ras_protectedAreasPoly
ras_protectedPixels[is.na(ras_protectedPixels) &
                      !is.na(ras_protectedAreasLine)] <-
  ras_protectedAreasLine[is.na(ras_protectedPixels) &
                           !is.na(ras_protectedAreasLine)]

# mask dummy raster with protected areas to create new risk rasters
ras_protectedRisk50pct <- ras_dummy  # initiate new raster w/ same extent, resolution, etc.
ras_protectedRisk50pct[!is.na(ras_protectedRisk50pct)] <- 1
ras_protectedRisk50pct <-
  raster::mask(ras_protectedRisk50pct, sf_protectedAreas,
               updatevalue = 0.50, inverse = TRUE)
ras_protectedRisk0pct <- ras_protectedRisk50pct
ras_protectedRisk0pct[ras_protectedRisk0pct == 0.50 &
                        !is.na(ras_protectedRisk0pct)] <- 0.00
gc()

# find which watersheds have protected pixels
vec_watershedsProtectedPixels <-
  exact_extract(ras_protectedPixels, sf_watershed, fun = 'sum')
vec_watershedsProtectedPixels[!is.na(vec_watershedsProtectedPixels) &
                                vec_watershedsProtectedPixels > 0] <- 1  # convert to dummy vector (1 = watershed has protected pixels)




##### add covariates to sf_watershed data #####

# current dry season fire risk of grass-dominated pixels
list_fireRisk_Current <-
  exact_extract(ras_fireRiskCurrentDry,
                sf_watershed)
sf_watershed$fireRisk_75thPctileCurrentDrySeason <-
  sapply(list_fireRisk_Current,
         function(df){
           quantile(df$value, probs = 0.75, na.rm = TRUE)[[1]]
         })
sf_watershed$fireRisk_90thPctileCurrentDrySeason <-
  sapply(list_fireRisk_Current,
         function(df){
           quantile(df$value, probs = 0.90, na.rm = TRUE)[[1]]
         })
rm(list_fireRisk_Current)

# 2070 dry season fire risk of grass-dominated pixels
list_fireRisk_2070 <-
  exact_extract(ras_fireRisk2070Dry_sta,
                sf_watershed)
sf_watershed$fireRisk_75thPctile2070DrySeason <-
  sapply(list_fireRisk_2070,
         function(df){
           quantile(df$value, probs = 0.75, na.rm = TRUE)[[1]]
         })
sf_watershed$fireRisk_90thPctile2070DrySeason <-
  sapply(list_fireRisk_2070,
         function(df){
           quantile(df$value, probs = 0.90, na.rm = TRUE)[[1]]
         })
rm(list_fireRisk_2070)




##### adjust risk of watersheds with protected pixels #####

sf_watershed$fireRisk_75thPctileCurrentDrySeason_protected50pct <-
  sf_watershed$fireRisk_75thPctileCurrentDrySeason
sf_watershed$fireRisk_75thPctileCurrentDrySeason_protected50pct[vec_watershedsProtectedPixels == 1] <-
  sf_watershed$fireRisk_75thPctileCurrentDrySeason_protected50pct[vec_watershedsProtectedPixels == 1] * 0.50

sf_watershed$fireRisk_90thPctileCurrentDrySeason_protected50pct <-
  sf_watershed$fireRisk_90thPctileCurrentDrySeason
sf_watershed$fireRisk_90thPctileCurrentDrySeason_protected50pct[vec_watershedsProtectedPixels == 1] <-
  sf_watershed$fireRisk_90thPctileCurrentDrySeason_protected50pct[vec_watershedsProtectedPixels == 1] * 0.50

sf_watershed$fireRisk_75thPctile2070DrySeason_protected50pct <-
  sf_watershed$fireRisk_75thPctile2070DrySeason
sf_watershed$fireRisk_75thPctile2070DrySeason_protected50pct[vec_watershedsProtectedPixels == 1] <-
  sf_watershed$fireRisk_75thPctile2070DrySeason_protected50pct[vec_watershedsProtectedPixels == 1] * 0.50

sf_watershed$fireRisk_90thPctile2070DrySeason_protected50pct <-
  sf_watershed$fireRisk_90thPctile2070DrySeason
sf_watershed$fireRisk_90thPctile2070DrySeason_protected50pct[vec_watershedsProtectedPixels == 1] <-
  sf_watershed$fireRisk_90thPctile2070DrySeason_protected50pct[vec_watershedsProtectedPixels == 1] * 0.50

sf_watershed$fireRisk_75thPctileCurrentDrySeason_protected100pct <-
  sf_watershed$fireRisk_75thPctileCurrentDrySeason
sf_watershed$fireRisk_75thPctileCurrentDrySeason_protected100pct[vec_watershedsProtectedPixels == 1] <-
  sf_watershed$fireRisk_75thPctileCurrentDrySeason_protected100pct[vec_watershedsProtectedPixels == 1] * 0.00

sf_watershed$fireRisk_90thPctileCurrentDrySeason_protected100pct <-
  sf_watershed$fireRisk_90thPctileCurrentDrySeason
sf_watershed$fireRisk_90thPctileCurrentDrySeason_protected100pct[vec_watershedsProtectedPixels == 1] <-
  sf_watershed$fireRisk_90thPctileCurrentDrySeason_protected100pct[vec_watershedsProtectedPixels == 1] * 0.00

sf_watershed$fireRisk_75thPctile2070DrySeason_protected100pct <-
  sf_watershed$fireRisk_75thPctile2070DrySeason
sf_watershed$fireRisk_75thPctile2070DrySeason_protected100pct[vec_watershedsProtectedPixels == 1] <-
  sf_watershed$fireRisk_75thPctile2070DrySeason_protected100pct[vec_watershedsProtectedPixels == 1] * 0.00

sf_watershed$fireRisk_90thPctile2070DrySeason_protected100pct <-
  sf_watershed$fireRisk_90thPctile2070DrySeason
sf_watershed$fireRisk_90thPctile2070DrySeason_protected100pct[vec_watershedsProtectedPixels == 1] <-
  sf_watershed$fireRisk_90thPctile2070DrySeason_protected100pct[vec_watershedsProtectedPixels == 1] * 0.00




saveRDS(sf_watershed,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                      '09a - watershed dataset.rds'))
