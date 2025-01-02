
# This script uses the dataset generated in 09a to predict future forest loss
# at the watershed level.

library(sf)
library(raster)
library(exactextractr)
library(mgcv)
library(rms)
library(tidyverse)
library(ggplot2)
library(rgdal)




##### load data #####

# load fire size model - created in
# H:/My Drive/Projects/PICASC Land-to-sea/Hawaii-fire-risk/Code/16c - model validation - OOS predicitions - non-temporal model.R
gam_forestLoss <-
  readRDS(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/",
                 "16c - model validation - final RCS 3 knots.rds")
  )

# load watershed-level dataset
dat_watershed <-
  readRDS(file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                        '09a - watershed dataset.rds')
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

# load dummy raster
ras_dummy <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/16 predicted forest loss/',
                'raster_dummy_master.tif'))

# resample fire rasters to match master dummy
ras_fireRisk2070Dry_sta <-
  resample(ras_fireRisk2070Dry_sta, ras_dummy)
ras_fireRiskCurrentDry <-
  resample(ras_fireRiskCurrentDry, ras_dummy)




##### calculate fires #####

# estimate number of fires between now and 2070 in each watershed
dat_watershed$numFiresBy2070_currentRisk <-
  (dat_watershed$fireRisk_90thPctileCurrentDrySeason) * (2070 - 2016)
dat_watershed$numFiresBy2070_currentRisk_protected50pct <-
  (dat_watershed$fireRisk_90thPctileCurrentDrySeason_protected50pct) * (2070 - 2016)
dat_watershed$numFiresBy2070_currentRisk_protected100pct <-
  (dat_watershed$fireRisk_90thPctileCurrentDrySeason_protected100pct) * (2070 - 2016)
dat_watershed$numFiresBy2070_futureRisk <-
  (dat_watershed$fireRisk_90thPctile2070DrySeason) * (2070 - 2016)
dat_watershed$numFiresBy2070_futureRisk_protected50pct <-
  (dat_watershed$fireRisk_90thPctile2070DrySeason_protected50pct) * (2070 - 2016)
dat_watershed$numFiresBy2070_futureRisk_protected100pct <-
  (dat_watershed$fireRisk_90thPctile2070DrySeason_protected100pct) * (2070 - 2016)

# create simulation data: 500 repeated datasets
dat_watershed2 <-
  as.data.frame(dat_watershed[c("rainfall_mmMeanAnn", "numGrassPixels2016", "numForestPixels2016",
                                
                                "fireRisk_90thPctileCurrentDrySeason", "fireRisk_90thPctileCurrentDrySeason_protected50pct", "fireRisk_90thPctileCurrentDrySeason_protected100pct",
                                "fireRisk_90thPctile2070DrySeason",    "fireRisk_90thPctile2070DrySeason_protected50pct",    "fireRisk_90thPctile2070DrySeason_protected100pct",
                                
                                "numFiresBy2070_currentRisk", "numFiresBy2070_currentRisk_protected50pct", "numFiresBy2070_currentRisk_protected100pct",
                                "numFiresBy2070_futureRisk",  "numFiresBy2070_futureRisk_protected50pct",  "numFiresBy2070_futureRisk_protected100pct")
                              ]
                )
dat_watershed2$geometry <- NULL
list_simulationDat <- rep(list(dat_watershed2), 500)
gc()

# draw a random fire risk between current and future values as final fire risk value
list_simulationDat <-
  lapply(list_simulationDat,
         function(df){
           df %>% mutate(fireRisk =
                           mapply(function(x, y) runif(1, min(x,y), max(x,y)),
                                  fireRisk_90thPctileCurrentDrySeason,
                                  fireRisk_90thPctile2070DrySeason),
                         fireRisk_protectedArea50pctReduction =
                           mapply(function(x, y) runif(1, min(x,y), max(x,y)),
                                  fireRisk_90thPctileCurrentDrySeason_protected50pct,
                                  fireRisk_90thPctile2070DrySeason_protected50pct),
                         fireRisk_protectedArea100pctReduction =
                           mapply(function(x, y) runif(1, min(x,y), max(x,y)),
                                  fireRisk_90thPctileCurrentDrySeason_protected100pct,
                                  fireRisk_90thPctile2070DrySeason_protected100pct)
                         )
         })
gc()

# create separate datasets for each scenario (no protection, 50%, and 100%), and
# rename columns to match model covariates
list_simulationDat_noProtection <-
  lapply(list_simulationDat,
         function(df){
           df_reduced <- df[c("rainfall_mmMeanAnn", "numGrassPixels2016", "numForestPixels2016",
                              "numFiresBy2070_currentRisk", "numFiresBy2070_futureRisk",
                              "fireRisk")]
           colnames(df_reduced) <-
             c("rainfall_mmMeanAnn", "numGrassPixels1999", "numForestPixels1999",
               "numFiresBy2070_currentRisk", "numFiresBy2070_futureRisk",
               "fireRisk_90thPctileMaxHistorical")
           df_reduced
  })
list_simulationDat_protection050pct <-
  lapply(list_simulationDat,
         function(df){
           df_reduced <- df[c("rainfall_mmMeanAnn", "numGrassPixels2016", "numForestPixels2016",
                              "numFiresBy2070_currentRisk", "numFiresBy2070_futureRisk",
                              "fireRisk_protectedArea50pctReduction")]
           colnames(df_reduced) <-
             c("rainfall_mmMeanAnn", "numGrassPixels1999", "numForestPixels1999",
               "numFiresBy2070_currentRisk", "numFiresBy2070_futureRisk",
               "fireRisk_90thPctileMaxHistorical")
           df_reduced
         })
list_simulationDat_protection100pct <-
  lapply(list_simulationDat,
         function(df){
           df_reduced <- df[c("rainfall_mmMeanAnn", "numGrassPixels2016", "numForestPixels2016",
                              "numFiresBy2070_currentRisk", "numFiresBy2070_futureRisk",
                              "fireRisk_protectedArea50pctReduction")]
           colnames(df_reduced) <-
             c("rainfall_mmMeanAnn", "numGrassPixels1999", "numForestPixels1999",
               "numFiresBy2070_currentRisk", "numFiresBy2070_futureRisk",
               "fireRisk_90thPctileMaxHistorical")
           df_reduced
         })
rm(list_simulationDat)

# calculate pixels lost for each watershed
vec_knots <- 3:5
m <- 1
list_simulationDat_noProtection <-
  lapply(list_simulationDat_noProtection,
         function(df){
           
           # get predicted value and its SE
           predValInterval <-
             predict(object = gam_forestLoss,
                     newdata = df,
                     se.fit = TRUE
                     )
           
           # for each observation, use predval and SE to draw a single predicted value
           vec_predvals <- c()
           for(i in 1:length(predValInterval$linear.predictors)){
             vec_predvals[[i]] <-
               rnorm(n = 1,
                     mean = predValInterval$linear.predictors[[i]],
                     sd = predValInterval$se.fit[[i]] /
                       sqrt(length(gam_forestLoss$residuals)))
           }
           vec_predvals <- unlist(vec_predvals)
           vec_predvals[vec_predvals < 0] <- 0
           
           # add vector of predicted forest loss to simulation data.frame
           df$forestPixelsLostPerFire <-
             vec_predvals
           
           df
         })
list_simulationDat_protection050pct <-
  lapply(list_simulationDat_protection050pct,
         function(df){
           
           # get predicted value and its SE
           predValInterval <-
             predict(object = gam_forestLoss,
                     newdata = df,
                     se.fit = TRUE
             )
           
           # for each observation, use predval and SE to draw a single predicted value
           vec_predvals <- c()
           for(i in 1:length(predValInterval$linear.predictors)){
             vec_predvals[[i]] <-
               rnorm(n = 1,
                     mean = predValInterval$linear.predictors[[i]],
                     sd = predValInterval$se.fit[[i]] /
                       sqrt(length(gam_forestLoss$residuals)))
           }
           vec_predvals <- unlist(vec_predvals)
           vec_predvals[vec_predvals < 0] <- 0
           
           # add vector of predicted forest loss to simulation data.frame
           df$forestPixelsLostPerFire <-
             vec_predvals
           
           df
         })
list_simulationDat_protection100pct <-
  lapply(list_simulationDat_protection100pct,
         function(df){
           
           # get predicted value and its SE
           predValInterval <-
             predict(object = gam_forestLoss,
                     newdata = df,
                     se.fit = TRUE
             )
           
           # for each observation, use predval and SE to draw a single predicted value
           vec_predvals <- c()
           for(i in 1:length(predValInterval$linear.predictors)){
             vec_predvals[[i]] <-
               rnorm(n = 1,
                     mean = predValInterval$linear.predictors[[i]],
                     sd = predValInterval$se.fit[[i]] /
                       sqrt(length(gam_forestLoss$residuals)))
           }
           vec_predvals <- unlist(vec_predvals)
           vec_predvals[vec_predvals < 0] <- 0
           
           # add vector of predicted forest loss to simulation data.frame
           df$forestPixelsLostPerFire <-
             vec_predvals
           
           df
         })


# for each watershed, get distribution of forest loss (from all simulations) - EACH ROW IS A WATERSHED SIMULATED 1000 TIMES (1000 COLUMNS)
list_forestLossSimResults_pixelsLostPerFire_noProtection <-
  lapply(list_simulationDat_noProtection,
         function(df){
           df$forestPixelsLostPerFire
         })
dat_forestLossSimResults_pixelsLostPerFire_noProtection <-
  as.data.frame(do.call(cbind, list_forestLossSimResults_pixelsLostPerFire_noProtection))

list_forestLossSimResults_pixelsLostPerFire_protection050pct <-
  lapply(list_simulationDat_protection050pct,
         function(df){
           df$forestPixelsLostPerFire
         })
dat_forestLossSimResults_pixelsLostPerFire_protection050pct <-
  as.data.frame(do.call(cbind, list_forestLossSimResults_pixelsLostPerFire_protection050pct))

list_forestLossSimResults_pixelsLostPerFire_protection100pct <-
  lapply(list_simulationDat_protection100pct,
         function(df){
           df$forestPixelsLostPerFire
         })
dat_forestLossSimResults_pixelsLostPerFire_protection100pct <-
  as.data.frame(do.call(cbind, list_forestLossSimResults_pixelsLostPerFire_protection100pct))

# compile and save
dat_watershed$forestPixelsLostPerFire_mean_noProtection <-
  apply(dat_forestLossSimResults_pixelsLostPerFire_noProtection,
        MARGIN = 1, mean, na.rm = TRUE)
dat_watershed$forestPixelsLostPerFire_mean_protection050pct <-
  apply(dat_forestLossSimResults_pixelsLostPerFire_protection050pct,
        MARGIN = 1, mean, na.rm = TRUE)
dat_watershed$forestPixelsLostPerFire_mean_protection100pct <-
  apply(dat_forestLossSimResults_pixelsLostPerFire_protection100pct,
        MARGIN = 1, mean, na.rm = TRUE)
saveRDS(dat_watershed,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                      '09b - sf_watershedData with forest loss.rds'))
