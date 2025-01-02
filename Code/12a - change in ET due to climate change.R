
# This script uses the ET regression to model changes due to climate change. This can be
# compared to the results of changes in ET due to land cover change.

library(terra)
library(ggplot2)
library(dplyr)




##### load data #####

# # statistical downscale data
# ras_staMidCent_dryRCP45_pctChg <-
#   rast("D:/OneDrive - hawaii.edu/Documents/Projects/Water/Climate change/Data/Raw/Climate/Climate projections/downscaled_statistical/precip/SDSRA-DATA/NETCDF/cmip5_rcp45_ensemble_precip_ano_stat_dry_OA_2041-2071_kc.nc", varname = 'pr')

# dynamical downscale data
ras_rain_dynBaseline <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_baseline.tif")
ras_rain_dynRcp45Iprc <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_future_projection_rcp45_IPRC.tif")
ras_rain_dynRcp85Iprc <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_future_projection_rcp85_IPRC.tif")
ras_rain_dynRcp85Ncar <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_future_projection_rcp85_NCAR.tif")

# load regression data
dat_landcover <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                 '11b - dat_landcover.rds'))
dat_landcover_coords <- dat_landcover[c('x', 'y')]
dat_landcover <- dat_landcover[!is.na(dat_landcover$AET), c('x', 'y', 'landcover', 'AET', 'PT', 'rainfall')]; gc()
list_regression <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                 '11b - reg_list.rds'))




##### format data #####

# create raster from original dat_landcover to use to resample rainfall
ras_baseline <- rast(data.frame(x = dat_landcover_coords$x,
                                y = dat_landcover_coords$y,
                                z = NA),
                     crs = '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
                     type = 'xyz')
gc()

# resample rainfall rasters to match regression data
# ras_rain_dynBaseline <- resample(projectrast(ras_rain_dynBaseline,
#                                                ras_baseline),
#                                  ras_baseline); gc()
# ras_rain_dynRcp45Iprc <- resample(projectrast(ras_rain_dynRcp45Iprc,
#                                                ras_baseline),
#                                   ras_baseline); gc()
# ras_rain_dynRcp85Iprc <- resample(projectrast(ras_rain_dynRcp85Iprc,
#                                                 ras_baseline),
#                                   ras_baseline); gc()
# ras_rain_dynRcp85Ncar <- resample(projectrast(ras_rain_dynRcp85Ncar,
#                                                 ras_baseline),
#                                   ras_baseline); gc()
# writerast(ras_rain_dynBaseline,
#             filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
#                               '12a - ras_rain_dynBaseline resampled.tif'),
#             overwrite = TRUE)
# writerast(ras_rain_dynRcp45Iprc,
#             filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
#                               '12a - ras_rain_dynRcp45Iprc resampled.tif'),
#             overwrite = TRUE)
# writerast(ras_rain_dynRcp85Iprc,
#             filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
#                               '12a - ras_rain_dynRcp85Iprc resampled.tif'),
#             overwrite = TRUE)
# writerast(ras_rain_dynRcp85Ncar,
#             filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
#                               '12a - ras_rain_dynRcp85Ncar resampled.tif'),
#             overwrite = TRUE)
ras_rain_dynBaseline <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                                      '12a - ras_rain_dynBaseline resampled.tif'))
ras_rain_dynRcp45Iprc <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                                       '12a - ras_rain_dynRcp45Iprc resampled.tif'))
ras_rain_dynRcp85Iprc <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                                       '12a - ras_rain_dynRcp85Iprc resampled.tif'))
ras_rain_dynRcp85Ncar <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                                       '12a - ras_rain_dynRcp85Ncar resampled.tif'))
gc()

# combine rasters into list
list_rastersRainfall <-
  list(ras_rain_dynBaseline, ras_rain_dynRcp45Iprc, ras_rain_dynRcp85Iprc,
       ras_rain_dynRcp85Ncar)
names(list_rastersRainfall) <-
  c('dynBaseline', 'dynRcp45Iprc', 'dynRcp85Iprc', 'dynRcp85Ncar')

# rasters are monthly; multiply by 12 to get annual values
list_rastersRainfall <-
  lapply(list_rastersRainfall,
         function(r) r * 12)




##### run new ET predictions for each landcover and rainfall scenario #####

# initiate data.frame and predicted value column names
vec_scenarioColnames <- c('AET_dynBaseline',  'AET_dynRcp45Iprc',
                          'AET_dynRcp85Iprc', 'AET_dynRcp85Ncar')

# run predictions for each scenario
list_datBaseline <- list()
list_datRcp45Iprc <- list()
list_datRcp85Iprc <- list()
list_datRcp85Ncar <- list()
for(s in 1:length(list_rastersRainfall)){

  # create copy of regression data without rainfall variable
  dat_landcover2 <- dat_landcover
  dat_landcover2$rainfall <- NULL
  gc()

  # replace rainfall values with climate change raster values
  dat_rain <- as.data.frame(list_rastersRainfall[[s]], xy = TRUE)
  colnames(dat_rain) <- c('x', 'y', 'rainfall')
  dat_landcover2 <- left_join(dat_landcover2, dat_rain, by = c('x', 'y'))
  rm(dat_rain); gc()

  # split data by landcover type
  dat_landcover2_split <- split(dat_landcover2, dat_landcover2$landcover)

  # predict new ET for each landcover
  for(l in 1:length(dat_landcover2_split)){

    # get landcover code l
    lc <- names(dat_landcover2_split)[[l]]

    # get predicted values for scenario s, landcover l
    reg_l <- list_regression[[which(names(list_regression)==lc)]]
    dat_l <- dat_landcover2_split[[which(names(dat_landcover2_split)==lc)]]
    dat_l <- dat_l[c('x', 'y', 'PT', 'rainfall')]
    dat_l$rainfall <- as.matrix(dat_l$rainfall)
    dat_l$predET <- predict(reg_l, newdata = dat_l)
    dat_l$predET[dat_l$predET < 0] <- 0
    colnames(dat_l)[ncol(dat_l)] <- vec_scenarioColnames[[s]]
    gc()
    if(s == 1){list_datBaseline[[l]] <- dat_l}
    if(s == 2){list_datRcp45Iprc[[l]] <- dat_l}
    if(s == 3){list_datRcp85Iprc[[l]] <- dat_l}
    if(s == 4){list_datRcp85Ncar[[l]] <- dat_l}

    rm(reg_l, dat_l)
    gc()
  }

  rm(dat_landcover2, dat_landcover2_split)
  gc()
}

rm(s, l, lc, vec_scenarioColnames, list_regression, list_rastersRainfall)

# recombine newly predicted AET values
dat_baseline <- do.call(rbind, list_datBaseline); rm(list_datBaseline); gc()
dat_Rcp45Iprc <- do.call(rbind, list_datRcp45Iprc); rm(list_datRcp45Iprc); gc()
dat_Rcp85Iprc <- do.call(rbind, list_datRcp85Iprc); rm(list_datRcp85Iprc); gc()
dat_Rcp85Ncar <- do.call(rbind, list_datRcp85Ncar); rm(list_datRcp85Ncar); gc()

# add predicted AET values to dat_landcover
dat_landcover <- left_join(dat_landcover, dat_baseline[c(1,2,5)], c('x', 'y')); rm(dat_baseline); gc()
dat_landcover <- left_join(dat_landcover, dat_Rcp45Iprc[c(1,2,5)], c('x', 'y')); rm(dat_Rcp45Iprc); gc()
dat_landcover <- left_join(dat_landcover, dat_Rcp85Iprc[c(1,2,5)], c('x', 'y')); rm(dat_Rcp85Iprc); gc()
dat_landcover <- left_join(dat_landcover, dat_Rcp85Ncar[c(1,2,5)], c('x', 'y')); rm(dat_Rcp85Ncar); gc()

saveRDS(dat_landcover,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                      '12a - ET under all scenarios.rds'))

dat_landcover <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                 '12a - ET under all scenarios.rds'))


##### calculate aggregate change in water yield (ET * -1) #####

# sum each scenario to get total mm
val_changeWaterYield_Rcp45Iprc <- sum(dat_landcover$AET_dynRcp45Iprc - dat_landcover$AET_dynBaseline, na.rm = TRUE) * -1
val_changeWaterYield_Rcp85Iprc <- sum(dat_landcover$AET_dynRcp85Iprc - dat_landcover$AET_dynBaseline, na.rm = TRUE) * -1
val_changeWaterYield_Rcp85Ncar <- sum(dat_landcover$AET_dynRcp85Ncar - dat_landcover$AET_dynBaseline, na.rm = TRUE) * -1

# convert to million L per day
val_changeWaterYield_Rcp45Iprc <- val_changeWaterYield_Rcp45Iprc * 30000 * 30000 * 0.000001 / 365 / 1e6
val_changeWaterYield_Rcp85Iprc <- val_changeWaterYield_Rcp85Iprc * 30000 * 30000 * 0.000001 / 365 / 1e6
val_changeWaterYield_Rcp85Ncar <- val_changeWaterYield_Rcp85Ncar * 30000 * 30000 * 0.000001 / 365 / 1e6
