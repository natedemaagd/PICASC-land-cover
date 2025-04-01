
# This script uses the ET regression to model changes due to climate change. This can be
# compared to the results of changes in ET due to land cover change.

library(terra)
library(ggplot2)
library(dplyr)




##### load data #####

# # statistical downscale data
# ras_staMidCent_dryRCP45_pctChg <-
#   rast('D:/OneDrive - hawaii.edu/Documents/Projects/Water/Climate change/Data/Raw/Climate/Climate projections/downscaled_statistical/precip/SDSRA-DATA/NETCDF/cmip5_rcp45_ensemble_precip_ano_stat_dry_OA_2041-2071_kc.nc')

# dynamical and statistical downscale data
ras_rain_dynBaseline <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureRainfall/DynDS_3PresentDay_SeasMeans_RF_250m_OBS/Obs_HI_RF_present_mean_mm_Ann_2009.tif")
ras_rain_dynRcp85 <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureRainfall/DynDS_2FutureMeans_AbsChange_OBS/DynDS_FutureSeasMeans_RF_mm_250m_obs/DynDS_HI_RF_rcp85_mean_mm_Ann_2100_obs.tif")
ras_rain_staBaseline <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_3PresentDay_SeasMeans_RF_250m/StatDS_HI_RF_present_mean_mm_ann_2007.tif")
ras_rain_staRcp85 <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_2FutureMeans_AbsChange/StatDS_FutureSeasMeans_RF_mm_250m/StatDS_HI_RF_rcp85_mean_mm_ann_2100.tif")

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

# # create raster from original dat_landcover to use to resample rainfall
# ras_baseline <- rast(data.frame(x = dat_landcover_coords$x,
#                                 y = dat_landcover_coords$y,
#                                 z = NA),
#                      crs = '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
#                      type = 'xyz')
# gc()
# 
# # resample rainfall rasters to match regression data
# ras_rain_dynBaseline <-
#   resample(project(ras_rain_dynBaseline, ras_baseline, threads = TRUE),
#            ras_baseline, threads = TRUE); gc()
# ras_rain_dynRcp85 <-
#   resample(project(ras_rain_dynRcp85, ras_baseline, threads = TRUE),
#            ras_baseline, threads = TRUE); gc()
# ras_rain_staBaseline <-
#   resample(project(ras_rain_staBaseline, ras_baseline, threads = TRUE),
#            ras_baseline, threads = TRUE); gc()
# ras_rain_staRcp85 <-
#   resample(project(ras_rain_staRcp85, ras_baseline, threads = TRUE),
#            ras_baseline, threads = TRUE); gc()
# 
# writeRaster(ras_rain_dynBaseline,
#             filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
#                               '12a - ras_rain_dynBaseline resampled.tif'),
#             overwrite = TRUE)
# writeRaster(ras_rain_dynRcp85,
#             filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
#                               '12a - ras_rain_dynRcp85 resampled.tif'),
#             overwrite = TRUE)
# writeRaster(ras_rain_staBaseline,
#           filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
#                             '12a - ras_rain_staBaseline resampled.tif'),
#           overwrite = TRUE)
# writeRaster(ras_rain_staRcp85,
#           filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
#                             '12a - ras_rain_staRcp85 resampled.tif'),
#           overwrite = TRUE)
ras_rain_dynBaseline <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                                      '12a - ras_rain_dynBaseline resampled.tif'))
ras_rain_dynRcp85 <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                                       '12a - ras_rain_dynRcp85 resampled.tif'))
ras_rain_staBaseline <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                                    '12a - ras_rain_staBaseline resampled.tif'))
ras_rain_staRcp85 <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                                 '12a - ras_rain_staRcp85 resampled.tif'))
gc()

# combine rasters into list
list_rastersRainfall <-
  list(ras_rain_dynBaseline, ras_rain_dynRcp85, ras_rain_staBaseline, ras_rain_staRcp85)
names(list_rastersRainfall) <-
  c('rain_dynBaseline', 'rain_dynRcp85',
    'rain_staBaseline', 'ras_rain_staRcp85')




##### run new ET predictions for each landcover and rainfall scenario #####

# initiate data.frame and predicted value column names
vec_scenarioColnames <- c('AET_dynBaseline',  'AET_dynRcp85',
                          'AET_staBaseline',  'AET_staRcp85')

# run predictions for each scenario
list_etDynBaseline <- list()
list_etDynRcp85 <- list()
list_etStaBaseline <- list()
list_etStaRcp85 <- list()
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
    if(s == 1){list_etDynBaseline[[l]] <- dat_l}
    if(s == 2){list_etDynRcp85[[l]] <- dat_l}
    if(s == 3){list_etStaBaseline[[l]] <- dat_l}
    if(s == 4){list_etStaRcp85[[l]] <- dat_l}

    rm(reg_l, dat_l)
    gc()
  }

  rm(dat_landcover2, dat_landcover2_split)
  gc()
}

rm(s, l, lc, vec_scenarioColnames, list_regression)

# recombine newly predicted AET values
dat_etDynbaseline <- do.call(rbind, list_etDynBaseline); rm(list_etDynBaseline); gc()
dat_etDynRcp85 <- do.call(rbind, list_etDynRcp85); rm(list_etDynRcp85); gc()
dat_etStabaseline <- do.call(rbind, list_etStaBaseline); rm(list_etStaBaseline); gc()
dat_etStaRcp85 <- do.call(rbind, list_etStaRcp85); rm(list_etStaRcp85); gc()

# add predicted AET values to dat_landcover
dat_landcover <- left_join(dat_landcover, dat_etDynbaseline[c(1,2,5)], c('x', 'y')); rm(dat_etDynbaseline); gc()
dat_landcover <- left_join(dat_landcover, dat_etDynRcp85[c(1,2,5)], c('x', 'y')); rm(dat_etDynRcp85); gc()
dat_landcover <- left_join(dat_landcover, dat_etStabaseline[c(1,2,5)], c('x', 'y')); rm(dat_etStabaseline); gc()
dat_landcover <- left_join(dat_landcover, dat_etStaRcp85[c(1,2,5)], c('x', 'y')); rm(dat_etStaRcp85); gc()

# add rainfall values to dat_landcover
dat_rain <- as.data.frame(list_rastersRainfall$rain_dynBaseline, xy = TRUE)
for(i in 2:length(list_rastersRainfall)){
  dat <- as.data.frame(list_rastersRainfall[[i]], xy = TRUE)
  dat_rain <- left_join(dat_rain, dat, by = c('x', 'y'))
  rm(dat); gc()
}
rm(i)
colnames(dat_rain) <- c('x', 'y', names(list_rastersRainfall))
dat_landcover <- left_join(dat_landcover, dat_rain, by = c('x', 'y'))
gc()

# ensure predicted ET is less than or equal to that model's rainfall
dat_landcover$AET_dynBaseline[dat_landcover$AET_dynBaseline > dat_landcover$rain_dynBaseline &
                                !is.na(dat_landcover$AET_dynBaseline) & !is.na(dat_landcover$rain_dynBaseline)] <-
  dat_landcover$rain_dynBaseline[dat_landcover$AET_dynBaseline > dat_landcover$rain_dynBaseline &
                                   !is.na(dat_landcover$AET_dynBaseline) & !is.na(dat_landcover$rain_dynBaseline)]
dat_landcover$AET_dynRcp85[dat_landcover$AET_dynRcp85 > dat_landcover$rain_dynRcp85 &
                                 !is.na(dat_landcover$AET_dynRcp85) & !is.na(dat_landcover$rain_dynRcp85)] <-
  dat_landcover$rain_dynRcp85[dat_landcover$AET_dynRcp85 > dat_landcover$rain_dynRcp85 &
                                    !is.na(dat_landcover$AET_dynRcp85) & !is.na(dat_landcover$rain_dynRcp85)]
dat_landcover$AET_staBaseline[dat_landcover$AET_staBaseline > dat_landcover$rain_staBaseline &
                                !is.na(dat_landcover$AET_staBaseline) & !is.na(dat_landcover$rain_staBaseline)] <-
  dat_landcover$rain_staBaseline[dat_landcover$AET_staBaseline > dat_landcover$rain_staBaseline &
                                   !is.na(dat_landcover$AET_staBaseline) & !is.na(dat_landcover$rain_staBaseline)]
dat_landcover$AET_staRcp85[dat_landcover$AET_staRcp85 > dat_landcover$rain_staRcp85 &
                             !is.na(dat_landcover$AET_staRcp85) & !is.na(dat_landcover$rain_staRcp85)] <-
  dat_landcover$rain_staRcp85[dat_landcover$AET_staRcp85 > dat_landcover$rain_staRcp85 &
                                !is.na(dat_landcover$AET_staRcp85) & !is.na(dat_landcover$rain_staRcp85)]

saveRDS(dat_landcover,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                      '12a - ET under all scenarios.rds'))

dat_landcover <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/',
                 '12a - ET under all scenarios.rds'))
rm(list_regression)
gc()




##### convert ET estimates to rasters #####

# get baseline raster
rast_baseline <- list_rastersRainfall[[1]]
gc()

# convert to data.frame
dat_baseline <- as.data.frame(rast_baseline, xy = TRUE, na.rm = FALSE)
dat_baseline <- dat_baseline[c('x', 'y')]
gc()

# merge ET values to spatial data.frame
dat_baseline <- left_join(dat_baseline, dat_landcover[c('x', 'y', 'AET_dynBaseline', 'AET_dynRcp85', 'AET_staBaseline', 'AET_staRcp85')])

# convert to rasters
list_rastersET <- list()
for(i in 1:(length(colnames(dat_baseline)[3:ncol(dat_baseline)]))){
  list_rastersET[[i]] <-
    rast(dat_baseline[,c(1, 2, i+2)], type = 'xyz', crs = crs(rast_baseline))
  gc()
}
names(list_rastersET) <- colnames(dat_baseline)[3:ncol(dat_baseline)]

# clean up
rm(dat_baseline, dat_landcover, dat_landcover_coords, rast_baseline, i)
gc()

# # check percent change in ET - all look good
# list_rastersET_pctChange <-
#   with(list_rastersET,
#        list((AET_dynRcp85 - AET_dynBaseline) / AET_dynBaseline * 100,
#             (AET_staRcp85 - AET_staBaseline) / AET_staBaseline * 100)
#   )
# names(list_rastersET_pctChange) <- names(list_rastersET)[c(2,4)]
# rm(i, list_rastersET_pctChange)




##### create water yield rasters #####

# water yield = rainfall - ET
list_rastersWY <- list()
for(i in 1:length(list_rastersET)){
  list_rastersWY[[i]] <-
    list_rastersRainfall[[i]] - list_rastersET[[i]]
}
names(list_rastersWY) <- names(list_rastersET)
rm(i)
gc()

# double check that estimated water yield is no lower than 0 (regression ET estimates don't guarantee it's lower than rainfall)
list_rastersWY <-
  lapply(list_rastersWY,
         function(r){
           r[r<0] <- 0
           r
         }
  )
gc()




##### save output #####

# model names
vec_modelNames <- names(list_rastersET)
vec_modelNames <- strsplit(vec_modelNames, '_')
vec_modelNames <- sapply(vec_modelNames, function(x) x[[2]])

# write ET rasters
for(i in 1:length(list_rastersET)){
  writeRaster(list_rastersET[[i]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/12a rasters ET/',
                                'rast_ET_', vec_modelNames[[i]], '.tif'),
              overwrite = TRUE)
}

# write rain rasters
for(i in 1:length(list_rastersRainfall)){
  writeRaster(list_rastersRainfall[[i]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/12a rasters rainfall/',
                                'rast_rain_', vec_modelNames[[i]], '.tif'),
              overwrite = TRUE)
}

# write water yield rasters
for(i in 1:length(list_rastersWY)){
  writeRaster(list_rastersWY[[i]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/12a rasters water yield/',
                                'rast_WY', vec_modelNames[[i]], '.tif'),
              overwrite = TRUE)
}




##### compare statistical and dynamical downscale #####

# load pct change rasters
rast_dynPctChange <- rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/DynDS_FutureRainfall/DynDS_1FutureSeasChange_RF_Pct_250m/DynDS_HI_RF_pct_chng_rcp85_Ann_2100.tif")
rast_staPctChange <- rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/HI_climate_change/HI_downscaling_final/StatDS_FutureRainfall/StatDS_1FutureSeasChange_RF_Pct_250m/StatDS_HI_RF_pct_chng_rcp85_ann_2100.tif")

# statistics of values
median(values(rast_dynPctChange), na.rm = TRUE)
mean(values(rast_dynPctChange), na.rm = TRUE)
sd(values(rast_dynPctChange), na.rm = TRUE)
median(values(rast_staPctChange), na.rm = TRUE)
mean(values(rast_staPctChange), na.rm = TRUE)
sd(values(rast_staPctChange), na.rm = TRUE)