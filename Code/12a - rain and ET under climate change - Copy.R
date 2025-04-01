
# This script uses the ET regression to model changes due to climate change. This can be
# compared to the results of changes in ET due to land cover change.

library(terra)
library(ggplot2)
library(dplyr)




##### load data #####

# # statistical downscale data
# ras_staMidCent_dryRCP45_pctChg <-
#   rast('D:/OneDrive - hawaii.edu/Documents/Projects/Water/Climate change/Data/Raw/Climate/Climate projections/downscaled_statistical/precip/SDSRA-DATA/NETCDF/cmip5_rcp45_ensemble_precip_ano_stat_dry_OA_2041-2071_kc.nc')

# dynamical downscale data
ras_rain_dynBaseline <-
  rast('D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_baseline.tif')
ras_rain_dynRcp45Iprc <-
  rast('D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_future_projection_rcp45_IPRC.tif')
ras_rain_dynRcp85Iprc <-
  rast('D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_future_projection_rcp85_IPRC.tif')
ras_rain_dynRcp85Ncar <-
  rast('D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_future_projection_rcp85_NCAR.tif')

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
  c('rain_dynBaseline', 'rain_dynRcp45Iprc', 'rain_dynRcp85Iprc', 'rain_dynRcp85Ncar')

# rasters are monthly; multiply by 12 to get annual values
list_rastersRainfall <-
  lapply(list_rastersRainfall,
         function(r) r * 12)
rm(ras_rain_dynBaseline, ras_rain_dynRcp45Iprc,
   ras_rain_dynRcp85Iprc, ras_rain_dynRcp85Ncar)
gc()




##### run new ET predictions for each landcover and rainfall scenario #####

# initiate data.frame and predicted value column names
vec_scenarioColnames <- c('AET_dynBaseline',  'AET_dynRcp45Iprc',
                          'AET_dynRcp85Iprc', 'AET_dynRcp85Ncar')

# run predictions for each scenario
list_etBaseline <- list()
list_etRcp45Iprc <- list()
list_etRcp85Iprc <- list()
list_etRcp85Ncar <- list()
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
    if(s == 1){list_etBaseline[[l]] <- dat_l}
    if(s == 2){list_etRcp45Iprc[[l]] <- dat_l}
    if(s == 3){list_etRcp85Iprc[[l]] <- dat_l}
    if(s == 4){list_etRcp85Ncar[[l]] <- dat_l}

    rm(reg_l, dat_l)
    gc()
  }

  rm(dat_landcover2, dat_landcover2_split)
  gc()
}

rm(s, l, lc, vec_scenarioColnames, list_regression)

# recombine newly predicted AET values
dat_etbaseline <- do.call(rbind, list_etBaseline); rm(list_etBaseline); gc()
dat_etRcp45Iprc <- do.call(rbind, list_etRcp45Iprc); rm(list_etRcp45Iprc); gc()
dat_etRcp85Iprc <- do.call(rbind, list_etRcp85Iprc); rm(list_etRcp85Iprc); gc()
dat_etRcp85Ncar <- do.call(rbind, list_etRcp85Ncar); rm(list_etRcp85Ncar); gc()

# add predicted AET values to dat_landcover
dat_landcover <- left_join(dat_landcover, dat_etbaseline[c(1,2,5)], c('x', 'y')); rm(dat_etbaseline); gc()
dat_landcover <- left_join(dat_landcover, dat_etRcp45Iprc[c(1,2,5)], c('x', 'y')); rm(dat_etRcp45Iprc); gc()
dat_landcover <- left_join(dat_landcover, dat_etRcp85Iprc[c(1,2,5)], c('x', 'y')); rm(dat_etRcp85Iprc); gc()
dat_landcover <- left_join(dat_landcover, dat_etRcp85Ncar[c(1,2,5)], c('x', 'y')); rm(dat_etRcp85Ncar); gc()

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

# ensure predicted ET is less than or equal to that model's rainfall
dat_landcover$AET_dynBaseline[dat_landcover$AET_dynBaseline > dat_landcover$rain_dynBaseline &
                                !is.na(dat_landcover$AET_dynBaseline) & !is.na(dat_landcover$rain_dynBaseline)] <-
  dat_landcover$rain_dynBaseline[dat_landcover$AET_dynBaseline > dat_landcover$rain_dynBaseline &
                                   !is.na(dat_landcover$AET_dynBaseline) & !is.na(dat_landcover$rain_dynBaseline)]
dat_landcover$AET_dynRcp45Iprc[dat_landcover$AET_dynRcp45Iprc > dat_landcover$rain_dynRcp45Iprc &
                                 !is.na(dat_landcover$AET_dynRcp45Iprc) & !is.na(dat_landcover$rain_dynRcp45Iprc)] <-
  dat_landcover$rain_dynRcp45Iprc[dat_landcover$AET_dynRcp45Iprc > dat_landcover$rain_dynRcp45Iprc &
                                    !is.na(dat_landcover$AET_dynRcp45Iprc) & !is.na(dat_landcover$rain_dynRcp45Iprc)]
dat_landcover$AET_dynRcp85Iprc[dat_landcover$AET_dynRcp85Iprc > dat_landcover$rain_dynRcp85Iprc &
                                 !is.na(dat_landcover$AET_dynRcp85Iprc) & !is.na(dat_landcover$rain_dynRcp85Iprc)] <-
  dat_landcover$rain_dynRcp85Iprc[dat_landcover$AET_dynRcp85Iprc > dat_landcover$rain_dynRcp85Iprc &
                                    !is.na(dat_landcover$AET_dynRcp85Iprc) & !is.na(dat_landcover$rain_dynRcp85Iprc)]
dat_landcover$AET_dynRcp85Ncar[dat_landcover$AET_dynRcp85Ncar > dat_landcover$rain_dynRcp85Ncar &
                                 !is.na(dat_landcover$AET_dynRcp85Ncar) & !is.na(dat_landcover$rain_dynRcp85Ncar)] <-
  dat_landcover$rain_dynRcp85Ncar[dat_landcover$AET_dynRcp85Ncar > dat_landcover$rain_dynRcp85Ncar &
                                    !is.na(dat_landcover$AET_dynRcp85Ncar) & !is.na(dat_landcover$rain_dynRcp85Ncar)]

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
dat_baseline <- left_join(dat_baseline, dat_landcover[c('x', 'y', 'AET_dynBaseline', 'AET_dynRcp45Iprc', 'AET_dynRcp85Iprc', 'AET_dynRcp85Ncar')])

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
# list_rastersET_pctChange <- list()
# for(i in 2:length(list_rastersET)){
#   list_rastersET_pctChange[[i-1]] <-
#     (list_rastersET[[i]] - list_rastersET[[1]]) / list_rastersET[[1]] * 100
# }
# names(list_rastersET_pctChange) <- names(list_rastersET)[2:length(list_rastersET)]
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
