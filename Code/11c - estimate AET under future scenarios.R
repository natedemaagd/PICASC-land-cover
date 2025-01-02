
# This script estimates AET under the future worst, middle, and best case
# scenarios, using the models created in script 11b.

library(raster)
library(dplyr)




##### load data #####

# load vector of affected landcovers
vec_landcovers <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                 '11b - vec_landcovers.rds'))

# load future scenario rasters
list_futureLandcoverRas <-
  list(ras_lc_best2070 =
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "best case 2070 - moisture zones consistent w original raster - finalizedV2.tif")),
       ras_lc_best2100 =
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "best case 2100 - moisture zones consistent w original raster - finalizedV2.tif")),
       ras_lc_middle2070 =
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "middle case 2070 - moisture zones consistent w original raster - finalizedV2.tif")),
       ras_lc_middle2100 =
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "middle case 2100 - moisture zones consistent w original raster - finalizedV2.tif")),
       ras_lc_worst2070 =
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "worst case 2070 - moisture zones consistent w original raster - finalizedV2.tif")),
       ras_lc_worst2100 =
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "worst case 2100 - moisture zones consistent w original raster - finalizedV2.tif"))
       )
list_futureLandcoverRas_names <- names(list_futureLandcoverRas)

# load regression data
dat_landcover <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                 '11b - dat_landcover.rds'))
list_regression <-
  readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                 '11b - reg_list.rds'))
# dat_medianLAI <-
#   readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
#                  '11b - median LAI by landcover.rds'))
gc()




##### create dataset for each future scenario and year combination #####

# for each future scenario, predict the new AET value
for(i in 1:length(list_futureLandcoverRas)){
  
  # convert raster to data.frame
  dat <- as.data.frame(list_futureLandcoverRas[[i]], xy = TRUE)
  colnames(dat) <- c('x', 'y', 'landcover')
  gc()
  
  # merge with current landcover data
  dat_landcover2 <- dat_landcover
  dat_landcover2$landcover <- NULL; gc()
  dat_landcover2 <-
    left_join(dat_landcover2, dat[c('x', 'y', 'landcover')]); gc()
  dat_landcover2$AET_predicted_preInvasion <- NULL; gc()
  rm(dat)
  gc()
  
  # # replace LAI with values from new landcovers
  # dat_landcover2$LAI <- NULL
  # dat_landcover2 <-
  #   left_join(dat_landcover2, dat_medianLAI, 'landcover')
  # gc()
  
  # split data by landcover and predict new AET values
  dat_landcover2_split <- split(dat_landcover2, dat_landcover2$landcover)
  gc()
  
  # predict new AET values
  for(j in 1:length(vec_landcovers)){
    
    # get dataset for landcover j
    dat_j_ID <- which(names(dat_landcover2_split) == vec_landcovers[[j]])
    
    # if no pixels for this landcover, skip it
    if(length(dat_j_ID) == 0){
      next
    }
    
    # predict new AET value using appropriate model
    dat_landcover2_split[[dat_j_ID]]$AET_predicted <-
      predict(list_regression[[j]], newdata = dat_landcover2_split[[dat_j_ID]])
    
    gc()
  }
  
  # recombine data
  dat_landcover2 <- do.call(plyr::rbind.fill, dat_landcover2_split)
  rm(dat_landcover2_split)
  gc()
  
  # add new predicted AET variable to original dataset, with appropriate name
  val_modelName <-
    strsplit(list_futureLandcoverRas_names[[i]], split = '_')[[1]][[3]]
  colnames(dat_landcover2)[colnames(dat_landcover2) == 'AET_predicted'] <-
    'AET_temp'
  dat_landcover <-
    left_join(dat_landcover, dat_landcover2[c('x', 'y', 'AET_temp')])
  colnames(dat_landcover)[colnames(dat_landcover) == 'AET_temp'] <-
    paste0('AET_', val_modelName)
  gc()
  
  # save vector of values individually
  saveRDS(dat_landcover[[ncol(dat_landcover)]],
          file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                        '11c - AET ', val_modelName, '.rds'))
  
  rm(dat_landcover2, dat_j_ID, val_modelName)
  gc(); print(paste(i, '-', Sys.time()))
}

# # load individual scenario data as needed and add to data.frame (if above loop not completed in one run)
# dat_landcover <- readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
#                                 '11c - dat_landcover predicted AET all scenarios.rds')); gc()
# list_dat_names <- list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
#                              pattern = '11c - AET', full.names = TRUE)
# list_dat <- lapply(list_dat_names, readRDS); gc()
# list_dat_names <- list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
#                              pattern = '11c - AET', full.names = FALSE)
# list_dat_names <- sapply(list_dat_names,
#                          function(c){
#                            gsub(' ', '_', c)
#                          })
# list_dat_names <- sapply(list_dat_names,
#                           function(c){
#                             substr(c, 7, nchar(c) - 4)
#                           })
# list_dat_names <- sapply(list_dat_names,
#                          function(c){
#                            gsub('AET_', 'AET_predicted_', c)
#                          })
# for(i in 1:length(list_dat)){
#   dat_landcover[,ncol(dat_landcover)+1] <-
#     list_dat[[i]]
#   colnames(dat_landcover)[ncol(dat_landcover)] <- list_dat_names[[i]]
#   gc()
# }
# 
# saveRDS(dat_landcover,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
#                       '11c - dat_landcover predicted AET all scenarios.rds'))
# 
# dat_landcover <- readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
#                                 '11c - dat_landcover predicted AET all scenarios.rds')); gc()




##### save rasters #####

ras_AET_predictedPreInvasion <-
  rasterFromXYZ(dat_landcover[c('x', 'y', 'AET_predicted_preInvasion')],
                crs = crs(list_futureLandcoverRas[[1]]))
writeRaster(ras_AET_predictedPreInvasion,
            filename = paste0(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                                     '11c - ras AET predicted pre-invasion.tif')),
            overwrite = TRUE)
rm(ras_AET_predictedPreInvasion); gc()

ras_AET_best2070 <-
  rasterFromXYZ(dat_landcover[c('x', 'y', 'AET_best2070')],
                crs = crs(list_futureLandcoverRas[[1]]))
writeRaster(ras_AET_best2070,
            filename = paste0(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                                     '11c - ras AET predicted best case 2070.tif')),
            overwrite = TRUE)
rm(ras_AET_best2070); gc()

ras_AET_best2100 <-
  rasterFromXYZ(dat_landcover[c('x', 'y', 'AET_best2100')],
                crs = crs(list_futureLandcoverRas[[1]]))
writeRaster(ras_AET_best2100,
            filename = paste0(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                                     '11c - ras AET predicted best case 2100.tif')),
            overwrite = TRUE)
rm(ras_AET_best2100); gc()

ras_AET_middle2070 <-
  rasterFromXYZ(dat_landcover[c('x', 'y', 'AET_middle2070')],
                crs = crs(list_futureLandcoverRas[[1]]))
writeRaster(ras_AET_middle2070,
            filename = paste0(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                                     '11c - ras AET predicted middle case 2070.tif')),
            overwrite = TRUE)
rm(ras_AET_middle2070); gc()

ras_AET_middle2100 <-
  rasterFromXYZ(dat_landcover[c('x', 'y', 'AET_middle2100')],
                crs = crs(list_futureLandcoverRas[[1]]))
writeRaster(ras_AET_middle2100,
            filename = paste0(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                                     '11c - ras AET predicted middle case 2100.tif')),
            overwrite = TRUE)
rm(ras_AET_middle2100); gc()

ras_AET_worst2070 <-
  rasterFromXYZ(dat_landcover[c('x', 'y', 'AET_worst2070')],
                crs = crs(list_futureLandcoverRas[[1]]))
writeRaster(ras_AET_worst2070,
            filename = paste0(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                                     '11c - ras AET predicted worst case 2070.tif')),
            overwrite = TRUE)
rm(ras_AET_worst2070); gc()

ras_AET_worst2100 <-
  rasterFromXYZ(dat_landcover[c('x', 'y', 'AET_worst2100')],
                crs = crs(list_futureLandcoverRas[[1]]))
writeRaster(ras_AET_worst2100,
            filename = paste0(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                                     '11c - ras AET predicted worst case 2100.tif')),
            overwrite = TRUE)
rm(ras_AET_worst2100); gc()

