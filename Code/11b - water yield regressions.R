
# This script runs the water yield regression for each landcover type used in
# the landcover simulation.

library(dplyr)
library(ggplot2)
library(terra)

# load land cover and climate rasters
rast_landcover <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
              "mhi_s0_baseline_noNames.tif"))
rast_PT <-
  rast(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_PriestlyTaylorPotentialET/pr0_mm_ann/",
              "w001001.adf"))
rast_rain <-
  rast(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_ann_rainfall_inches/Hawaii-State/",
              "rfgrid_inches_state_ann.txt"))
rast_AET <-
  rast(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_AET/",
              "aet_mm_ann.txt"))

# re-project climate rasters
rast_PT <- project(rast_PT, rast_landcover); gc()
rast_rain <- project(rast_rain*25.4, rast_landcover); gc()  # also convert in to mm
rast_AET <- project(rast_AET, rast_landcover); gc()

# create regression data
dat_landcover <- as.data.frame(rast_landcover, xy = TRUE)
colnames(dat_landcover) <- c('x', 'y', 'landcover')
dat_landcover$PT = values(rast_PT)
dat_landcover$rainfall = values(rast_rain)
dat_landcover$AET = values(rast_AET)
rm(rast_landcover, rast_PT, rast_rain, rast_AET)
gc()

# remove ocean pixels
dat_landcover <- dat_landcover[dat_landcover$landcover != 65535,]
gc()

# get unique landcovers landcovers as needed
vec_landcovers <- unique(dat_landcover$landcover)



##### run sample model and compare AIC #####

# # run for landcover = 100 since it has the most pixels
# reg_list <-
#   list(lm(AET ~ LAI + SM + U + T + Rnet,
#           data = dat_landcover[dat_landcover$landcover == 100,]),
#        lm(AET ~ LAI + SM + U + T,
#           data = dat_landcover[dat_landcover$landcover == 100,]),
#        lm(AET ~ LAI + SM + U + Rnet,
#           data = dat_landcover[dat_landcover$landcover == 100,]),
#        lm(AET ~ LAI + SM + T + Rnet,
#           data = dat_landcover[dat_landcover$landcover == 100,]),
#        lm(AET ~ LAI + U + T + Rnet,
#           data = dat_landcover[dat_landcover$landcover == 100,]),
#        lm(AET ~ SM + U + T + Rnet,
#           data = dat_landcover[dat_landcover$landcover == 100,])
#        )
# which.min(sapply(reg_list, AIC))
# 
# rm(reg_list); gc()

# # try new models: PT*rainfall w/ interaction
# lm1 <- lm(AET ~ PT + rainfall,
#           data = dat_landcover[dat_landcover$landcover == 100,])
# lm2 <- lm(AET ~ PT + rainfall + PT*rainfall,
#           data = dat_landcover[dat_landcover$landcover == 100,])
# rm(lm1, lm2); gc()




##### run model for all landcovers #####

# note we must run regression on all landcover types, even if not involved with
# landcover spread, since we also use it to model climate change effects in part 12.

# if no observations for given landcover type, skip it
reg_list <- list()
for(i in 1:length(vec_landcovers)){
  reg_list[[i]] <-
    lm(AET ~ PT + rainfall,
       data = dat_landcover[dat_landcover$landcover ==
                              vec_landcovers[[i]],])
  gc()
  print(paste(i, '-', Sys.time()))
}
rm(i); gc()
names(reg_list) <- vec_landcovers

saveRDS(reg_list,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                      '11b - reg_list.rds'))
# reg_list <-
#   readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
#                  '11b - reg_list.rds'))

# analyze coefficients
dat_coefs <-
  lapply(reg_list, coef)
dat_coefs <- as.data.frame(do.call(rbind, dat_coefs))
ggplot(dat_coefs, aes(PT)) + geom_density()
ggplot(dat_coefs, aes(rainfall)) + geom_density()




##### replace LAI with median value for each landcover type #####

# Regression no longer relies on LAI for predicting AET

# # This is required to get a good comparison between pre- and post-invasion
# # ET values.
# 
# # by landcover, get median LAI
# median_LAI <-
#   aggregate(dat_landcover$LAI, list(dat_landcover$landcover), median, na.rm = T)
# colnames(median_LAI) <- c('landcover', 'LAI')
# 
# # replace LAI values
# dat_landcover$LAI <- NULL
# dat_landcover <- left_join(dat_landcover, median_LAI, 'landcover')
# gc()
# 
# saveRDS(median_LAI,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
#                       '11b - median LAI by landcover.rds'))




##### get predicted AET values #####

# split data by landcover
dat_landcover_split <-
  split(dat_landcover, dat_landcover$landcover)
rm(dat_landcover)
gc()

# use regression i to predict AET values of landcover i
for(i in 1:length(vec_landcovers)){
  
  # get dataset for landcover i
  dat_i_ID <- which(names(dat_landcover_split) == vec_landcovers[[i]])
  
  # predict new AET value using appropriate model
  dat_landcover_split[[dat_i_ID]]$AET_predicted_preInvasion <-
    predict(reg_list[[i]], newdata = dat_landcover_split[[dat_i_ID]])
  
  gc()
}
rm(i, dat_i_ID); gc()

# re-combine data
dat_landcover <- do.call(bind_rows, dat_landcover_split)
rm(dat_landcover_split)
gc()

# compare original AET and newly-modeled AET
diff_aet <- dat_landcover$AET_predicted_preInvasion - dat_landcover$AET
plot(density(diff_aet, na.rm = TRUE))
sum(is.na(diff_aet))




##### save #####

saveRDS(dat_landcover,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                      '11b - dat_landcover.rds'))

saveRDS(vec_landcovers,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/',
                      '11b - vec_landcovers.rds'))

