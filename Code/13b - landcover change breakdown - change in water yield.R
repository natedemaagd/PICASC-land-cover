
# This script uses the pixel tabulation in 13a and the landcover ET models from
# 05d to calculate overall change in ET/water yield for the scenarios.

library(ggplot2)
library(terra)

##### load data #####

# load landcover rasters
ras_lc_current <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
              "mhi_s0_baseline_noNames.tif"))
list_futureLandcoverRas <-
  list(ras_lc_best2070 = 
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "best case 2070.tif")),
       ras_lc_best2100 = 
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "best case 2100.tif")),
       ras_lc_middle2070 =
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "middle case 2070.tif")),
       ras_lc_middle2100 =
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "middle case 2100.tif")),
       ras_lc_worst2070 =
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "worst case 2070.tif")),
       ras_lc_worst2100 =
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "worst case 2100.tif"))
  )
list_futureLandcoverRas_names <- names(list_futureLandcoverRas)

# define dummy variables
vec_pixelTypes <- c(0, 1, -2, -5)
names(vec_pixelTypes) <- c('converted to alien forest', 'converted to native forest', 'burned forest', 'non-fire forest to grass')

# load ET rasters
ras_currentET <-
  rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted pre-invasion.tif")
list_ras_futureET <-
  list(rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted best case 2070.tif"),
       rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted best case 2100.tif"),
       rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted middle case 2070.tif"),
       rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted middle case 2100.tif"),
       rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted worst case 2070.tif"),
       rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted worst case 2100.tif")
  )
names(list_ras_futureET) <- names(list_futureLandcoverRas)

# crop to match extents
ras_lc_current <- crop(ras_lc_current, list_ras_futureET$ras_lc_best2070$`11c - ras AET predicted best case 2070`)
list_futureLandcoverRas <-
  lapply(list_futureLandcoverRas,
         function(r) crop(r, list_ras_futureET$ras_lc_best2070$`11c - ras AET predicted best case 2070`))




##### define land covers #####

# alien forest
vec_nonnativeForest <- 0

# native forest restored
vec_nativeForest <- 1

# alien grasses (from fire-driven forest loss)
vec_grasses <- -2

# forest to grass
vec_grasses2 <- -5

# nonnative forest
vec_forestnonnative_codes <-
  c(1600, 1700, 1900, 2000, 11900, 12000)

# native forest
vec_forestNative_codes <-
  c(100, 200, 300, 400, 500, 1200, 10100, 10200, 10300, 10400, 10500, 10600,
    600, 700, 800, 1500, 10700, 10800, 10900, 11700)

# nonnative grass
vec_nonnativeGrass_codes <- 
  c(3700, 3800, 3900)




##### calculate change in ET for each scenario #####

# create difference rasters
list_ras_changeET <-
  lapply(list_ras_futureET, function(r){r - ras_currentET})
names(list_ras_changeET) <- names(list_ras_futureET)
rm(list_ras_futureET, ras_currentET); gc()

# mask each ET scenario s with the types of converted pixels t
list_maskedET <- list()
for(s in 1:length(list_ras_changeET)){
  
  # get raster
  ras_et <- list_ras_changeET[[s]]
  
  # mask ras s with pixel type t
  list_maskedET[[s]] <- list()
  for(t in 1:length(vec_pixelTypes)){
    ras_et_type <- ras_et
    ras_et_type[list_futureLandcoverRas[[s]] != vec_pixelTypes[[t]]] <- NA
    list_maskedET[[s]][[t]] <- ras_et_type
    rm(ras_et_type); gc()
    print(paste0(s, ' - ', t, ' - ', Sys.time()))
  }
  names(list_maskedET[[s]]) <- names(vec_pixelTypes)
  rm(ras_et); gc()
}
names(list_maskedET) <- list_futureLandcoverRas_names
rm(s, t); gc()

# split fire-driven forest loss between native and nonnative forest
rast_forestNative <- ras_lc_current
rast_forestNonnative <- ras_lc_current
rast_forestNative[rast_forestNative %in% vec_forestNative_codes] <- -100
rast_forestNative[rast_forestNative != -100] <- NA
rast_forestNonnative[rast_forestNonnative %in% vec_forestnonnative_codes] <- -100
rast_forestNonnative[rast_forestNonnative != -100] <- NA
gc()
list_maskedET$ras_lc_best2070$`burned forest native` <- list_maskedET$ras_lc_best2070$`burned forest`
list_maskedET$ras_lc_best2070$`burned forest nonnative` <- list_maskedET$ras_lc_best2070$`burned forest`
list_maskedET$ras_lc_best2070$`burned forest native` <- mask(list_maskedET$ras_lc_best2070$`burned forest`, rast_forestNative)
list_maskedET$ras_lc_best2070$`burned forest nonnative` <- mask(list_maskedET$ras_lc_best2070$`burned forest`, rast_forestNonnative)
list_maskedET$ras_lc_best2070$`burned forest` <- NULL
list_maskedET$ras_lc_best2100$`burned forest native` <- list_maskedET$ras_lc_best2100$`burned forest`
list_maskedET$ras_lc_best2100$`burned forest nonnative` <- list_maskedET$ras_lc_best2100$`burned forest`
list_maskedET$ras_lc_best2100$`burned forest native` <- mask(list_maskedET$ras_lc_best2100$`burned forest`, rast_forestNative)
list_maskedET$ras_lc_best2100$`burned forest nonnative` <- mask(list_maskedET$ras_lc_best2100$`burned forest`, rast_forestNonnative)
list_maskedET$ras_lc_best2100$`burned forest` <- NULL
list_maskedET$ras_lc_middle2070$`burned forest native` <- list_maskedET$ras_lc_middle2070$`burned forest`
list_maskedET$ras_lc_middle2070$`burned forest nonnative` <- list_maskedET$ras_lc_middle2070$`burned forest`
list_maskedET$ras_lc_middle2070$`burned forest native` <- mask(list_maskedET$ras_lc_middle2070$`burned forest`, rast_forestNative)
list_maskedET$ras_lc_middle2070$`burned forest nonnative` <- mask(list_maskedET$ras_lc_middle2070$`burned forest`, rast_forestNonnative)
list_maskedET$ras_lc_middle2070$`burned forest` <- NULL
list_maskedET$ras_lc_middle2100$`burned forest native` <- list_maskedET$ras_lc_middle2100$`burned forest`
list_maskedET$ras_lc_middle2100$`burned forest nonnative` <- list_maskedET$ras_lc_middle2100$`burned forest`
list_maskedET$ras_lc_middle2100$`burned forest native` <- mask(list_maskedET$ras_lc_middle2100$`burned forest`, rast_forestNative)
list_maskedET$ras_lc_middle2100$`burned forest nonnative` <- mask(list_maskedET$ras_lc_middle2100$`burned forest`, rast_forestNonnative)
list_maskedET$ras_lc_middle2100$`burned forest` <- NULL
list_maskedET$ras_lc_worst2070$`burned forest native` <- list_maskedET$ras_lc_worst2070$`burned forest`
list_maskedET$ras_lc_worst2070$`burned forest nonnative` <- list_maskedET$ras_lc_worst2070$`burned forest`
list_maskedET$ras_lc_worst2070$`burned forest native` <- mask(list_maskedET$ras_lc_worst2070$`burned forest`, rast_forestNative)
list_maskedET$ras_lc_worst2070$`burned forest nonnative` <- mask(list_maskedET$ras_lc_worst2070$`burned forest`, rast_forestNonnative)
list_maskedET$ras_lc_worst2070$`burned forest` <- NULL
list_maskedET$ras_lc_worst2100$`burned forest native` <- list_maskedET$ras_lc_worst2100$`burned forest`
list_maskedET$ras_lc_worst2100$`burned forest nonnative` <- list_maskedET$ras_lc_worst2100$`burned forest`
list_maskedET$ras_lc_worst2100$`burned forest native` <- mask(list_maskedET$ras_lc_worst2100$`burned forest`, rast_forestNative)
list_maskedET$ras_lc_worst2100$`burned forest nonnative` <- mask(list_maskedET$ras_lc_worst2100$`burned forest`, rast_forestNonnative)
list_maskedET$ras_lc_worst2100$`burned forest` <- NULL
gc()

# split water yield from native forest restoration between non-native forest and grass/shrub
rast_restoredNNF <- ras_lc_current
rast_restoredGS <- ras_lc_current
rast_restoredNNF[rast_restoredNNF %in% vec_forestnonnative_codes] <- -200
rast_restoredNNF[rast_restoredNNF != -200] <- NA
rast_restoredGS[!(rast_restoredGS %in% vec_forestnonnative_codes)] <- -200
rast_restoredGS[rast_restoredGS != -200] <- NA
gc()
list_maskedET$ras_lc_best2070$`converted to native forest NNF` <- list_maskedET$ras_lc_best2070$`converted to native forest`
list_maskedET$ras_lc_best2070$`converted to native forest GS` <- list_maskedET$ras_lc_best2070$`converted to native forest`
list_maskedET$ras_lc_best2070$`converted to native forest NNF` <- mask(list_maskedET$ras_lc_best2070$`converted to native forest`, rast_restoredNNF)
list_maskedET$ras_lc_best2070$`converted to native forest GS` <- mask(list_maskedET$ras_lc_best2070$`converted to native forest`, rast_restoredGS)
list_maskedET$ras_lc_best2070$`converted to native forest` <- NULL
list_maskedET$ras_lc_best2100$`converted to native forest NNF` <- list_maskedET$ras_lc_best2100$`converted to native forest`
list_maskedET$ras_lc_best2100$`converted to native forest GS` <- list_maskedET$ras_lc_best2100$`converted to native forest`
list_maskedET$ras_lc_best2100$`converted to native forest NNF` <- mask(list_maskedET$ras_lc_best2100$`converted to native forest`, rast_restoredNNF)
list_maskedET$ras_lc_best2100$`converted to native forest GS` <- mask(list_maskedET$ras_lc_best2100$`converted to native forest`, rast_restoredGS)
list_maskedET$ras_lc_best2100$`converted to native forest` <- NULL
list_maskedET$ras_lc_middle2070$`converted to native forest NNF` <- list_maskedET$ras_lc_middle2070$`converted to native forest`
list_maskedET$ras_lc_middle2070$`converted to native forest GS` <- list_maskedET$ras_lc_middle2070$`converted to native forest`
list_maskedET$ras_lc_middle2070$`converted to native forest NNF` <- mask(list_maskedET$ras_lc_middle2070$`converted to native forest`, rast_restoredNNF)
list_maskedET$ras_lc_middle2070$`converted to native forest GS` <- mask(list_maskedET$ras_lc_middle2070$`converted to native forest`, rast_restoredGS)
list_maskedET$ras_lc_middle2070$`converted to native forest` <- NULL
list_maskedET$ras_lc_middle2100$`converted to native forest NNF` <- list_maskedET$ras_lc_middle2100$`converted to native forest`
list_maskedET$ras_lc_middle2100$`converted to native forest GS` <- list_maskedET$ras_lc_middle2100$`converted to native forest`
list_maskedET$ras_lc_middle2100$`converted to native forest NNF` <- mask(list_maskedET$ras_lc_middle2100$`converted to native forest`, rast_restoredNNF)
list_maskedET$ras_lc_middle2100$`converted to native forest GS` <- mask(list_maskedET$ras_lc_middle2100$`converted to native forest`, rast_restoredGS)
list_maskedET$ras_lc_middle2100$`converted to native forest` <- NULL
list_maskedET$ras_lc_worst2070$`converted to native forest NNF` <- list_maskedET$ras_lc_worst2070$`converted to native forest`
list_maskedET$ras_lc_worst2070$`converted to native forest GS` <- list_maskedET$ras_lc_worst2070$`converted to native forest`
list_maskedET$ras_lc_worst2070$`converted to native forest NNF` <- mask(list_maskedET$ras_lc_worst2070$`converted to native forest`, rast_restoredNNF)
list_maskedET$ras_lc_worst2070$`converted to native forest GS` <- mask(list_maskedET$ras_lc_worst2070$`converted to native forest`, rast_restoredGS)
list_maskedET$ras_lc_worst2070$`converted to native forest` <- NULL
list_maskedET$ras_lc_worst2100$`converted to native forest NNF` <- list_maskedET$ras_lc_worst2100$`converted to native forest`
list_maskedET$ras_lc_worst2100$`converted to native forest GS` <- list_maskedET$ras_lc_worst2100$`converted to native forest`
list_maskedET$ras_lc_worst2100$`converted to native forest NNF` <- mask(list_maskedET$ras_lc_worst2100$`converted to native forest`, rast_restoredNNF)
list_maskedET$ras_lc_worst2100$`converted to native forest GS` <- mask(list_maskedET$ras_lc_worst2100$`converted to native forest`, rast_restoredGS)
list_maskedET$ras_lc_worst2100$`converted to native forest` <- NULL
gc()

# convert to vectors
list_pixelETchange <- list()
for(i in 1:length(list_maskedET)){
  list_pixelETchange[[i]] <- list()
  for(j in 1:length(list_maskedET[[i]])){
    list_pixelETchange[[i]][[j]] <-
      values(list_maskedET[[i]][[j]])[!is.na(values(list_maskedET[[i]][[j]]))]
    gc()
  }
}
names(list_pixelETchange) <- names(list_maskedET)
rm(i, j)
gc()

# # limit major outliers in predicted ET
# list_pixelETchange$ras_lc_middle2100[[3]][list_pixelETchange$ras_lc_middle2100[[3]] > quantile(list_pixelETchange$ras_lc_middle2100[[3]], probs = 0.9999, na.rm = TRUE)] <- quantile(list_pixelETchange$ras_lc_middle2100[[3]], probs = 0.9999, na.rm = TRUE)

# convert to liters/year
for(i in 1:length(list_pixelETchange)){
  for(j in 1:length(list_pixelETchange[[i]])){
    list_pixelETchange[[i]][[j]] <-
      list_pixelETchange[[i]][[j]] * 30000*30000 * 0.000001  # cubic mm to L
  }
}
gc()

# sum change in ET for each scenario then name list elements
list_changeET_liters_summarized <- list()
for(i in 1:length(list_pixelETchange)){
  list_changeET_liters_summarized[[i]] <- list()
  for(j in 1:length(list_pixelETchange[[i]])){
    list_changeET_liters_summarized[[i]][[j]] <-
      sum(list_pixelETchange[[i]][[j]], na.rm = TRUE)
  }
}
names(list_changeET_liters_summarized) <- names(list_ras_changeET)
vec_names <- c('converted to alien forest', 'non-fire forest to grass', 'burned forest native', 'burned forest nonnative', 'converted to native forest NNF', 'converted to native forest GS')
for(i in 1:length(list_changeET_liters_summarized)){
  names(list_changeET_liters_summarized[[i]]) <- vec_names
}
rm(i, j, vec_names)
gc()

# convert list of lists to data.frame
dat_ETsums <-
  data.frame(best = rep(NA, times = 12), middle = NA, worst = NA)
rownames(dat_ETsums) <- c('invadedByNonnativeForest - 2070',      'invadedByNonnativeForest - 2100',
                          'restoredNativeForestNNF - 2070',       'restoredNativeForestNNF - 2100',
                          'restoredNativeForestGS - 2070',        'restoredNativeForestGS - 2100',
                          'fireDrivenForestLossNative - 2070',    'fireDrivenForestLossNative - 2100',
                          'fireDrivenForestLossNonnative - 2070', 'fireDrivenForestLossNonnative - 2100',
                          'forestToGrass - 2070',                 'forestToGrass - 2100')

dat_ETsums$best[[1]]  <- list_changeET_liters_summarized$ras_lc_best2070$`converted to alien forest`       #'invadedByNonnativeForest - 2070'
dat_ETsums$best[[3]]  <- list_changeET_liters_summarized$ras_lc_best2070$`converted to native forest NNF`  #'restoredNativeForestNNF - 2070'
dat_ETsums$best[[5]]  <- list_changeET_liters_summarized$ras_lc_best2070$`converted to native forest GS`   #'restoredNativeForestGS - 2070'
dat_ETsums$best[[7]]  <- list_changeET_liters_summarized$ras_lc_best2070$`burned forest native`            #'fireDrivenForestLossNative - 2070'
dat_ETsums$best[[9]]  <- list_changeET_liters_summarized$ras_lc_best2070$`burned forest nonnative`         #'fireDrivenForestLossNonnative - 2070'
dat_ETsums$best[[11]] <- list_changeET_liters_summarized$ras_lc_best2070$`non-fire forest to grass`        #'forestToGrass - 2070'
dat_ETsums$best[[2]]  <- list_changeET_liters_summarized$ras_lc_best2100$`converted to alien forest`       #'invadedByNonnativeForest - 2100'
dat_ETsums$best[[4]]  <- list_changeET_liters_summarized$ras_lc_best2100$`converted to native forest NNF`  #'restoredNativeForestNNF - 2100'
dat_ETsums$best[[6]]  <- list_changeET_liters_summarized$ras_lc_best2100$`converted to native forest GS`   #'restoredNativeForestGS - 2100'
dat_ETsums$best[[8]]  <- list_changeET_liters_summarized$ras_lc_best2100$`burned forest native`            #'fireDrivenForestLossNative - 2100'
dat_ETsums$best[[10]] <- list_changeET_liters_summarized$ras_lc_best2100$`burned forest nonnative`         #'fireDrivenForestLossNonnative - 2100'
dat_ETsums$best[[12]] <- list_changeET_liters_summarized$ras_lc_best2100$`non-fire forest to grass`        #'forestToGrass - 2100'

dat_ETsums$middle[[1]]  <- list_changeET_liters_summarized$ras_lc_middle2070$`converted to alien forest`   #'invadedByNonnativeForest - 2070'
dat_ETsums$middle[[3]]  <- list_changeET_liters_summarized$ras_lc_middle2070$`converted to native forest NNF`  #'restoredNativeForestNNF - 2070'
dat_ETsums$middle[[5]]  <- list_changeET_liters_summarized$ras_lc_middle2070$`converted to native forest GS`  #'restoredNativeForestGS - 2070'
dat_ETsums$middle[[7]]  <- list_changeET_liters_summarized$ras_lc_middle2070$`burned forest native`        #'fireDrivenForestLossNative - 2070'
dat_ETsums$middle[[9]]  <- list_changeET_liters_summarized$ras_lc_middle2070$`burned forest nonnative`     #'fireDrivenForestLossNonnative - 2070'
dat_ETsums$middle[[11]] <- list_changeET_liters_summarized$ras_lc_middle2070$`non-fire forest to grass`    #'forestToGrass - 2070'
dat_ETsums$middle[[2]]  <- list_changeET_liters_summarized$ras_lc_middle2100$`converted to alien forest`   #'invadedByNonnativeForest - 2100'
dat_ETsums$middle[[4]]  <- list_changeET_liters_summarized$ras_lc_middle2100$`converted to native forest NNF`  #'restoredNativeForestNNF - 2100'
dat_ETsums$middle[[6]]  <- list_changeET_liters_summarized$ras_lc_middle2100$`converted to native forest GS`  #'restoredNativeForestGS - 2100'
dat_ETsums$middle[[8]]  <- list_changeET_liters_summarized$ras_lc_middle2100$`burned forest native`        #'fireDrivenForestLossNative - 2100'
dat_ETsums$middle[[10]] <- list_changeET_liters_summarized$ras_lc_middle2100$`burned forest nonnative`     #'fireDrivenForestLossNonnative - 2100'
dat_ETsums$middle[[12]] <- list_changeET_liters_summarized$ras_lc_middle2100$`non-fire forest to grass`    #'forestToGrass - 2100'

dat_ETsums$worst[[1]]  <- list_changeET_liters_summarized$ras_lc_worst2070$`converted to alien forest`   #'invadedByNonnativeForest - 2070'
dat_ETsums$worst[[3]]  <- list_changeET_liters_summarized$ras_lc_worst2070$`converted to native forest NNF`  #'restoredNativeForestNNF - 2070'
dat_ETsums$worst[[5]]  <- list_changeET_liters_summarized$ras_lc_worst2070$`converted to native forest GS`  #'restoredNativeForestGS - 2070'
dat_ETsums$worst[[7]]  <- list_changeET_liters_summarized$ras_lc_worst2070$`burned forest native`        #'fireDrivenForestLossNative - 2070'
dat_ETsums$worst[[9]]  <- list_changeET_liters_summarized$ras_lc_worst2070$`burned forest nonnative`     #'fireDrivenForestLossNonnative - 2070'
dat_ETsums$worst[[11]] <- list_changeET_liters_summarized$ras_lc_worst2070$`non-fire forest to grass`    #'forestToGrass - 2070'
dat_ETsums$worst[[2]]  <- list_changeET_liters_summarized$ras_lc_worst2100$`converted to alien forest`   #'invadedByNonnativeForest - 2100'
dat_ETsums$worst[[4]]  <- list_changeET_liters_summarized$ras_lc_worst2100$`converted to native forest NNF`  #'restoredNativeForestNNF - 2100'
dat_ETsums$worst[[6]]  <- list_changeET_liters_summarized$ras_lc_worst2100$`converted to native forest GS`  #'restoredNativeForestGS - 2100'
dat_ETsums$worst[[8]]  <- list_changeET_liters_summarized$ras_lc_worst2100$`burned forest native`        #'fireDrivenForestLossNative - 2100'
dat_ETsums$worst[[10]] <- list_changeET_liters_summarized$ras_lc_worst2100$`burned forest nonnative`     #'fireDrivenForestLossNonnative - 2100'
dat_ETsums$worst[[12]] <- list_changeET_liters_summarized$ras_lc_worst2100$`non-fire forest to grass`    #'forestToGrass - 2100'

# convert from yearly liters to daily millions of liters, mult by -1 to change from ET to water yield
dat_ETsums <- dat_ETsums / 365 / 1e6 * -1




##### create figure - levels #####

# melt data
dat_ETsums_melt <-
  data.frame(Category = rep(rownames(dat_ETsums), times = 3),
             Scenario = rep(c('Targeted protection\nand restoration', 'Targeted\nprotection', 'No protection'), each = 12),
             Year = c('2070', '2100'),
             value = c(dat_ETsums$best, dat_ETsums$middle, dat_ETsums$worst))
dat_ETsums_melt$Scenario <-
  factor(dat_ETsums_melt$Scenario,
         levels = c('No protection', 'Targeted\nprotection', 'Targeted protection\nand restoration'))

# add label data
dat_ETsums_melt$barLabel <- round(dat_ETsums_melt$value, 1)
dat_ETsums_melt$barLabel[dat_ETsums_melt$barLabel == 0] <- NA
dat_ETsums_melt$barLabelPos <- NA

# format data for plotting
dat_ETsums_melt$Category <-
  substr(dat_ETsums_melt$Category, 1, nchar(dat_ETsums_melt$Category)-7)
dat_ETsums_melt$Category[dat_ETsums_melt$Category == 'fireDrivenForestLossNative'] <-
  'Fire-driven\nnative forest loss'
dat_ETsums_melt$Category[dat_ETsums_melt$Category == 'fireDrivenForestLossNonnative'] <-
  'Fire-driven\nnon-native\nforest loss'
dat_ETsums_melt$Category[dat_ETsums_melt$Category == 'invadedByNonnativeForest'] <-
  'Non-native\nforest spread'
dat_ETsums_melt$Category[dat_ETsums_melt$Category == 'restoredNativeForestNNF'] <-
  'Restored\nnative forest\nfrom\nnon-native forest'
dat_ETsums_melt$Category[dat_ETsums_melt$Category == 'restoredNativeForestGS'] <-
  'Restored\nnative forest\nfrom\ngrass/shrub'
dat_ETsums_melt$Category[dat_ETsums_melt$Category == 'forestToGrass'] <-
  'Non-fire-driven\nnative forest loss'
dat_ETsums_melt$Category <-
  factor(dat_ETsums_melt$Category,
         levels = c('Non-native\nforest spread',
                    'Fire-driven\nnative forest loss', 'Fire-driven\nnon-native\nforest loss',
                    'Non-fire-driven\nnative forest loss',
                    'Restored\nnative forest\nfrom\nnon-native forest', 'Restored\nnative forest\nfrom\ngrass/shrub'))

# plot
ggplot(dat_ETsums_melt, aes(fill = Year)) + 
  geom_bar(aes(x = Scenario, y = value),
           position = "dodge", stat = "identity", alpha = 0.7) +
  geom_text(aes(x = Scenario, y = value, label = ifelse(value == 0, '', sprintf("%0.1f", round(value, digits = 1))),
                vjust = 0.5 - sign(value)/2),
            position = position_dodge(width = 0.9), size = 6) +
  geom_hline(yintercept = 0, linewidth = 1) +
  facet_grid(rows = vars(Category), scales = 'free_y') +
  scale_fill_viridis_d() +
  labs(y = 'Change in water yield (million L per day)',x = NULL) +
  theme(text = element_text(size = 26)) +
  ggh4x::facetted_pos_scales(y = list(
    Category == 'Non-native\nforest spread' ~
      scale_y_continuous(limits = c(-1200, 0),
                         breaks = seq(-1200, 0, 400)),
    Category == 'Restored\nnative forest\nfrom\nnon-native forest' ~
      scale_y_continuous(limits = c(0, 60),
                         breaks = seq(0, 60, 20)),
    Category == 'Restored\nnative forest\nfrom\ngrass/shrub' ~
      scale_y_continuous(limits = c(0, 4),
                         breaks = seq(0, 4, 2)),
    Category == 'Fire-driven\nnative forest loss' ~
      scale_y_continuous(limits = c(-3, 0),
                         breaks = seq(-3, 0, 1)),
    Category == 'Fire-driven\nnon-native\nforest loss' ~
      scale_y_continuous(limits = c(0, 16),
                         breaks = seq(0, 16, 4)),
    Category == 'Non-fire-driven\nnative forest loss' ~
      scale_y_continuous(limits = c(0, 6),
                         breaks = seq(0, 6, 2))))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13b - change in statewide water yield by scenario.png'),
       dpi = 300, height = 15, width = 10)
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13b - change in statewide water yield by scenario.pdf'),
       dpi = 300, height = 15, width = 10)




##### create plot - comparison #####

# create differences
dat_ETsums$diffBest <- dat_ETsums$best - dat_ETsums$worst
dat_ETsums$diffMiddle <- dat_ETsums$middle - dat_ETsums$worst

# melt data
dat_ETsums_melt2 <-
  data.frame(Category = rep(rownames(dat_ETsums), times = 2),
             Scenario = rep(c('Targeted protection\nand restoration vs.\nno protection', 'Targeted\nprotection vs.\nno protection'), each = 12),
             Year = c('2070', '2100'),
             value = c(dat_ETsums$diffBest, dat_ETsums$diffMiddle))
dat_ETsums_melt2$Scenario <-
  factor(dat_ETsums_melt2$Scenario,
         levels = c('Targeted\nprotection vs.\nno protection', 'Targeted protection\nand restoration vs.\nno protection'))

# format data for plotting
dat_ETsums_melt2$Category <-
  substr(dat_ETsums_melt2$Category, 1, nchar(dat_ETsums_melt2$Category)-7)
dat_ETsums_melt2$Category[dat_ETsums_melt2$Category == 'fireDrivenForestLossNative'] <-
  'Fire-driven\nnative forest loss'
dat_ETsums_melt2$Category[dat_ETsums_melt2$Category == 'fireDrivenForestLossNonnative'] <-
  'Fire-driven\nnon-native\nforest loss'
dat_ETsums_melt2$Category[dat_ETsums_melt2$Category == 'invadedByNonnativeForest'] <-
  'Non-native\nforest spread'
dat_ETsums_melt2$Category[dat_ETsums_melt2$Category == 'restoredNativeForestNNF'] <-
  'Restored\nnative forest\nfrom\nnon-native forest'
dat_ETsums_melt2$Category[dat_ETsums_melt2$Category == 'restoredNativeForestGS'] <-
  'Restored\nnative forest\nfrom\ngrass/shrub'
dat_ETsums_melt2$Category[dat_ETsums_melt2$Category == 'forestToGrass'] <-
  'Non-fire-driven\nnative forest loss'
dat_ETsums_melt2$Category <-
  factor(dat_ETsums_melt2$Category,
         levels = c('Non-native\nforest spread',
                    'Fire-driven\nnative forest loss', 'Fire-driven\nnon-native\nforest loss',
                    'Non-fire-driven\nnative forest loss',
                    'Restored\nnative forest\nfrom\nnon-native forest', 'Restored\nnative forest\nfrom\ngrass/shrub'))

# plot
ggplot(dat_ETsums_melt2[dat_ETsums_melt2$Scenario != 'No protection' & dat_ETsums_melt2$Category != 'Non-fire-driven\nnative forest loss',], aes(fill = Year)) + 
  geom_bar(aes(x = Scenario, y = value),
           position = "dodge", stat = "identity", alpha = 0.7) +
  geom_text(aes(x = Scenario, y = value, label = ifelse(value == 0, '', sprintf("%0.1f", round(value, digits = 1))),
                vjust = 0.5 - sign(value)/2),
            position = position_dodge(width = 0.9), size = 6) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(rows = vars(Category), scales = 'free_y') +
  scale_fill_viridis_d() +
  labs(y = 'Difference in water yield (million L per day)',x = NULL) +
  theme(text = element_text(size = 26)) +
  ggh4x::facetted_pos_scales(y = list(
    Category == 'Non-native\nforest spread' ~
      scale_y_continuous(limits = c(0, 275),
                         breaks = seq(0, 275, 50)),
    Category == 'Restored\nnative forest\nfrom\nnon-native forest' ~
      scale_y_continuous(limits = c(0, 60),
                         breaks = seq(0, 60, 20)),
    Category == 'Restored\nnative forest\nfrom\ngrass/shrub' ~
      scale_y_continuous(limits = c(0, 4),
                         breaks = seq(0, 4, 2)),
    Category == 'Fire-driven\nnative forest loss' ~
      scale_y_continuous(limits = c(0, 3),
                         breaks = seq(0, 3, 1)),
    Category == 'Fire-driven\nnon-native\nforest loss' ~
      scale_y_continuous(limits = c(-16, 0),
                         breaks = seq(-16, 0, 4))))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13b - change in statewide water yield by scenario comparison to worst case.png'),
       dpi = 300, height = 15, width = 10)
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13b - change in statewide water yield by scenario comparison to worst case.pdf'),
       dpi = 300, height = 15, width = 10)
