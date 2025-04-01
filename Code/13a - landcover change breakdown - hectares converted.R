
# This script looks at the overall landcover change under each scenario and
# tabulates hectares.

library(terra)
library(ggplot2)
#library(facetscales)




##### load data #####

# load current landcover raster
ras_lc_current <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
                "mhi_land_cover_names.tif"))

# load future scenario rasters
list_futureLandcoverRas <-
  list(ras_lc_best2070 = 
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "best case 2070 - moisture zones consistent w original raster - finalizedV2.tif")),
       ras_lc_best2100 = 
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "best case 2100 - moisture zones consistent w original raster - finalizedV2.tif")),
       ras_lc_middle2070 =
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "middle case 2070 - moisture zones consistent w original raster - finalizedV2.tif")),
       ras_lc_middle2100 =
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "middle case 2100 - moisture zones consistent w original raster - finalizedV2.tif")),
       ras_lc_worst2070 =
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "worst case 2070 - moisture zones consistent w original raster - finalizedV2.tif")),
       ras_lc_worst2100 =
         rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
                       "worst case 2100 - moisture zones consistent w original raster - finalizedV2.tif"))
  )
list_futureLandcoverRas_names <- names(list_futureLandcoverRas)

# load future scenario rasters - dummies
list_futureLandcoverRas_dummy <-
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
list_futureLandcoverRas_names <- names(list_futureLandcoverRas_dummy)





##### define landcovers #####

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




##### change land cover values to dummies for tabulation #####

# replace land cover values to dummies in baseline raster
ras_lc_current_dummy <- ras_lc_current
ras_lc_current_dummy[ras_lc_current_dummy %in% vec_forestnonnative_codes] <- vec_nonnativeForest
ras_lc_current_dummy[ras_lc_current_dummy %in% vec_forestNative_codes] <- vec_nativeForest
ras_lc_current_dummy[ras_lc_current_dummy %in% vec_nonnativeGrass_codes] <- vec_grasses2
gc()

# replace land cover values to dummies in each scenario raster
list_futureLandcoverRas_dummyOnly <-
  lapply(list_futureLandcoverRas,
         function(r){
           r[r %in% vec_forestnonnative_codes] <- vec_nonnativeForest
           r[r %in% vec_forestNative_codes] <- vec_nativeForest
           r[r %in% vec_nonnativeGrass_codes] <- vec_grasses2
           gc()
           r
         })
gc()




##### count number of pixels that change landcover under each scenario/model #####

# initiate tabulation
list_pixelConversion <- list()

# get values of current landcover
vals_currentLC <- values(ras_lc_current)
gc()

# for each scenario, find relevant changes to landcover
for(i in 1:length(list_futureLandcoverRas)){
  
  
  
  
  ### find which pixels converted in scenario i ###
  
  # pixels invaded by non-native forest
  vec_invadedByNonnativeForest_pixelIDs <-
    which(values(list_futureLandcoverRas[[i]]) %in% vec_forestnonnative_codes & !(values(ras_lc_current) %in% vec_forestnonnative_codes))
  gc()
  
  # pixels with restored native forest - nonnative forest to native forest
  vec_restoredNativeForestFromNonnativeForest_pixelIDs <-
    which(values(list_futureLandcoverRas[[i]]) %in% vec_forestNative_codes & values(ras_lc_current) %in% vec_forestnonnative_codes)
  gc()
  
  # pixels with restored native forest - grass/shrub to native forest
  vec_restoredNativeForestFromGrassShrub_pixelIDs <-
    which(values(list_futureLandcoverRas[[i]]) %in% vec_forestNative_codes & !(values(ras_lc_current) %in% c(vec_forestnonnative_codes, vec_forestNative_codes)))
  gc()
  
  # # forest pixels lost to fire (converted to grass)
  # vec_fireDrivenForestLoss_pixelIDs <-
  #   which(values(list_futureLandcoverRas_dummy[[i]]) %in% vec_grasses & !(values(ras_lc_current) %in% vec_nonnativeGrass_codes))
  # gc()
  
  # nonnative forest pixels lost to fire (converted to grass)
  vec_fireDrivenForestLoss_nonnative_pixelIDs <-
    which(values(list_futureLandcoverRas_dummy[[i]]) %in% vec_grasses & values(ras_lc_current) %in% vec_forestnonnative_codes)
  gc()
  
  # native forest pixels lost to fire (converted to grass)
  vec_fireDrivenForestLoss_native_pixelIDs <-
    which(values(list_futureLandcoverRas_dummy[[i]]) %in% vec_grasses & values(ras_lc_current) %in% vec_forestNative_codes)
  gc()
  
  # forest pixels converted to grass w/o fire (exclude those that were burned)
  vec_forestLostToGrass_pixelIDs <-
    which(values(list_futureLandcoverRas[[i]]) %in% vec_nonnativeGrass_codes & !(values(ras_lc_current_dummy) %in% vec_grasses2) & !(values(ras_lc_current) %in% vec_nonnativeGrass_codes) &
            !(values(list_futureLandcoverRas_dummy[[i]]) %in% vec_grasses) & !(values(list_futureLandcoverRas_dummyOnly[[i]]) %in% vec_grasses))
  gc()
  
  # # native forest pixels converted to grass w/o fire (exclude those that were burned)
  # vec_forestLostToGrass_native_pixelIDs <-
  #   which(values(list_futureLandcoverRas[[i]]) %in% vec_nonnativeGrass_codes & !(values(ras_lc_current_dummy) %in% vec_grasses2) & !(values(ras_lc_current) %in% vec_nonnativeGrass_codes) &
  #           !(values(list_futureLandcoverRas_dummy[[i]]) %in% vec_grasses) & !(values(list_futureLandcoverRas_dummyOnly[[i]]) %in% vec_grasses) & values(ras_lc_current) %in% vec_forestNative_codes)
  # gc()
  # 
  # # nonnative forest pixels converted to grass w/o fire (exclude those that were burned)
  # vec_forestLostToGrass_nonnative_pixelIDs <-
  #   which(values(list_futureLandcoverRas[[i]]) %in% vec_nonnativeGrass_codes & !(values(ras_lc_current_dummy) %in% vec_grasses2) & !(values(ras_lc_current) %in% vec_nonnativeGrass_codes) &
  #           !(values(list_futureLandcoverRas_dummy[[i]]) %in% vec_grasses) & !(values(list_futureLandcoverRas_dummyOnly[[i]]) %in% vec_grasses) & values(ras_lc_current) %in% vec_forestnonnative_codes)
  # gc()
  
  
  
  
  ##### add pixel IDs to results list #####
  
  # list_pixelConversion[[i]] <-
  #   list(vec_invadedByNonnativeForest_pixelIDs,
  #        vec_restoredNativeForest_pixelIDs,
  #        vec_fireDrivenForestLoss_pixelIDs,
  #        vec_forestLostToGrass_pixelIDs)
  # names(list_pixelConversion[[i]]) <-
  #   c('invadedByNonnativeForest', 'restoredNativeForest', 'fireDrivenForestLoss', 'forestLostToGrass')
  
  list_pixelConversion[[i]] <-
    list(vec_invadedByNonnativeForest_pixelIDs,
         vec_restoredNativeForestFromNonnativeForest_pixelIDs,
         vec_restoredNativeForestFromGrassShrub_pixelIDs,
         vec_fireDrivenForestLoss_native_pixelIDs,
         vec_fireDrivenForestLoss_nonnative_pixelIDs,
         vec_forestLostToGrass_pixelIDs)
  names(list_pixelConversion[[i]]) <-
    c('invadedByNonnativeForest', 'restoredNativeForestFromNonnativeForest', 'restoredNativeForestFromGrassShrub', 'fireDrivenForestLoss_native', 'fireDrivenForestLoss_nonnative', 'forestLostToGrass')
  
}
names(list_pixelConversion) <- list_futureLandcoverRas_names
rm(i, vec_invadedByNonnativeForest_pixelIDs, vec_fireDrivenForestLoss_native_pixelIDs, vec_fireDrivenForestLoss_nonnative_pixelIDs,
   vec_restoredNativeForestFromNonnativeForest_pixelIDs,vec_restoredNativeForestFromGrassShrub_pixelIDs)
gc()




##### under each scenario, how many hectares converted? #####

# initiate data.frame - pixels
dat_pixelSums <-
  data.frame(best = rep(NA, times = 12), middle = NA, worst = NA)
rownames(dat_pixelSums) <- c('invadedByNonnativeForest - 2070',                'invadedByNonnativeForest - 2100',
                             'restoredNativeForestFromNonnativeForest - 2070', 'restoredNativeForestFromNonnativeForest - 2100',
                             'restoredNativeForestFromGrassShrub - 2070',      'restoredNativeForestFromGrassShrub - 2100',
                             'fireDrivenForestLoss_native - 2070',             'fireDrivenForestLoss_native - 2100',
                             'fireDrivenForestLoss_nonnative - 2070',          'fireDrivenForestLoss_nonnative - 2100',
                             'forestLostToGrass - 2070',                       'forestLostToGrass - 2100')

# fill data.frame with number of pixels converted
dat_pixelSums$best[[1]]   <- length(list_pixelConversion$ras_lc_best2070$invadedByNonnativeForest)
dat_pixelSums$best[[3]]   <- length(list_pixelConversion$ras_lc_best2070$restoredNativeForestFromNonnativeForest)
dat_pixelSums$best[[5]]   <- length(list_pixelConversion$ras_lc_best2070$restoredNativeForestFromGrassShrub)
dat_pixelSums$best[[7]]   <- length(list_pixelConversion$ras_lc_best2070$fireDrivenForestLoss_native)
dat_pixelSums$best[[9]]   <- length(list_pixelConversion$ras_lc_best2070$fireDrivenForestLoss_nonnative)
dat_pixelSums$best[[11]]  <- length(list_pixelConversion$ras_lc_best2070$forestLostToGrass)
dat_pixelSums$best[[2]]   <- length(list_pixelConversion$ras_lc_best2100$invadedByNonnativeForest)
dat_pixelSums$best[[4]]   <- length(list_pixelConversion$ras_lc_best2100$restoredNativeForestFromNonnativeForest)
dat_pixelSums$best[[6]]   <- length(list_pixelConversion$ras_lc_best2100$restoredNativeForestFromGrassShrub)
dat_pixelSums$best[[8]]   <- length(list_pixelConversion$ras_lc_best2100$fireDrivenForestLoss_native)
dat_pixelSums$best[[10]]  <- length(list_pixelConversion$ras_lc_best2100$fireDrivenForestLoss_nonnative)
dat_pixelSums$best[[12]]  <- length(list_pixelConversion$ras_lc_best2100$forestLostToGrass)

dat_pixelSums$middle[[1]]   <- length(list_pixelConversion$ras_lc_middle2070$invadedByNonnativeForest)
dat_pixelSums$middle[[3]]   <- length(list_pixelConversion$ras_lc_middle2070$restoredNativeForestFromNonnativeForest)
dat_pixelSums$middle[[5]]   <- length(list_pixelConversion$ras_lc_middle2070$restoredNativeForestFromGrassShrub)
dat_pixelSums$middle[[7]]   <- length(list_pixelConversion$ras_lc_middle2070$fireDrivenForestLoss_native)
dat_pixelSums$middle[[9]]   <- length(list_pixelConversion$ras_lc_middle2070$fireDrivenForestLoss_nonnative)
dat_pixelSums$middle[[11]]  <- length(list_pixelConversion$ras_lc_middle2070$forestLostToGrass)
dat_pixelSums$middle[[2]]   <- length(list_pixelConversion$ras_lc_middle2100$invadedByNonnativeForest)
dat_pixelSums$middle[[4]]   <- length(list_pixelConversion$ras_lc_middle2100$restoredNativeForestFromNonnativeForest)
dat_pixelSums$middle[[6]]   <- length(list_pixelConversion$ras_lc_middle2100$restoredNativeForestFromGrassShrub)
dat_pixelSums$middle[[8]]   <- length(list_pixelConversion$ras_lc_middle2100$fireDrivenForestLoss_native)
dat_pixelSums$middle[[10]]  <- length(list_pixelConversion$ras_lc_middle2100$fireDrivenForestLoss_nonnative)
dat_pixelSums$middle[[12]]  <- length(list_pixelConversion$ras_lc_middle2100$forestLostToGrass)

dat_pixelSums$worst[[1]]   <- length(list_pixelConversion$ras_lc_worst2070$invadedByNonnativeForest)
dat_pixelSums$worst[[3]]   <- length(list_pixelConversion$ras_lc_worst2070$restoredNativeForestFromNonnativeForest)
dat_pixelSums$worst[[5]]   <- length(list_pixelConversion$ras_lc_worst2070$restoredNativeForestFromGrassShrub)
dat_pixelSums$worst[[7]]   <- length(list_pixelConversion$ras_lc_worst2070$fireDrivenForestLoss_native)
dat_pixelSums$worst[[9]]   <- length(list_pixelConversion$ras_lc_worst2070$fireDrivenForestLoss_nonnative)
dat_pixelSums$worst[[11]]  <- length(list_pixelConversion$ras_lc_worst2070$forestLostToGrass)
dat_pixelSums$worst[[2]]   <- length(list_pixelConversion$ras_lc_worst2100$invadedByNonnativeForest)
dat_pixelSums$worst[[4]]   <- length(list_pixelConversion$ras_lc_worst2100$restoredNativeForestFromNonnativeForest)
dat_pixelSums$worst[[6]]   <- length(list_pixelConversion$ras_lc_worst2100$restoredNativeForestFromGrassShrub)
dat_pixelSums$worst[[8]]   <- length(list_pixelConversion$ras_lc_worst2100$fireDrivenForestLoss_native)
dat_pixelSums$worst[[10]]  <- length(list_pixelConversion$ras_lc_worst2100$fireDrivenForestLoss_nonnative)
dat_pixelSums$worst[[12]]  <- length(list_pixelConversion$ras_lc_worst2100$forestLostToGrass)

# convert number of pixels to hectares
dat_hectareSums <- dat_pixelSums * 0.09  # 30x30m pixel = 900 sq m = 0.09 hectares
gc()




##### create figure - levels #####

# melt data
dat_hectareSums_melt <-
  data.frame(Category = rep(rownames(dat_hectareSums), times = 3),
             Scenario = rep(c('Targeted protection\nand restoration', 'Targeted protection', 'No protection'), each = 12),
             Year = c('2070', '2100'),
             value = c(dat_hectareSums$best, dat_hectareSums$middle, dat_hectareSums$worst)/1e3)
dat_hectareSums_melt$Scenario <-
  factor(dat_hectareSums_melt$Scenario,
         levels = c('No protection', 'Targeted protection', 'Targeted protection\nand restoration'))
dat_hectareSums_melt$value[dat_hectareSums_melt$value < 0.01] <- 0

# format data for plotting
dat_hectareSums_melt$Category <-
  substr(dat_hectareSums_melt$Category, 1, nchar(dat_hectareSums_melt$Category)-7)
dat_hectareSums_melt$Category[dat_hectareSums_melt$Category == 'fireDrivenForestLoss_native'] <-
  'Fire-driven\nnative forest loss'
dat_hectareSums_melt$Category[dat_hectareSums_melt$Category == 'fireDrivenForestLoss_nonnative'] <-
  'Fire-driven\nnon-native\nforest loss'
dat_hectareSums_melt$Category[dat_hectareSums_melt$Category == 'invadedByNonnativeForest'] <-
  'Non-native\nforest spread'
dat_hectareSums_melt$Category[dat_hectareSums_melt$Category == 'restoredNativeForestFromNonnativeForest'] <-
  'Restored\nnative forest\nfrom\nnon-native forest'
dat_hectareSums_melt$Category[dat_hectareSums_melt$Category == 'restoredNativeForestFromGrassShrub'] <-
  'Restored\nnative forest\nfrom grass\nand shrub'
dat_hectareSums_melt$Category[dat_hectareSums_melt$Category == 'forestLostToGrass'] <-
  'Non-fire-driven\nnative forest loss'
dat_hectareSums_melt$Category <-
  factor(dat_hectareSums_melt$Category,
         levels = c('Non-native\nforest spread', 'Fire-driven\nnative forest loss','Fire-driven\nnon-native\nforest loss',
                    'Non-fire-driven\nnative forest loss', 'Restored\nnative forest\nfrom\nnon-native forest', 'Restored\nnative forest\nfrom grass\nand shrub'))

# plot
ggplot(dat_hectareSums_melt, aes(fill = Year)) +
  geom_bar(aes(x = Scenario, y = value),
           position = "dodge", stat = "identity", alpha = 0.7) +
  geom_text(aes(x = Scenario, y = value, label = ifelse(value == 0, '', sprintf("%0.1f", round(value, digits = 1)))),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
  geom_hline(yintercept = 0, linewidth = 1) +
  facet_grid(rows = vars(Category), scales = 'free_y') +
  scale_fill_viridis_d() +
  labs(y = 'Land converted (1000s ha)') +
  theme(text = element_text(size = 26)) +
  ggh4x::facetted_pos_scales(y = list(
    Category == 'Non-native\nforest spread' ~
      scale_y_continuous(limits = c(0, 250),
                         breaks = seq(0, 250, 50)),
    Category == 'Restored\nnative forest\nfrom\nnon-native forest' ~
      scale_y_continuous(limits = c(0, 15),
                         breaks = seq(0, 15, 5)),
    Category == 'Restored\nnative forest\nfrom grass\nand shrub' ~
      scale_y_continuous(limits = c(0, 15),
                         breaks = seq(0, 15, 5)),
    Category == 'Fire-driven\nnative forest loss' ~
      scale_y_continuous(limits = c(0, 6),
                         breaks = seq(0, 6, 2)),
    Category == 'Fire-driven\nnon-native\nforest loss' ~
      scale_y_continuous(limits = c(0, 5),
                         breaks = seq(0, 5, 1)),
    Category == 'Non-fire-driven\nnative forest loss' ~
      scale_y_continuous(limits = c(0, 25),
                         breaks = seq(0, 25, 5))))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13a - number of hectares converted.png'),
       dpi = 300, height = 15, width = 12)
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13a - number of hectares converted.pdf'),
       dpi = 300, height = 15, width = 12)




##### create figure - compare with worst case #####

# create differences data (compared to worst case)
dat_hectareSums$diffBest <- dat_hectareSums$best - dat_hectareSums$worst
dat_hectareSums$diffMiddle <- dat_hectareSums$middle - dat_hectareSums$worst

# melt data
dat_hectareSums_melt2 <-
  data.frame(Category = rep(rownames(dat_hectareSums), times = 3),
             Scenario = rep(c('Targeted protection\nand restoration', 'Targeted protection', 'No protection'), each = 12),
             Year = c('2070', '2100'),
             value = c(dat_hectareSums$diffBest, dat_hectareSums$diffMiddle, dat_hectareSums$worst)/1e3)

# format data for plotting
dat_hectareSums_melt2$Category <-
  substr(dat_hectareSums_melt2$Category, 1, nchar(dat_hectareSums_melt2$Category)-7)
dat_hectareSums_melt2$Category[dat_hectareSums_melt2$Category == 'fireDrivenForestLoss_native'] <-
  'Fire-driven\nnative forest loss'
dat_hectareSums_melt2$Category[dat_hectareSums_melt2$Category == 'fireDrivenForestLoss_nonnative'] <-
  'Fire-driven\nnon-native\nforest loss'
dat_hectareSums_melt2$Category[dat_hectareSums_melt2$Category == 'invadedByNonnativeForest'] <-
  'Non-native\nforest spread'
dat_hectareSums_melt2$Category[dat_hectareSums_melt2$Category == 'restoredNativeForestFromNonnativeForest'] <-
  'Restored\nnative forest\nfrom\nnon-native forest'
dat_hectareSums_melt2$Category[dat_hectareSums_melt2$Category == 'restoredNativeForestFromGrassShrub'] <-
  'Restored\nnative forest\nfrom grass\nand shrub'
dat_hectareSums_melt2$Category[dat_hectareSums_melt2$Category == 'forestLostToGrass'] <-
  'Non-fire-driven\nnative forest loss'
dat_hectareSums_melt2$Category <-
  factor(dat_hectareSums_melt2$Category,
         levels = c('Non-native\nforest spread', 'Fire-driven\nnative forest loss','Fire-driven\nnon-native\nforest loss',
                    'Non-fire-driven\nnative forest loss', 'Restored\nnative forest\nfrom\nnon-native forest','Restored\nnative forest\nfrom grass\nand shrub'))
dat_hectareSums_melt2$Scenario <-
  factor(dat_hectareSums_melt2$Scenario,
         levels = c('No protection', 'Targeted protection', 'Targeted protection\nand restoration'))

# plot
ggplot(dat_hectareSums_melt2[dat_hectareSums_melt2$Scenario != 'No protection' &
                               dat_hectareSums_melt2$Category != 'Non-fire-driven\nnative forest loss',], aes(fill = Year)) +
  geom_bar(aes(x = Scenario, y = value),
           position = "dodge", stat = "identity", alpha = 0.7) +
  geom_text(aes(x = Scenario, y = value, label = ifelse(value == 0, '', sprintf("%0.1f", round(value, digits = 1))),
                vjust = 0.5 - sign(value)/2),
            position = position_dodge(width = 0.9), size = 6) +
  geom_hline(yintercept = 0, linewidth = 1) +
  facet_grid(rows = vars(Category), scales = 'free_y') +
  scale_fill_viridis_d() +
  labs(y = 'Difference in land converted (1000s ha)') +
  theme(text = element_text(size = 26)) +
  ggh4x::facetted_pos_scales(y = list(
    Category == 'Non-native\nforest spread' ~
      scale_y_continuous(limits = c(-65, 0),
                         breaks = seq(-60, 0, 20)),
    Category == 'Restored\nnative forest\nfrom\nnon-native forest' ~
      scale_y_continuous(limits = c(0, 15),
                         breaks = seq(0, 15, 5)),
    Category == 'Restored\nnative forest\nfrom grass\nand shrub' ~
      scale_y_continuous(limits = c(0, 15),
                         breaks = seq(0, 15, 5)),
    Category == 'Fire-driven\nnative forest loss' ~
      scale_y_continuous(limits = c(-6, 0),
                         breaks = seq(-6, 0, 2)),
    Category == 'Fire-driven\nnon-native\nforest loss' ~
      scale_y_continuous(limits = c(-4, 0),
                         breaks = seq(-4, 0, 1))))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13a - number of hectares converted comparison.png'),
       dpi = 300, height = 13, width = 10)
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13a - number of hectares converted comparison.pdf'),
       dpi = 300, height = 13, width = 10)




##### what is the breakdown of alien forest/grass/shrub restored to native forest? #####

# create vector of values of current landcover raster
vals_currentLC <- values(ras_lc_current)

# get landcover codes of pixels converted to native forest
list_landcoversConvertedToNativeForest <-
  list(worst2070 = vals_currentLC[list_pixelConversion$ras_lc_worst2070$restoredNativeForest],
       worst2100 = vals_currentLC[list_pixelConversion$ras_lc_worst2100$restoredNativeForest])

# create tables of values
list_tabLandcoversConvertedToNativeForest <-
  list(worst2070 = table(list_landcoversConvertedToNativeForest$worst2070),
       worst2100 = table(list_landcoversConvertedToNativeForest$worst2100))

# convert tables to data.frames
dat_LandcoversConvertedToNativeForest <-
  data.frame(lcCode = names(list_tabLandcoversConvertedToNativeForest$worst2070),
             pixels_worst2070 = c(list_tabLandcoversConvertedToNativeForest$worst2070),
             pixels_worst2100 = c(list_tabLandcoversConvertedToNativeForest$worst2100)
             )

# convert pixels to hectares
dat_LandcoversConvertedToNativeForest$hectares_worst2070 <-
  dat_LandcoversConvertedToNativeForest$pixels_worst2070 * 0.09
dat_LandcoversConvertedToNativeForest$hectares_worst2100 <-
  dat_LandcoversConvertedToNativeForest$pixels_worst2100 * 0.09

# define each landcover code as grass, shrub, or forest
dat_LandcoversConvertedToNativeForest$category <-
  c('native', 'native', 'native', 'native', 'native', 'native', 'native', 'native', 'native', 'native', 'native',
    'alien forest', 'alien forest','alien forest',
    'alien grass', 'alien grass', 'alien grass',
    'native', 'native', 'native', 'native',
    'alien forest', 'alien forest','alien forest')

# aggregate hectares by landcover category
dat_LandcoversConvertedToNativeForest_aggregate <-
  aggregate(list(dat_LandcoversConvertedToNativeForest$hectares_worst2070,
                 dat_LandcoversConvertedToNativeForest$hectares_worst2100),
            by = list(dat_LandcoversConvertedToNativeForest$category),
            sum)
colnames(dat_LandcoversConvertedToNativeForest_aggregate) <-
  c('type', '2070', '2100')


rm(vals_currentLC, list_tabLandcoversConvertedToNativeForest); gc()



##### save data #####

# save pixel IDs
saveRDS(list_pixelConversion,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/13 summary of landcover and water yield changes/',
                      '13a - landcover change pixel indices - all scenarios.rds'))

# save table
writexl::write_xlsx(dat_hectareSums,
                    path = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/13 summary of landcover and water yield changes/',
                                  '13a - total hectares converted - all scenarios.xlsx'))

gc()
