
# This script looks at the overall landcover change under each scenario and
# tabulates hectares.

library(raster)
library(ggplot2)
#library(facetscales)




##### load data #####

# load current landcover raster
ras_lc_current <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
                "mhi_land_cover_names.tif"))

# load future scenario rasters
list_futureLandcoverRas <-
  list(ras_lc_best2070 = 
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "best case 2070.tif")),
       ras_lc_best2100 = 
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "best case 2100.tif")),
       ras_lc_middle2070 =
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "middle case 2070.tif")),
       ras_lc_middle2100 =
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "middle case 2100.tif")),
       ras_lc_worst2070 =
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "worst case 2070.tif")),
       ras_lc_worst2100 =
         raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                       "worst case 2100.tif"))
  )
list_futureLandcoverRas_names <- names(list_futureLandcoverRas)





##### define landcovers #####

# alien forest
vec_nonnativeForest <- 0

# native forest restored
vec_nativeForest <- 1

# alien grasses (from fire-driven forest loss)
vec_grasses <- -2

# forest to grass
vec_grasses2 <- -5

# alien forest
vec_forestAlien_codes <-
  c(1600, 1700, 1900, 2000, 11900, 12000)

# native forest
vec_forestNative_codes <-
  c(100, 200, 300, 400, 500, 1200, 10100, 10200, 10300, 10400, 10500, 10600,
    600, 700, 800, 1500, 10700, 10800, 10900, 11700)

# alien grass
vec_alienGrass_codes <- 
  c(3700, 3800, 3900)




##### change land cover values to dummies for tabulation #####

# replace land cover values to dummies in baseline raster
ras_lc_current[ras_lc_current %in% vec_forestAlien_codes] <- vec_nonnativeForest
ras_lc_current[ras_lc_current %in% vec_forestNative_codes] <- vec_nativeForest
ras_lc_current[ras_lc_current %in% vec_alienGrass_codes] <- vec_grasses2
gc()

# replace land cover values to dummies in each scenario raster
list_futureLandcoverRas_dummyOnly <-
  lapply(list_futureLandcoverRas,
         function(r){
           r[r %in% vec_forestAlien_codes] <- vec_nonnativeForest
           r[r %in% vec_forestNative_codes] <- vec_nativeForest
           r[r %in% vec_alienGrass_codes] <- vec_grasses2
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
    which(values(list_futureLandcoverRas[[i]]) %in% vec_nonnativeForest & !(values(ras_lc_current) %in% vec_nonnativeForest))
  gc()
  
  # pixels with restored native forest
  vec_restoredNativeForest_pixelIDs <-
    which(values(list_futureLandcoverRas[[i]]) %in% vec_nativeForest & !(values(ras_lc_current) %in% vec_nativeForest))
  gc()
  
  # forest pixels lost to fire (converted to grass)
  vec_fireDrivenForestLoss_pixelIDs <-
    which(values(list_futureLandcoverRas[[i]]) %in% vec_grasses & !(values(ras_lc_current) %in% vec_grasses))
  gc()
  
  # forest pixels converted to grass w/o fire
  vec_forestLostToGrass_pixelIDs <-
    which(values(list_futureLandcoverRas[[i]]) %in% vec_grasses2 & !(values(ras_lc_current) %in% vec_grasses2))
  gc()
  
  
  
  
  ##### add pixel IDs to results list #####
  
  list_pixelConversion[[i]] <-
    list(vec_invadedByNonnativeForest_pixelIDs,
         vec_restoredNativeForest_pixelIDs,
         vec_fireDrivenForestLoss_pixelIDs,
         vec_forestLostToGrass_pixelIDs)
  names(list_pixelConversion[[i]]) <-
    c('invadedByNonnativeForest', 'restoredNativeForest', 'fireDrivenForestLoss', 'forestLostToGrass')
  
}
names(list_pixelConversion) <- list_futureLandcoverRas_names
rm(i, vec_invadedByNonnativeForest_pixelIDs, vec_fireDrivenForestLoss_pixelIDs,
   vec_restoredNativeForest_pixelIDs)
gc()




##### under each scenario, how many hectares converted? #####

# initiate data.frame - pixels
dat_pixelSums <-
  data.frame(best = rep(NA, times = 8), middle = NA, worst = NA)
rownames(dat_pixelSums) <- c('invadedByNonnativeForest - 2070', 'invadedByNonnativeForest - 2100',
                             'restoredNativeForest - 2070',     'restoredNativeForest - 2100',
                             'fireDrivenForestLoss - 2070',     'fireDrivenForestLoss - 2100',
                             'forestLostToGrass - 2070',        'forestLostToGrass - 2100')

# fill data.frame with number of pixels converted
dat_pixelSums$best[[1]] <- length(list_pixelConversion$ras_lc_best2070$invadedByNonnativeForest)
dat_pixelSums$best[[3]] <- length(list_pixelConversion$ras_lc_best2070$restoredNativeForest)
dat_pixelSums$best[[5]] <- length(list_pixelConversion$ras_lc_best2070$fireDrivenForestLoss)
dat_pixelSums$best[[7]] <- length(list_pixelConversion$ras_lc_best2070$forestLostToGrass)
dat_pixelSums$best[[2]] <- length(list_pixelConversion$ras_lc_best2100$invadedByNonnativeForest)
dat_pixelSums$best[[4]] <- length(list_pixelConversion$ras_lc_best2100$restoredNativeForest)
dat_pixelSums$best[[6]] <- length(list_pixelConversion$ras_lc_best2100$fireDrivenForestLoss)
dat_pixelSums$best[[8]] <- length(list_pixelConversion$ras_lc_best2100$forestLostToGrass)

dat_pixelSums$middle[[1]] <- length(list_pixelConversion$ras_lc_middle2070$invadedByNonnativeForest)
dat_pixelSums$middle[[3]] <- length(list_pixelConversion$ras_lc_middle2070$restoredNativeForest)
dat_pixelSums$middle[[5]] <- length(list_pixelConversion$ras_lc_middle2070$fireDrivenForestLoss)
dat_pixelSums$middle[[7]] <- length(list_pixelConversion$ras_lc_middle2070$forestLostToGrass)
dat_pixelSums$middle[[2]] <- length(list_pixelConversion$ras_lc_middle2100$invadedByNonnativeForest)
dat_pixelSums$middle[[4]] <- length(list_pixelConversion$ras_lc_middle2100$restoredNativeForest)
dat_pixelSums$middle[[6]] <- length(list_pixelConversion$ras_lc_middle2100$fireDrivenForestLoss)
dat_pixelSums$middle[[8]] <- length(list_pixelConversion$ras_lc_middle2100$forestLostToGrass)

dat_pixelSums$worst[[1]] <- length(list_pixelConversion$ras_lc_worst2070$invadedByNonnativeForest)
dat_pixelSums$worst[[3]] <- length(list_pixelConversion$ras_lc_worst2070$restoredNativeForest)
dat_pixelSums$worst[[5]] <- length(list_pixelConversion$ras_lc_worst2070$fireDrivenForestLoss)
dat_pixelSums$worst[[7]] <- length(list_pixelConversion$ras_lc_worst2070$forestLostToGrass)
dat_pixelSums$worst[[2]] <- length(list_pixelConversion$ras_lc_worst2100$invadedByNonnativeForest)
dat_pixelSums$worst[[4]] <- length(list_pixelConversion$ras_lc_worst2100$restoredNativeForest)
dat_pixelSums$worst[[6]] <- length(list_pixelConversion$ras_lc_worst2100$fireDrivenForestLoss)
dat_pixelSums$worst[[8]] <- length(list_pixelConversion$ras_lc_worst2100$forestLostToGrass)

# convert number of pixels to hectares
dat_hectareSums <- dat_pixelSums * 0.09  # 30x30m pixel = 900 sq m = 0.09 hectares
gc()




##### create figure - levels #####

# melt data
dat_hectareSums_melt <-
  data.frame(Category = rep(rownames(dat_hectareSums), times = 3),
             Scenario = rep(c('Targeted protection\nand restoration', 'Targeted protection', 'No protection'), each = 8),
             Year = c('2070', '2100'),
             value = c(dat_hectareSums$best, dat_hectareSums$middle, dat_hectareSums$worst)/1e3)
dat_hectareSums_melt$Scenario <-
  factor(dat_hectareSums_melt$Scenario,
         levels = c('No protection', 'Targeted protection', 'Targeted protection\nand restoration'))

# format data for plotting
dat_hectareSums_melt$Category <-
  substr(dat_hectareSums_melt$Category, 1, nchar(dat_hectareSums_melt$Category)-7)
dat_hectareSums_melt$Category[dat_hectareSums_melt$Category == 'fireDrivenForestLoss'] <-
  'Forest to grass\n(fire-driven)'
dat_hectareSums_melt$Category[dat_hectareSums_melt$Category == 'invadedByNonnativeForest'] <-
  'Nonnative\nforest spread'
dat_hectareSums_melt$Category[dat_hectareSums_melt$Category == 'restoredNativeForest'] <-
  'Restored native forest'
dat_hectareSums_melt$Category[dat_hectareSums_melt$Category == 'forestLostToGrass'] <-
  'Forest to grass\n(non-fire-driven)'
dat_hectareSums_melt$Category <-
  factor(dat_hectareSums_melt$Category,
         levels = c('Nonnative\nforest spread', 'Forest to grass\n(fire-driven)', 'Forest to grass\n(non-fire-driven)', 'Restored native forest' ))

# plot
ggplot(dat_hectareSums_melt, aes(fill = Year)) +
  geom_bar(aes(x = Scenario, y = value),
           position = "dodge", stat = "identity", alpha = 0.7) +
  geom_text(aes(x = Scenario, y = value, label = ifelse(value == 0, '', sprintf("%0.2f", round(value, digits = 2)))),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
  geom_hline(yintercept = 0, linewidth = 1) +
  facet_grid(rows = vars(Category), scales = 'free_y') +
  scale_fill_viridis_d() +
  labs(y = 'Land converted (1000s hectares)') +
  theme(text = element_text(size = 26)) +
  ggh4x::facetted_pos_scales(y = list(
    Category == 'Nonnative\nforest spread' ~
      scale_y_continuous(limits = c(0, 250),
                         breaks = seq(0, 250, 50)),
    Category == 'Restored native forest' ~
      scale_y_continuous(limits = c(0, 20),
                         breaks = seq(0, 20, 5)),
    Category == 'Forest to grass\n(fire-driven)' ~
      scale_y_continuous(limits = c(0, 15),
                         breaks = seq(0, 15, 5)),
    Category == 'Forest to grass\n(non-fire-driven)' ~
      scale_y_continuous(limits = c(0, 25),
                         breaks = seq(0, 25, 5))))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13a - number of hectares converted.png'),
       dpi = 300, height = 13, width = 12)
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13a - number of hectares converted.pdf'),
       dpi = 300, height = 13, width = 12)




##### create figure - compare with worst case #####

# # create differences data (compared to worst case)
# dat_hectareSums$diffBest <- dat_hectareSums$best - dat_hectareSums$worst
# dat_hectareSums$diffMiddle <- dat_hectareSums$middle - dat_hectareSums$worst
# 
# # melt data
# dat_hectareSums_melt2 <-
#   data.frame(Category = rep(rownames(dat_hectareSums), times = 3),
#              Scenario = rep(c('Targeted protection\nand restoration\nvs. no protection', 'Targeted protection\nvs. no protection', 'No protection vs. baseline'), each = 8),
#              Year = c('2070', '2100'),
#              value = c(dat_hectareSums$diffBest, dat_hectareSums$diffMiddle, dat_hectareSums$worst)/1e3)
# 
# # format data for plotting
# dat_hectareSums_melt2$Category <-
#   substr(dat_hectareSums_melt2$Category, 1, nchar(dat_hectareSums_melt2$Category)-7)
# dat_hectareSums_melt2$Category[dat_hectareSums_melt2$Category == 'fireDrivenForestLoss'] <-
#   'Forest to grass\n(fire-driven)'
# dat_hectareSums_melt2$Category[dat_hectareSums_melt2$Category == 'invadedByNonnativeForest'] <-
#   'Nonnative\nforest spread'
# dat_hectareSums_melt2$Category[dat_hectareSums_melt2$Category == 'restoredNativeForest'] <-
#   'Restored native forest'
# dat_hectareSums_melt2$Category[dat_hectareSums_melt2$Category == 'forestLostToGrass'] <-
#   'Forest to grass\n(non-fire-driven)'
# dat_hectareSums_melt2$Category <-
#   factor(dat_hectareSums_melt2$Category,
#          levels = c('Nonnative\nforest spread', 'Forest to grass\n(fire-driven)', 'Forest to grass\n(non-fire-driven)', 'Restored native forest'))
# dat_hectareSums_melt2$Scenario <-
#   factor(dat_hectareSums_melt2$Scenario,
#          levels = c('No protection vs. baseline', 'Targeted protection\nvs. no protection', 'Targeted protection\nand restoration\nvs. no protection'))
# 
# # plot
# ggplot(dat_hectareSums_melt2[dat_hectareSums_melt2$Scenario != 'No protection vs. baseline',], aes(fill = Year)) +
#   geom_bar(aes(x = Scenario, y = value),
#            position = "dodge", stat = "identity", alpha = 0.7) +
#   geom_text(aes(x = Scenario, y = value, label = ifelse(value == 0, '', sprintf("%0.2f", round(value, digits = 2)))),
#             position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
#   geom_hline(yintercept = 0, linewidth = 1) +
#   facet_grid(rows = vars(Category), scales = 'free_y') +
#   scale_fill_viridis_d() +
#   labs(y = 'Land converted (1000s hectares)') +
#   theme(text = element_text(size = 26))
# 
# ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
#                          '13a - number of hectares converted comparison.png'),
#        dpi = 300, height = 13, width = 12)
# ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
#                          '13a - number of hectares converted comparison.pdf'),
#        dpi = 300, height = 13, width = 12)




##### what is the breakdown of alien forest/grass/shrub restored to native forest? #####

# create vector of values of current landcover raster
vals_currentLC <- values(ras_lc_current)

# get landcover codes of pixels converted to native forest
list_landcoversConvertedToNativeForest <-
  list(best2070 = vals_currentLC[list_pixelConversion$ras_lc_best2070$restoredNativeForest],
       best2100 = vals_currentLC[list_pixelConversion$ras_lc_best2100$restoredNativeForest])

# create tables of values
list_tabLandcoversConvertedToNativeForest <-
  list(best2070 = table(list_landcoversConvertedToNativeForest$best2070),
       best2100 = table(list_landcoversConvertedToNativeForest$best2100))

# convert tables to data.frames
dat_LandcoversConvertedToNativeForest <-
  data.frame(lcCode = names(list_tabLandcoversConvertedToNativeForest$best2070),
             pixels_best2070 = c(list_tabLandcoversConvertedToNativeForest$best2070),
             pixels_best2100 = c(list_tabLandcoversConvertedToNativeForest$best2100)
             )

# convert pixels to hectares
dat_LandcoversConvertedToNativeForest$hectares_best2070 <-
  dat_LandcoversConvertedToNativeForest$pixels_best2070 * 0.09
dat_LandcoversConvertedToNativeForest$hectares_best2100 <-
  dat_LandcoversConvertedToNativeForest$pixels_best2100 * 0.09

# define each landcover code as grass, shrub, or forest
dat_LandcoversConvertedToNativeForest$category <-
  c('native', 'native', 'native', 'native', 'native', 'native', 'native', 'native', 'native', 'native', 'native',
    'alien forest', 'alien forest','alien forest',
    'alien grass', 'alien grass', 'alien grass',
    'native', 'native', 'native', 'native',
    'alien forest', 'alien forest','alien forest')

# aggregate hectares by landcover category
dat_LandcoversConvertedToNativeForest_aggregate <-
  aggregate(list(dat_LandcoversConvertedToNativeForest$hectares_best2070,
                 dat_LandcoversConvertedToNativeForest$hectares_best2100),
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
