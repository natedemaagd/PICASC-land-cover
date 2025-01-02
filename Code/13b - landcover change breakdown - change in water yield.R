
# This script uses the pixel tabulation in 13a and the landcover ET models from
# 05d to calculate overall change in ET/water yield for the scenarios.

library(ggplot2)
library(raster)


##### FOR FOREST TO GRASS TAKE OUT PIXELS THAT AREN'T ORIGINALLY FOREST (NEED TO LOAD IN BASELINE LAND COVER) #####

##### load data #####

# load landcover rasters
ras_lc_current <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
                "mhi_land_cover_names.tif"))
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

# define dummy variables
vec_pixelTypes <- c(0, 1, -2, -5)
names(vec_pixelTypes) <- c('converted to alien forest', 'converted to native forest', 'burned forest', 'non-fire forest to grass')

# load ET rasters
ras_currentET <-
       raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted pre-invasion.tif")
list_ras_futureET <-
  list(raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted best case 2070.tif"),
       raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted best case 2100.tif"),
       raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted middle case 2070.tif"),
       raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted middle case 2100.tif"),
       raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted worst case 2070.tif"),
       raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted worst case 2100.tif")
       )
names(list_ras_futureET) <- names(list_futureLandcoverRas)




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
for(i in 1:length(list_changeET_liters_summarized)){
  names(list_changeET_liters_summarized[[i]]) <-
    names(vec_pixelTypes)
}
rm(i, j)
gc()

# convert list of lists to data.frame
dat_ETsums <-
  data.frame(best = rep(NA, times = 8), middle = NA, worst = NA)
rownames(dat_ETsums) <- c('invadedByNonnativeForest - 2070', 'invadedByNonnativeForest - 2100',
                          'restoredNativeForest - 2070',     'restoredNativeForest - 2100',
                          'fireDrivenForestLoss - 2070',     'fireDrivenForestLoss - 2100',
                          'forestToGrass - 2070',            'forestToGrass - 2100')

dat_ETsums$best[[1]] <- list_changeET_liters_summarized$ras_lc_best2070$`converted to alien forest`
dat_ETsums$best[[3]] <- list_changeET_liters_summarized$ras_lc_best2070$`converted to native forest`
dat_ETsums$best[[5]] <- list_changeET_liters_summarized$ras_lc_best2070$`burned forest`
dat_ETsums$best[[7]] <- list_changeET_liters_summarized$ras_lc_best2070$`non-fire forest to grass`
dat_ETsums$best[[2]] <- list_changeET_liters_summarized$ras_lc_best2100$`converted to alien forest`
dat_ETsums$best[[4]] <- list_changeET_liters_summarized$ras_lc_best2100$`converted to native forest`
dat_ETsums$best[[6]] <- list_changeET_liters_summarized$ras_lc_best2100$`burned forest`
dat_ETsums$best[[8]] <- list_changeET_liters_summarized$ras_lc_best2100$`non-fire forest to grass`

dat_ETsums$middle[[1]] <- list_changeET_liters_summarized$ras_lc_middle2070$`converted to alien forest`
dat_ETsums$middle[[3]] <- list_changeET_liters_summarized$ras_lc_middle2070$`converted to native forest`
dat_ETsums$middle[[5]] <- list_changeET_liters_summarized$ras_lc_middle2070$`burned forest`
dat_ETsums$middle[[7]] <- list_changeET_liters_summarized$ras_lc_middle2070$`non-fire forest to grass`
dat_ETsums$middle[[2]] <- list_changeET_liters_summarized$ras_lc_middle2100$`converted to alien forest`
dat_ETsums$middle[[4]] <- list_changeET_liters_summarized$ras_lc_middle2100$`converted to native forest`
dat_ETsums$middle[[6]] <- list_changeET_liters_summarized$ras_lc_middle2100$`burned forest`
dat_ETsums$middle[[8]] <- list_changeET_liters_summarized$ras_lc_middle2100$`non-fire forest to grass`

dat_ETsums$worst[[1]] <- list_changeET_liters_summarized$ras_lc_worst2070$`converted to alien forest`
dat_ETsums$worst[[3]] <- list_changeET_liters_summarized$ras_lc_worst2070$`converted to native forest`
dat_ETsums$worst[[5]] <- list_changeET_liters_summarized$ras_lc_worst2070$`burned forest`
dat_ETsums$worst[[7]] <- list_changeET_liters_summarized$ras_lc_worst2070$`non-fire forest to grass`
dat_ETsums$worst[[2]] <- list_changeET_liters_summarized$ras_lc_worst2100$`converted to alien forest`
dat_ETsums$worst[[4]] <- list_changeET_liters_summarized$ras_lc_worst2100$`converted to native forest`
dat_ETsums$worst[[6]] <- list_changeET_liters_summarized$ras_lc_worst2100$`burned forest`
dat_ETsums$worst[[8]] <- list_changeET_liters_summarized$ras_lc_worst2100$`non-fire forest to grass`

# convert from yearly liters to daily millions of liters, mult by -1 to change from ET to water yield
dat_ETsums <- dat_ETsums / 365 / 1e6 * -1




##### create figure - levels #####

# melt data
dat_ETsums_melt <-
  data.frame(Category = rep(rownames(dat_ETsums), times = 3),
             Scenario = rep(c('Targeted protection\nand restoration', 'Targeted\nprotection', 'No protection'), each = 8),
             Year = c('2070', '2100'),
             value = c(dat_ETsums$best, dat_ETsums$middle, dat_ETsums$worst))
dat_ETsums_melt$Scenario <-
  factor(dat_ETsums_melt$Scenario,
         levels = c('No protection', 'Targeted\nprotection', 'Targeted protection\nand restoration'))

# add label data
dat_ETsums_melt$barLabel <- round(dat_ETsums_melt$value, 1)
dat_ETsums_melt$barLabel[dat_ETsums_melt$barLabel == 0] <- NA
dat_ETsums_melt$barLabelPos <-
  dat_ETsums_melt$value +
  c(-50, -50,  4,  4,  NA,   NA,  0.25, 0.25,
    -50, -50, NA, NA,  1.5,  1.5, 0.25, 0.25,
    -50, -50, NA, NA,  1.5,  1.5, 0.25, 0.25)

# format data for plotting
dat_ETsums_melt$Category <-
  substr(dat_ETsums_melt$Category, 1, nchar(dat_ETsums_melt$Category)-7)
dat_ETsums_melt$Category[dat_ETsums_melt$Category == 'fireDrivenForestLoss'] <-
  'Forest to grass\n(fire-driven)'
dat_ETsums_melt$Category[dat_ETsums_melt$Category == 'invadedByNonnativeForest'] <-
  'Alien forest spread'
dat_ETsums_melt$Category[dat_ETsums_melt$Category == 'restoredNativeForest'] <-
  'Restored native forest'
dat_ETsums_melt$Category[dat_ETsums_melt$Category == 'forestToGrass'] <-
  'Forest to grass\n(non-fire-driven)'
dat_ETsums_melt$Category <-
  factor(dat_ETsums_melt$Category,
         levels = c('Alien forest spread', 'Forest to grass\n(fire-driven)', 'Forest to grass\n(non-fire-driven)', 'Restored native forest'))

# plot
ggplot(dat_ETsums_melt, aes(fill = Year)) + 
  geom_bar(aes(x = Scenario, y = value),
           position = "dodge", stat = "identity", alpha = 0.7) +
  geom_text(aes(x = Scenario, y = barLabelPos,
                label = ifelse(is.na(barLabel), '', sprintf("%0.1f", round(barLabel, digits = 1))
                               )
                ),
            position = position_dodge(width=0.9), size = 6) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(rows = vars(Category), scales = 'free_y') +
  scale_fill_viridis_d() +
  labs(y = 'Change in water yield (million L per day)',x = NULL) +
  theme(text = element_text(size = 26))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13b - change in statewide water yield by scenario.png'),
       dpi = 300, height = 14, width = 10)
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13b - change in statewide water yield by scenario.pdf'),
       dpi = 300, height = 14, width = 10)




##### create plot - comparison #####

# create differences
dat_ETsums$diffBest <- dat_ETsums$best - dat_ETsums$worst
dat_ETsums$diffMiddle <- dat_ETsums$middle - dat_ETsums$worst

# melt data
dat_ETsums_melt2 <-
  data.frame(Category = rep(rownames(dat_ETsums), times = 2),
             Scenario = rep(c('Targeted protection\nand restoration', 'Targeted\nprotection'), each = 8),
             Year = c('2070', '2100'),
             value = c(dat_ETsums$diffBest, dat_ETsums$diffMiddle))
dat_ETsums_melt2$Scenario <-
  factor(dat_ETsums_melt2$Scenario,
         levels = c('Targeted\nprotection', 'Targeted protection\nand restoration'))

# format data for plotting
dat_ETsums_melt2$Category <-
  substr(dat_ETsums_melt2$Category, 1, nchar(dat_ETsums_melt2$Category)-7)
dat_ETsums_melt2$Category[dat_ETsums_melt2$Category == 'fireDrivenForestLoss'] <-
  'Forest to grass\n(fire-driven)'
dat_ETsums_melt2$Category[dat_ETsums_melt2$Category == 'invadedByNonnativeForest'] <-
  'Nonnative forest spread'
dat_ETsums_melt2$Category[dat_ETsums_melt2$Category == 'restoredNativeForest'] <-
  'Restored native forest'
dat_ETsums_melt2$Category[dat_ETsums_melt2$Category == 'forestToGrass'] <-
  'Forest to grass\n(non-fire-driven)'
dat_ETsums_melt2$Category <-
  factor(dat_ETsums_melt2$Category,
         levels = c('Nonnative forest spread', 'Forest to grass\n(fire-driven)', 'Forest to grass\n(non-fire-driven)', 'Restored native forest'))

# plot
ggplot(dat_ETsums_melt2[dat_ETsums_melt2$Scenario != 'No protection',], aes(fill = Year)) + 
  geom_bar(aes(x = Scenario, y = value),
           position = "dodge", stat = "identity", alpha = 0.7) +
  geom_hline(yintercept = 0, size = 1) +
  facet_grid(rows = vars(Category), scales = 'free_y') +
  scale_fill_viridis_d() +
  labs(y = 'Change in water yield (million L per day)',x = NULL) +
  theme(text = element_text(size = 26))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13b - change in statewide water yield by scenario comparison to worst case.png'),
       dpi = 300, height = 14, width = 10)
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13b - change in statewide water yield by scenario comparison to worst case.pdf'),
       dpi = 300, height = 14, width = 10)
