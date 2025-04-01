
# This script creates figures for publication: plots of landcover maps

library(raster)
library(ggplot2)
library(ggsn)
library(viridis)
library(cowplot)
library(tidyverse)




##### define landcover groups for plots #####

# alien forest
vec_forestNonnative <-
  c(1600, 1700, 1900, 2000, 11900, 12000)

# native forest
vec_forestNative <-
  c(100, 200, 300, 400, 500, 10100, 10200, 10300, 10400, 10500, 10600,
    600, 700, 800, 1500, 10700, 10800, 10900, 11700)

# alien grass
vec_alienGrass <- 
  c(3700, 3800, 3900)

# dummy variables
val_dummyForestNonnative <- 0
val_dummyForestNative <- 1
val_dummyForestBurned <- -2
val_dummyOcean <- -3
val_dummyGrass <- -5
val_dummyOther <- -1



##### load data #####

# load landcover rasters
list_landcoverRasters <-
  list(
    
    # baseline landcover
    ras_baseline_lc =
      raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
                    "mhi_s0_baseline_noNames.tif")),
    
    # best case landcover
    ras_best2070_lc =
      raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                    "best case 2070.tif")),
    ras_best2100_lc =
      raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                    "best case 2100.tif")),
    
    # middle case landcover
    ras_middle2070_lc =
      raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                    "middle case 2070.tif")),
    ras_middle2100_lc =
      raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                    "middle case 2100.tif")),
    
    # worst case landcover
    ras_worst2070_lc =
      raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                    "worst case 2070.tif")),
    ras_worst2100_lc =
      raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
                    "worst case 2100.tif"))
    
    )




##### format rasters for plotting #####

# in all landcover rasters, replace landcover codes with dummy groupings
for(i in 1:length(list_landcoverRasters)){
  ras_temp <- list_landcoverRasters[[i]]
  ras_temp[ras_temp %in% vec_forestNonnative] <- val_dummyForestNonnative; gc()
  ras_temp[ras_temp %in% vec_forestNative] <- val_dummyForestNative; gc()
  ras_temp[ras_temp %in% vec_alienGrass] <- val_dummyGrass; gc()
  list_landcoverRasters[[i]] <- ras_temp
  rm(ras_temp); gc(); print(i)
}
rm(i); gc()

# in all landcover rasters, replace other landcovers with "other" values
for(i in 1:length(list_landcoverRasters)){
  ras_temp <- list_landcoverRasters[[i]]
  ras_temp[ras_temp == 65535] <- val_dummyOcean
  ras_temp[!(ras_temp %in% c(val_dummyForestNonnative, val_dummyForestBurned,
                             val_dummyForestNative, val_dummyOcean,
                             val_dummyGrass))] <- val_dummyOther
  list_landcoverRasters[[i]] <- ras_temp
  rm(ras_temp); gc(); print(i)
}

# ensure protected forest remains coded as native forest
for(i in 1:length(list_landcoverRasters)){
  ras_temp <- list_landcoverRasters[[i]]
  ras_temp[ras_temp == val_dummyOther & list_landcoverRasters$ras_baseline_lc %in% c(val_dummyForestNative, vec_forestNative)] <- val_dummyForestNative
  list_landcoverRasters[[i]] <- ras_temp
}
rm(i, ras_temp)
gc()




##### plot landcover raster LEGEND #####

# plot and save LEGEND ONLY of the landcover map

# convert raster to data.frame
dat <- as.data.frame(list_landcoverRasters[[7]], xy = TRUE)
gc()

# remove ocean pixels
dat <- dat[dat$layer != val_dummyOcean,]
gc()

# convert values to labels
dat$layer[dat$layer == val_dummyForestNonnative]  <- 'Nonnative forest'
dat$layer[dat$layer == val_dummyForestBurned] <- 'Burned forest'
dat$layer[dat$layer == val_dummyForestNative] <- 'Native forest'
dat$layer[dat$layer == val_dummyGrass] <- 'Nonnative grass'
dat$layer[dat$layer == val_dummyOther] <- 'Other'
gc()

# plot
p <-
  ggplot(data = dat, aes(x = x, y = y, fill = layer)) + 
  geom_raster() +
  coord_equal() +
  scale_fill_manual(values = c('Nonnative forest' = "#7A0403FF",
                               'Nonnative grass' = "#FABA39FF",
                               'Burned forest' = "#72FE5EFF",
                               'Native forest' = "#4662D7FF",
                               'Other' = 'lightgray'),
                    drop = FALSE) +
  labs(fill = NULL) +
  north(data = dat, location = 'topright', symbol = 9) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        text = element_text(size = 15))

# get plot legend
p <- get_legend(p)

ggsave(
  filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/',
                    '12-05-00 map color legend.png'),
  dpi = 300, plot = p)

rm(dat, p); gc()




##### plot landcover raster maps #####

# plot and save landcover maps
list_plot <- list()
for(i in 1:length(list_landcoverRasters)){
  
  # convert raster to data.frame
  dat <- as.data.frame(list_landcoverRasters[[i]], xy = TRUE)
  gc()
  
  # remove ocean pixels
  dat <- dat[dat$layer != val_dummyOcean,]
  gc()
  
  # convert values to labels
  dat$layer[dat$layer == val_dummyForestNonnative]  <- 'Nonnative forest'
  dat$layer[dat$layer == val_dummyGrass] <- 'Nonnative grass'
  dat$layer[dat$layer == val_dummyForestBurned] <- 'Burned forest'
  dat$layer[dat$layer == val_dummyForestNative] <- 'Native forest'
  dat$layer[dat$layer == val_dummyOther] <- 'Other'
  gc()
  
  # change coordinate var names for use with ggsn package
  colnames(dat) <- c('long', 'lat', 'layer')
  
  # plot
  p <-
    ggplot(data = dat, aes(x = long, y = lat, fill = layer)) +
    geom_raster() +
    coord_equal() +
    scale_fill_manual(values = c('Nonnative forest' = "#7A0403FF",
                                 'Nonnative grass' = "#FABA39FF",
                                 'Burned forest' = "#72FE5EFF",
                                 'Native forest' = "#4662D7FF",
                                 'Other' = 'lightgray')) +
    ggsn::north(data = dat, location = 'topright', symbol = 9) +
    ggsn::scalebar(data = dat,
                   location = "bottomleft", transform = FALSE,
                   dist = 100, dist_unit = 'km', st.dist = 0.04) +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), panel.background = element_blank(),
          text = element_text(size = 15), legend.position = 'none')
  
  ggsave(
    filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/',
                      '12-05-01 ', names(list_landcoverRasters)[[i]], '.png'),
    dpi = 300, plot = p)
  
  list_plot[[i]] <- p
  rm(dat, p); gc()
}




##### plot panel #####

# upscale rasters
list_landcoverRasters_upscaled <-
  lapply(list_landcoverRasters, aggregate, fact = 2, fun = 'modal', expand = FALSE, cores = 7)
rm(list_landcoverRasters)
gc()

# convert rasters to data.frames and remove ocean pixels
list_df <- lapply(list_landcoverRasters_upscaled, as.data.frame, xy = TRUE)
list_df <- lapply(list_df, function(df){
  x <- df[df$layer != val_dummyOcean,]
  x})
gc()
for(i in 1:length(list_df)){
  list_df[[i]]$model <- strsplit(names(list_df)[[i]], '_')[[1]][[2]]
}
rm(list_landcoverRasters_upscaled, i)
gc()

# melt data
dat_plot <- do.call(rbind, list_df)
rm(list_df)
gc()
saveRDS(dat_plot,
        file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/13 summary of landcover and water yield changes/13c - land cover maps panel plot data.rds')

# factorize model variable and layer (land cover) variable
dat_plot <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/13 summary of landcover and water yield changes/13c - land cover maps panel plot data.rds')
dat_plot$model_lab <-
  factor(dat_plot$model,
         levels = c('baseline', 'worst2070', 'worst2100', 'middle2070', 'middle2100', 'best2070', 'best2100'),
         labels = c('(a) Baseline', '(b) No protection - 2070', '(c) No protection - 2100', '(d) Targeted protection - 2070', '(e) Targeted protection - 2100', '(f) Targeted prot. and rest. - 2070', '(g) Targeted prot. and rest. - 2100'))
dat_plot$layer_factor <-
  factor(dat_plot$layer,
         levels = c(0, -5, -2, 1, -1),
         labels = c('Nonnative forest', 'Nonnative grass', 'Burned forest', 'Native forest', 'Other'))
gc()

# adjust column names for ggsn
colnames(dat_plot)[colnames(dat_plot) == 'x'] <- 'long'
colnames(dat_plot)[colnames(dat_plot) == 'y'] <- 'lat'

# plot baseline
dat_plot2 <- dat_plot[dat_plot$model_lab == '(a) Baseline',]
p <- ggplot(data = dat_plot2,
                 aes(x = long, y = lat, fill = layer_factor)) +
  geom_raster(show.legend=TRUE) +
  coord_equal() +
  facet_wrap(vars(model_lab), nrow = 3) +
  scale_fill_manual(values = c('Nonnative forest' = "#7A0403FF",
                               'Nonnative grass' = "#FABA39FF",
                               'Burned forest' = "#72FE5EFF",
                               'Native forest' = "#4662D7FF",
                               'Other' = 'lightgray'),
                    drop = FALSE) +
  scalebar(data = dat_plot, dist_unit = 'km', dist = 100, transform = FALSE,
           location = 'bottomleft', st.dist = 0.05, st.size = 5) +
  north(data = dat_plot, location = 'topright', symbol = 9) +
  labs(fill = NULL) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), text = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13c - change in land cover baseline.png'),
       dpi = 300, height = 7, width = 7)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13c - change in land cover baseline.pdf'),
       dpi = 300, height = 7, width = 7)

# plot panel
dat_plot2 <- dat_plot[dat_plot$model_lab != '(a) Baseline',]
dat_plot2$model_lab <-
  factor(as.character(dat_plot2$model_lab),
         levels = c('(b) No protection - 2070',            '(c) No protection - 2100',
                    '(d) Targeted protection - 2070',      '(e) Targeted protection - 2100',
                    '(f) Targeted prot. and rest. - 2070', '(g) Targeted prot. and rest. - 2100'))
p <- ggplot(data = dat_plot2,
                 aes(x = long, y = lat, fill = layer_factor)) +
  geom_raster() +
  coord_equal() +
  facet_wrap(vars(model_lab), nrow = 3) +
  scale_fill_manual(values = c('Nonnative forest' = "#7A0403FF",
                               'Nonnative grass' = "#FABA39FF",
                               'Burned forest' = "#72FE5EFF",
                               'Native forest' = "#4662D7FF",
                               'Other' = 'lightgray')) +
  scalebar(data = dat_plot, dist_unit = 'km', dist = 100, transform = FALSE,
           facet.var = 'model_lab', facet.lev = '(f) Targeted prot. and rest. - 2070',
           location = 'bottomleft', st.dist = 0.05, st.size = 5) +
  north(data = dat_plot, location = 'topright', symbol = 9) +
  labs(fill = NULL) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), text = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13c - change in land cover panel.png'),
       dpi = 300, height = 9, width = 9)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13c - change in land cover panel.pdf'),
       dpi = 300, height = 9, width = 9)

rm(p, list_plot, dat_plot2)
gc()




# ##### create bar chart - compare middle and best cases to worst case #####
# 
# # tally number of pixels in each category
# dat_count <- dat_plot %>% count(model, layer_factor)
# 
# # pixels were aggregated by a factor of 2, so multiply by 2 to get the original number of pixels
# dat_count$n <- dat_count$n * 2
# 
# # save amount of burned forest in worst case for use in difference figure below
# vec_burnedForestWorstCase <- dat_count$n[dat_count$model %in% c('worst2070', 'worst2100') & dat_count$layer_factor == 'Burned forest']
# 
# # realign data so we can subtract worst case pixel count from the scenario pixel count
# dat_count_worst <- dat_count[dat_count$model %in% c('worst2070', 'worst2100'),]
# colnames(dat_count_worst) <- c('model', 'layer_factor', 'n_worstCase')
# dat_count_worst$year <-
#   substr(dat_count_worst$model,
#          nchar(dat_count_worst$model)-3, nchar(dat_count_worst$model))
# dat_count_worst$model <- 'worst'
# dat_count <- dat_count[!(dat_count$model %in% c('baseline', 'worst2070', 'worst2100')),]
# dat_count$year <-
#   substr(dat_count$model,
#          nchar(dat_count$model)-3, nchar(dat_count$model))
# dat_count$model <- substr(dat_count$model, 1, nchar(dat_count$model)-4)
# dat_count <- left_join(dat_count, dat_count_worst, c('layer_factor', 'year'))
# rm(dat_count_worst)
# 
# # manually add best case burned forest since there is no burned forest in best case
# dat_count <-
#   rbind(dat_count,
#         data.frame(model.x = 'best',
#                    layer_factor = factor('Burned forest', levels = levels(dat_count$layer_factor)),
#                    n = NA,
#                    year = c('2070', '2100'),
#                    model.y = 'worst',
#                    n_worstCase = NA
#                    )
#         )
# 
# 
# # get change in pixels and convert to hectares
# dat_count$n_diff <- dat_count$n - dat_count$n_worstCase
# dat_count$n_diff[dat_count$model.x == 'best' & dat_count$layer_factor == 'Burned forest'] <- vec_burnedForestWorstCase * -1  # add in forest prevented from burning in best case
# dat_count$n_diff_hectares <- dat_count$n_diff * 0.09
# rm(vec_burnedForestWorstCase)
# 
# # format label variables
# dat_count$Scenario <-
#   ifelse(dat_count$model.x == 'worst', 'No protection',
#          ifelse(dat_count$model.x == 'middle', 'Targeted protection',
#                 'Targeted protection\nand restoration'))
# dat_count$layer_factor2 <-
#   ifelse(dat_count$layer_factor == 'Nonnative forest', 'Nonnative\nforest spread',
#   ifelse(dat_count$layer_factor == 'Native forest', 'Restored native forest',
#   ifelse(dat_count$layer_factor == 'Burned forest', 'Fire-driven forest loss',
#   ifelse(dat_count$layer_factor == 'Nonnative grass', 'Non-fire-driven\nforest loss',
#          'Other'))))
# dat_count$layer_factor2 <-
#   factor(dat_count$layer_factor2,
#          levels = c('Nonnative\nforest spread', 'Restored native forest', 'Fire-driven forest loss',
#                     'Non-fire-driven\nforest loss', 'Other'))
# 
# # manually edit restored native forest: middle case shouldn't have any (fire-driven forest loss reduces native forest, but doesn't "take away" from restoration)
# dat_count$n_diff_hectares[dat_count$layer_factor2 == 'Restored native forest' & dat_count$model.x == 'middle'] <- 0
# 
# # plot
# ggplot(dat_count[dat_count$layer_factor2 != 'Other',], aes(fill = year)) + 
#   geom_bar(aes(x = Scenario, y = n_diff_hectares/1e3),
#            position = "dodge", stat = "identity", alpha = 0.7) +
#   geom_text(aes(x = Scenario, y = n_diff_hectares/1e3,
#                 label = ifelse(n_diff_hectares == 0, '', sprintf("%0.2f", round(n_diff_hectares/1e3, digits = 1))),
#                 vjust = 0.7 - sign(n_diff_hectares/1e3)/2),
#             position = position_dodge(width = 0.9), size = 6) +
#   geom_hline(yintercept = 0, linewidth = 1) +
#   facet_grid(rows = vars(layer_factor2), scales = 'free_y') +
#   scale_fill_viridis_d() +
#   labs(y = 'Land converted compared to no protection\n(1000s hectares)', fill = 'Year') +
#   theme(text = element_text(size = 26)) +
#   ggh4x::facetted_pos_scales(y = list(
#     layer_factor2 == 'Nonnative\nforest spread' ~
#       scale_y_continuous(limits = c(-35, 0),
#                          breaks = seq(-35, 0, 10)),
#     layer_factor2 == 'Restored native forest' ~
#       scale_y_continuous(limits = c(-20, 45),
#                          breaks = seq(-20, 45, 10)),
#     layer_factor2 == 'Fire-driven forest loss' ~
#       scale_y_continuous(limits = c(-6, 0),
#                          breaks = seq(-6, 0, 2)),
#     layer_factor2 == 'Non-fire-driven\nforest loss' ~
#       scale_y_continuous(limits = c(-10, 0),
#                          breaks = seq(-10, 0, 2))))
# 
# ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
#                          '13c - number of hectares converted.png'),
#        dpi = 300, height = 13, width = 12)
# ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
#                          '13c - number of hectares converted.pdf'),
#        dpi = 300, height = 13, width = 12)
