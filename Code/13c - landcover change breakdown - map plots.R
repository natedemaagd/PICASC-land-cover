
# This script creates figures for publication: plots of landcover maps

library(raster)
library(ggplot2)
library(ggsn)
library(viridis)
library(cowplot)
library(tidyverse)




##### define landcover groups for plots #####

# alien forest
vec_forestAlien <-
  c(1600, 1700, 1900, 2000, 11900, 12000)

# native forest
vec_forestNative <-
  c(100, 200, 300, 400, 500, 10100, 10200, 10300, 10400, 10500, 10600,
    600, 700, 800, 1500, 10700, 10800, 10900, 11700)

# alien grass
vec_alienGrass <- 
  c(3700, 3800, 3900)

# dummy variables
val_dummyForestAlien <- 0
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
      raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/","mhi_s0_baseline_names.tif")),
    
    # best case landcover
    ras_best2070_lc =
      raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/30m rasters/unmasked/",
                    "year070.tif")),
    ras_best2100_lc =
      raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/30m rasters/unmasked/",
                    "year100.tif")),
    
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
  ras_temp[ras_temp %in% vec_forestAlien] <- val_dummyForestAlien; gc()
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
  ras_temp[!(ras_temp %in% c(val_dummyForestAlien, val_dummyForestBurned,
                             val_dummyForestNative, val_dummyOcean,
                             val_dummyGrass))] <- val_dummyOther
  list_landcoverRasters[[i]] <- ras_temp
  rm(ras_temp); gc(); print(i)
}

rm(i)




##### plot landcover raster LEGEND #####

# plot and save LEGEND ONLY of the landcover map

# convert raster to data.frame
dat <- as.data.frame(list_landcoverRasters[[7]], xy = TRUE)
gc()

# remove ocean pixels
dat <- dat[dat$layer != val_dummyOcean,]
gc()

# convert values to labels
dat$layer[dat$layer == val_dummyForestAlien]  <- 'Alien forest'
dat$layer[dat$layer == val_dummyForestBurned] <- 'Burned forest'
dat$layer[dat$layer == val_dummyForestNative] <- 'Native forest'
dat$layer[dat$layer == val_dummyGrass] <- 'Alien grass'
dat$layer[dat$layer == val_dummyOther] <- 'Other'
gc()

# plot
p <-
  ggplot(data = dat, aes(x = x, y = y, fill = layer)) + 
  geom_raster() +
  coord_equal() +
  scale_fill_manual(values = c('Alien forest' = "#7A0403FF",
                               'Alien grass' = "#FABA39FF",
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
  dat$layer[dat$layer == val_dummyForestAlien]  <- 'Alien forest'
  dat$layer[dat$layer == val_dummyGrass] <- 'Alien grass'
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
    scale_fill_manual(values = c('Alien forest' = "#7A0403FF",
                                 'Alien grass' = "#FABA39FF",
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
  lapply(list_landcoverRasters, aggregate, fact = 3, fun = 'modal', expand = FALSE)
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
dat_plot$model <-
  factor(dat_plot$model,
         levels = c('baseline', 'worst2070', 'worst2100', 'middle2070', 'middle2100', 'best2070', 'best2100'),
         labels = c('Baseline', 'Scenario 1 - 2070', 'Scenario 1 - 2100', 'Scenario 2 - 2070', 'Scenario 2 - 2100', 'Scenario 3 - 2070', 'Scenario 3 - 2100'))
dat_plot$layer_factor <-
  factor(dat_plot$layer,
         levels = c(0, -5, -2, 1, -1),
         labels = c('Alien forest', 'Alien grass', 'Burned forest', 'Native forest', 'Other'))
gc()

# adjust column names for ggsn
colnames(dat_plot)[colnames(dat_plot) == 'x'] <- 'long'
colnames(dat_plot)[colnames(dat_plot) == 'y'] <- 'lat'

# plot baseline
dat_plot2 <- dat_plot[dat_plot$model == 'Baseline',]
p <- ggplot(data = dat_plot2,
                 aes(x = long, y = lat, fill = layer_factor)) +
  geom_raster(show.legend=TRUE) +
  coord_equal() +
  facet_wrap(vars(model), nrow = 3) +
  scale_fill_manual(values = c('Alien forest' = "#7A0403FF",
                               'Alien grass' = "#FABA39FF",
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

# plot panel
dat_plot2 <- dat_plot[dat_plot$model != 'Baseline',]
p <- ggplot(data = dat_plot2,
                 aes(x = long, y = lat, fill = layer_factor)) +
  geom_raster() +
  coord_equal() +
  facet_wrap(vars(model), nrow = 3) +
  scale_fill_manual(values = c('Alien forest' = "#7A0403FF",
                               'Alien grass' = "#FABA39FF",
                               'Burned forest' = "#72FE5EFF",
                               'Native forest' = "#4662D7FF",
                               'Other' = 'lightgray')) +
  scalebar(data = dat_plot, dist_unit = 'km', dist = 100, transform = FALSE,
           facet.var = 'model', facet.lev = 'Scenario 3 - 2070',
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




##### create bar chart #####

# tally number of pixels in each category
dat_count <- dat_plot %>% count(model, layer_factor)

# pixels were ggregated by a factor of 3, so multiply by 3 to get the original number of pixels
dat_count$n <- dat_count$n * 3

# realign data so we can subtract baseline pixel count from the scenario pixel count
dat_count_baseline <- dat_count[dat_count$model == 'Baseline',]
colnames(dat_count_baseline) <- c('model', 'layer_factor', 'n_baseline')
dat_count <- dat_count[dat_count$model != 'Baseline',]
dat_count <- left_join(dat_count, dat_count_baseline, 'layer_factor')
rm(dat_count_baseline)

# get change in pixels and convert to hectares
dat_count$n_diff <- dat_count$n - dat_count$n_baseline
dat_count$n_diff_hectares <- dat_count$n_diff * 0.09

# format label variables
dat_count$Scenario <- strsplit(as.character(dat_count$model.x), ' - ')
dat_count$Scenario <- sapply(dat_count$Scenario, function(x) x[[1]])
dat_count$Scenario <-
  ifelse(dat_count$Scenario == 'Scenario 1', 'No protection',
         ifelse(dat_count$Scenario == 'Scenario 2', 'Targeted protection',
                'Targeted protection and restoration'))
dat_count$Year <- strsplit(as.character(dat_count$model.x), ' - ')
dat_count$Year <- sapply(dat_count$Year, function(x) x[[2]])

# plot
ggplot(dat_count, aes(fill = Year)) + 
  geom_bar(aes(x = Scenario, y = n_diff_hectares),
           position = "dodge", stat = "identity", alpha = 0.7) +
  geom_text(aes(x = Scenario, y = value, label = ifelse(value == 0, '', sprintf("%0.2f", round(value, digits = 2)))),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 6) +
  geom_hline(yintercept = 0, linewidth = 1) +
  facet_grid(rows = vars(Category), scales = 'free_y') +
  scale_fill_viridis_d() +
  labs(y = 'Land converted (100,000 hectares)') +
  theme(text = element_text(size = 26)) +
  ggh4x::facetted_pos_scales(y = list(
    Category == 'Alien forest spread' ~
      scale_y_continuous(limits = c(0, 4.2),
                         breaks = seq(0, 4, 1)),
    Category == 'Restored native forest' ~
      scale_y_continuous(limits = c(0, 1.1),
                         breaks = seq(0, 1, 0.25)),
    Category == 'Fire-driven forest loss' ~
      scale_y_continuous(limits = c(0, 0.12),
                         breaks = seq(0, 0.12, 0.04)),
    Category == 'Forest to grass' ~
      scale_y_continuous(limits = c(0, 1.4),
                         breaks = seq(0, 1.4, 0.2))))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '13a - number of hectares converted.png'),
       dpi = 300, height = 13, width = 12)
