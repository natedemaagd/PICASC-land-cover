
# this script examines a few questions that came up about woody cover

library(raster); library(ggplot2); library(sf)
library(rgdal); library(readxl); library(tidyverse)




##### distribution of woody cover by native forest type #####

# load data
r_landcover <- raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_landcover_types/land_cv_tp.txt")
r_woody     <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/HI_FracLC_wood_2016.tif")

# resample woody landcover to match landover raster
r_woody <- resample(r_woody, r_landcover)

# convert to data.frame
dat <- data.frame(lc = values(r_landcover),
                  pct_woody = values(r_woody))
dat <- dat[dat$lc %in% 8:15,]
dat$lc <- as.character(dat$lc)
dat2 <- data.frame(Landcover = c('Montane rainforest', 'Wet cliff and ridge crest shrubland', 'Lowland dry forest',
                                  'Lowland mesic forest', 'Montane-subalpine dry forest and woodland',
                                  'Montane-subalpine mesic forest', 'Lowland rainforest', 'Montane cloud forest'),
                   lc = as.character(c(10,11,12,13,14,15,8,9)))
dat <- dplyr::left_join(dat, dat2, 'lc')

pctWoodyByLC <- aggregate(dat$pct_woody, list(dat$lc), summary)

pctWoodyByLC2 <- aggregate(dat$pct_woody, list(dat$lc), function(x){
  quantile(x, probs = c(0.01, 0.05, 0.10, 0.15, 0.20, 0.25),
           na.rm = TRUE)
})

# plot distributions
ggplot(data = dat) +
  geom_histogram(aes(pct_woody, fill = Landcover, group = Landcover),
                 color = 'black', bins = 50) +
  labs(x = 'Percent woody cover', y = 'Pixel count') +
  theme(text = element_text(size = 20))

rm(list=ls())
gc()




##### change in woody cover as function of mean annual rainfall #####

# load woody cover and mean annual rainfall
r_woody2016 <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/HI_FracLC_wood_2016.tif")
r_woody1999 <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/HI_FracLC_wood_1999.tif")
r_rainfall <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")

# resample rainfall
# beginCluster(8)
# r_rainfall <- resample(r_rainfall, r_woody2016)
# endCluster()
# writeRaster(r_rainfall, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/06 annual rainfall resmapled to match landcover pct rasters.tif',
#             overwrite = TRUE)
r_rainfall <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/06 annual rainfall resmapled to match landcover pct rasters.tif')
gc()

# get change in woody cover

r_woodyPctChange <- (r_woody2016 - r_woody1999) / r_woody1999
gc()

# plotting all data didn't give anything useful, so limit to puuwaawaa
shp_pww <- readOGR("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/fenced regions/puu waawaa", "units")
shp_pww <- spTransform(shp_pww, crs(r_rainfall))
r_woodyPctChange_pww <- mask(r_woodyPctChange, shp_pww); gc()
r_rainfall_pww <- mask(r_rainfall, shp_pww); gc()

# for comparison, get set of non-puuwaawaa pixels
r_woodyPctChange_nonPww <- mask(r_woodyPctChange, shp_pww, inverse = TRUE); gc()
r_rainfall_nonPww <- mask(r_rainfall, shp_pww, inverse = TRUE); gc()

# convert to data.frame
dat <- data.frame(woodyCovChange_pct = c(values(r_woodyPctChange_pww), values(r_woodyPctChange_nonPww)),
                  rainAnn_mm = c(values(r_rainfall_pww), values(r_rainfall_nonPww)),
                  location = c(rep('Protected', times = length(values(r_woodyPctChange_pww))),
                               rep('Unprotected', times = length(values(r_woodyPctChange_nonPww)))))
gc()
dat <- dat[complete.cases(dat),]
gc()

# if delta is more than 100%, limit it to 100%
dat$woodyCovChange_pct[dat$woodyCovChange_pct >  1] <- 1
dat$woodyCovChange_pct[dat$woodyCovChange_pct < -1] <- -1

# plot diff b/w woody growth in puuwaawaa and entire state
ggplot(data = dat, aes(x = woodyCovChange_pct*100, fill = location, color = location)) +
  geom_density(alpha = 0.5) +
  labs(x = '% change in woody cover, 1999 - 2016', y = 'Pixel density',
       color = NULL, fill = NULL) +
  geom_vline(xintercept = 0, linetype = 'longdash', size = 1) +
  annotate("text", x = -90, y = 0.03,
           label = paste0('Mean unprotected = ',
                          round(mean(dat$woodyCovChange_pct[dat$location == 'Unprotected']*100)),
                          '%\n',
                          'Mean protected = ',
                          round(mean(dat$woodyCovChange_pct[dat$location == 'Protected']*100)), '%'),
           hjust = 0) +
  theme(text = element_text(size = 20))

# plot change in woody cover as function of rainfall
ggplot(data = dat[dat$location == 'Unprotected',], aes(x = rainAnn_mm, y = woodyCovChange_pct*100)) +
  geom_point() +
  scale_fill_viridis_c() +
  labs(x = 'Mean annual rainfall (mm)',
       y = 'Change in woody cover (%)') +
  theme(text = element_text(size = 20))

rm(list=ls())
gc()




##### summary of LC w/in fenced regions #####

# load landcover raster and fence shapefile
r_landcover <- raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_landcover_types/land_cv_tp.txt")
shp_fence <- readOGR(dsn = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/fenced regions",
                     layer = "AllUngulateUnit_Sept2019")

# separate landcover raster by island
island_extents <- list(
  extent(-156.2, -154.8, 18.8, 20.28),
  extent(-158.4, -157.5,  21.2, 21.8),
  extent(-159.9, -159.2 , 21.8, 22.3),
  extent(-157.5, -155.9, 20.45, 21.3)
)
names(island_extents) <- c('Hawai\'i', 'O\'ahu', 'Kaua\'i', 'Maui County')
r_landcover_byIsland <- list()
for(i in 1:length(island_extents)){
  r_landcover_byIsland[[i]] <-
    crop(r_landcover, island_extents[[i]])
}

# reproject landcover rasters to match fence polygons
r_landcover_byIsland <- lapply(r_landcover_byIsland,  function(r){
  projectRaster(r, crs = crs(shp_fence), method = 'ngb')
})

# mask raster with polygons to keep only pixels within fenced regions
r_landcoverFenced_byIsland <- lapply(r_landcover_byIsland, function(r){
  mask(r, shp_fence)
})

# convert rasters to data.frames
dat_byIsland <- lapply(r_landcoverFenced_byIsland, function(r){
  as.data.frame(r, xy = TRUE)
})
dat_byIsland <- lapply(dat_byIsland,function(df){
  df[!is.na(df$land_cv_tp),]
})

# add island name variable and remove x,y coords
for(i in 1:length(dat_byIsland)){
  dat_byIsland[[i]]$island <- names(island_extents)[[i]]
  dat_byIsland[[i]]$x <- dat_byIsland[[i]]$y <- NULL
}
rm(r_landcover,shp_fence, r_landcover_byIsland,
   r_landcoverFenced_byIsland, i, island_extents); gc()

# merge into one data.frame
dat <- do.call(rbind, dat_byIsland)
rm(dat_byIsland); gc()
colnames(dat) <- c('lc', 'Island')

# merge landcover names
dat_LCnames <- readxl::read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_landcover_types/lc codes and names.xlsx")
colnames(dat_LCnames) <- c('lc', 'Land cover')
dat <- left_join(dat, dat_LCnames, 'lc')
rm(dat_LCnames)

# plot - combined
ggplot(data = dat,
       aes(x = `Land cover`, fill = Island)) +
  geom_bar(color = 'black', alpha = 0.5) +
  theme(axis.text.x = element_text(angle = -45, hjust=0),
        text = element_text(size = 9),
        plot.margin = margin(0.5, 2.5, 0.5, 0.5, 'cm')) +
  labs(x = NULL, y = 'Pixel count')
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '06 distribution of landcovers within fenced regions.png'),
       height = 4, width = 7, dpi = 300)

# plot - separate by island
ggplot(data = dat,
       aes(x = `Land cover`)) +
  geom_bar(color = 'black', alpha = 0.5) +
  facet_wrap(vars(Island), nrow = 2, ncol = 2, scales = 'free') +
  theme(axis.text.x = element_text(angle = -45, hjust=0),
        text = element_text(size = 9),
        plot.margin = margin(0.5, 2.5, 0.5, 0.5, 'cm')) +
  labs(x = NULL, y = 'Pixel count')
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '06 distribution of landcovers within fenced regions by island.png'),
       height = 8, width = 12, dpi = 300)

