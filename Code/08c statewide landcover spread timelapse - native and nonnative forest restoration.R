
# this script uses the landcover rasters created in 7a to create a timelapse of landcover spread

library(doParallel)
library(raster)
library(ggplot2)
library(viridis)
library(tidyverse)
library(magick)
library(sf)
library(ggnewscale)
library(dplyr)

registerDoParallel(cores = 10)




##### load rasters and format #####

# list rasters
list_rasters <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/07 statewide landcover spread/rasters/Native and non-native forest restoration - 7.5 pct spread rate - landcover rasters',
                           pattern = '.tif', full.names = TRUE)

# re-order rasters according to year
vec_names <- sapply(strsplit(list_rasters, ' '), function(l) l[[22]])
vec_names <- as.numeric(substr(vec_names, 1, nchar(vec_names) - 4))
list_rasters <- list_rasters[order(vec_names)]

# load rasters
list_rasters <- lapply(list_rasters, raster)

rm(vec_names)
gc()

# convert rasters to data.frames
list_dfs <- lapply(list_rasters, function(r){
  as.data.frame(r, xy = TRUE)
})
list_dfs <- lapply(list_dfs, function(df){
  colnames(df) <- c('x', 'y', 'lc')
  df <- df[!is.na(df$lc),]
  df
})
gc()

# create landcover plot variable
dat_lc <- readxl::read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_landcover_types/lc codes and names.xlsx")
# colnames(dat_lc) <- c('lc', 'name', 'Land cover')
# dat_lc2 <- data.frame(`Land cover` = unique(dat_lc$`Land cover`))
# colnames(dat_lc2) <- 'Land cover'
# dat_lc2$Category <- c('Developed', 'Barren', 'Agriculture', 'Other',
#                       'Native forest', 'Native shrubland', 'Native forest',
#                       'Native forest', 'Native shrubland', 'Native shrubland',
#                       'Native grassland', 'Native grassland',
#                       'Introduced shrubland', 'Introduced forest',
#                       'Introduced forest', 'Introduced shrubland',
#                       'Introduced grassland', 'Introduced forest')
# dat_lc <- left_join(dat_lc, dat_lc2, 'Land cover')
# dat_lc$name <- dat_lc$`Land cover` <- NULL 
# list_dfs <- lapply(list_dfs, function(df){
#   df <- left_join(df, dat_lc, 'lc')
#   df
# })
# list_dfs <- lapply(list_dfs, function(df){
#   df[!is.na(df$Category),]
# })
# rm(dat_lc2)

list_dfs <- lapply(list_dfs,   # basic 'native', 'non-native', 'other' categorization
                   function(df){
                     df$`Land cover` <- ifelse(df$lc %in% (c(1:2, 30:36) + 1000) &
                                                 !is.na(df$lc),
                                               'Grass and shrubland\nin protected area',
                                        ifelse(df$lc %in% c(2000) &
                                                 !is.na(df$lc),
                                               'Forest',
                                               'Other')
                                        )
                     df
                   })
list_dfs <- lapply(list_dfs,
                   function(df){
                     df$`Land cover` <- factor(df$`Land cover`,
                                               levels = c('Grass and shrubland\nin protected area',
                                                          'Forest',
                                                          'Other'))
                     df
                   })




##### plot as PNGs #####

# load protected areas shapefile
shp_protectedAreas <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Priority areas/Shapefiles/UngulateUnit/AllUngulateUnit_Sept2019.shp")
shp_protectedAreas_lonlat <- st_transform(shp_protectedAreas,
                                          crs = st_crs('+proj=longlat +datum=WGS84 +no_defs'))

# create year variable with leading 0s for single-digit numbers
vec_years <- sprintf("%02d", seq(1:50))

# create plot for each year i
foreach(i = 1:length(list_dfs),
        .packages = c('ggplot2', 'ggnewscale')) %dopar% {
  
  # create plot
  p <- ggplot() +
    
    # plot raster
    geom_raster(data = list_dfs[[i]],
                aes(x = x, y = y, fill = `Land cover`)) +
    scale_fill_viridis_d(direction = -1) +
    
    new_scale_fill() +
    new_scale_color() +
    
    # plot polygons
    geom_sf(data = shp_protectedAreas_lonlat,
            aes(color = as.character(FID_)),
            fill = 'transparent',
            size = 0.5) +
    scale_color_manual(values = 'red') +
    
    coord_sf() +
    labs(title = paste('Year', vec_years[[i]])) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.background = element_blank(),
          text = element_text(size = 25)) +
    guides(color = 'none')
  
  # save plot
  ggsave(plot = p,
         filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/08 statewide landcover spread - reforestation/landcover spread timelapse individual yearly plots - all forest/',
                           'land cover year ', vec_years[[i]], '.png'),
         dpi = 300, height = 8, width = 13.33)
  
  gc()
  
  #p
  
}

gc()




##### create timelapse of PNGs #####

# list PNGs
list_pngs <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/08 statewide landcover spread - reforestation/landcover spread timelapse individual yearly plots - all forest',
                        pattern = '.png',
                        full.names = TRUE)
list_pngs %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps = 2) %>% # animates
  image_write("H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/08 all forest reforestation timelapse 7pct.gif")
