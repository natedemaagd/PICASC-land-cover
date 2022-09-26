
# This script looks at the conversion of native forest in Hakalau Forest Reserve
# for each year (script 01 looked only at difference b/w 1999 and 2016)

setwd('H:/My Drive/Projects/PICASC Land-to-sea/Data/')

library(raster); library(sf); library(ggplot2); library(viridis)
library(doParallel); library(viridis); library(dplyr); library(magick)
library(tidyverse)

registerDoParallel(cores = 8)
rasterOptions(maxmemory = 1e+09)

# load data
sf_hfr <- read_sf("Raw/Water yield/fenced regions/Hakalau GIS data/HFR complete.kml")
sf_hfr <- st_zm(sf_hfr)  # coerce to two-dimensional polygons
# r_woodList <- list.files('Processed/Fire/interpolated_yearly_landcover_percentages',
#                          pattern = 'wood', full.names = TRUE)
# r_woodList <- lapply(r_woodList, raster)
# r_woodList <- lapply(r_woodList, function(r) r * 100)  # convert from proportion to percent
# 
# # mask and trim rasters to Hakalau
# r_woodList_hfr <- foreach(i = 1:length(r_woodList),
#                           .packages = c('raster', 'sf')) %dopar% {
#   r <- r_woodList[[i]]
#   ras <- mask(x = r, mask = sf_hfr)
#   ras <- trim(ras)
#   ras
# }
# saveRDS(r_woodList_hfr, 'Intermediate/Water yield/Misc/Reforestation rate - Hakalau/02 r_woodlist_hfr.rds')
# rm(r_woodList)

r_woodList_hfr <-
  readRDS('Intermediate/Water yield/Misc/Reforestation rate - Hakalau/02 r_woodlist_hfr.rds')




##### how many pixels crossed threshold into native forest? #####

# set threshold
threshold <- 40

# convert rasters to data.frames
df_woodList_hfr <-
  lapply(r_woodList_hfr, function(r){
    
    # convert raster to data.frame and rename columns, keep pixels with data
    df <- as.data.frame(r, xy = TRUE)
    colnames(df) <- c('x', 'y', 'pctWoody')
    df <- df[!is.na(df$pctWoody),]
    
    # add dummy column for pct woody above threshold
    df$isWoody <-
      ifelse(df$pctWoody >= threshold & !is.na(df$pctWoody), 1, 0)
    
    df
  })
gc()

# sum number of forest pixels each year
vec_isForest_yearlyPixels <-
  sapply(df_woodList_hfr, function(df){
    sum(df$isWoody, na.rm = TRUE)
  })

# convert number of pixels to number of hectares
vec_isForest_yearlyHectares <- vec_isForest_yearlyPixels * 0.09
rm(vec_isForest_yearlyPixels)

# create data.frame
df_woodyTimeSeries <- data.frame(year = 1999:2016,
                                 woodyHectares = vec_isForest_yearlyHectares)

# calculate year-over-year change in woody hectarage
df_woodyTimeSeries$woodyHectares_YOYpctChange <- 0
for(y in 2:nrow(df_woodyTimeSeries)){
  df_woodyTimeSeries$woodyHectares_YOYpctChange[[y]] <-
    with(df_woodyTimeSeries,
         (woodyHectares[[y]] - woodyHectares[[y-1]]) /
           woodyHectares[[y-1]] * 100)
}




##### plots - time series of woody hectares #####

ggplot(data = df_woodyTimeSeries,
       aes(x = year, y = woodyHectares)) +
  geom_line(size = 1.5, color = viridis(2)[[1]]) +
  scale_y_continuous(sec.axis =
                       sec_axis(~ . /
                                  (min(vec_isForest_yearlyHectares)) * 100,
                                name = '% of 1999 hectarage')) +
  labs(x = NULL, y = 'Hectares') +
  theme(text = element_text(size = 15))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/Hakalau landcover change/',
                         '02 time series of aggregate woody hectares in HFR.png'),
       height = 6, width = 8, dpi = 300)




##### plots - year-over-year percent change in woody hectares #####

ggplot(data = df_woodyTimeSeries,
       aes(x = year, y = woodyHectares_YOYpctChange)) +
  geom_line(size = 1.5, color = viridis(2)[[1]]) +
  labs(x = NULL, y = 'YOY % change in woody hectarage') +
  theme(text = element_text(size = 15))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/Hakalau landcover change/',
                         '02 YOY pct change in woody hectares in HFR.png'),
       height = 6, width = 8, dpi = 300)




##### plots - timelapse map #####

# plot each year individually
foreach(i = 1:length(df_woodList_hfr),
        .packages = c('ggplot2', 'viridis')) %dopar% {
  
  # get data for year i
  dat <- df_woodList_hfr[[i]]
  
  # create better dummy variable for plotting
  dat$isWoody_plot <- ifelse(dat$isWoody == 1, 'Woody', 'Non-woody')
  dat$isWoody_plot <- factor(dat$isWoody_plot,
                             levels = c('Woody', 'Non-woody'))
  
  # plot map of year i
  p <- ggplot(data = dat,
                          aes(x = x, y = y, fill = isWoody_plot)) +
    geom_raster() +
    scale_fill_viridis_d(name = NULL) +
    coord_equal() +
    labs(title = 1998+i) +
    theme(panel.background = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank(),
          text = element_text(size = 15))
  
  # save plot
  ggsave(plot = p,
         filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/Hakalau landcover change/02 yearly HFR isWoody dummy maps/',
                           1998+i, '.png'),
         dpi = 300, height = 8, width = 7)
  
  gc()
}

# list PNGs
list_pngs <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/Hakalau landcover change/02 yearly HFR isWoody dummy maps/',
                        pattern = '.png',
                        full.names = TRUE)

# create gif
list_pngs %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps = 4) %>% # animates
  image_write("H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/Hakalau landcover change/02 yearly HFR isWoody dummy maps/HFR woody conversion.gif")




##### new strategy - how long did it take for pixels to convert from non-forest to forest? #####

# for pixels that are forest, find adjacent pixels that are non-forest
list_yearsToConvert <- list()
for(Y in 1:length(r_woodList_hfr)){
  
  # define raster for base year Y
  ras <- r_woodList_hfr[[Y]]
  
  # get vector indices of forest pixels
  vec_forestPixels <- which(values(ras) >= threshold)
  
  # get pixels adjacent to year-y forest pixels
  dat_adjacentPixels <- as.data.frame(adjacent(ras, vec_forestPixels,
                                               directions = 8))
  
  # define whether adjacent pixels in year Y and all subsequent years are forest
  dat_adjacentPixels[paste0('year', Y:length(r_woodList_hfr))] <- NA
  for(y in 1:(ncol(dat_adjacentPixels)-2)){
    
    # get values of adjacent pixels
    dat_adjacentPixels[y+2] <- 
      values(r_woodList_hfr[[y]])[dat_adjacentPixels$to]
    
    # convert value to isForest dummy
    dat_adjacentPixels[y+2][dat_adjacentPixels[y+2] >= threshold] <- 9999  # temporary isForest == 1 value
    dat_adjacentPixels[y+2][dat_adjacentPixels[y+2] < threshold]  <- 0
    dat_adjacentPixels[y+2][dat_adjacentPixels[y+2] == 9999] <- 1
    
  }
  
  # keep only adjacent pixels that were NOT forest in year Y
  dat_adjacentPixels <-
    dat_adjacentPixels[dat_adjacentPixels[,3] == 0 &
                         !is.na(dat_adjacentPixels[,3]),]
  
  # gather yearly history for each of these adjacent non-forest pixels
  list_adjacentPixelForestHistory <-
    apply(dat_adjacentPixels, MARGIN = 1, function(r){
      vec_isForest <- which(r == 1)  # index of isForest == 1 in row r
    })
  
  # return number of years it took for each adjacent non-forest pixel to convert to forest
  list_yearsToConvert[[Y]] <-
    sapply(list_adjacentPixelForestHistory, function(r){
      min(r - 3)  # subtract 3 to convert from index to number of years
                  # (column 1 and 2 of data.frame are `to` and `from` columns)
  })
  
  rm(dat_adjacentPixels, list_adjacentPixelForestHistory, ras,
     vec_forestPixels, y, Y)
  
  gc()
  
}

# warnings are for adjacent pixels that never converted - change Inf to NA
for(i in 1:length(list_yearsToConvert)){
  vec <- list_yearsToConvert[[i]]
  vec[is.infinite(vec)] <- NA
  list_yearsToConvert[[i]] <- vec
  rm(vec)
}

# combine list of years to convert into single vector
vec_yearsToConvert <- unlist(list_yearsToConvert)

# analyze
summary(vec_yearsToConvert)
dat_yearsToConvert <- as.data.frame(vec_yearsToConvert)
ggplot(data = dat_yearsToConvert) +
  geom_histogram(aes(vec_yearsToConvert),
                 bins = 17,
                 alpha = 0.5,
                 color = 'black') +
  labs(x = 'Number of years to convert',
       y = 'Number of pixels') +
  theme(text = element_text(size = 15))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/Hakalau landcover change/',
                         '02 number of years for adjacent non-forest pixel to convert to forest.png'),
       dpi = 300, height = 6, width = 10)
