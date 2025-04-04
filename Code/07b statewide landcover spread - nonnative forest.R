
# This script spreads invasive pixels according to proportion of invadable area and an
# invasibility index.
# E.g., spread rate of 5% would covert 5% of remaining susceptible land annually, choosing
# pixels (nearly) adjacent to existing invasive landcover according to invasibility probability.

library(raster); library(rasterVis); library(ggplot2); library(viridis)
library(gganimate)

set.seed(42)


# define spread rate and number of years to spread
r <- 0.02
y <- 100

# define susceptible pixels, invasive pixels (see "H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Landcover_MetaCategories_GM.xlsx" for conversion)
# val_susceptible <- c(8, 10, 13)  # Tom G values
val_susceptible <- c(100, 200, 300, 400, 500,  # Tom G 10 equivalent (no code for Tom G 8 in new raster)
                     600, 700, 800)  # Tom G 13 equivalent
#val_invasive <- 32
val_invasive <- c(1600, 1700, 1900, 2000)  # Tom G 32 equivalent

# load landcover raster and convert values of invasive pixels to be one value
ras <-
  # raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
  #               "mhi_land_cover_names.tif"))
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
                "mhi_s0_baseline_names.tif"))
ras[ras %in% val_invasive] <- 0; val_invasive <- 0

# load invasibility raster and re-project
ras_probs <-
  raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07a_max_forest_invasibility.tif')
ras_probs[is.na(ras_probs)] <- 0
ras_probs[ras_probs == 0] <- 1e-10
ras_probs <- projectRaster(ras_probs, ras, method = 'ngb')
gc()

# run simulation for y years
list_ras <- list()  # initiate list of rasters - one for each year of simulation
list_ras[[1]] <- ras  # first year will be initial raster
list_pixelsConverted <- list()  # initiate list to track number of pixels converted each year
for(i in 2:y){
  
  # determine the number of pixels that need to be converted: r*100 susceptible pixels
  num_pixels_to_convert <-
    round(r * length(list_ras[[i-1]][list_ras[[i-1]] %in% val_susceptible]))
  list_pixelsConverted[[i]] <- num_pixels_to_convert  # record number of pixels converted this year
  
  # get indices of all susceptible pixels
  pixels_susceptible <- which(values(list_ras[[i-1]]) %in% val_susceptible)
  
  # get indices of all invaded pixels
  pixels_invaded <- which(values(list_ras[[i-1]]) %in% val_invasive)
  
  # find pixels_invaded that are adjacent to susceptible pixels
  pixels_adjacent <-
    adjacent(list_ras[[i-1]],
             cells = pixels_susceptible,
             target = pixels_invaded,
             directions = 8)
  pixels_adjacent <- pixels_adjacent[,1]  # keep only vector of susceptible pixels adjacent to invaded pixels
  pixels_adjacent <- pixels_adjacent[!duplicated(pixels_adjacent)]  # remove duplicates
  
  # Convert `num_pixels_to_convert` random adjacent pixels.
  # If num_pixels_to_convert > length(pixels_adjacent), run through additional loop
  ras_temp <- list_ras[[i-1]] # create new raster layer for year i
  if(num_pixels_to_convert <= length(pixels_adjacent)){
    
    # convert susceptible, adjacent pixels in `ras_temp` based on invasibility metric
    pixels_adjacent_probs <- ras_probs[pixels_adjacent]
    ras_temp[sample(pixels_adjacent,
                    size = num_pixels_to_convert,
                    prob = pixels_adjacent_probs)] <-
      val_invasive
    
    # add new raster to list
    list_ras[[i]] <- ras_temp
    
  } else {  # case if number of pixels to convert > number of adjacent pixels
    
    # create additional copy of the raster - used to add additional layer(s) of susceptible pixels
    ras_temp2 <- ras_temp
    
    # keep adding additional layers of adjacency until enough pixels can be converted
    while(num_pixels_to_convert > length(pixels_adjacent)){
      
      # convert all susceptible pixels in temp2 and find new susceptible pixels
      ras_temp2[pixels_adjacent] <- val_invasive
      pixels_susceptible2 <- which(values(ras_temp2) == val_susceptible)
      pixels_invaded2 <- which(values(ras_temp2) == val_invasive)
      
      # find all adjacent pixels in temp2 and add to vector of adjacent pixels
      pixels_adjacent2 <- adjacent(ras_temp2,
                                   cells = pixels_susceptible2,
                                   target = pixels_invaded2,
                                   directions = 8)
      pixels_adjacent2 <- pixels_adjacent2[!duplicated(pixels_adjacent2)]
      pixels_adjacent <- c(pixels_adjacent, pixels_adjacent2)
      pixels_adjacent <- pixels_adjacent[!duplicated(pixels_adjacent)]
      
    }
    
    # convert susceptible, adjacent pixels in `ras_temp` based on invasibility metric
    pixels_adjacent_probs <- ras_probs[pixels_adjacent]
    ras_temp[sample(pixels_adjacent,
                    size = num_pixels_to_convert,
                    prob = pixels_adjacent_probs)] <-
      val_invasive
    
    # add new raster to list
    list_ras[[i]] <- ras_temp
  }
  
  print(paste(i, Sys.time()))
  gc()
  
}

gc()

# save yearly rasters
yearVec <- 1:100
yearVec <- stringr::str_pad(yearVec, 3, pad = "0")
for(i in 1:length(list_ras)){
  writeRaster(list_ras[[i]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07b unprotected/',
                                'year ', yearVec[[i]], '.tif'),
              overwrite = TRUE)
}
rm(yearVec)
gc()




##### plot line charts #####

# create line graph dataset
dat_lines <-
  data.frame(year = 1:y,
             pixels_converted = c(NA, do.call(c, list_pixelsConverted)),
             pixels_cumulativeConverted = NA)
for(i in 2:nrow(dat_lines)){
  dat_lines$pixels_cumulativeConverted[[i]] <-
    sum(dat_lines$pixels_converted[1:i], na.rm = TRUE)
}

# convert 30m x 30m pixel count to hectares
dat_lines$hectares_converted <-
  0.09 * dat_lines$pixels_converted
dat_lines$hectares_cumulativeConverted <-
  0.09 * dat_lines$pixels_cumulativeConverted

saveRDS(dat_lines,
        file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/',
                      '07b - yearly forest conversion - unprotected.rds'))

# plot
ggplot(data = dat_lines,
       aes(x = year, y = hectares_cumulativeConverted/1000)) +
  geom_line(size = 2) +
  labs(x = 'Year', y = 'Hectares converted (1000s)') +
  theme(text = element_text(size = 15))




##### plot timelapse #####

# convert data to data.frames and add year variables
list_df <- lapply(list_ras, function(r){
  as.data.frame(r, xy = TRUE)
})
list_df <- list()
for(i in 1:length(list_ras)){
  df <- as.data.frame(list_ras[[i]], xy = TRUE)
  df <- df[df$layer != 65535,]; gc()
}
for(i in 1:length(list_df)){
  list_df[[i]]$year <- i
}

# combine into one data.frame
df <- do.call(rbind, list_df)

# simplify landcover categories and remove NAs
df$land_cv_tp_simple <- NA
df$land_cv_tp_simple[df$land_cv_tp %in% val_invasive] <- 'Non-native forest'
df$land_cv_tp_simple[df$land_cv_tp %in% val_susceptible] <- 'Native forest'
df$land_cv_tp_simple[!(df$land_cv_tp %in% c(val_invasive, val_susceptible)) &
                       !is.na(df$land_cv_tp)
                     ] <- 'Other'
df <- df[!is.na(df$land_cv_tp_simple),]
gc()

# plot
p <- ggplot(df, aes(x = x, y = y, fill = land_cv_tp_simple)) +
  geom_raster() +
  scale_fill_manual(values = viridis(4)[c(2,4,1)]) +
  coord_equal() +
  # Here comes the gganimate specific bits
  labs(title = 'Year {frame_time}',
       fill = NULL) +
  transition_time(year) +
  ease_aes('linear') +
  theme(panel.background = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        text = element_text(size = 15))
#print(p)
anim_save(filename = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/07 statewide landcover spread/landcover timelapse - no protection.gif",
          animation = p)
