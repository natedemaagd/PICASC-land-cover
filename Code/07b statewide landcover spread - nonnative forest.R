
# This script spreads invasive pixels according to proportion of invadable area and an
# invasibility index.
# E.g., spread rate of 5% would covert 5% of remaining susceptible land annually, choosing
# pixels (nearly) adjacent to existing invasive landcover according to invasibility probability.

library(raster); library(rasterVis); library(ggplot2); library(viridis)
library(gganimate)


# define spread rate and number of years to spread
r <- 0.02
y <- 100

# define susceptible pixels, invasive pixels
val_susceptible <- c(8, 10, 13)
val_invasive <- 32

# load landcover raster
ras <-
  raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_landcover_types/land_cv_tp.txt")

# load invasibility raster and re-project
ras_probs <-
  raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07a_mean_forest_invasibility.tif')
ras_probs[is.na(ras_probs)] <- 0
ras_probs[ras_probs == 0] <- 1e-10
ras_probs <- projectRaster(ras_probs, ras)

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
}

gc()




##### plot #####

# convert data to data.frames and add year variables
list_df <- lapply(list_ras, function(r){
  as.data.frame(r, xy = TRUE)
})
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
ggplot(df, aes(x = x, y = y, fill = land_cv_tp_simple)) +
  geom_raster() +
  scale_fill_manual(values = viridis(3)[c(2,3,1)]) +
  coord_equal() +
  # Here comes the gganimate specific bits
  labs(title = 'Year {frame_time}',
       fill = NA) +
  transition_time(year) +
  ease_aes('linear') +
  theme(panel.background = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(),
        axis.title = element_blank(),
        text = element_text(size = 15))
anim_save("C:/Users/nated/OneDrive/Desktop/test.gif")
