
# This script spreads grass into native and alien forest

library(raster); library(rasterVis); library(ggplot2); library(viridis)
library(gganimate)

set.seed(42)


# define number of years to spread
y <- 100

# define susceptible pixels, invasive pixels (see "H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Landcover_MetaCategories_GM.xlsx" for conversion)
val_susceptible <- c(1100, 1200, 1300, 1400, 1500, 1800, 2100, 2200,  # dry forest (alien and native)
                     600, 800, 900, 1000, 1700, 2000,  # mesic forest (alien and native)
                     0) # 0 is dummy variable used for forest in spread

val_invasive <- c(3700, 3800, 3900)  # grasses

# load landcover raster and convert values of invasive pixels to be one value - INITIAL RASTER HERE WILL BE THE POST-FIRE RASTER FROM 09c
ras <-
  # raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
  #               "mhi_land_cover_names.tif"))
  raster(paste0(
    'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
    'worst case 2100.tif'
  ))
ras[ras %in% val_invasive] <- -5; val_invasive <- -5



# load invasibility raster and re-project
ras_probs <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07a_max_grass_invasibility.tif'))
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
  num_pixels_to_convert <- 9104  # from 09d. Pixels crossed the 40% threshold for loss of forest and gain of herbaceous cover
    #round(r * length(list_ras[[i-1]][list_ras[[i-1]] %in% val_susceptible]))  # IN THIS SIMULATION, IT'S A FIXED RATE PER YEAR
  list_pixelsConverted[[i]] <- num_pixels_to_convert  # record number of pixels converted this year
  
  # get indices of all susceptible pixels
  pixels_susceptible <- which(values(list_ras[[i-1]]) %in% val_susceptible)
  
  # get indices of all invaded pixels
  pixels_invaded <- which(values(list_ras[[i-1]]) %in% val_invasive)
  
  # # find pixels_invaded that are adjacent to susceptible pixels - IN THIS SIMULATION, ADJACENCY DOESNT MATTER
  pixels_adjacent <-
    adjacent(list_ras[[i-1]],
             cells = pixels_susceptible,
             target = pixels_invaded,
             directions = 8)
  pixels_adjacent <- pixels_adjacent[,1]  # keep only vector of susceptible pixels adjacent to invaded pixels
  pixels_adjacent <- pixels_adjacent[!duplicated(pixels_adjacent)]  # remove duplicates
  #pixels_adjacent <- pixels_susceptible  # if we want random rather than adjacent spread
  
  # Convert `num_pixels_to_convert` random adjacent pixels.
  # If num_pixels_to_convert > length(pixels_adjacent), run through additional loop
  ras_temp <- list_ras[[i-1]] # create new raster layer for year i
  if(num_pixels_to_convert <= length(pixels_adjacent)){
    
    # convert susceptible, adjacent pixels in `ras_temp` based on invasibility metric
    pixels_adjacent_probs <- ras_probs[pixels_adjacent]
    pixels_adjacent_probs[is.na(pixels_adjacent_probs)] <- median(pixels_adjacent_probs, na.rm = TRUE)
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
    pixels_adjacent_probs[is.na(pixels_adjacent_probs)] <- median(pixels_adjacent_probs, na.rm = TRUE)
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

# save yearly rasters
yearVec <- 1:100
yearVec <- stringr::str_pad(yearVec, 3, pad = "0")
for(i in 1:length(list_ras)){
  writeRaster(list_ras[[i]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09e forest lost to grass yearly rasters/09e-a unprotected/',
                                'year ', yearVec[[i]], '.tif'),
              overwrite = TRUE)
}
rm(yearVec)
gc()




##### mask with alien forest spread #####

# setup
library(terra)
yearVec <- 1:100
yearVec <- stringr::str_pad(yearVec, 3, pad = "0")

# list forest to grass rasters
ras_listGrassSpread <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09e forest lost to grass yearly rasters/09e-a unprotected/',
             full.names = TRUE, pattern = '.tif')

# list alien forest spread rasters
ras_listAlienSpread <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07b unprotected/',
             full.names = TRUE, pattern = '.tif')

# replace any grass with alien forest (i.e., alien forest spread takes precedence over grass spread)
for(i in 1:length(ras_listGrassSpread)){
  
  # get rasters
  ras_grassSpread <- rast(ras_listGrassSpread[[i]])
  ras_alienSpread <- rast(ras_listAlienSpread[[i]])
  
  # replace grass spread with alien forest spread
  ras_grassSpread[ras_grassSpread == -5 & ras_alienSpread == 0] <- 0
  
  # save raster
  writeRaster(ras_grassSpread,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09e forest lost to grass yearly rasters/09e-a unprotected/masked by alien forest spread/',
                                'year ', yearVec[[i]], '.tif'),
              overwrite = TRUE)
  print(paste(i, Sys.time()))
  gc()
}














##### plot line charts #####

# setup
library(terra)
library(doParallel)
registerDoParallel(cores = 8)

# count pixels converted
yearVec <- 1:100
yearVec <- stringr::str_pad(yearVec, 3, pad = "0")
vec_numGrass <- foreach(i = 1:100,
                        .combine = 'c', .packages = 'terra') %dopar% {
  
  # convert to data.frame
  dat <- as.data.frame(rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09e forest lost to grass yearly rasters/09e-a unprotected/masked by alien forest spread/',
                                   'year ', yearVec[[i]], '.tif')),
                       xy = FALSE); gc()
  colnames(dat) <- 'lc'
  
  # count number of forest to grass pixels
  return(length(dat$lc[dat$lc == -5]))
  gc()
  
}


# create line graph dataset
dat_lines <-
  data.frame(year = 1:length(yearVec),
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