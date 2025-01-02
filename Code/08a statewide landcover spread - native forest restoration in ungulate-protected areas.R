
# This script spreads native forest pixels into non-native forest pixels
# within protected areas only.
# E.g., spread rate of 5% would covert 5% of remaining susceptible land annually, choosing
# pixels (nearly) adjacent to existing invasive landcover according to invasibility probability.

# NOTE: "Invasive" here means it's a landcover that is spreading. I.e. native forest is "invading"
# into the "susceptible" non-native forest. This keeps the script conceptually equivalent to the
# previous spread of non-native forest into native forest.

library(raster); library(rasterVis); library(ggplot2); library(viridis)
library(gganimate); library(sf); library(gdalUtilities); library(sf)
library(doParallel)
registerDoParallel(cores = 4)




##### data setup #####

# define spread rate and number of years to spread
r <- 0.03
y <- 100

# create year vector
vecYear <- 1:y
vecYear <- stringr::str_pad(vecYear, width = 3, side = 'left', pad = '0')

# define susceptible pixels, invasive pixels (see "H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Landcover_MetaCategories_GM.xlsx" for conversion)
# val_susceptible <- 32  # Tom G values, non-native forest
val_susceptible <- c(1600, 1700, 1800, 1900, 2000, 2100,
                     3700, 3800, 3900)  # ALL non-native landcover types are converted back to native forest
#val_invasive <- c(8, 10, 13)  # Tom G values, native forest
val_invasive <- c(100, 200, 300, 400, 500,  # Tom G 10 equivalent (no code for Tom G 8 in new raster)
                  600, 700, 800)  # Tom G 13 equivalent

# load landcover raster and convert values of invasive pixels to be one value
ras <-
  # raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
  #               "mhi_land_cover_names.tif"))
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
                "mhi_s0_baseline_names.tif"))
ras[ras %in% val_invasive] <- 0; val_invasive <- 0

# load ungulate shapefile
sf_ungulateProtection <-
  read_sf(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Priority areas/Shapefiles/UngulateUnit/",
                 "AllUngulateUnit_Sept2019.shp"))

# load national parks
sf_np <- 
  read_sf("H:/My Drive/Projects/Data/Shapefiles/NPS_Land_Resources_Division_Boundary_and_Tract_Data_Service/nps_boundary.shp")
sf_np <- st_transform(sf_np, crs(sf_ungulateProtection))
sf_np <- sf_np[sf_np$UNIT_NAME %in% c('Hawai\'i Volcanoes National Park', 'Haleakala National Park'),]

# merge national parks and management units
sf_ungulateProtection$type = 'mgmtUnit'
sf_np$type = 'np'
sf_ungulateProtection <- sf_ungulateProtection[c('type', 'geometry')]
sf_np <- sf_np[c('type', 'geometry')]
sf_ungulateProtection <- rbind(sf_ungulateProtection, sf_np)

# reproject protection shapefile to match rasters
sf_ungulateProtection <-
  st_transform(sf_ungulateProtection, "+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs")
# spTransform(sf_ungulateProtection,
#             CRS("+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs"))

# drop empty polygons in shapefile
sf_ungulateProtection <-
  sf_ungulateProtection[!st_is_empty(sf_ungulateProtection),,drop=FALSE]

# keep original raster separate to use as mask
ras_original <- ras

# # mask landcover raster so that unprotected areas won't be affected by spread
# ras <- mask(ras, mask = sf_ungulateProtection, updatevalue = -99)

# create ras_probs: equal probability for each pixel
ras_probs <- ras
values(ras_probs) <- 1




##### perform spread simulation #####

# save first year raster
writeRaster(ras,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08a - statewide landcover spread - native forest restoration in ungulate-protected areas - masked/',
                              'year ', vecYear[[1]], '.tif'),
            overwrite = TRUE)

# run simulation for y years - ### COMPLETES AT 39 YEARS ###
list_ras <- list()  # initiate list of rasters - one for each year of simulation
list_ras[[1]] <- ras  # first year will be initial raster
list_pixelsConverted <- list()  # initiate list to track number of pixels converted each year
for(i in 2:y){  # 2:y
  
  # load previous year's raster
  ras_temp <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08a - statewide landcover spread - native forest restoration in ungulate-protected areas - masked/',
                            'year ', vecYear[[i-1]], '.tif')) # read raster i-1
  
  # determine the number of pixels that need to be converted: r*100 susceptible pixels
  num_pixels_to_convert <-
    round(r * length(ras_temp[ras_temp %in% val_susceptible]))
  list_pixelsConverted[[i]] <- num_pixels_to_convert  # record number of pixels converted this year
  
  # get indices of all susceptible pixels
  pixels_susceptible <- which(values(ras_temp) %in% val_susceptible)
  
  # get indices of all invaded pixels
  pixels_invaded <- which(values(ras_temp) %in% val_invasive)
  
  # find pixels_invaded that are adjacent to susceptible pixels
  pixels_adjacent <-
    adjacent(ras_temp,
             cells = pixels_susceptible,
             target = pixels_invaded,
             directions = 8)
  pixels_adjacent <- pixels_adjacent[,1]  # keep only vector of susceptible pixels adjacent to invaded pixels
  pixels_adjacent <- pixels_adjacent[!duplicated(pixels_adjacent)]  # remove duplicates
  
  # Convert `num_pixels_to_convert` random adjacent pixels.
  # If num_pixels_to_convert > length(pixels_adjacent), run through additional loop
  if(num_pixels_to_convert <= length(pixels_adjacent)){
    
    # convert susceptible, adjacent pixels in `ras_temp` based on invasibility metric
    pixels_adjacent_probs <- ras_probs[pixels_adjacent]
    ras_temp[sample(pixels_adjacent,
                    size = num_pixels_to_convert,
                    prob = pixels_adjacent_probs)] <-
      val_invasive
    
    # save raster
    writeRaster(ras_temp,
                filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08a - statewide landcover spread - native forest restoration in ungulate-protected areas - masked/',
                                  'year ', vecYear[[i]], '.tif'),
                overwrite = TRUE)
    
  } else {  # case if number of pixels to convert > number of adjacent pixels
    
    # create additional copy of the raster - used to add additional layer(s) of susceptible pixels
    ras_temp2 <- ras_temp
    
    # keep adding additional layers of adjacency until enough pixels can be converted
    while(num_pixels_to_convert > length(pixels_adjacent)){
      
      # convert all susceptible pixels in temp2 and find new susceptible pixels
      ras_temp2[pixels_adjacent] <- val_invasive
      pixels_susceptible2 <- which(values(ras_temp2) %in% val_susceptible)
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
    
    # save raster
    writeRaster(ras_temp,
                filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08a - statewide landcover spread - native forest restoration in ungulate-protected areas - masked/',
                                  'year ', vecYear[[i]], '.tif'),
                overwrite = TRUE)
  }
  gc()
  removeTmpFiles(h = 2)
  print(paste0(i, ' - ', Sys.time()))
}
rm(list_ras)

gc()




##### for years 37 to 100, repeat year 36 #####

for(i in 37:y){  
  
  # load year 39
  r <- raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08a - statewide landcover spread - native forest restoration in ungulate-protected areas - masked/',
                     'year 036.tif'))
  
  # save it as year i's raster
  writeRaster(r,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08a - statewide landcover spread - native forest restoration in ungulate-protected areas - masked/',
                                'year ', vecYear[[i]], '.tif'),
              overwrite = TRUE)
  
  removeTmpFiles(h = 2); gc()
  print(paste0(i, ' - ', Sys.time()))
}

rm(r,i)




##### mask rasters with protected area #####

# get list of completed rasters, repeat last one until total is 100 years
list_rasters <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08a - statewide landcover spread - native forest restoration in ungulate-protected areas - masked/',
             pattern = '.tif', full.names = TRUE)
if(length(list_rasters) < y){
  list_rasters_last <- list_rasters[[length(list_rasters)]]
  list_rasters[(length(list_rasters)+1):y] <- list_rasters_last
  rm(list_rasters_last)
}

# mask each raster so it only shows ungulate-protected areas
for(i in 41:length(list_rasters)){
  
  # load raster i
  ras <- raster(list_rasters[[i]])
  
  # mask raster with ungulate protected area
  ras <- mask(ras, sf_ungulateProtection)
  
  # write raster (overwrite existing ones)
  writeRaster(ras,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08a - statewide landcover spread - native forest restoration in ungulate-protected areas - masked/',
                                'year ', vecYear[[i]], '.tif'),
              overwrite = TRUE
              )
  
   rm(ras); gc(); removeTmpFiles(h = 1)
   print(paste0(i, ' - ', Sys.time()))
}




##### save dummy rasters for 2070 and 2100 #####

ras_2070_dummy <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/30m rasters/masked/",
                "year054.tif"))
ras_2100_dummy <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/30m rasters/masked/",
                "year084.tif"))

writeRaster(ras_2070_dummy,
            paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
                   'best case 2070.tif'),
            overwrite = TRUE)
writeRaster(ras_2100_dummy,
            paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
                   'best case 2100.tif'),
            overwrite = TRUE)
