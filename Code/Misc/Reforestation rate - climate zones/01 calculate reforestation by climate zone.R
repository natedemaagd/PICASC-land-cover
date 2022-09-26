
# this script tries to calculate how quickly reforestation occurs by climate zone

library(raster); library(sf); library(doParallel)
registerDoParallel(cores = 7)
library(ggplot2)

setwd("H:/My Drive/Projects/PICASC Land-to-sea/Data/")

# load data - grass and forest cover
ras_grass1999 <- raster("Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_herb_1999.tif")
ras_grass2016 <- raster("Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_herb_2016.tif")
ras_wood1999 <- raster("Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_wood_1999.tif")
ras_wood2016 <- raster("Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_wood_2016.tif")

# load data - moisture zones
sf_moistZones <- read_sf("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii moisture zones/moisture_zones.shp")




##### subset rasters by moisture zone #####

# change projection of sf to match rasters
sf_moistZones<- st_transform(sf_moistZones,
                             crs = st_crs(ras_grass1999))

# split sf by moisture zone
sf_moistZones_split <- split(sf_moistZones, sf_moistZones$MoistureZo)
vec_moistureZones <- names(sf_moistZones_split)

# subset grass rasters by moisture zone
list_grass1999ByMoistZone <-
  foreach(z = 1:length(vec_moistureZones),
          .packages = c('sf', 'raster')) %dopar% {
            mask(ras_grass1999, sf_moistZones_split[[z]])
          }
gc()

list_grass2016ByMoistZone <-
  foreach(z = 1:length(vec_moistureZones),
          .packages = c('sf', 'raster')) %dopar% {
            mask(ras_grass2016, sf_moistZones_split[[z]])
          }
gc()

# subset wood rasters by moisture zone
list_wood1999ByMoistZone <-
  foreach(z = 1:length(vec_moistureZones),
          .packages = c('sf', 'raster')) %dopar% {
            mask(ras_wood1999, sf_moistZones_split[[z]])
          }
gc()

list_wood2016ByMoistZone <-
  foreach(z = 1:length(vec_moistureZones),
          .packages = c('sf', 'raster')) %dopar% {
            mask(ras_wood2016, sf_moistZones_split[[z]])
          }
gc()




##### rate of change calculation #####

# # find grass-dominated pixels in 1999 - load shortcut below
grass_dominated <- 0.60  # define proportion of grass needed to be considered dominant
# list_grassDominated1999 <-
#   foreach(z = 1:length(vec_moistureZones),
#           .packages = 'raster') %dopar% {
# 
#             # get raster for moisture zone z
#             ras <- list_grass1999ByMoistZone[[z]]
# 
#             # create dummy raster: 1 = grass-dominated
#             ras[ras >= grass_dominated] <- 1
#             ras[ras < grass_dominated] <- NA
# 
#             writeRaster(ras,
#                         filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/',
#                                           'Intermediate/Water yield/Misc/Reforestation rate - moisture zones/01a 1999 grass dominated pixels by moisture zone/',
#                                           '01 grass dominated 1999 moisture zone ', vec_moistureZones[[z]], '.tif'),
#                         overwrite = TRUE)
#             
#             gc()
# 
#           }
# gc()

list_grassDominated1999 <-
  list.files(path = 'Intermediate/Water yield/Misc/Reforestation rate - moisture zones/01a 1999 grass dominated pixels by moisture zone',
             pattern = '.tif', full.names = TRUE)
list_grassDominated1999 <- lapply(list_grassDominated1999, raster)


# # find wood-dominated pixels in 1999 - load shortcut below
wood_dominated <- 0.60  # define proportion of wood needed to be considered dominant
# list_woodDominated1999 <-
#   foreach(z = 1:length(vec_moistureZones),
#           .packages = 'raster') %dopar% {
# 
#             # get raster for moisture zone z
#             ras <- list_wood1999ByMoistZone[[z]]
# 
#             # create dummy raster: 1 = wood-dominated
#             ras[ras >= wood_dominated] <- 1
#             ras[ras < wood_dominated] <- NA
# 
#             writeRaster(ras,
#                         filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/',
#                                           'Intermediate/Water yield/Misc/Reforestation rate - moisture zones/01b 1999 wood dominated pixels by moisture zone/',
#                                           '01 wood dominated 1999 moisture zone ', vec_moistureZones[[z]], '.tif'),
#                         overwrite = TRUE)
# 
#             gc()
# 
#           }
# gc()

list_woodDominated1999 <-
  list.files(path = 'Intermediate/Water yield/Misc/Reforestation rate - moisture zones/01b 1999 wood dominated pixels by moisture zone',
             pattern = '.tif', full.names = TRUE)
list_woodDominated1999 <- lapply(list_woodDominated1999, raster)


# # find wood-dominated pixels in 2016 - load shortcut below
# list_woodDominated2016 <-
#   foreach(z = 1:length(vec_moistureZones),
#           .packages = 'raster') %dopar% {
# 
#             # get raster for moisture zone z
#             ras <- list_wood2016ByMoistZone[[z]]
# 
#             # create dummy raster: 1 = wood-dominated
#             ras[ras >= wood_dominated] <- 1
#             ras[ras < wood_dominated] <- NA
# 
#             writeRaster(ras,
#                         filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/',
#                                           'Intermediate/Water yield/Misc/Reforestation rate - moisture zones/01c 2016 wood dominated pixels by moisture zone/',
#                                           '01 wood dominated 2016 moisture zone ', vec_moistureZones[[z]], '.tif'),
#                         overwrite = TRUE)
# 
#             gc()
# 
#           }
# gc()

list_woodDominated2016 <-
  list.files(path = 'Intermediate/Water yield/Misc/Reforestation rate - moisture zones/01c 2016 wood dominated pixels by moisture zone',
             pattern = '.tif', full.names = TRUE)
list_woodDominated2016 <- lapply(list_woodDominated2016, raster)


# count number of forest-dominated pixels in 1999 and 2016 by moisture zone
vec_forestDomPixels1999 <- sapply(list_woodDominated1999,
                                  function(r){
                                    sum(values(r), na.rm = TRUE)
                                  })

vec_forestDomPixels2016 <- sapply(list_woodDominated2016,
                                  function(r){
                                    sum(values(r), na.rm = TRUE)
                                  })

# convert pixels to hectares
vec_forestDomHectares1999 <- vec_forestDomPixels1999 * 0.09
vec_forestDomHectares2016 <- vec_forestDomPixels2016 * 0.09

# percent change in forest area by moisture layer
vec_pctChangeForest <- rep(NA, times = length(vec_moistureZones))
for(z in 1:length(vec_moistureZones)){
  vec_pctChangeForest[[z]] <-
    (vec_forestDomPixels2016[[z]] - vec_forestDomPixels1999[[z]]) /
    vec_forestDomPixels1999[[z]] * 100
}

# percent change in forest area by moisture layer PER YEAR
vec_pctChangeForestPerYr <- vec_pctChangeForest / (2016 - 1999)

# plot
vec_grassHectaresByClimate <- sapply(list_grassDominated1999, function(r){
  length(values(r)[!is.na(values(r))]) * 0.09  # hectares of grass-dominant land in 1999
})
dat_grassHecratesByClimate <- data.frame(climate = names(sf_moistZones_split),
                                         hectaresGrassDominant1999 = vec_grassHectaresByClimate)
plotdat <- data.frame(climate = names(sf_moistZones_split),
                      forestGrowthRate_pctPerYr = vec_pctChangeForestPerYr)
plotdat <- dplyr::left_join(plotdat, dat_grassHecratesByClimate, 'climate')
plotdat$climate <-
  factor(plotdat$climate,
         levels = c('Arid', 'Very Dry', 'Moderately Dry', 'Seasonal Mesic', 'Moist Mesic', 'Moderately Wet', 'Very Wet'))
rm(dat_grassHecratesByClimate)
ggplot(data = plotdat,
       aes(x = climate,
           y = forestGrowthRate_pctPerYr)) +
  geom_bar(stat = 'identity', color = 'black') +
  theme(text = element_text(size = 15)) +
  labs(x = 'Climate', y = 'Mean forest growth rate (% / yr)') +
  geom_hline(yintercept = 0)

gc()
