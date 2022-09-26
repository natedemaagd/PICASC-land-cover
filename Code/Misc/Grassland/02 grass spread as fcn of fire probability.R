
# this script tries to find a relationship b/w grass growth rate and fire risk

library(sf); library(raster); library(ggplot2); library(viridis); library(snow)
library(doParallel); library(rgdal)

registerDoParallel(cores = 5)

setwd("H:/My Drive/Projects/PICASC Land-to-sea/Data/")




##### load data #####

# load fire perimeters
polys_fires <- readOGR(paste0("Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/",
                               "2019_1999_Hawaii_Fire_Perimeters.shp"))

# list fire probability rasters
rasters_fireProb_names <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_MonthlyHistorical',
                                     pattern = '.tif', full.names = TRUE)

# load monthly grass cover
rasters_grass <- list.files('Processed/Fire/interpolated_yearly_landcover_percentages',
                            pattern = 'herb', full.names = TRUE)
rasters_grass <- lapply(rasters_grass, raster)

# define islands
vec_islands <- c('Oahu', 'Kauai', 'MauiCounty', 'Hawaii')




##### create grass-dominant dummy rasters #####

# find pixels with more than 50% grass cover and create raster - shortcut load below
# foreach(r = 1:length(rasters_grass), .packages = 'raster') %dopar% {
#   
#   # get raster r
#   ras <- rasters_grass[[r]]
#   
#   # replace values with 1 if grass dominant, 0 otherwise
#   ras[ras >= 0.5 & !is.na(ras)] <- 1
#   ras[ras  < 0.5 & !is.na(ras)] <- 0
#   
#   writeRaster(ras,
#               paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Misc/Grassland/02 yearly grass dominated pixels/',
#                      'grass_dominated_pixels_', 1998+r, '.tif'),
#               overwrite = TRUE)
#   
#   gc()
# }
rasters_grassDom <- list.files('Intermediate/Misc/Grassland/02 yearly grass dominated pixels',
                                pattern = '.tif', full.names = TRUE)
rasters_grassDom <- lapply(rasters_grassDom, raster)




##### find max fire risk across monthly mean rasters #####

# load monthly mean rasters
rasterNames_monthlyMeanRisk <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean',
                                          pattern = '.tif')
rasters_monthlyMeanRisk <- lapply(rasterNames_monthlyMeanRisk, function(r){
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/prediction_rasters_mean/',
                r))
})

# limit names list to just the island
rasterNames_monthlyMeanRisk <- sapply(rasterNames_monthlyMeanRisk, function(x){
  island <- strsplit(x, '_')[[1]][[5]]  # fifth element of split filename is island
})

# within list of rasters, stack all rasters by island
rasterStacks_monthlyMeanRisk <- list()
for(i in 1:length(vec_islands)){
  rasterStacks_monthlyMeanRisk[[i]] <-
    stack(rasters_monthlyMeanRisk[which(rasterNames_monthlyMeanRisk == vec_islands[[i]])])
}

# for each island, find pixel-wise max fire risk across all months
  # e.g. pixel value will be max(mean jan, mean feb, ..., mean dec)
rasters_maxMonthlyMeanRisk <-
  foreach(s = 1:length(rasterStacks_monthlyMeanRisk),
          .packages = 'raster') %dopar% {
            calc(rasterStacks_monthlyMeanRisk[[s]], mean)
          }

# merge all fire risk rasters
raster_maxMonthlyMeanRisk <- do.call(merge, rasters_maxMonthlyMeanRisk)

# crop grassDom rasters to match maxMonthlyMeanRisk
rasters_grassDom <- lapply(rasters_grassDom, function(r){
  crop(r, raster_maxMonthlyMeanRisk)
})

# limit fire risk to grass-dominant pixels (i.e. make raster of fire risk with NA for non-grass-dominated pixels)
rasters_maxMonthlyMeanRisk_grassDom <-
  foreach(r = 1:length(rasters_grassDom),
          .packages = 'raster') %dopar% {
            mask(raster_maxMonthlyMeanRisk, mask = rasters_grassDom[[r]],
                 maskvalue = 1, inverse = TRUE)
          }
names(rasters_maxMonthlyMeanRisk_grassDom) <- 1999:2016

# clean up
rm(raster_maxMonthlyMeanRisk, rasters_maxMonthlyMeanRisk, rasters_grass, rasters_monthlyMeanRisk,
   rasterStacks_monthlyMeanRisk, i, rasterNames_monthlyMeanRisk, rasters_fireProb_names,
   vec_islands)
gc()




##### for each fire, get change in grassland-dominated pixels over time #####

# create vector of years aligning with Matty's landcover data
vec_years <- 1999:2016

# split fire polygons
polys_fires <- split(polys_fires, polys_fires$UH_ID)

gc()

# perform operation on each fire
dat_changeInGrassHectares <-
  foreach(f = 1:length(polys_fires),
          .packages = 'raster') %dopar% {
            
            # get fire i
            poly_fire_i <- polys_fires[[f]]
            
            # year of fire i
            year_fire_i <- poly_fire_i$Year
            
            # skip fire if it happened in 2014 or later (data ends in 2016 and need couple years to measure change)
            if(year_fire_i >= 2014) next
            
            # get grass dominated rasters for year of fire and all subsequent years
            rasters_grassDom_postFire_fire_i <-
              rasters_grassDom[(which(vec_years == year_fire_i)+1):length(vec_years)]  # omit year-of-fire raster
            
            # subset grass-dominated rasters for fire i, same year and all subsequent years
            rasters_grassDom_postFire_fire_i <- lapply(rasters_grassDom_postFire_fire_i,
                                                       function(r){
                                                         mask(r, poly_fire_i)
                                                       })
            
            # get number of grass-dominated pixels within fire i perimeter each year after fire
            vec_yearlyTotalGrassDom_pixels_fire_i <- sapply(rasters_grassDom_postFire_fire_i,
                                                            function(r){
                                                              sum(values(r), na.rm = TRUE)
                                                            })
            
            # convert number of pixels to hectares, get year-over-year change
            vec_yearlyTotalGrassDom_hectares_fire_i <-
              vec_yearlyTotalGrassDom_pixels_fire_i * 0.09
            vec_yearlyChangeGrassDom_hectares_fire_i <- 
              c(NA, diff(vec_yearlyTotalGrassDom_hectares_fire_i))
            
            # if change is negative, replace with NA (subsequent fire reduced value, etc.)
            vec_yearlyChangeGrassDom_hectares_fire_i[vec_yearlyChangeGrassDom_hectares_fire_i < 0] <- NA
            
            
            
            ### for each fire i, get mean fire risk within perimeter ###
            
            # subset fire risk raster using perimeter of fire i
            raster_fireRisk_fire_i <-
              mask(rasters_maxMonthlyMeanRisk_grassDom[[which(vec_years == year_fire_i)]],
                   poly_fire_i)
            
            # get mean fire risk for grass-dominated pixels in fire i
            meanFireRisk_fire_i <-
              mean(values(raster_fireRisk_fire_i),
                   na.rm = TRUE)
            
            
            
            ### create data.frame ###
            
            df <- data.frame(fireID = polys_fires[[1]]$UH_ID,
                             yearsPostFire = 1:length(vec_yearlyChangeGrassDom_hectares_fire_i),
                             changeGrassDomArea_hectares = vec_yearlyChangeGrassDom_hectares_fire_i)
            
            return(df)
            gc()
            
          }
