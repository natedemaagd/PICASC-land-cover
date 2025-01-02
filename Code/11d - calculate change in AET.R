
# This script calculates the change in AET between the current AET and AET under
# future scenarios.

library(raster)




##### load data #####

# load baseline landcover raster
ras_landcover <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
                "mhi_land_cover_names.tif"))

# load AET rasters
list_ras_AET <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield',
             pattern = '11c - ras AET', full.names = TRUE)
list_ras_AET_names <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield',
             pattern = '11c - ras AET')
list_ras_AET <-
  lapply(list_ras_AET, raster)
names(list_ras_AET) <- list_ras_AET_names
rm(list_ras_AET_names)




##### create AET difference rasters #####

# extract current-day AET from list
ras_AET_currentFitted <-
  list_ras_AET$`11c - ras AET predicted pre-invasion.tif`
list_ras_AET$`11c - ras AET predicted pre-invasion.tif` <- NULL
gc()

# create list of difference rasters - one raster for each scenario
list_ras_AET_difference <-
  lapply(list_ras_AET,
         function(r){
           r - ras_AET_currentFitted
         })
gc()

# name difference rasters according to model
vec_rasNames <- names(list_ras_AET_difference)
list_rasNamesSplit <- strsplit(vec_rasNames, ' ')
list_diffRasNames <-
  lapply(list_rasNamesSplit,
         function(c){
           x <- paste(c[6:8], collapse = ' ')
           x <- substr(x, 1, nchar(x)-4)
           paste0('11d - AET diff ras ', x)
         })
names(list_ras_AET_difference) <- list_diffRasNames
rm(list_rasNamesSplit, vec_rasNames)

# write rasters
for(i in 1:length(list_ras_AET_difference)){
  writeRaster(list_ras_AET_difference[[i]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11d - change in AET/',
                                names(list_ras_AET_difference)[[i]],  ' - mm per year.tif'),
              overwrite = TRUE)
}




##### convert to liters/day #####

# create conversion factor
val_mmPerYear_to_litersPerDay <-
  30 * 30 * 0.001 * 1000 / 365
  # 30m resolution, current val is mm so convert to meters by multiplying by 0.001 -> cubic m
  # 1 cubic meter = 1000 liters, so multiply by 1000
  # convert from "per year" to "per day" by dividing by 365

# convert mm/year difference rasters to liters/day rasters
list_ras_AET_difference_litersPerDay <-
  lapply(list_ras_AET_difference,
         function(r){r * val_mmPerYear_to_litersPerDay})
gc()

# write rasters
for(i in 1:length(list_ras_AET_difference_litersPerDay)){
  writeRaster(list_ras_AET_difference_litersPerDay[[i]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11d - change in AET/',
                                names(list_ras_AET_difference_litersPerDay)[[i]],
                                ' - L per day.tif'),
              overwrite = TRUE)
}

# calculate change (MLD) in each scenario - statewide
vec_statewide_MLD <-
  sapply(list_ras_AET_difference_litersPerDay,
         function(r){
           sum(values(r), na.rm = TRUE) / 1e6
           })
gc()
names(vec_statewide_MLD) <- names(list_ras_AET_difference_litersPerDay)

# save data
dat <-
  data.frame(model = c('best case 2070', 'best case 2100',
                       'middle case 2070',  'middle case 2100',
                       'worst case 2070',  'worst case 2100'),
             MLD_statewide = vec_statewide_MLD)
write.csv(dat,
          file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11d - change in AET/',
                        'table - statewide mil liters per day by model.csv'),
          row.names = FALSE)
