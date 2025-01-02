
# This script integrates the pixels where forest converts to grass with the other
# spread scenarios.

library(terra)




##### load data #####

# scenario rasters - load and name
list_ras <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
             full.names = TRUE, pattern = '.tif')
list_ras_names <- list_ras
list_ras_names <- strsplit(list_ras_names, split = '/')
list_ras_names <- sapply(list_ras_names, function(x){
  x[[length(x)]]
})
list_ras_names <- substr(list_ras_names, 1, nchar(list_ras_names)-4)
list_ras <- lapply(list_ras, rast)
names(list_ras) <- list_ras_names
rm(list_ras_names)

# forest to grass rasters for 2070 and 2100 (years 54 and 84 of simulation)
ras_forestToGrass2070 <-
  rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09e forest lost to grass yearly rasters/09e-a unprotected/masked by alien forest spread/year 054.tif")
ras_forestToGrass2100 <-
  rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09e forest lost to grass yearly rasters/09e-a unprotected/masked by alien forest spread/year 084.tif")




##### integrate forest lost to grass with other scenarios #####

# keep protected areas masked, and make all other spreads take precedent over forest lost to grass in areas of overlap
list_ras$`worst case 2070`[ras_forestToGrass2070 == -5 & !(list_ras$`middle case 2070` %in% c(-2, -1, 0 ,1))] <- -5
list_ras$`worst case 2100`[ras_forestToGrass2100 == -5 & !(list_ras$`middle case 2070` %in% c(-2, -1, 0 ,1))] <- -5
list_ras$`middle case 2070`[ras_forestToGrass2070 == -5 & !(list_ras$`middle case 2070` %in% c(-2, -1, 0 ,1))] <- -5
list_ras$`middle case 2100`[ras_forestToGrass2100 == -5 & !(list_ras$`middle case 2100` %in% c(-2, -1, 0 ,1))] <- -5
list_ras$`best case 2070`[ras_forestToGrass2070 == -5 & !(list_ras$`best case 2070` %in% c(-2, -1, 0 ,1))] <- -5
list_ras$`best case 2100`[ras_forestToGrass2100 == -5 & !(list_ras$`best case 2100` %in% c(-2, -1, 0 ,1))] <- -5
gc()




##### write rasters #####

for(i in 1:length(list_ras)){
  
  writeRaster(list_ras[[i]],
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/',
                                names(list_ras)[[i]], '.tif'),
              overwrite = TRUE)
  
}
