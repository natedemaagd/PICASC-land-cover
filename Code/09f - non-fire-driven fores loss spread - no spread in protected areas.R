library(raster); library(rasterVis); library(ggplot2); library(viridis)
library(gganimate); library(sf); library(gdalUtilities); library(sf)




##### load data #####

# list yearly landcover rasters
list_landcoverRasters <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09e forest lost to grass yearly rasters/09e-a unprotected/masked by alien forest spread/',
             pattern = '.tif', full.names = TRUE)

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




##### in each yearly raster, mask the protected areas with original landcover #####

# keep original raster separate to use as mask
ras_original <- raster(list_landcoverRasters[[1]])

# reproject protection shapefile to match rasters
sf_ungulateProtection <-
  st_transform(sf_ungulateProtection, "+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs")
# spTransform(sf_ungulateProtection,
#             CRS("+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs"))

# drop empty polygons in shapefile
sf_ungulateProtection <-
  sf_ungulateProtection[!st_is_empty(sf_ungulateProtection),,drop=FALSE]

# create year vector for raster filenames when saved
vecYear <- 1:100
vecYear <- stringr::str_pad(vecYear, 3, pad = "0")

# mask years >= 2 with original raster
for(i in 1:length(list_landcoverRasters)){
  
  # keep original raster separate to use as mask
  ras_original <- raster(list_landcoverRasters[[1]])
  
  # load raster i
  ras <- raster(list_landcoverRasters[[i]])
  
  # mask with original landcover values
  ras_protected <-
    raster::mask(ras, sf_ungulateProtection, updatevalue = -1, inverse = TRUE)
  
  # replace masked area with original landcover values
  lcVals <- ras_original[ras_protected == -1]
  ras_protected2 <-
    ras_protected
  ras_protected2[ras_protected == -1] <- lcVals
  
  # save masked rasters
  writeRaster(ras_protected,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09f forest lost to grass yearly rasters - no spread in protected areas/protected pixels set to -1/',
                                'year ', vecYear[[i]], '.tif'),
              overwrite = TRUE)
  writeRaster(ras_protected2,
              filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/09f forest lost to grass yearly rasters - no spread in protected areas/protected pixels set to original values/',
                                'year ', vecYear[[i]], '.tif'),
              overwrite = TRUE)
  
  print(paste(i, '---', Sys.time())); gc()
  removeTmpFiles(h = 2)
}
