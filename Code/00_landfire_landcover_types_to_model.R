library(raster); library(rgdal); library(maptools); library(ggplot2)

# load raster - landcover data
raster_landcover <- raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_landcover_types/land_cv_tp.txt")
raster_landcover_df <- as.data.frame(raster_landcover, xy = TRUE)

# load priority areas - fence regions and landcovers to protect
shapefile_fences <- readOGR('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Priority areas/Shapefiles/UngulateUnit',
                            layer = 'AllUngulateUnit_Sept2019')
shapefile_priorityDry <- readOGR('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Priority areas/Shapefiles/MS_MESIC_Dec1',
                                 layer = 'MS_DRY_Dec1')
shapefile_priorityMesic <- readOGR('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Priority areas/Shapefiles/MS_MESIC_Dec1',
                                 layer = 'MS_MESIC_Dec1')
shapefile_priorityWet <- readOGR('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Priority areas/Shapefiles/MS_MESIC_Dec1',
                                 layer = 'MS_WET_Dec1')

# project shapefiles to match landcover projection
shapefile_fences <- spTransform(shapefile_fences, CRSobj = crs(raster_landcover))
shapefile_priorityDry <- spTransform(shapefile_priorityDry, CRSobj = crs(raster_landcover))
shapefile_priorityMesic <- spTransform(shapefile_priorityMesic, CRSobj = crs(raster_landcover))
shapefile_priorityWet <- spTransform(shapefile_priorityWet, CRSobj = crs(raster_landcover))

# # plot data - fences only
# ggplot() +
#   # landcover base plot
#   geom_tile(data = raster_landcover_df[!is.na(raster_landcover_df$land_cv_tp),], aes(x = x, y = y, fill = as.character(land_cv_tp))) +
#   # fence units
#   geom_polygon(data = shapefile_fences, aes())
#   coord_equal()+
#   theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), panel.background = element_blank(), legend.position = 'none')

# for each shapefile, combine SpatialPolygons into one polygon
raster_landcover_fences <- mask(raster_landcover, shapefile_fences)
raster_landcover_priorityDry <- mask(raster_landcover, shapefile_priorityDry)
raster_landcover_priorityMesic <- mask(raster_landcover, shapefile_priorityMesic)
raster_landcover_priorityWet <- mask(raster_landcover, shapefile_priorityWet)

# create table of each cropped raster to get types of landcover within each shapefile
tab_landcover_fences <- as.data.frame(table(values(raster_landcover_fences)[!is.na(values(raster_landcover_fences))])); names(tab_landcover_fences) <- c('lc_code', 'n_cell')
tab_landcover_priorityDry <- as.data.frame(table(values(raster_landcover_priorityDry)[!is.na(values(raster_landcover_priorityDry))])); names(tab_landcover_priorityDry) <- c('lc_code', 'n_cell')
tab_landcover_priorityMesic <- as.data.frame(table(values(raster_landcover_priorityMesic)[!is.na(values(raster_landcover_priorityMesic))])); names(tab_landcover_priorityMesic) <- c('lc_code', 'n_cell')
tab_landcover_priorityWet <- as.data.frame(table(values(raster_landcover_priorityWet)[!is.na(values(raster_landcover_priorityWet))])); names(tab_landcover_priorityWet) <- c('lc_code', 'n_cell')

# order tables and write csv
tab_landcover_fences <- tab_landcover_fences[order(tab_landcover_fences$n_cell, decreasing = TRUE),]
tab_landcover_priorityDry <- tab_landcover_priorityDry[order(tab_landcover_priorityDry$n_cell, decreasing = TRUE),]
tab_landcover_priorityMesic <- tab_landcover_priorityMesic[order(tab_landcover_priorityMesic$n_cell, decreasing = TRUE),]
tab_landcover_priorityWet <- tab_landcover_priorityWet[order(tab_landcover_priorityWet$n_cell, decreasing = TRUE),]
write.csv(tab_landcover_fences, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/00_landfire_landcover_types_to_model/lc_types_ungulateFences.csv', row.names = FALSE)
write.csv(tab_landcover_priorityDry, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/00_landfire_landcover_types_to_model/lc_types_priorityDry.csv', row.names = FALSE)
write.csv(tab_landcover_priorityMesic, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/00_landfire_landcover_types_to_model/lc_types_priorityMesic.csv', row.names = FALSE)
write.csv(tab_landcover_priorityWet, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/00_landfire_landcover_types_to_model/lc_types_priorityWet.csv', row.names = FALSE)
