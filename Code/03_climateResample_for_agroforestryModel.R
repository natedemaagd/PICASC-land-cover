
library(raster)

# load agroforestry 30m landcover raster
raster_landcover <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/baselineagroforestry/baselineagroforestry_snapped1.tif")

# load allvars data
dat <- readxl::read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/DWS/Land_cover/Data/Raw/AllVars_MeanAnnVals.xlsx")

# create function to rasterize variable from `dat`
df_to_raster <- function(var){
  df <- dat[c('POINT_X', 'POINT_Y', var)]
  r <- rasterFromXYZ(df, crs = CRS("+proj=longlat +zone=4 +datum=NAD83 +units=m +no_defs"))
  return(r)
}

# rasterize all necessary variables - ET, LAI, SM, U, T, Rnet
vars_to_rasterize <- c('AET', 'LAI', 'SM', 'U', 'T', 'Rnet')
rasters <- lapply(vars_to_rasterize, function(x){
  df_to_raster(x)
})
names(rasters) <- vars_to_rasterize
rm(vars_to_rasterize)

# resample all rasters to match `raster_landcover`
rasters_resampled <- lapply(rasters, function(r){
  projectRaster(r, raster_landcover)  # replace values with nearest neighbor
})

# save resampled rasters
for (i in 1:length(rasters_resampled)) {
  writeRaster(rasters_resampled[[i]], filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/03_agroforestry/resampled allvars rasters/', names(rasters_resampled)[[i]], '.tif'),
              overwrite = TRUE)
}
