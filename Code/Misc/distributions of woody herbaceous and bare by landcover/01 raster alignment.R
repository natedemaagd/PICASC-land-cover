
# this script aligns landcover raster with wood/herb/bare rasters

library(raster); library(ggplot2)

# load landcover raster
r_landcover <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/mhi_land_cover_names.tif")

# load woody, herbaceous, and bare rasters
r_wood <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/HI_FracLC_wood_2016.tif")
r_herb  <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/HI_FracLC_herb_2016.tif")
r_bare  <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/HI_Fractional_LC_statewide_split/HI_FracLC_bare_2016.tif")

# resample wood, herb, and bare to match landcover raster
r_landcover <- projectRaster(r_landcover, r_bare, method = 'ngb')  # reproject landcover raster to match other rasters
r_wood <- resample(r_wood, r_landcover)
r_herb <- resample(r_herb, r_landcover)
r_bare <- resample(r_bare, r_landcover)

# write rasters
writeRaster(r_landcover, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Misc/distributions of woody herbaceous and bare by landcover/01 raster alignment/raster_landcover.tif')
writeRaster(r_wood,  filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Misc/distributions of woody herbaceous and bare by landcover/01 raster alignment/raster_woodCover.tif')
writeRaster(r_herb,  filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Misc/distributions of woody herbaceous and bare by landcover/01 raster alignment/raster_herbcover.tif')
writeRaster(r_bare,  filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Misc/distributions of woody herbaceous and bare by landcover/01 raster alignment/raster_barecover.tif')
