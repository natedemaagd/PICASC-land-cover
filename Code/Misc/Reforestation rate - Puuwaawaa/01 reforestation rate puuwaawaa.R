
# this script finds the reforestation rate within Puuwaawaa protected areas

library(sf); library(raster); library(ggplot2)

# load data
sf_pww <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/fenced regions/puu waawaa/units.shp")
  sf_pww <- sf_pww[sf_pww$ID != 1632,]  # per Clay, remove large enclosed area for this analysis
r_wood1999 <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_wood_1999.tif")
r_wood2016 <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_wood_2016.tif")

# calculate change in wood cover and subset using pww fences
r_woodChange <- r_wood2016 - r_wood1999
sf_pww <- st_zm(sf_pww)  # coerce to two-dimensional polygons
r_woodChange_pww <- mask(x = r_woodChange, mask = sf_pww)
r_wood1999_pww <- mask(x = r_wood1999, mask = sf_pww)  # subset start year woody pct
r_wood2016_pww <- mask(x = r_wood2016, mask = sf_pww)  # subset end year woody pct
rm(sf_pww, r_wood1999, r_wood2016, r_woodChange)
gc()




##### how many pixels converted to forest? #####

# define forest threshold (percent landcover)
threshold <- 30
length_years <- 2016 - 1999

# dummy raster of forest - 1999
r_wood1999_pww <- r_wood1999_pww * 100
r_wood1999_pww_dummy <- r_wood1999_pww
r_wood1999_pww_dummy[r_wood1999_pww >= threshold] <- 1
r_wood1999_pww_dummy[r_wood1999_pww  < threshold] <- 0

# dummy raster of forest - 2016
r_wood2016_pww <- r_wood2016_pww * 100
r_wood2016_pww_dummy <- r_wood2016_pww
r_wood2016_pww_dummy[r_wood2016_pww >= threshold] <- 1
r_wood2016_pww_dummy[r_wood2016_pww  < threshold] <- 0

# find pixels that were converted
r_woodConverted_pww <- r_wood2016_pww
r_woodConverted_pww[r_wood1999_pww_dummy == 1] <- 0

# change in number of pixels
pixels_forest1999 <- sum(values(r_wood1999_pww_dummy), na.rm = TRUE)
pixels_forest2016 <- sum(values(r_wood2016_pww_dummy), na.rm = TRUE)
pct_change_per_year <- (pixels_forest2016 / pixels_forest1999) / length_years * 100

