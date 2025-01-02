
# This script calculates the rate of forest-driven forest loss by island.

library(raster)
library(sf)
library(rgdal)
library(exactextractr)
library(ggplot2)




##### load data #####

# load forest rasters
ras_wood1999 <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
                "HI_FracLC_wood_1999.tif"))
ras_wood2016 <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
                "HI_FracLC_wood_2016.tif"))

# define number of years for yearly average rate
n_years <- 2016 - 1999

# load fire perimeters
sf_fires <-
  read_sf(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/",
                 "2019_1999_Hawaii_Fire_Perimeters.shp"))

# load island outlines
sf_islands <-
  read_sf(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii coastlines/",
                 "coast_n83.shp"))

# load existing management areas
sf_mgmtPoly <-
  readOGR("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Fire/management area shapefiles/",
          "Hawaii_Veg_Management_C_poly")
sf_mgmtLine <-
  readOGR("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Fire/management area shapefiles/",
          "Hawaii_Veg_Management_C_ln")

# load watersheds
sf_watersheds <-
  read_sf("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii watersheds/",
          "Watersheds")



##### format island data #####

# create island ID variable in sf_islands
sf_islands$island <- factor(1:nrow(sf_islands))
sf_islands$island <-
  c('Kauai', 'Minor island 1', 'Niihau', 'Oahu', 'Minor island 2',
    'Minor island 3', 'Minor island 4', 'Molokai', 'Maui', 'Lanai',
    'Minor island 5', 'Kahoolawe', 'Big Island')

# remove minor islands
sf_islands <-
  sf_islands[sf_islands$island %in%
               c('Kauai', 'Oahu', 'Molokai', 'Maui', 'Lanai', 'Big Island'),]

# # check labels
# ggplot(data = sf_islands,
#        aes(fill = island)) +
#   geom_sf() +
#   scale_fill_manual(values =
#                       c('red', 'orange', 'yellow', 'green', 'blue', 'purple'))




##### find pixels where forest was lost between 1999 and 2016 #####

# create dummy variables: 40% is woody-dominant
ras_wood1999Dummy <- ras_wood1999
ras_wood1999Dummy[ras_wood1999Dummy >= 0.40] <- 1
ras_wood1999Dummy[ras_wood1999Dummy < 0.40] <- 0
ras_wood2016Dummy <- ras_wood2016
ras_wood2016Dummy[ras_wood2016Dummy >= 0.40] <- 1
ras_wood2016Dummy[ras_wood2016Dummy < 0.40] <- 0
gc(); removeTmpFiles()

# create difference rasters
ras_woodDiff <- ras_wood2016 - ras_wood1999
ras_woodDiffDummy <- ras_wood2016Dummy - ras_wood1999Dummy

# find pixels where forest is lost (CONSERVATIVE: difference of at least 40%)
ras_woodLostDummyConservative <- ras_wood1999Dummy
ras_woodLostDummyConservative[!is.na(ras_woodLostDummyConservative)] <- 0
ras_woodLostDummyConservative[ras_wood1999Dummy == 1 &
                                ras_wood2016Dummy == 0 &
                                ras_woodDiff <= -0.40] <- 1
gc()

# find pixels where fores is lost (any pixels where 1 in 1999 and 0 in 2016)
ras_woodLostDummy <- ras_wood1999Dummy
ras_woodLostDummy[!is.na(ras_woodLostDummy)] <- 0
ras_woodLostDummy[ras_wood1999Dummy == 1 &
                    ras_wood2016Dummy == 0] <- 1
gc()




##### split forest loss raster by island and compute pixels lost per year w/in fire perimeters #####

# mask forest loss dummy raster with fire perimeters
ras_woodLostDummy_maskedFires <-
  mask(ras_woodLostDummy, sf_fires)
ras_woodLostDummyConservative_maskedFires <-
  mask(ras_woodLostDummyConservative, sf_fires)

# calculate statewide fire-driven forest loss per year
val_statewideAnnualWoodPixelsLost <-
  sum(values(ras_woodLostDummy_maskedFires), na.rm = TRUE) / n_years
val_statewideAnnualWoodHectaresLost <-
  val_statewideAnnualWoodPixelsLost * 0.09
val_statewideAnnualWoodPixelsLostConservative <-
  sum(values(ras_woodLostDummyConservative_maskedFires), na.rm = TRUE) / n_years
val_statewideAnnualWoodHectaresLostConservative <-
  val_statewideAnnualWoodPixelsLostConservative * 0.09
gc()

# extract forest loss pixel values by island
list_woodLostDummy_byIsland <-
  exact_extract(ras_woodLostDummy_maskedFires, sf_islands)
names(list_woodLostDummy_byIsland) <- sf_islands$island
list_woodLostDummyConservative_byIsland <-
  exact_extract(ras_woodLostDummyConservative_maskedFires, sf_islands)
names(list_woodLostDummyConservative_byIsland) <- sf_islands$island
gc()

# get forest lost per year by island
vec_islandAnnualWoodPixelsLost <-
  sapply(list_woodLostDummy_byIsland,
         function(x){
           sum(x$value, na.rm = TRUE) / n_years
         })
vec_islandAnnualWoodHectaresLost <-
  vec_islandAnnualWoodPixelsLost * 0.09
names(vec_islandAnnualWoodPixelsLost) <- 
  names(vec_islandAnnualWoodHectaresLost) <- sf_islands$island

# get forest lost per year by island - conservative
vec_islandAnnualWoodPixelsLostConservative <-
  sapply(list_woodLostDummyConservative_byIsland,
         function(x){
           sum(x$value, na.rm = TRUE) / n_years
         })
vec_islandAnnualWoodHectaresLostConservative <-
  vec_islandAnnualWoodPixelsLostConservative * 0.09
names(vec_islandAnnualWoodPixelsLostConservative) <- 
  names(vec_islandAnnualWoodHectaresLostConservative) <- sf_islands$island
gc()




##### does management areas reduce forest loss due to fire? #####

# rasterize management area sfs
# sf_mgmtPoly <-spTransform(sf_mgmtPoly, crs(ras_wood1999Dummy))
# ras_mgmtPoly <- rasterize(sf_mgmtPoly, ras_wood1999Dummy)
# sf_mgmtLine <- spTransform(sf_mgmtLine, crs(ras_wood1999Dummy))
# ras_mgmtLine <- terra::rasterize(sf_mgmtLine, ras_wood1999Dummy)
# ras_mgmt <- ras_mgmtLine
# ras_mgmt[!is.na(ras_mgmt)] <- 1
# ras_mgmtPoly[!is.na(ras_mgmtPoly)] <- 1
# ras_mgmt[ras_mgmtPoly == 1] <- 1
# writeRaster(ras_mgmt,
#             filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
#                               '09a - ras_mgmt - dummy raster current mgmt areas lines and polys.tif'),
#             overwrite = TRUE)
# gc()
ras_mgmt <-
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                '09a - ras_mgmt - dummy raster current mgmt areas lines and polys.tif'))

# find watersheds with management areas
ras_mgmt[is.na(ras_mgmt)] <- 0
dat_watersheds <-
  exact_extract(ras_mgmt, sf_watersheds)
dat_watersheds <-
  sapply(dat_watersheds,
         function(x){
           sum(x$value, na.rm = TRUE)
         })
dat_watersheds[dat_watersheds > 0] <- 1
sf_watersheds$hasMgmtArea <- dat_watersheds
rm(dat_watersheds)

# convert watershed size from sq miles to hectares
sf_watersheds$area_hectares <- sf_watersheds$area_sqmi * 258.998811
gc()

# sum forest pixels lost by watershed
dat_firesByWatershed <-
  exact_extract(ras_woodLostDummy_maskedFires, sf_watersheds)
dat_firesByWatershedConservative <-
  exact_extract(ras_woodLostDummyConservative_maskedFires, sf_watersheds)
sf_watersheds$forestLostPixels <-
  sapply(dat_firesByWatershed,
         function(x){
           sum(x$value, na.rm = TRUE)
         })
sf_watersheds$forestLostPixelsConservative <-
  sapply(dat_firesByWatershedConservative,
         function(x){
           sum(x$value, na.rm = TRUE)
         })

# convert pixels lost per watershed to hectares per watershed
sf_watersheds$forestLostHectares <-
  sf_watersheds$forestLostPixels * 0.09
sf_watersheds$forestLostHectaresConservative <-
  sf_watersheds$forestLostPixelsConservative * 0.09

# calculate proportion of watershed's forest lost to fire
sf_watersheds$forestLostProportion <-
  with(sf_watersheds,
       forestLostHectares / area_hectares)
sf_watersheds$forestLostProportionConservative <-
  with(sf_watersheds,
       forestLostHectaresConservative / area_hectares)




##### save rates of fire-driven forest loss #####

save(val_statewideAnnualWoodHectaresLost, val_statewideAnnualWoodHectaresLostConservative,
     val_statewideAnnualWoodPixelsLost, val_statewideAnnualWoodPixelsLostConservative,
     vec_islandAnnualWoodHectaresLost, vec_islandAnnualWoodHectaresLostConservative,
     vec_islandAnnualWoodPixelsLost, vec_islandAnnualWoodPixelsLostConservative,
     file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/09 forest loss modeling/',
                   '09a - rates of fire-driven forest loss.Rdata'))
