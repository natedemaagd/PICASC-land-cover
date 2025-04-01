
# This script combines the water budget shapefiles for all islands to create a
# recharge ratio raster.

library(sf); library(terra)




##### load data #####

# load water budget shapefiles
list_sf <- list(
  sf_haw1 = read_sf("H:/My Drive/Projects/Data/Shapefiles/Recharge ratios/Hawaii_water_budget_components_subarea_inches_P1/Hawaii_water_budget_components_subarea_inches_P1.shp"),
  sf_haw2 = read_sf("H:/My Drive/Projects/Data/Shapefiles/Recharge ratios/Hawaii_water_budget_components_subarea_inches_P2/Hawaii_water_budget_components_subarea_inches_P2.shp"),
  sf_kau = read_sf("H:/My Drive/Projects/Data/Shapefiles/Recharge ratios/Kauai_water_budget_components_subarea_inches/Kauai_water_budget_components_subarea_inches.shp"),
  sf_lan = read_sf("H:/My Drive/Projects/Data/Shapefiles/Recharge ratios/Lanai_water_budget_components_subarea_inches/Lanai_water_budget_components_subarea_inches.shp"),
  sf_mau = read_sf("H:/My Drive/Projects/Data/Shapefiles/Recharge ratios/Maui_water_budget_components_subarea_inches/Maui_water_budget_components_subarea_inches.shp"),
  sf_mol = read_sf("H:/My Drive/Projects/Data/Shapefiles/Recharge ratios/Molokai_water_budget_components_subarea_inches/Molokai_water_budget_components_subarea_inches.shp"),
  sf_oah = read_sf("H:/My Drive/Projects/Data/Shapefiles/Recharge ratios/Oahu_water_budget_components_subarea_inches/Oahu_water_budget_components_subarea_inches.shp")
)
gc()

# load land cover raster for raster creation
rast_lc <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/mhi_s0_baseline_noNames.tif")

# match sf CRSs with raster
list_sf <- lapply(list_sf, function(s) st_transform(s, crs(rast_lc)))




##### create raster from shapefiles #####

# rasterize recharge
list_recharge <-
  lapply(list_sf, function(s){
    rasterize(vect(s), rast_lc, field = 'S1_Tot_rc')
  })
gc()

# rasterize runoff
list_runoff <-
  lapply(list_sf, function(s){
    rasterize(vect(s), rast_lc, field = 'S1_Runoff')
  })
rm(list_sf)
gc()

# create recharge ratio
list_rechargeRatio <- list()
for(i in 1:length(list_recharge)){
  list_rechargeRatio[[i]] <-
    list_recharge[[i]] / (list_recharge[[i]] + list_runoff[[i]])
}
rm(i, list_recharge, list_runoff)
gc()

# combine into single raster
sprc_rechargeRatio <- sprc(list_rechargeRatio)
rast_rechargeRatio <- merge(sprc_rechargeRatio)
rm(list_rechargeRatio, sprc_rechargeRatio)
gc()

# write raster
writeRaster(rast_rechargeRatio,
            filename = paste0('H:/My Drive/Projects/Data/Rasters/recharge ratio statewide/',
                              'rechargeRatioHawaii.tif'),
            overwrite = TRUE)
