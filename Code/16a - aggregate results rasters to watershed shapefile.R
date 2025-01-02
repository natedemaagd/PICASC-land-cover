
# This script aggregates the water yield results by watershed using Jade's
# watershed shapefile.

library(terra); library(sf); library(ggplot2)




##### load data #####

# watershed shapefile template
sf_ws <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Jade soil application/mhi_scale_results/mhi_sed_export_by_scenario_delta_mauka_by_watershed_min.shp")

# water yield results rasters
rast_waterYield_baseline <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted pre-invasion.tif")
rast_waterYield_best <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted best case 2100.tif")
rast_waterYield_middle <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted middle case 2100.tif")
rast_waterYield_worst <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted worst case 2100.tif")

# climate change water yield data
dat_waterYield_climateChange <-readRDS("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/12a - ET under all scenarios.rds")




##### format data #####

# get difference rasters
rast_waterYield_best <- rast_waterYield_best - rast_waterYield_baseline
rast_waterYield_middle <- rast_waterYield_middle - rast_waterYield_baseline
rast_waterYield_worst <- rast_waterYield_worst - rast_waterYield_baseline

# convert from mm to L
rast_waterYield_best <- rast_waterYield_best * 900  # convert 30m^2 raster values to cubic mm then to L (900000000 cubic mm * 0.000001 L = 900)

# match raster CRSs to shapefile
rast_waterYield_baseline <- project(rast_waterYield_baseline, crs(sf_ws), threads = TRUE, method = 'near')
rast_waterYield_best <- project(rast_waterYield_best, crs(sf_ws), threads = TRUE, method = 'near')
rast_waterYield_middle <- project(rast_waterYield_middle, crs(sf_ws), threads = TRUE, method = 'near')
rast_waterYield_worst <- project(rast_waterYield_worst, crs(sf_ws), threads = TRUE, method = 'near')





##### aggregate pixels by watershed - land cover #####

# extract pixels within each watershed
dat_sumValues_best <- extract(rast_waterYield_best, sf_ws, fun = sum, na.rm = TRUE)
dat_sumValues_middle <- extract(rast_waterYield_middle, sf_ws, fun = sum, na.rm = TRUE)
dat_sumValues_worst <- extract(rast_waterYield_worst, sf_ws, fun = sum, na.rm = TRUE)

# add water yield changes as 1000s L/day (*-1 to convert from change in ET to change in water yield)
sf_ws$wy_pr_tlpd <- dat_sumValues_best[,2] / 1000 / 365 * -1
sf_ws$wy_p_tlpd <- dat_sumValues_middle[,2] / 1000 / 365 * -1
sf_ws$wy_np_tlpd <- dat_sumValues_worst[,2] / 1000 / 365 * -1

# isolate water yield variables as their own shapefiles
sf_waterYield_protAndRest <- sf_ws[c('Island', 'ws_id', 'wsid', 'km2', 'wy_pr_tlpd', 'geometry')]
sf_waterYield_protOnly <- sf_ws[c('Island', 'ws_id', 'wsid', 'km2', 'wy_p_tlpd', 'geometry')]
sf_waterYield_noProt <- sf_ws[c('Island', 'ws_id', 'wsid', 'km2', 'wy_np_tlpd', 'geometry')]

# abbreviate column names for ESRI writing
colnames(sf_waterYield_noProt) <- 
  colnames(sf_waterYield_protAndRest) <- colnames(sf_waterYield_protOnly) <-
  c("Island", "ws_id", "wsid", "km2", "tlpd", "geometry")

# create liters per day per km2
sf_waterYield_noProt$tlpd_km2 <- sf_waterYield_noProt$tlpd / sf_waterYield_noProt$km2
sf_waterYield_protAndRest$tlpd_km2 <- sf_waterYield_protAndRest$tlpd / sf_waterYield_protAndRest$km2
sf_waterYield_protOnly$tlpd_km2 <- sf_waterYield_protOnly$tlpd / sf_waterYield_protOnly$km2

# # plot to check
# ggplot(data = sf_waterYield_noProt,
#        aes(fill = waterYield_noProt_litersPerDay)) +
#   scale_fill_viridis_c(option = 'turbo') +
#   geom_sf()

# write shapefiles
st_write(sf_waterYield_noProt,
         paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                'water yield by watershed - no protection.shp'),
         delete_layer = TRUE)
st_write(sf_waterYield_protAndRest,
         paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                'water yield by watershed - protection and restoration.shp'),
         delete_layer = TRUE)
st_write(sf_waterYield_protOnly,
         paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                'water yield by watershed - protection only.shp'),
         delete_layer = TRUE)

# write rasters
writeRaster(rast_waterYield_best,
            paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                   'water yield raster - protection and restoration.tif'),
            overwrite = TRUE)
writeRaster(rast_waterYield_middle,
            paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                   'water yield raster - protection only.tif'),
            overwrite = TRUE)
writeRaster(rast_waterYield_worst,
            paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                   'water yield raster - no protection.tif'),
            overwrite = TRUE)




##### aggregate pixels by watershed - climate change #####

# change in AET under climate change
dat_waterYield_climateChange$AET_dynRcp45Iprc_delta <- dat_waterYield_climateChange$AET_dynRcp45Iprc - dat_waterYield_climateChange$AET_dynBaseline
dat_waterYield_climateChange$AET_dynRcp85Iprc_delta <- dat_waterYield_climateChange$AET_dynRcp85Iprc - dat_waterYield_climateChange$AET_dynBaseline
dat_waterYield_climateChange$AET_dynRcp85Ncar_delta <- dat_waterYield_climateChange$AET_dynRcp85Ncar - dat_waterYield_climateChange$AET_dynBaseline

# rasterize climate change data.frames then project to match watershed shapefile
rast_rcp45Iprc_delta <- rast(dat_waterYield_climateChange[c('x', 'y', 'AET_dynRcp45Iprc_delta')], type = 'xyz', crs = 'EPSG:26904')
rast_rcp85Iprc_delta <- rast(dat_waterYield_climateChange[c('x', 'y', 'AET_dynRcp85Iprc_delta')], type = 'xyz', crs = 'EPSG:26904')
rast_rcp85Ncar_delta <- rast(dat_waterYield_climateChange[c('x', 'y', 'AET_dynRcp85Ncar_delta')], type = 'xyz', crs = 'EPSG:26904')
rast_rcp45Iprc_delta <- project(rast_rcp45Iprc_delta, crs(sf_ws), threads = TRUE, method = 'near')
rast_rcp85Iprc_delta <- project(rast_rcp85Iprc_delta, crs(sf_ws), threads = TRUE, method = 'near')
rast_rcp85Ncar_delta <- project(rast_rcp85Ncar_delta, crs(sf_ws), threads = TRUE, method = 'near')

# extract pixels within each watershed
dat_sumValues_rcp45Iprc <- extract(rast_rcp45Iprc_delta, sf_ws, fun = sum, na.rm = TRUE)
dat_sumValues_rcp85Iprc <- extract(rast_rcp85Iprc_delta, sf_ws, fun = sum, na.rm = TRUE)
dat_sumValues_rcp85Ncar <- extract(rast_rcp85Ncar_delta, sf_ws, fun = sum, na.rm = TRUE)

# add water yield changes as 1000s L/day (*-1 to convert from change in ET to change in water yield)
sf_ws$wy_4i_tlpd <- dat_sumValues_rcp45Iprc[,2] / 1000 / 365 * -1
sf_ws$wy_8i_tlpd <- dat_sumValues_rcp85Iprc[,2] / 1000 / 365 * -1
sf_ws$wy_8n_tlpd <- dat_sumValues_rcp85Ncar[,2] / 1000 / 365 * -1

# isolate water yield variables as their own shapefiles
sf_waterYield_rcp45Iprc <- sf_ws[c('Island', 'ws_id', 'wsid', 'km2', 'wy_4i_tlpd', 'geometry')]
sf_waterYield_rcp85Iprc <- sf_ws[c('Island', 'ws_id', 'wsid', 'km2', 'wy_8i_tlpd', 'geometry')]
sf_waterYield_rcp85Ncar <- sf_ws[c('Island', 'ws_id', 'wsid', 'km2', 'wy_8n_tlpd', 'geometry')]

# abbreviate column names for ESRI writing
colnames(sf_waterYield_rcp85Ncar) <- 
  colnames(sf_waterYield_rcp45Iprc) <- colnames(sf_waterYield_rcp85Iprc) <-
  c("Island", "ws_id", "wsid", "km2", "tlpd", "geometry")

# create liters per day per km2
sf_waterYield_rcp85Ncar$tlpd_km2 <- sf_waterYield_rcp85Ncar$tlpd / sf_waterYield_rcp85Ncar$km2
sf_waterYield_rcp45Iprc$tlpd_km2 <- sf_waterYield_rcp45Iprc$tlpd / sf_waterYield_rcp45Iprc$km2
sf_waterYield_rcp85Iprc$tlpd_km2 <- sf_waterYield_rcp85Iprc$tlpd / sf_waterYield_rcp85Iprc$km2

# write shapefiles
st_write(sf_waterYield_rcp85Ncar,
         paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                'water yield by watershed - RCP85 NCAR.shp'),
         delete_layer = TRUE)
st_write(sf_waterYield_rcp85Iprc,
         paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                'water yield by watershed - RCP85 IPRC.shp'),
         delete_layer = TRUE)
st_write(sf_waterYield_rcp45Iprc,
         paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                'water yield by watershed - RCP 45 IPRC.shp'),
         delete_layer = TRUE)

# write rasters
writeRaster(rast_rcp45Iprc_delta,
            paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                   'water yield raster - RCP45 IPRC.tif'),
            overwrite = TRUE)
writeRaster(rast_rcp85Iprc_delta,
            paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                   'water yield raster - RCP85 IPRC.tif'),
            overwrite = TRUE)
writeRaster(rast_rcp85Ncar_delta,
            paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/16a - watershed aggregated results/',
                   'water yield raster - RCP85 NCAR.tif'),
            overwrite = TRUE)
