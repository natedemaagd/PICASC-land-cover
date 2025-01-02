
# This script analyzes the overlap between land cover spread and the agroforestry regions

library(sf)
library(raster)
library(tidyverse)




##### load data #####

# load landcover
ras_lc <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/",
                "mhi_s0_baseline_names.tif"))

# load agroforestry regions
ras_af <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Agroforestry and silvopasture restoration/Agroforestry_current_climate/",
                "Oct_2023_MS_currentclimate_landuse_scenario1.tif"))

# load silvopasture regions
ras_sp <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Agroforestry and silvopasture restoration/Silvopasture_current_climate/",
                "restoration_silvopasture_clim_current_target_area.tif"))

# adjust extents
# ras_af_expanded <- resample(ras_af, ras_lc, method = 'ngb')
# writeRaster(ras_af_expanded,
#             filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Agroforestry and silvopasture restoration/Agroforestry_current_climate/",
#                               "Oct_2023_MS_currentclimate_landuse_scenario1_resampledToMatchUpdatedLandcover.tif"),
#             overwrite = TRUE)
ras_af_expanded <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Agroforestry and silvopasture restoration/Agroforestry_current_climate/",
                "Oct_2023_MS_currentclimate_landuse_scenario1_resampledToMatchUpdatedLandcover.tif"))

# ras_sp_expanded <- resample(ras_sp, ras_lc, method = 'ngb')
# writeRaster(ras_sp_expanded,
#             filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Agroforestry and silvopasture restoration/Silvopasture_current_climate/",
#                               "restoration_silvopasture_clim_current_target_area_resampledToMatchUpdatedLandcover.tif"),
#             overwrite = TRUE)
ras_sp_expanded <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Agroforestry and silvopasture restoration/Silvopasture_current_climate/",
                "restoration_silvopasture_clim_current_target_area_resampledToMatchUpdatedLandcover.tif"))




##### analyze overlap #####

# convert to data.frame
dat <-
  data.frame(lc = values(ras_lc),
             af = values(ras_af_expanded),
             sp = values(ras_sp_expanded))
gc()

# keep only pixels where we have agroforestry or silvopasture data (which we know don't overlap)
dat <- dat[!is.na(dat$af) | !is.na(dat$sp),]
gc()

# summarize landcover codes contained within agroforestry regions
tab_af <- as.data.frame(table(dat$lc[!is.na(dat$af)]))
colnames(tab_af) <- c('lcCode', 'numPixels')
tab_af$lcCode <- as.numeric(tab_af$lcCode)
tab_sp <- as.data.frame(table(dat$lc[!is.na(dat$sp)]))
colnames(tab_sp) <- c('lcCode', 'numPixels')
tab_sp$lcCode <- as.numeric(tab_sp$lcCode)

# order by prevalence
tab_af <- tab_af[order(tab_af$numPixels, decreasing = TRUE),]
tab_sp <- tab_sp[order(tab_sp$numPixels, decreasing = TRUE),]

# merge with landcover names
dat_lcCodes <-
  readxl::read_xlsx(path = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/",
                                  "lc codes and names_nativeforest_spread.xlsx"),
                    skip = 1)
dat_lcCodes <- dat_lcCodes[,c('lc', 'name')]
colnames(dat_lcCodes) <- c('lcCode', 'name')
dat_lcCodes <- dat_lcCodes[dat_lcCodes$lcCode %in% c(tab_af$lcCode, tab_sp$lcCode),]
tab_af <- left_join(tab_af, dat_lcCodes, 'lcCode')
tab_sp <- left_join(tab_sp, dat_lcCodes, 'lcCode')

# export table
write.csv(tab_af,
          file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                        'agroforestry and original land cover overlap.csv'),
          row.names = FALSE)
write.csv(tab_sp,
          file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                        'silvopasture and original land cover overlap.csv'),
          row.names = FALSE)
