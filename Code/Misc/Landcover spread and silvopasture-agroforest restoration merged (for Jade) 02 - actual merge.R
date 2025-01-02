
# This script merges the worst and middle case land cover scenarios with the
# agroforest/silvopasture restoration scenarios for Leah and Jade.

library(terra)




##### load rasters #####

# load best, middle, and worst case land cover rasters
ras_lcBest2070_dummy <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
              "best case 2070.tif"))
ras_lcMiddle2070_dummy <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
              "middle case 2070.tif"))
ras_lcWorst2070_dummy <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/09c - final rasters with dummy values/",
              "worst case 2070.tif"))
ras_lcBest2070 <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
              "best case 2070 - moisture zones consistent w original raster - finalizedV2.tif"))
ras_lcMiddle2070 <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
              "middle case 2070 - moisture zones consistent w original raster - finalizedV2.tif"))
ras_lcWorst2070 <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/",
              "worst case 2070 - moisture zones consistent w original raster - finalizedV2.tif"))

# load agroforestry and silvopasture rasters - current climate
ras_agroforestry_currentClimate <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Agroforestry and silvopasture restoration/Agroforestry_current_climate/",
              "Oct_2023_MS_currentclimate_landuse_scenario1_resampledToMatchUpdatedLandcover.tif"))
ras_silvopasture_currentClimate <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Agroforestry and silvopasture restoration/Silvopasture_current_climate/",
              "restoration_silvopasture_clim_current_target_area_resampledToMatchUpdatedLandcover.tif"))

# load agroforestry and silvopasture rasters - future climate
ras_agroforestry_futureClimate <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Agroforestry and silvopasture restoration/Agroforestry_future_climate/",
              "Oct_2023_MS_RCP85_landusescenario1.tif"))
ras_silvopasture_futureClimate <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Agroforestry and silvopasture restoration/Silvopasture_future_climate/",
              "silvopasture_RCP851.tif"))




##### reformat rasters as necessary #####

# resample future rasters (missing extent)
ras_agroforestry_futureClimate <-
  resample(ras_agroforestry_futureClimate, ras_lcMiddle2070_dummy)
ras_silvopasture_futureClimate <-
  resample(ras_silvopasture_futureClimate, ras_lcMiddle2070_dummy)
gc()




##### create unique values #####

values(ras_agroforestry_currentClimate) <- values(ras_agroforestry_currentClimate) * - 10
values(ras_agroforestry_futureClimate)  <- values(ras_agroforestry_futureClimate)  * - 10
values(ras_silvopasture_currentClimate) <- values(ras_silvopasture_currentClimate) * -100
values(ras_silvopasture_futureClimate)  <- values(ras_silvopasture_futureClimate)  * -100
gc()



##### overlay af and sp rasters on lc #####

# create new rasters for overlay
ras_lcBest2070_merged_currentClimate_dummy <- ras_lcBest2070_dummy
ras_lcMiddle2070_merged_currentClimate_dummy <- ras_lcMiddle2070_dummy
ras_lcWorst2070_merged_currentClimate_dummy  <- ras_lcWorst2070_dummy
ras_lcBest2070_merged_futureClimate_dummy <- ras_lcBest2070_dummy
ras_lcMiddle2070_merged_futureClimate_dummy <- ras_lcMiddle2070_dummy
ras_lcWorst2070_merged_futureClimate_dummy  <- ras_lcWorst2070_dummy
ras_lcBest2070_merged_currentClimate <- ras_lcBest2070
ras_lcMiddle2070_merged_currentClimate <- ras_lcMiddle2070
ras_lcWorst2070_merged_currentClimate  <- ras_lcWorst2070
ras_lcBest2070_merged_futureClimate <- ras_lcBest2070
ras_lcMiddle2070_merged_futureClimate <- ras_lcMiddle2070
ras_lcWorst2070_merged_futureClimate  <- ras_lcWorst2070
gc()

# overlay rasters by replacing respective raster cell values - dummy rasters
ras_lcBest2070_merged_currentClimate_dummy[ras_agroforestry_currentClimate == -10] <- -10
ras_lcBest2070_merged_currentClimate_dummy[ras_agroforestry_currentClimate == -20] <- -20
ras_lcBest2070_merged_currentClimate_dummy[ras_agroforestry_currentClimate == -30] <- -30
ras_lcMiddle2070_merged_currentClimate_dummy[ras_agroforestry_currentClimate == -10] <- -10
ras_lcMiddle2070_merged_currentClimate_dummy[ras_agroforestry_currentClimate == -20] <- -20
ras_lcMiddle2070_merged_currentClimate_dummy[ras_agroforestry_currentClimate == -30] <- -30
ras_lcWorst2070_merged_currentClimate_dummy[ras_agroforestry_currentClimate == -10] <- -10
ras_lcWorst2070_merged_currentClimate_dummy[ras_agroforestry_currentClimate == -20] <- -20
ras_lcWorst2070_merged_currentClimate_dummy[ras_agroforestry_currentClimate == -30] <- -30
ras_lcBest2070_merged_futureClimate_dummy[ras_agroforestry_futureClimate == -10] <- -10
ras_lcBest2070_merged_futureClimate_dummy[ras_agroforestry_futureClimate == -20] <- -20
ras_lcBest2070_merged_futureClimate_dummy[ras_agroforestry_futureClimate == -30] <- -30
ras_lcMiddle2070_merged_futureClimate_dummy[ras_agroforestry_futureClimate == -10] <- -10
ras_lcMiddle2070_merged_futureClimate_dummy[ras_agroforestry_futureClimate == -20] <- -20
ras_lcMiddle2070_merged_futureClimate_dummy[ras_agroforestry_futureClimate == -30] <- -30
ras_lcWorst2070_merged_futureClimate_dummy[ras_agroforestry_futureClimate == -10] <- -10
ras_lcWorst2070_merged_futureClimate_dummy[ras_agroforestry_futureClimate == -20] <- -20
ras_lcWorst2070_merged_futureClimate_dummy[ras_agroforestry_futureClimate == -30] <- -30

ras_lcBest2070_merged_currentClimate_dummy[ras_silvopasture_currentClimate == -100] <- -100
ras_lcMiddle2070_merged_currentClimate_dummy[ras_silvopasture_currentClimate == -100] <- -100
ras_lcWorst2070_merged_currentClimate_dummy[ras_silvopasture_currentClimate == -100] <- -100
ras_lcBest2070_merged_futureClimate_dummy[ras_silvopasture_futureClimate == -100] <- -100
ras_lcMiddle2070_merged_futureClimate_dummy[ras_silvopasture_futureClimate == -100] <- -100
ras_lcWorst2070_merged_futureClimate_dummy[ras_silvopasture_futureClimate == -100] <- -100
gc()

# overlay rasters by replacing respective raster cell values - rasters w/ land cover values
ras_lcBest2070_merged_currentClimate[ras_agroforestry_currentClimate == -10] <- -10
ras_lcBest2070_merged_currentClimate[ras_agroforestry_currentClimate == -20] <- -20
ras_lcBest2070_merged_currentClimate[ras_agroforestry_currentClimate == -30] <- -30
ras_lcMiddle2070_merged_currentClimate[ras_agroforestry_currentClimate == -10] <- -10
ras_lcMiddle2070_merged_currentClimate[ras_agroforestry_currentClimate == -20] <- -20
ras_lcMiddle2070_merged_currentClimate[ras_agroforestry_currentClimate == -30] <- -30
ras_lcWorst2070_merged_currentClimate[ras_agroforestry_currentClimate == -10] <- -10
ras_lcWorst2070_merged_currentClimate[ras_agroforestry_currentClimate == -20] <- -20
ras_lcWorst2070_merged_currentClimate[ras_agroforestry_currentClimate == -30] <- -30
ras_lcBest2070_merged_futureClimate[ras_agroforestry_futureClimate == -10] <- -10
ras_lcBest2070_merged_futureClimate[ras_agroforestry_futureClimate == -20] <- -20
ras_lcBest2070_merged_futureClimate[ras_agroforestry_futureClimate == -30] <- -30
ras_lcMiddle2070_merged_futureClimate[ras_agroforestry_futureClimate == -10] <- -10
ras_lcMiddle2070_merged_futureClimate[ras_agroforestry_futureClimate == -20] <- -20
ras_lcMiddle2070_merged_futureClimate[ras_agroforestry_futureClimate == -30] <- -30
ras_lcWorst2070_merged_futureClimate[ras_agroforestry_futureClimate == -10] <- -10
ras_lcWorst2070_merged_futureClimate[ras_agroforestry_futureClimate == -20] <- -20
ras_lcWorst2070_merged_futureClimate[ras_agroforestry_futureClimate == -30] <- -30

ras_lcBest2070_merged_currentClimate[ras_silvopasture_currentClimate == -100] <- -100
ras_lcMiddle2070_merged_currentClimate[ras_silvopasture_currentClimate == -100] <- -100
ras_lcWorst2070_merged_currentClimate[ras_silvopasture_currentClimate == -100] <- -100
ras_lcBest2070_merged_futureClimate[ras_silvopasture_futureClimate == -100] <- -100
ras_lcMiddle2070_merged_futureClimate[ras_silvopasture_futureClimate == -100] <- -100
ras_lcWorst2070_merged_futureClimate[ras_silvopasture_futureClimate == -100] <- -100
gc()



##### write rasters #####

# write rasters - dummy
writeRaster(ras_lcBest2070_dummy,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_best_noAgroSilvo_current_dummy.tif'),
            overwrite = TRUE)
writeRaster(ras_lcMiddle2070_dummy,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_middle_noAgroSilvo_current_dummy.tif'),
            overwrite = TRUE)
writeRaster(ras_lcWorst2070_dummy,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_worst_noAgroSilvo_current_dummy.tif'),
            overwrite = TRUE)

writeRaster(ras_lcBest2070_merged_currentClimate_dummy,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_best_agroSilvo_current_dummy.tif'),
            overwrite = TRUE)
writeRaster(ras_lcMiddle2070_merged_currentClimate_dummy,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_middle_agroSilvo_current_dummy.tif'),
            overwrite = TRUE)
writeRaster(ras_lcWorst2070_merged_currentClimate_dummy,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_worst_agroSilvo_current_dummy.tif'),
            overwrite = TRUE)

writeRaster(ras_lcBest2070_merged_futureClimate_dummy,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_best_agroSilvo_future_dummy.tif'),
            overwrite = TRUE)
writeRaster(ras_lcMiddle2070_merged_futureClimate_dummy,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_middle_agroSilvo_future_dummy.tif'),
            overwrite = TRUE)
writeRaster(ras_lcWorst2070_merged_futureClimate_dummy,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_worst_agroSilvo_future_dummy.tif'),
            overwrite = TRUE)

gc()

# write rasters - actual land cover values
writeRaster(ras_lcBest2070,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_best_noAgroSilvo_current.tif'),
            overwrite = TRUE)
writeRaster(ras_lcMiddle2070,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_middle_noAgroSilvo_current.tif'),
            overwrite = TRUE)
writeRaster(ras_lcWorst2070,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_worst_noAgroSilvo_current.tif'),
            overwrite = TRUE)

writeRaster(ras_lcBest2070_merged_currentClimate,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_best_agroSilvo_current.tif'),
            overwrite = TRUE)
writeRaster(ras_lcMiddle2070_merged_currentClimate,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_middle_agroSilvo_current.tif'),
            overwrite = TRUE)
writeRaster(ras_lcWorst2070_merged_currentClimate,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_worst_agroSilvo_current.tif'),
            overwrite = TRUE)

writeRaster(ras_lcBest2070_merged_futureClimate,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_best_agroSilvo_future.tif'),
            overwrite = TRUE)
writeRaster(ras_lcMiddle2070_merged_futureClimate,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_middle_agroSilvo_future.tif'),
            overwrite = TRUE)
writeRaster(ras_lcWorst2070_merged_futureClimate,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/Misc/Silvopasture-agroforest restoration/',
                              'ras_worst_agroSilvo_future.tif'),
            overwrite = TRUE)

gc()
