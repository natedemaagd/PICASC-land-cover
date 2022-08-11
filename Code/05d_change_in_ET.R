
library(raster); library(ggplot2)

# load rasters
r_baseline        <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/05 agroforestry rasters/05_agroforestry_baselineET.tif")
r_futureForest    <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/05 agroforestry rasters/05_agroforestry_predictedFutureET_forestAbove3000m.tif")
r_futureGrassland <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/05 agroforestry rasters/05_agroforestry_predictedFutureET_grasslandAbove3000m.tif")

# calculate difference rasters
r_differenceForest    <- r_futureForest    - r_baseline
r_differenceGrassland <- r_futureGrassland - r_baseline

# calculate ratio rasters
r_ratioForest    <- r_futureForest    / r_baseline
r_ratioGrassland <- r_futureGrassland / r_baseline

# save rasters
writeRaster(r_differenceForest,    filename = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/05 agroforestry rasters/05_agroforestry_predictedChangeET_forestAbove3000m.tif",
            overwrite = TRUE)
writeRaster(r_differenceGrassland, filename = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/05 agroforestry rasters/05_agroforestry_predictedChangeET_grasslandAbove3000m.tif",
            overwrite = TRUE)
writeRaster(r_ratioForest,         filename = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/05 agroforestry rasters/05_agroforestry_predictedChangeETratio_forestAbove3000m.tif",
            overwrite = TRUE)
writeRaster(r_ratioGrassland,      filename = "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/05 agroforestry rasters/05_agroforestry_predictedChangeETratio_grasslandAbove3000m.tif",
            overwrite = TRUE)
