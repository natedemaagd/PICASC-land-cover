
# This script resamples the AllVarsMean spreadsheet variables (from Tom G's rasters)
# for use with the 30m landcover data used in this project, for use with the 
# water yield function in later scripts.

library(raster)

setwd('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11a - Tom G rasters resampled')





##### load data #####

# define landcovers whose water yield needs to be modeled - taken from all simulation and landcover assignment scripts
vec1 <- c(100, 200, 300, 400, 500, 10100, 10200, 10300, 10400, 10500, 10600,
          600, 700, 800, 1500, 10700, 10800, 10900, 11700)
vec2 <- c(1600, 1700, 1900, 2000, 11900, 12000)
vec3 <- c(1600, 1700, 1800, 1900, 2000, 2100,
          3700, 3800, 3900, 11800, 11900, 12000, 12100,
          13700, 13800, 13900, 14100)
vec4 <- c(1900, 2000, 2100)
vec5 <- c(3700, 3800, 3900)
vec6 <- c(100, 600, 1100)
vec7 <- c(100, 200, 300, 400, 500, 1600, 1900, 2400, 2500, 3100,
          3300, 3700)
vec8 <- c(600, 700, 800, 900, 1000, 1500, 1700, 2000, 2600, 3200,
          3400, 3800)
vec9 <- c(1100, 1200, 1300, 1400, 1800, 2100, 2200, 2700, 2800, 3500, 3900)

# keep only unique landcover types
vec_landcovers <- unique(c(vec1, vec2, vec3, vec4, vec5, vec6, vec7, vec8, vec9))
rm(vec1, vec2, vec3, vec4, vec5, vec6, vec7, vec8, vec9)

# load raster data (AllVarsMean)
dat_allVars <-
  readxl::read_xlsx("D:/OneDrive - hawaii.edu/Documents/Projects/DWS/Land_cover/Data/Raw/AllVars_MeanAnnVals.xlsx",
                    sheet = 1)

# load landcover raster
ras_landcover <-
  raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/mhi_s0_baseline_names.tif")
# ras_landcover_reproject <-
#   projectRaster(ras_landcover,
#                 crs = '+proj=longlat +zone=4 +datum=NAD83 +units=m +no_defs',
#                 method = 'ngb')
# writeRaster(ras_landcover_reproject,
#             filename = 'ras_landcover_reproject.tif', overwrite = TRUE)
# ras_landcover_reproject <-
#   raster('ras_landcover_reproject.tif')
gc()



##### convert dat_allVars to rasters and resample to match ras_landcover #####

# convert each required variable from data.frame to raster
ras_AET <-
  rasterFromXYZ(dat_allVars[c('POINT_X', 'POINT_Y', 'AET')],
                crs = '+proj=longlat +zone=4 +datum=NAD83 +units=m +no_defs')
ras_LAI <-
  rasterFromXYZ(dat_allVars[c('POINT_X', 'POINT_Y', 'LAI')],
                crs = '+proj=longlat +zone=4 +datum=NAD83 +units=m +no_defs')
ras_SM <-
  rasterFromXYZ(dat_allVars[c('POINT_X', 'POINT_Y', 'SM')],
                crs = '+proj=longlat +zone=4 +datum=NAD83 +units=m +no_defs')
ras_U <-
  rasterFromXYZ(dat_allVars[c('POINT_X', 'POINT_Y', 'U')],
                crs = '+proj=longlat +zone=4 +datum=NAD83 +units=m +no_defs')
ras_T <-
  rasterFromXYZ(dat_allVars[c('POINT_X', 'POINT_Y', 'T')],
                crs = '+proj=longlat +zone=4 +datum=NAD83 +units=m +no_defs')
ras_Rnet <-
  rasterFromXYZ(dat_allVars[c('POINT_X', 'POINT_Y', 'Rnet')],
                crs = '+proj=longlat +zone=4 +datum=NAD83 +units=m +no_defs')
ras_PT <-
  rasterFromXYZ(dat_allVars[c('POINT_X', 'POINT_Y', 'PT')],
                crs = '+proj=longlat +zone=4 +datum=NAD83 +units=m +no_defs')

# reproject from longlat to utm
ras_AET <-
  projectRaster(ras_AET, crs = '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
                method = 'ngb')
ras_LAI <-
  projectRaster(ras_LAI, crs = '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
                method = 'ngb')
ras_SM <-
  projectRaster(ras_SM, crs = '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
                method = 'ngb')
ras_U <-
  projectRaster(ras_U, crs = '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
                method = 'ngb')
ras_T <-
  projectRaster(ras_T, crs = '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
                method = 'ngb')
ras_Rnet <-
  projectRaster(ras_Rnet, crs = '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
                method = 'ngb')
ras_PT <-
  projectRaster(ras_PT, crs = '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
                method = 'ngb')
gc()

# resample rasters to match landcover raster
ras_AET <-
  raster::resample(x = ras_AET, y = ras_landcover,
                   method = 'ngb',
                   filename = 'ras_AET_resampled.tif',
                   overwrite = TRUE)
ras_LAI <-
  raster::resample(x = ras_LAI, y = ras_landcover,
                   method = 'ngb',
                   filename = 'ras_LAI_resampled.tif',
                   overwrite = TRUE)
ras_SM <-
  raster::resample(x = ras_SM, y = ras_landcover,
                   method = 'ngb',
                   filename = 'ras_SM_resampled.tif',
                   overwrite = TRUE)
ras_U <-
  raster::resample(x = ras_U, y = ras_landcover,
                   method = 'ngb',
                   filename = 'ras_U_resampled.tif',
                   overwrite = TRUE)
ras_T <-
  raster::resample(x = ras_T, y = ras_landcover,
                   method = 'ngb',
                   filename = 'ras_T_resampled.tif',
                   overwrite = TRUE)
ras_Rnet <-
  raster::resample(x = ras_Rnet, y = ras_landcover,
                   method = 'ngb',
                   filename = 'ras_Rnet_resampled.tif',
                   overwrite = TRUE)
ras_PT <-
  raster::resample(x = ras_PT, y = ras_landcover,
                   method = 'ngb',
                   filename = 'ras_PT_resampled.tif',
                   overwrite = TRUE)
gc()




##### reproject rainfall atlas mean ann rainfall #####

# load rainfall raster
ras_rainfall <-
  raster(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_ann_rainfall_inches/Hawaii-State/",
                "rfgrid_inches_state_ann.txt"))

# reproject from longlat to utm
ras_rainfall <-
  projectRaster(ras_rainfall, crs = '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
                method = 'ngb')

# convert from inches to mm
ras_rainfall <- ras_rainfall * 25.4

# resample to match landcover raster
ras_rainfall <-
  raster::resample(x = ras_rainfall, y = ras_landcover,
                   method = 'ngb',
                   filename = 'ras_rainfall_resampled.tif',
                   overwrite = TRUE)
gc()




##### convert rasters into data.frame #####

# create data.frame from all rasters
dat_allVars_reproject <-
  as.data.frame(ras_landcover, xy = TRUE)
gc()
colnames(dat_allVars_reproject) <- c('x', 'y', 'landcover')
dat_allVars_reproject$landcover <- values(ras_landcover)
dat_allVars_reproject$AET       <- values(ras_AET);      gc()
dat_allVars_reproject$LAI       <- values(ras_LAI);      gc()
dat_allVars_reproject$SM        <- values(ras_SM);       gc()
dat_allVars_reproject$U         <- values(ras_U);        gc()
dat_allVars_reproject$T         <- values(ras_T);        gc()
dat_allVars_reproject$Rnet      <- values(ras_Rnet);     gc()
dat_allVars_reproject$PT        <- values(ras_PT);       gc()
dat_allVars_reproject$rainfall  <- values(ras_rainfall); gc()

dat_allVars_reproject <-
  dat_allVars_reproject[c("x", "y", "landcover", "AET", "LAI", "SM", "U", "T",
                          "Rnet", "PT", "rainfall")]
gc()

# # keep only non-NA pixels and non-ocean pixels
# dat_allVars_reproject_cleaned <-
#   dat_allVars_reproject[!is.na(dat_allVars_reproject$landcover),]
# dat_allVars_reproject_cleaned <-
#   dat_allVars_reproject_cleaned[dat_allVars_reproject_cleaned$landcover != 65535,]
# gc()

saveRDS(dat_allVars_reproject,
        file = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11a - Tom G rasters resampled/",
                      "ras_data_combined_df.rds"))
