
library(terra); library(doParallel); library(sf); library(ggplot2); library(lfe); library(nlme)
#registerDoParallel(cores = 2)

# load data
load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/01_regression_data_setup.Rdata')
reg_LaiRain <- readRDS("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/04_LAI_rainfall_regression.rds")
reg_PtRain <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions/02_regressions lm PtRain all landcovers.rds')
datSp <- dat_sp; rm(dat_sp)
rm(dat_moku, moku)




##### replace LAI with median LAI for the landcover #####

# for each landcover, get median LAI
median_LAI <- aggregate(dat$LAI, list(dat$LC), median, na.rm = TRUE)
colnames(median_LAI) <- c('LC', 'LAI_median')




##### regression setup #####

# define regression equations
reg_eqn <- formula(AET ~ PT + rain_ann_in)
# error_corr <- formula( ~ POINT_X + POINT_Y)

# load regressions
reg_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions', pattern = '_PtSm.rds')
# reg_data <- lapply(reg_filenames, function(d) readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions/', d)))

# get landcover code for each regression
reg_landcoverCode <- gsub('.*-([0-9]+).*', '\\1', substr(reg_filenames, 3, nchar(reg_filenames)))
reg_landcoverCode <- stringr::str_extract(reg_landcoverCode, "[[:digit:]]+")

gc()




##### data setup #####

# load baseline LC
raster_baseline <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/baselineagroforestry/baselineagroforestry_snapped1.tif")

# load resampled climate variable rasters
rasters_climate_names <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/03_agroforestry/resampled allvars rasters', pattern = '.tif')
rasters_climate <- lapply(rasters_climate_names, function(r) rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/03_agroforestry/resampled allvars rasters/', r)))
names(rasters_climate) <- rasters_climate_names
rm(rasters_climate_names)
rasters_climate <- c(rasters_climate, raster_baseline)  # add baseline LC
rm(raster_baseline)
names(rasters_climate) <- c('AET', 'LAI', 'Rnet', 'SM', 'T', 'U', 'LC')

# create stack of rasters
rasterStack_baseline <- rast(rasters_climate)
names(rasterStack_baseline) <- c('AET', 'LAI', 'Rnet', 'SM', 'T', 'U', 'LC')
rm(rasters_climate)

# add PT and rain
rast_pt <- rast("H:/My Drive/Projects/Data/rainfall_atlas_PriestlyTaylorPotentialET/pr0_mm_ann/w001001.adf")
names(rast_pt) <- 'PT'
rast_pt <- project(rast_pt, rasterStack_baseline, threads = TRUE)
rast_pt <- resample(rast_pt, rasterStack_baseline, threads = TRUE)
rasterStack_baseline <- c(rasterStack_baseline, rast_pt)
rast_rain <- rast("H:/My Drive/Projects/Data/rainfall_atlas_ann_rainfall_inches/Hawaii-State/rfgrid_inches_state_ann.txt")
names(rast_rain) <- 'rain_ann_in'
rast_rain <- project(rast_rain, rasterStack_baseline, threads = TRUE)
rast_rain <- resample(rast_rain, rasterStack_baseline, threads = TRUE)
rasterStack_baseline <- c(rasterStack_baseline, rast_rain)
rm(rast_pt, rast_rain)
gc()




##### baseline ET #####

rm(dat, datSp, reg_LaiRain)
gc()

# unique landcover codes to model
lc_toModel <- unique(rasterStack_baseline$LC)

# for each landcover type, run model
ET_predicted <- list()
for(i in 1:length(reg_PtRain)){
  
  # get landcover code
  lc_code <- as.numeric(names(reg_PtRain)[[i]])
  
  # create new data where all LCs set to NA except landcover i
  model_dat <- rasterStack_baseline
  model_dat$LC[model_dat$LC != lc_code] <- NA
  model_dat$PT[is.na(model_dat$LC)] <- NA
  names(model_dat) <- names(rasterStack_baseline)
  gc()
  
  # # replace LAI with the median for that landcover
  # model_dat$LAI <- median_LAI[median_LAI$LC == lc_code, 'LAI_median']
  
  # get ET predicted values
  ET_predicted[[i]] <- predict(object = model_dat, model = reg_PtRain[names(reg_PtRain) == lc_code][[1]])
  
  gc()
  
}

names(ET_predicted) <- names(reg_PtRain)

# merge all ET predictions into one raster
ET_predicted2 <- sprc(ET_predicted)
ET_predicted2 <- merge(ET_predicted2)
gc()

# lowest ET can be is 0
ET_predicted2[ET_predicted2 < 0 & !is.na(ET_predicted2)] <- 0

# save baseline ET raster
writeRaster(ET_predicted2, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/05 agroforestry rasters/05_agroforestry_baselineET.tif',
            overwrite = TRUE)
