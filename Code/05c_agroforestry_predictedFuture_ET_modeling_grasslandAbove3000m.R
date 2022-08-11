
library(rgdal); library(raster); library(doParallel); library(sp); library(ggplot2); library(lfe); library(nlme)
registerDoParallel(cores = 2)

# load data
load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/01_regression_data_setup.Rdata')
reg_LaiRain <- readRDS("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/04_LAI_rainfall_regression.rds")
datSp <- dat_sp; rm(dat_sp)

# for each landcover, get median LAI
median_LAI <- aggregate(dat$LAI, list(dat$LC), median, na.rm = TRUE)
colnames(median_LAI) <- c('LC', 'LAI_median')

# define regression equations
reg_eqn <- formula(AET ~ LAI + SM + U + T + Rnet)
error_corr <- formula( ~ POINT_X + POINT_Y)

# load regressions
reg_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions', pattern = '.rds')
reg_data <- lapply(reg_filenames, function(d) readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions/', d)))

# get landcover code for each regression
reg_landcoverCode <- gsub('.*-([0-9]+).*', '\\1', substr(reg_filenames, 3, nchar(reg_filenames)))
reg_landcoverCode <- as.numeric(stringr::str_extract(reg_landcoverCode, "[[:digit:]]+"))




##### data setup #####

# load future LC
raster_futureScenario <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/futureagroforestscenario_grassland_snapped_above3000m/futureagroforestscenario_grassland_snapped_above3000m1.tif")

# load resampled climate variable rasters
rasters_climate_names <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/03_agroforestry/resampled allvars rasters', pattern = '.tif')
rasters_climate <- lapply(rasters_climate_names, function(r) raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/03_agroforestry/resampled allvars rasters/', r)))
names(rasters_climate) <- rasters_climate_names
rm(rasters_climate_names)
rasters_climate <- c(rasters_climate, raster_futureScenario)  # add future LC
rm(raster_futureScenario)
names(rasters_climate) <- c('AET', 'LAI', 'Rnet', 'SM', 'T', 'U', 'LC')

# create stack of rasters
rasterStack_futureScenario <- raster::stack(rasters_climate)
names(rasterStack_futureScenario) <- c('AET', 'LAI', 'Rnet', 'SM', 'T', 'U', 'LC')
rm(rasters_climate)




##### future ET - forest above 3000m #####

rm(dat, dat_by_moku, datSp, reg_LaiRain)
gc()

# unique landcover codes to model
lc_toModel <- unique(rasterStack_futureScenario$LC)

# for each landcover type, run model
ET_predicted <- list()
for(i in 1:length(lc_toModel)){
  
  # get landcover code
  lc_code <- lc_toModel[[i]]
  
  # create new data where all LCs set to NA except landcover i
  model_dat <- rasterStack_futureScenario
  model_dat$LC[model_dat$LC != lc_code] <- NA
  names(model_dat) <- names(rasterStack_futureScenario)
  
  # replace LAI with the median for that landcover
  model_dat$LAI <- median_LAI[median_LAI$LC == lc_code, 'LAI_median']
  
  # get ET predicted values
  ET_predicted[[i]] <- predict(object = model_dat, model = reg_data[reg_landcoverCode == lc_code][[1]])
  
  gc()
  
}

names(ET_predicted) <- lc_toModel

# merge all ET predictions into one raster
ET_predicted <- Reduce(function(x, y) merge(x, y, all=TRUE), ET_predicted)

# lowest ET can be is 0
ET_predicted[ET_predicted < 0 & !is.na(ET_predicted)] <- 0

# save predicted ET raster
writeRaster(ET_predicted, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/05 agroforestry rasters/05_agroforestry_predictedFutureET_grasslandAbove3000m.tif',
            overwrite = TRUE)
