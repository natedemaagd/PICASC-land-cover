
# this script finds forest-dominant pixels with positive grass growth and finds
# spread rate

library(raster); library(ggplot2); library(viridis); library(sf)
library(doParallel)
registerDoParallel(cores = 4)

set.seed(42)

setwd("H:/My Drive/Projects/PICASC Land-to-sea/Data/")

# load landcover data
ras_wood1999 <- raster("Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_wood_1999.tif")
ras_grass1999 <- raster("Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_herb_1999.tif")
ras_grass2016 <- raster("Processed/Fire/interpolated_yearly_landcover_percentages/HI_FracLC_herb_2016.tif")

# load and combine mean annual fire risk rasters
list_fireRasters <- list.files('Processed/Fire/prediction_rasters_mean/prediction rasters mean by season',
                               pattern = 'annual.tif', full.names = TRUE)
list_fireRasters <- lapply(list_fireRasters, raster)
ras_meanFireRisk <- do.call(merge, list_fireRasters)
rm(list_fireRasters)
gc()

# load annual rainfall raster
ras_annualRainfall_mm <- raster("Raw/2021_HI_FIre_Model_Data/rainfall_ann/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")

# load fire shapefile
sf_fires <- read_sf("Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters/2019_1999_Hawaii_Fire_Perimeters.shp")




##### create rasters for analysis #####

# define what it means to have a dominant landcover
val_dominant <- 0.6

# create woody-dominant raster
ras_woodDom1999 <- ras_wood1999
ras_woodDom1999[ras_woodDom1999 >= val_dominant] <- 1
ras_woodDom1999[ras_woodDom1999 < val_dominant] <- NA
gc()

# create 1999 grass-dominant raster
ras_grassDom1999 <- ras_grass1999
ras_grassDom1999[ras_grassDom1999 >= val_dominant] <- 1
ras_grassDom1999[ras_grassDom1999 < val_dominant] <- NA
gc()

# create change in grass raster
ras_grassChange <- ras_grass2016 - ras_grass1999

# mask grass change raster with woody-dominant raster
ras_grassChange_woodDom <- mask(ras_grassChange,
                                ras_woodDom1999)
gc()

# in masked raster, keep only pixels with positive change in grass cover
ras_grassPositiveChange_woodDom <- ras_grassChange_woodDom
ras_grassPositiveChange_woodDom[ras_grassPositiveChange_woodDom <= 0] <- NA
gc()

# resample fire raster to match landcover rasters
ras_meanFireRisk <- resample(ras_meanFireRisk, ras_grassChange_woodDom)
gc()

# resample rain raster to match landcover rasters
ras_annualRainfall_mm <- resample(ras_annualRainfall_mm, ras_grassChange_woodDom)
gc()

# save grass dominant and grass change rasters
writeRaster(ras_grassDom1999,
            filename = 'Intermediate/Misc/Grassland/02 V2 grass spread as fcn of fire probability/grassDom1999.tif',
            overwrite = TRUE)
writeRaster(ras_grassPositiveChange_woodDom,
            filename = 'Intermediate/Misc/Grassland/02 V2 grass spread as fcn of fire probability/grassPosChange_woodDom1999.tif',
            overwrite = TRUE)




##### sample grass growth points and add to data.frame list #####

# set number of samples to be drawn and sample size
n_samples <- 50
sample_size <- 100

# get grass change pixel indices that are not NA
vec_nonNA_grassChange <- which(!is.na(values(ras_grassPositiveChange_woodDom)))
gc()

# sample pixels: get change in grass cover, fire risk, whether fire occurred, and distance to closest grass-dominant pixel
dat_pixelSamples <-
  foreach(s = 1:n_samples,
          .packages = c('raster', 'sf'),
          .combine = 'rbind') %dopar% {
            
            # determine which pixels to sample
            pixel_sample <- sample(vec_nonNA_grassChange,
                                   size = sample_size)
            
            # create raster that keeps only sampled pixels
            ras_sample <- ras_grassPositiveChange_woodDom
            ras_sample[pixel_sample] <- 9999     # dummy values have to temporarily be something other than 1 since
            ras_sample[ras_sample != 9999] <- NA # 1 means 0-100 grass change
            gc()
            
            # next sample should exclude pixels sampled this time
            vec_nonNA_grassChange <-
              vec_nonNA_grassChange[!(vec_nonNA_grassChange %in% pixel_sample)]
            
            # get sampled grass growth pixel values
            vec_grassChangeSample <- ras_grassPositiveChange_woodDom[pixel_sample]
            
            # get sampled rainfall pixels
            vec_RainfallSample <- ras_annualRainfall_mm[pixel_sample]
            
            # get sampled fire risk pixel values
            vec_fireRiskSample <- ras_meanFireRisk[pixel_sample]
            
            # get distance of each sample pixel to nearest grass-dominated pixel
            dat_sample <- as.data.frame(ras_sample,
                                        xy = TRUE, na.rm = TRUE)
            colnames(dat_sample) <- c('dummy', 'x', 'y')
            dat_sample <- dat_sample[!duplicated(dat_sample[c('x', 'y')]),]
            gc()
            dat_grassDom <- as.data.frame(ras_grassDom1999,
                                          xy = TRUE, na.rm = TRUE)
            colnames(dat_grassDom) <- c('grassDom1999Dummy', 'x', 'y')
            dat_grassDom$grassDom1999Dummy <- 1  # change dummy value back to 1 from 9999
            gc()
            vec_closestPixel_distance <- list()
            for(i in 1:nrow(dat_sample)){
              vec_closestPixel_distance[[i]] <-
                sqrt((dat_sample$x[[i]] - dat_grassDom$x)^2 +
                       (dat_sample$y[[i]] - dat_grassDom$y)^2)
            }
            gc()
            vec_closestPixel_distance <-
              sapply(vec_closestPixel_distance, min)
            
            # did a fire ever happen in these pixels?
            sf_sample <- st_as_sf(dat_sample,
                                  coords = c('x', 'y'), remove = FALSE)  # create sf object of sample's coordinates
            st_crs(sf_sample) <- st_crs(sf_fires)  # match CRSs
            dat_sampleBurned <- st_join(sf_sample, sf_fires)  # join data to see which points are within burn perimeters
            dat_sampleBurned <- dat_sampleBurned[!duplicated(dat_sampleBurned[c('x', 'y')]),]  # remove duplicate pixels (i.e. fire occurred in same pixel more than once)
            vec_burned <- ifelse(!is.na(dat_sampleBurned$UH_ID), 1, 0)
            
            # create data.frame of collected data and add to results
            dat_final <- data.frame(x = dat_sample$x,
                                    y = dat_sample$y,
                                    changeInGrassProportion = vec_grassChangeSample,
                                    fireRiskMean = vec_fireRiskSample,
                                    distClosestGrassDom = vec_closestPixel_distance,
                                    fireHistory = vec_burned,
                                    annualRainfall_mm = vec_RainfallSample)
            rm(pixel_sample, ras_sample, vec_grassChangeSample, vec_fireRiskSample,
               dat_sample, dat_grassDom, vec_closestPixel_distance, sf_sample,
               dat_sampleBurned, vec_burned, vec_RainfallSample)
            gc()
            return(dat_final)
            
          }




##### analysis #####

# scatterplot - fire risk
ggplot(data = dat_pixelSamples,
       aes(x = fireRiskMean, y = changeInGrassProportion)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = 'gam') +
  labs(x = 'Mean fire risk', y = 'Change in grass (proportion, 1999 — 2016)') +
  theme(text = element_text(size = 15))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Misc/Grassland/',
                         '02 V2 grass spread as fcn of fire probability - forest dominant to grass - fire risk.png'),
       dpi = 300, height = 5, width = 9)

# scatterplot - annual rainfall
ggplot(data = dat_pixelSamples,
       aes(x = annualRainfall_mm, y = changeInGrassProportion)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = 'gam') +
  labs(x = 'Mean annual rainfall (mm)', y = 'Change in grass (proportion, 1999 — 2016)') +
  theme(text = element_text(size = 15))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Misc/Grassland/',
                         '02 V2 grass spread as fcn of fire probability - forest dominant to grass - mean rainfall.png'),
       dpi = 300, height = 5, width = 9)

# binomial glm - fire risk
reg_glm_fireRisk <- glm(formula = changeInGrassProportion ~ fireRiskMean + fireHistory + distClosestGrassDom,
                        data = dat_pixelSamples,
                        family = 'binomial')

# binomial glm - annual rainfall
reg_glm_rainfall <- glm(formula = changeInGrassProportion ~ annualRainfall_mm + fireHistory + distClosestGrassDom,
                        data = dat_pixelSamples,
                        family = 'binomial')

save.image(file = paste0('Intermediate/Misc/Grassland/02 V2 grass spread as fcn of fire probability/',
                         '02 V2 grass spread as fcn of fire probability results.Rdata'))

load(paste0('Intermediate/Misc/Grassland/02 V2 grass spread as fcn of fire probability/',
            '02 V2 grass spread as fcn of fire probability results.Rdata'))

# predicted values of change in grass cover for range of fire risks
dat_predVals <- data.frame(fireRiskMean = rep(seq(0, 0.1, length.out = 1000),  # create data.frame of sample values for each covariate
                                              each = 3),
                           annualRainfall_mm = rep(seq(0, 10000, length.out = 1000),
                                              each = 3),
                           fireHistory = c(0, 0, 0, 1, 1, 1),
                           distClosestGrassDom = quantile(dat_pixelSamples$distClosestGrassDom,
                                                          probs = c(0.25, 0.50, 0.75)),
                           model = c('No fire history - lower quartile grass distance',
                                     'No fire history - median grass distance',
                                     'No fire history - upper quartile grass distance',
                                     'Fire history - lower quartile grass distance',
                                     'Fire history - median grass distance',
                                     'Fire history - upper quartile grass distance'))  # quartiles of sampled data
dat_predVals$model <- factor(dat_predVals$model,
                             levels = c('No fire history - lower quartile grass distance',
                                        'No fire history - median grass distance',
                                        'No fire history - upper quartile grass distance',
                                        'Fire history - lower quartile grass distance',
                                        'Fire history - median grass distance',
                                        'Fire history - upper quartile grass distance'))
dat_predVals$changeInGrassProportion_predVal_fireRisk <-
  predict(reg_glm_fireRisk,
          newdata = dat_predVals,
          type = 'response')
dat_predVals$changeInGrassProportion_predVal_rainfall <-
  predict(reg_glm_rainfall,
          newdata = dat_predVals,
          type = 'response')

# plot predicted values - fire risk model
ggplot(data = dat_predVals,
       aes(x = fireRiskMean, y = changeInGrassProportion_predVal_fireRisk,
           color = model)) +
  geom_line(alpha = 0.5, size = 1) +
  scale_color_viridis_d(name = 'Covariate values') +
  labs(x = 'Mean fire risk', y = 'Change in grass proportion') +
  theme(text = element_text(size = 15),
        legend.position = c(0.8, 0.2))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Misc/Grassland/',
                         '02 V2 grass spread as fcn of fire probability - forest dominant to grass predicted values - fire risk model.png'),
       dpi = 300, height = 8, width = 10)

# plot predicted values - rainfall model
ggplot(data = dat_predVals,
       aes(x = annualRainfall_mm, y = changeInGrassProportion_predVal_rainfall,
           color = model)) +
  geom_line(alpha = 0.5, size = 1) +
  scale_color_viridis_d(name = 'Covariate values') +
  labs(x = 'Mean annual rainfall (mm)', y = 'Change in grass proportion') +
  theme(text = element_text(size = 15),
        legend.position = c(0.8, 0.8))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Misc/Grassland/',
                         '02 V2 grass spread as fcn of fire probability - forest dominant to grass predicted values - rainfall model.png'),
       dpi = 300, height = 8, width = 10)
