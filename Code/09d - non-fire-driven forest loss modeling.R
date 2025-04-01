
# This script checks for conversion from forest to grassland outside of fires

library(terra); library(sf); library(ggplot2); library(mgcv)
library(doParallel)
registerDoParallel(cores = 8)

# define conversion threshold
val_threshold <- 0.40




##### load data #####

# land cover rasters
ras_grass1999 <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/HI_Fractional_LC_statewide_split/Raw 1999 2016/HI_FracLC_herb_1999.tif")
ras_grass2016 <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/HI_Fractional_LC_statewide_split/Raw 1999 2016/HI_FracLC_herb_2016.tif")
ras_wood1999 <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/HI_Fractional_LC_statewide_split/Raw 1999 2016/HI_FracLC_wood_1999.tif")
ras_wood2016 <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/HI_Fractional_LC_statewide_split/Raw 1999 2016/HI_FracLC_wood_2016.tif")

# management areas and fire perimeters
sf_mgmt <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Priority areas/Shapefiles/UngulateUnit/AllUngulateUnit_Sept2019.shp")
sf_fire <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/2019_1999_Hawaii_Fire_Perimeters/2022_1999_Hawaii_Large_Fire_Perimeters_UH_NREM/fires_1999_2022.shp")

# re-project shapefiles to match rasters
sf_mgmt <- st_transform(sf_mgmt, st_crs(ras_grass1999))
sf_fire <- st_transform(sf_fire, st_crs(ras_grass1999))

# land cover raster for masking
ras_lc <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/mhi_s0_baseline_noNames.tif")
ras_lc <- project(ras_lc, ras_grass1999, method = 'near', threads = TRUE)

# 2015 ag shapefile for masking
sf_ag2015 <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Agricultural_Land_Use_-_2015_Baseline/Agricultural_Land_Use_-_2015_Baseline.shp")
sf_ag2015 <- st_transform(sf_ag2015, st_crs(ras_grass1999))





##### find areas where forest converts to grass #####

# change rasters
ras_grassDiff <- ras_grass2016 - ras_grass1999
ras_woodDiff <- ras_wood2016 - ras_wood1999

# change greater than threshold
ras_grassGained <- classify(ras_grassDiff,
                            rcl = matrix(c(val_threshold,  Inf, 1,
                                           -Inf, val_threshold, 0),
                                         ncol = 3, nrow = 2, byrow = TRUE))
ras_woodLost <- classify(ras_woodDiff,
                           rcl = matrix(c(-Inf, -val_threshold, 1,
                                          -val_threshold,  Inf, 0),
                                        ncol = 3, nrow = 2, byrow = TRUE))

# find pixels that converted from wood to grass
ras_woodToGrass <- ras_grassGained
ras_woodToGrass[ras_woodToGrass == 1] <- 0
ras_woodToGrass[ras_grassGained == 1 & ras_woodLost == 1] <- 1
writeRaster(ras_woodToGrass,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                              'xx - ras_woodConverted.tif'),
            overwrite = TRUE)
rm(ras_grass1999, ras_grass2016, ras_wood1999, ras_wood2016)
gc()

# mask conversion raster with shapefiles
ras_fenced <- ras_woodToGrass
ras_fenced[!is.na(ras_fenced)] <- 0
ras_fenced2 <- mask(ras_fenced, sf_mgmt)
ras_fenced[ras_fenced2 == 0] <- 1
writeRaster(ras_fenced,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                              'xx - ras_fenced.tif'),
            overwrite = TRUE)
ras_woodToGrass <- mask(ras_woodToGrass, sf_fire,   inverse = TRUE)
ras_woodToGrass <- mask(ras_woodToGrass, sf_ag2015, inverse = TRUE)
rm(ras_grass1999, ras_fenced2)
gc()

# mask ag and developed pixels
vec_maskCodes <- c(4000:4600, 10000:14800)
ras_woodToGrass[ras_woodToGrass %in% vec_maskCodes] <- NA

# calculate total area converted (pixels and hectares): 
paste(sum(values(ras_woodToGrass), na.rm = TRUE)/(2016 - 1999), 'pixels per year')
paste(sum(values(ras_woodToGrass), na.rm = TRUE)/(2016 - 1999) * 30 * 30 * 0.0001, 'hectares per year')

# write raster
writeRaster(ras_woodToGrass,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                              'xx - non-fire-driven forest loss.tif'),
            overwrite = TRUE)
gc()




##### analyze distance to grass and rainfall for forest loss to grass #####

# load rasters
ras_grass1999 <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/HI_Fractional_LC_statewide_split/Raw 1999 2016/HI_FracLC_herb_1999.tif")
ras_wood1999 <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/HI_Fractional_LC_statewide_split/Raw 1999 2016/HI_FracLC_wood_1999.tif")
ras_woodToGrass <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                               'xx - non-fire-driven forest loss.tif'))

# convert rasters to dummy
ras_wood1999_dummy <- ras_wood1999
ras_wood1999_dummy[ras_wood1999_dummy >= val_threshold & !is.na(ras_wood1999_dummy)] <- 1
ras_wood1999_dummy[ras_wood1999_dummy <  val_threshold & !is.na(ras_wood1999_dummy)] <- 0
writeRaster(ras_wood1999_dummy,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                              'xx - ras_wood1999_dummy.tif'),
            overwrite = TRUE)
ras_grass1999_dummy <- ras_grass1999
ras_grass1999_dummy[ras_grass1999_dummy >= val_threshold & !is.na(ras_grass1999_dummy)] <- 1
ras_grass1999_dummy[ras_grass1999_dummy <  val_threshold & !is.na(ras_grass1999_dummy)] <- 0
gc()

# scale grass and risk rasters to 250m
ras_grass1999_250m <-
  terra::aggregate(ras_grass1999_dummy,
                   fact = round(250/30), fun = max, na.rm = TRUE, cores = 8)
gc()
ras_wood1999_250m <-
  terra::aggregate(ras_wood1999_dummy,
                   fact = round(250/30), fun = max, na.rm = TRUE, cores = 8)
gc()

# for pixel, find distance to closest grass pixel
ras_distToGrass <- distance(ras_grass1999_250m, target = 0)

# convert back to original resolution
ras_distToGrass <- project(ras_distToGrass, ras_grass1999, method = 'cubic', threads = TRUE)

writeRaster(ras_distToGrass,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                              'xx - dist m to grass pixel.tif'),
            overwrite = TRUE)
gc()

# load required rasters ##### 
ras_distToGrass <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                               'xx - dist m to grass pixel.tif'))
ras_woodConverted <-
  rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                  'xx - ras_woodConverted.tif'))
# ras_rainfall <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/rainfall_ann/HI_EVAP_mean_annual_rainfall__statewide_250m.tif")
# ras_rainfall <- project(ras_rainfall, ras_grass1999)
# writeRaster(ras_rainfall,
#             filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
#                               'xx - ras_rainfall.tif'),
#             overwrite = TRUE)
ras_rainfall <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                            'xx - ras_rainfall.tif'))
ras_wood1999_dummy <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                                  'xx - ras_wood1999_dummy.tif'))
ras_fenced <- rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                          'xx - ras_fenced.tif'))

# find which wood pixels converted
ras_woodConverted_comprehensive <- ras_woodConverted
ras_woodConverted_comprehensive[is.na(ras_woodConverted_comprehensive) & ras_wood1999_dummy == 1 & !is.na(ras_wood1999_dummy)] <- 0

# convert to data.frame
dat <- as.data.frame(ras_woodConverted_comprehensive, na.rm = FALSE)
colnames(dat) <- 'woodToGrass'
dat$rain_mm <- values(ras_rainfall)[,1]
dat$distToGrass_m <- values(ras_distToGrass)[,1]
dat$distToGrass_m[dat$distToGrass_m < 0] <- 0
dat$fenced <- values(ras_fenced)[,1]
dat_allRows <- dat
dat <- dat[complete.cases(dat),]
gc()

# create character value
dat$woodToGrass_char <- ifelse(dat$woodToGrass == 1, 'Yes', 'No')

# plots
ggplot(data = dat[dat$fenced == 1,],
       aes(x = rain_mm, color = woodToGrass_char, fill = woodToGrass_char)) +
  geom_density(alpha = 0.2) +
  labs(x = 'Mean annual rainfall (mm)', y = 'Pixel density',
       color = 'Forest converted to grass?', fill = 'Forest converted to grass?') +
  theme(text = element_text(size = 14))
ggplot(data = dat[dat$fenced == 1,],
       aes(x = distToGrass_m, color = woodToGrass_char, fill = woodToGrass_char)) +
  geom_density(alpha = 0.2) +
  labs(x = 'Distance to grass (m)', y = 'Pixel density',
       color = 'Forest converted to grass?', fill = 'Forest converted to grass?') +
  theme(text = element_text(size = 14))

gc()




##### are pixels more likely to convert if they're unfenced? #####

# # sample lots of times - same result each time
# list_coefs <- foreach(i = 1:100, .combine = 'rbind') %dopar% {
#   
#   # create sampled data: sample unfenced pixels to match number of fenced pixels
#   dat_model <- dat[dat$fenced == 1,]
#   dat_model2 <- dat[dat$fenced == 0,]
#   dat_model2 <- dat_model2[sample(nrow(dat_model2), size = nrow(dat_model)),]
#   dat_model <- rbind(dat_model, dat_model2)
#   rm(dat_model2)
#   gc()
#   
#   # run regression
#   reg <- lm(woodToGrass ~ fenced, data = dat_model)
#   
#   # save coefficient and SE
#   gc()
#   return(c(coef(reg)[[2]],
#            summary(reg)$coefficients[, 2][[2]]))
#   
# }
# dat_coefs <- as.data.frame(list_coefs)
# colnames(dat_coefs) <- c('coef', 'SE')
# rm(list_coefs)
# gc()

# ...so just run once
dat_model <- dat[dat$fenced == 1,]
dat_model2 <- dat[dat$fenced == 0,]
dat_model2 <- dat_model2[sample(nrow(dat_model2), size = nrow(dat_model)),]
dat_model <- rbind(dat_model, dat_model2)
rm(dat_model2)
gc()
reg <- lm(woodToGrass ~ fenced + rain_mm, data = dat_model)
summary(reg)
rm(reg, dat_model)




##### relationship between conversion, and being fenced and rain #####

# create regression data: unfenced areas only, then sample unconverted to match converted
dat_model <- dat[dat$woodToGrass == 1,]
dat_model2 <- dat[dat$woodToGrass == 0,]
dat_model2 <- dat_model2[sample(nrow(dat_model2), size = nrow(dat_model)),]
dat_model <- rbind(dat_model, dat_model2)
rm(dat_model2)

# run regression
reg <- lm(woodToGrass ~ rain_mm + distToGrass_m,
          data = dat_model)

summary(reg)

# run GAM
reg_gam <- gam(woodToGrass ~
                 s(rain_mm, bs = "tp", k = 5) +
                 s(distToGrass_m, bs = "tp", k = 5),
               data = dat_model, family = binomial)
summary(reg_gam)

# create probability raster using predicted values of GAM
x <- predict.gam(reg_gam, newdata = dat_allRows, type = 'response')
gc()

# create raster with predicted values
ras_conversionProbability <- ras_woodConverted_comprehensive
values(ras_conversionProbability) <- c(x)
gc()

writeRaster(ras_conversionProbability,
            filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                              'xx - ras_conversionProbability.tif'),
            overwrite = TRUE)



##### save land area converted #####

dat_conversion <- data.frame(
  'pixels_converted' = sum(values(ras_woodToGrass), na.rm = TRUE),
  'hectares_converted' = sum(values(ras_woodToGrass), na.rm = TRUE) * 30 * 30 * 0.0001
)
dat_conversion$pixels_converted_perYear <- dat_conversion$pixels_converted / (2016 - 1999)
dat_conversion$hectares_converted_perYear <- dat_conversion$hectares_converted / (2016 - 1999)
write.csv(dat_conversion,
          file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
                        'xx - total area converted.csv'),
          row.names = FALSE)
gc()




##### analyze aspect and conversion probability #####

library(terra)

# load rasters
ras_conversionProbability <-
  rast(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/xx/',
              'xx - ras_conversionProbability.tif'))
ras_aspect <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/Aspect/oahu_aspect_30m_firemod.tif")
ras_aspect <- project(ras_aspect, ras_conversionProbability)

# convert aspect from radians to northness
ras_northness <- ras_aspect
ras_northness <- (ras_northness * 180) / (pi)
vec_northness <- values(ras_northness)
vec_northness <- ifelse(
  vec_northness > 180,
  360 - vec_northness,
  vec_northness
)
vec_northness <- c(vec_northness)
vec_northness <- (1-vec_northness/180)*100
values(ras_northness) <- vec_northness
gc()

# create data.frame
dat_northness <- as.data.frame(ras_northness, na.rm = FALSE)
dat_conversionProbability <- as.data.frame(ras_conversionProbability, na.rm = FALSE)
dat <- data.frame(northness = dat_northness$oahu_aspect_30m_firemod,
                  prob = dat_conversionProbability$HI_FracLC_wood_1999)
dat <- dat[complete.cases(dat),]
rm(dat_conversionProbability, dat_northness, ras_aspect, ras_conversionProbability, ras_northness)
gc()

# regression and predicted values for each value of northness - 3 knots
library(rms)
reg_rcs3 <- ols(prob ~ rcs(northness, 3),
               data = dat)
dat_plot3 <- as.data.frame(Predict(reg_rcs3, northness = seq(0, 100, length.out = 1000)))
colnames(dat_plot3) <- c('northness', 'prob_pred', 'prob_pred_lower', 'prob_pred_upper')
dat_plot3 <-
  data.frame(northness = dat_plot3$northness,
             prob_pred = dat_plot3$prob_pred,
             prob_pred_lower = dat_plot3$prob_pred_lower,
             prob_pred_upper = dat_plot3$prob_pred_upper)
dat_plot3$Knots <- '3'
reg_rcs3_r2 <- round(reg_rcs3$stats[[4]]* 100, 2)

# regression and predicted values for each value of northness - 4 knots
reg_rcs4 <- ols(prob ~ rcs(northness, 4),
                data = dat)
dat_plot4 <- as.data.frame(Predict(reg_rcs4, northness = seq(0, 100, length.out = 1000)))
colnames(dat_plot4) <- c('northness', 'prob_pred', 'prob_pred_lower', 'prob_pred_upper')
dat_plot4 <-
  data.frame(northness = dat_plot4$northness,
             prob_pred = dat_plot4$prob_pred,
             prob_pred_lower = dat_plot4$prob_pred_lower,
             prob_pred_upper = dat_plot4$prob_pred_upper)
dat_plot4$Knots <- '4'
reg_rcs4_r2 <- round(reg_rcs4$stats[[4]]* 100, 2)
gc()

# regression and predicted values for each value of northness - 5 knots
reg_rcs5 <- ols(prob ~ rcs(northness, 5),
                data = dat)
dat_plot5 <- as.data.frame(Predict(reg_rcs5, northness = seq(0, 100, length.out = 1000)))
colnames(dat_plot5) <- c('northness', 'prob_pred', 'prob_pred_lower', 'prob_pred_upper')
dat_plot5 <-
  data.frame(northness = dat_plot5$northness,
             prob_pred = dat_plot5$prob_pred,
             prob_pred_lower = dat_plot5$prob_pred_lower,
             prob_pred_upper = dat_plot5$prob_pred_upper)
dat_plot5$Knots <- '5'
reg_rcs5_r2 <- round(reg_rcs5$stats[[4]]* 100, 2)
gc()

# combine plot data
dat_plot <- rbind(dat_plot3, dat_plot4, dat_plot5)
rm(dat_plot3, dat_plot4, dat_plot5)

# plot
ggplot() +
  # geom_polygon(aes(x = c(dat_plot$northness, rev(dat_plot$northness)),
  #                  y = c(dat_plot$prob_pred_lower, rev(dat_plot$prob_pred_upper)),
  #                  fill = dat_plot$Knots)) +
  geom_line(data = dat_plot,
            aes(x = northness, y = prob_pred, color = Knots)) +
  labs(x = '% northness', y = 'Prob. converting forest to grass') +
  theme(text = element_text(size = 14))
