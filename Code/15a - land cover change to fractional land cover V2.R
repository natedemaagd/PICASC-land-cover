
# This script assigns new fractional land cover values to pixels that were converted
# in the land cover scenarios.

library(terra); library(ggplot2)




##### load data #####

# land cover
list_lcRast <-
  list(ras_lcBaseline = rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Updated baseline landcover from Jade/2023_11_25_baseline/mhi_s0_baseline_noNames.tif"),
       ras_lcBest2070 = rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/best case 2070 - moisture zones consistent w original raster - finalizedV2.tif"),
       ras_lcBest2100 = rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/best case 2100 - moisture zones consistent w original raster - finalizedV2.tif"),
       ras_lcWorst2070 = rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/worst case 2070 - moisture zones consistent w original raster - finalizedV2.tif"),
       ras_lcWorst2100 = rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/09 rasters post-fire/10a - final rasters with assigned landcover values/worst case 2100 - moisture zones consistent w original raster - finalizedV2.tif"))

# fractional land cover
ras_fracHerb <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire - PROTECT/interpolated_yearly_landcover_percentages/HI_FracLC_herb_2016.tif")
ras_fracWood <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire - PROTECT/interpolated_yearly_landcover_percentages/HI_FracLC_wood_2016.tif")

# match CRSs
list_lcRast <-
  lapply(list_lcRast,
         function(r) project(r, ras_fracHerb, method = 'mode', threads = TRUE))
gc()




##### distribution of fractional land cover by land cover type #####

# convert baseline data to data.frame
dat <-
  data.frame(lc = values(list_lcRast$ras_lcBaseline),
             fracHerb = values(ras_fracHerb),
             fracWood = values(ras_fracWood))
colnames(dat) <- c('lc', 'fracHerb', 'fracWood')
dat <- dat[dat$lc != 65535,]
dat <- dat[!is.nan(dat$fracHerb) & !is.na(dat$fracHerb),]
gc()

# split data by land cover type
dat_split <- split(dat, dat$lc)
dat_split <- lapply(dat_split, function(df){ df$lc <- NULL; df})
rm(dat)
gc()




##### create fractional land cover values for future scenarios #####

# find which pixels have changed for each scenario
list_lcRastChange <- list()
for(i in 2:length(list_lcRast)){
  list_lcRastChange[[i]] <- list_lcRast[[i]] - list_lcRast[[1]]
  gc()
}
names(list_lcRastChange) <- names(list_lcRast)
rm(i)
gc()

# initiate new fractional rasters
list_fracHerbRast <- list(ras_fracHerb, ras_fracHerb, ras_fracHerb, ras_fracHerb, ras_fracHerb)
list_fracWoodRast <- list(ras_fracWood, ras_fracWood, ras_fracWood, ras_fracWood, ras_fracWood)
names(list_fracHerbRast) <- names(list_fracWoodRast) <- names(list_lcRastChange)
rm(ras_fracHerb, ras_fracWood)
gc()

# replace fractional values in each raster
for(i in 3:length(list_lcRastChange)){

  # convert to data.frames
  dat <- data.frame(lc = values(list_lcRast[[i]]),
                    lc_change = values(list_lcRastChange[[i]]),
                    herb = values(list_fracHerbRast[[i]]),
                    wood = values(list_fracWoodRast[[i]]))
  colnames(dat) <- c('lc', 'lc_change', 'herb', 'wood')
  gc()

  # get new herb and wood frac values
  for(j in 1:nrow(dat)){

    # if land cover didn't change or is a land cover we don't have regression data for (ocean), skip it
    if(dat$lc_change[[j]] == 0 | dat$lc[[j]] %in% c(65535) | is.na(dat$lc_change[[j]])){

      next

    # otherwise, draw new fractional value based on land cover
    } else {

      dat$herb[[j]] <- sample(dat_split[[which(names(dat_split) == dat$lc[[j]])]]$fracHerb, 1)
      dat$wood[[j]] <- sample(dat_split[[which(names(dat_split) == dat$lc[[j]])]]$fracWood, 1)
    }
  }
  gc()
  saveRDS(dat, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/15 - land cover change to fractional land cover/',
                             'herb and wood dat ', i, '.rds'))
  #list_dat[[i]] <- dat
  rm(dat); gc()
}
saveRDS(list_dat, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/15 - land cover change to fractional land cover/',
                                'list herb and wood all dat.rds'))

rm(dat_split); gc()




##### adjust values to sum to 1 #####

# load data
list_dat <- readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/15 - land cover change to fractional land cover/',
                           'list herb and wood all dat.rds'))
list_dat[[1]] <- NULL

# add bare column to each data.frame
list_dat <- lapply(list_dat, function(df){
  df$bare = 0
  df <- df[c('lc', 'lc_change', 'ID', 'bare', 'herb', 'wood')]
  df
})
gc()

# adjust values
for(l in 2:length(list_dat)){
    
    for(i in 1:nrow(list_dat[[l]])){
      
      # if sum of herb and wood > 1, scale to sum to 1
      if(list_dat[[l]]$herb[[i]] + list_dat[[l]]$wood[[i]] >= 1 & !is.na(list_dat[[l]]$herb[[i]]) & !is.na(list_dat[[l]]$wood[[i]])){
        
        # get herb and wood values
        herb_i <- list_dat[[l]]$herb[[i]]
        wood_i <- list_dat[[l]]$wood[[i]]
        
        # scale values
        herb_i_new <- c(c(herb_i, wood_i) / sum(herb_i + wood_i))[[1]]
        wood_i_new <- c(c(herb_i, wood_i) / sum(herb_i + wood_i))[[2]]
        
        # replace values in data.frame with scaled values
        list_dat[[l]]$herb[[i]] <- herb_i_new
        list_dat[[l]]$wood[[i]] <- wood_i_new
        
      # if sum is less than 1, fill in the rest with bare cover
      } else if(list_dat[[l]]$herb[[i]] + list_dat[[l]]$wood[[i]] < 1 & !is.na(list_dat[[l]]$herb[[i]]) & !is.na(list_dat[[l]]$wood[[i]])) {
        list_dat[[l]]$bare[[i]] <- 1 - list_dat[[l]]$herb[[i]] - list_dat[[l]]$wood[[i]]
      } else {
        next
      }
    }
    rm(i, herb_i, herb_i_new, wood_i, wood_i_new); gc()
    saveRDS(list_dat[[l]], file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/15 - land cover change to fractional land cover/',
                                         'list_dat ', l, '.rds'))
}




##### convert back to rasters #####

library(terra)

# load data
list_dat <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/15 - land cover change to fractional land cover/',
                       pattern = 'list_dat', full.names = TRUE)
list_dat <- lapply(list_dat, readRDS)
names(list_dat) <- c('Best2070', 'Best2100', 'Worst2070', 'Worst2100')

# load template rasters
ras_template_bare <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire - PROTECT/interpolated_yearly_landcover_percentages/HI_FracLC_bare_2016.tif")
ras_template_herb <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire - PROTECT/interpolated_yearly_landcover_percentages/HI_FracLC_herb_2016.tif")
ras_template_wood <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire - PROTECT/interpolated_yearly_landcover_percentages/HI_FracLC_wood_2016.tif")

# write data as rasters
for(i in 1:length(list_dat)){
  
  # initiate raster
  ras_template_bare_i <- ras_template_bare
  ras_template_herb_i <- ras_template_herb
  ras_template_wood_i <- ras_template_wood
  
  # get replacement data without NAs
  dat_i <- list_dat[[i]]
  dat_i <- dat_i[!is.na(dat_i$ID),]
  gc()
  
  # replace values that changed
  ras_template_bare_i[dat_i$ID] <- dat_i$bare
}


