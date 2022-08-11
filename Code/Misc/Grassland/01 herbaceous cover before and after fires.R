
# this script calculates changes to grassland after fires

library(raster); library(sf)

# load data
list_herbCoverFilenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/',
                                      pattern = 'herb')
list_herbCoverRasters <- lapply(list_herbCoverFilenames, function(r){
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/',
                r))
})
names(list_herbCoverRasters) <- list_herbCoverFilenames

list_bareCoverFilenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/',
                                      pattern = 'bare')
list_bareCoverRasters <- lapply(list_bareCoverFilenames, function(r){
  raster(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/',
                r))
})
names(list_bareCoverRasters) <- list_bareCoverFilenames

rm(list_bareCoverFilenames, list_herbCoverFilenames)

sf_fires <- read_sf('H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data/2019_1999_Hawaii_Fire_Perimeters',
                    '2019_1999_Hawaii_Fire_Perimeters')




##### analyze herbaceous cover before and after fire #####

# split fire polygons
sf_fires_split <- split(sf_fires, sf_fires$UH_ID)

# for each fire, get herb cover year before and year after fire, along with the size of the fire
dat_fireGrassCover <-
  data.frame(herbCov_meanYearBefore2yr = rep(NA, times = length(sf_fires_split)),
             herbCov_meanYearBefore1yr = NA,
             herbCov_meanYearAfter1yr = NA,
             herbCov_meanYearAfter2yr = NA,
             bareCov_meanYearBefore2yr = NA,
             bareCov_meanYearBefore1yr = NA,
             bareCov_meanYearAfter1yr = NA,
             bareCov_meanYearAfter2yr = NA,
             woodCov_meanYearBefore2yr = NA,
             woodCov_meanYearBefore1yr = NA,
             woodCov_meanYearAfter1yr = NA,
             woodCov_meanYearAfter2yr = NA,
             fireSizeAcres = NA)
for(f in 6:length(sf_fires_split)){
#for(f in 1:length(sf_fires_split)){
  
  # get fire f
  fire_f <- sf_fires_split[[f]]
  
  # get year of fire f
  year_f <- fire_f$Year[[1]]  # sometimes year is duplicated; take only first element
  
  # skip fire if out of range of pct cover rasters
  if(year_f <= 2001 | year_f >= 2014) next
  
  # get herb cover raster for years f-1, f-2, f+1 and f+2
  herb_before2yr <-
    list_herbCoverRasters[[grep(year_f-2, names(list_herbCoverRasters))]]
  herb_before1yr <-
    list_herbCoverRasters[[grep(year_f-1, names(list_herbCoverRasters))]]
  herb_after1yr <- 
    list_herbCoverRasters[[grep(year_f+1, names(list_herbCoverRasters))]]
  herb_after2yr <- 
    list_herbCoverRasters[[grep(year_f+2, names(list_herbCoverRasters))]]
  
  # get bare cover raster for years f-1, f-2, f+1 and f+2
  bare_before2yr <-
    list_bareCoverRasters[[grep(year_f-2, names(list_bareCoverRasters))]]
  bare_before1yr <-
    list_bareCoverRasters[[grep(year_f-1, names(list_bareCoverRasters))]]
  bare_after1yr <- 
    list_bareCoverRasters[[grep(year_f+1, names(list_bareCoverRasters))]]
  bare_after2yr <- 
    list_bareCoverRasters[[grep(year_f+2, names(list_bareCoverRasters))]]
  
  # mask herb cover rasters with fire polygon
  herb_before1yr <- mask(herb_before1yr, fire_f)
  herb_before2yr <- mask(herb_before1yr, fire_f)
  herb_after1yr  <- mask(herb_after1yr,  fire_f)
  herb_after2yr  <- mask(herb_after2yr,  fire_f)
  
  # mask bare cover rasters with fire polygon
  bare_before1yr <- mask(bare_before1yr, fire_f)
  bare_before2yr <- mask(bare_before1yr, fire_f)
  bare_after1yr  <- mask(bare_after1yr,  fire_f)
  bare_after2yr  <- mask(bare_after2yr,  fire_f)
  
  # save mean pct herbaceous before and after fire
  dat_fireGrassCover$herbCov_meanYearBefore1yr[[f]] <-
    mean(values(herb_before1yr), na.rm = TRUE)
  dat_fireGrassCover$herbCov_meanYearBefore2yr[[f]] <-
    mean(values(herb_before2yr), na.rm = TRUE)
  dat_fireGrassCover$herbCov_meanYearAfter1yr[[f]] <-
    mean(values(herb_after1yr), na.rm = TRUE)
  dat_fireGrassCover$herbCov_meanYearAfter2yr[[f]] <-
    mean(values(herb_after2yr), na.rm = TRUE)
  
  # save mean pct bare before and after fire
  dat_fireGrassCover$bareCov_meanYearBefore1yr[[f]] <-
    mean(values(bare_before1yr), na.rm = TRUE)
  dat_fireGrassCover$bareCov_meanYearBefore2yr[[f]] <-
    mean(values(bare_before2yr), na.rm = TRUE)
  dat_fireGrassCover$bareCov_meanYearAfter1yr[[f]] <-
    mean(values(bare_after1yr), na.rm = TRUE)
  dat_fireGrassCover$bareCov_meanYearAfter2yr[[f]] <-
    mean(values(bare_after2yr), na.rm = TRUE)
  
  # save fire size
  dat_fireGrassCover$fireSizeAcres[[f]] <-
    fire_f$Sat_sz_ac[[1]]
  
  gc()
}
