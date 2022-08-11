
# this script uses the aligned rasters to plot wood/herb/bare distribtuions by landcover type

library(raster); library(ggplot2); library(tidyverse)

# read rasters
r_landcover <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Misc/distributions of woody herbaceous and bare by landcover/01 raster alignment/raster_landcover.tif')
r_wood <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Misc/distributions of woody herbaceous and bare by landcover/01 raster alignment/raster_woodCover.tif')
r_herb <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Misc/distributions of woody herbaceous and bare by landcover/01 raster alignment/raster_herbcover.tif')
r_bare <- raster('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Misc/distributions of woody herbaceous and bare by landcover/01 raster alignment/raster_barecover.tif')

# define landcovers and their groupings
list_landcovers <- list(grassland = c(3600,               # native grassland
                                      3700, 3800, 3900),  # alien wet, alien mesic, alien dry grassland
                        shrubland = c(2500, 2600, 2800,   # native wet, native mesic, native dry shrubland
                                      3300, 3400, 3500))  # alien wet, alien mesic, alien dry shrubland

# create vectors of each raster, and remove ocean and NA pixels 
dat <- as.data.frame(r_landcover, xy = TRUE); gc()
landCov <- unlist(dat[!is.na(dat$raster_landcover) & dat$raster_landcover != 65535, 'raster_landcover']); gc()
bareCov <- values(r_bare); gc()
  bareCov <- bareCov[!is.na(dat$raster_landcover) & dat$raster_landcover != 65535]
herbCov <- values(r_herb); gc()
  herbCov <- herbCov[!is.na(dat$raster_landcover) & dat$raster_landcover != 65535]
woodCov <- values(r_wood); gc()
  woodCov <- woodCov[!is.na(dat$raster_landcover) & dat$raster_landcover != 65535]
dat <- dat[!is.na(dat$raster_landcover) & dat$raster_landcover != 65535, c('x','y')]; gc()

# create data.frame
dat <- dat %>%
  mutate(landCov = landCov,
         barecov = bareCov,
         herbCov = herbCov,
         woodCov = woodCov)
rm(landCov, herbCov, bareCov, woodCov,
   r_bare, r_herb, r_landcover, r_wood)
dat$x <- dat$y <- NULL
gc()




##### grassland plot #####

# melt grassland data
dat_grass <- dat[dat$landCov %in% list_landcovers$grassland,]
dat_grass <- reshape2::melt(dat_grass, id.vars = 'landCov')
dat_grass$variable <- as.character(dat_grass$variable)

# create label columns
dat_grass$Landcover <- with(dat_grass, ifelse(landCov == 3600, 'Native',
                                       ifelse(landCov == 3700, 'Alien wet',
                                       ifelse(landCov == 3800, 'Alien mesic',
                                       ifelse(landCov == 3900, 'Alien dry',
                                              NA))))
                       )
dat_grass$landCov <- NULL; gc()
dat_grass$Cover <- with(dat_grass, ifelse(variable == 'barecov', 'Bare',
                                   ifelse(variable == 'herbCov', 'Herbaceous',
                                   ifelse(variable == 'woodCov', 'Woody',
                                          NA)))
                        )
dat_grass$variable <- NULL; gc()

# plot
ggplot(data = dat_grass, aes(value, color = Landcover, fill = Landcover)) +
  geom_density(alpha = 0.3) +
  facet_grid(rows = vars(Cover), scales = 'free') +
  labs(x = '% of pixel', y = 'Pixel density', title = 'Grassland') +
  theme(text = element_text(size = 15)) +
  scale_fill_manual(values = c('brown4', 'brown3', 'brown1',
                               'royalblue')) +
  scale_color_manual(values = c('brown4', 'brown3', 'brown1',
                                'royalblue'))
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Misc/distributions of woody herbaceous and bare by landcover/02a grassland distribution.png',
       dpi = 300, height = 8, width = 8)

gc()




##### shrubland plot #####

# melt shrubland data
dat_shrub <- dat[dat$landCov %in% list_landcovers$shrubland,]
dat_shrub <- reshape2::melt(dat_shrub, id.vars = 'landCov')
dat_shrub$variable <- as.character(dat_shrub$variable)

# create label columns
dat_shrub$Landcover <- with(dat_shrub, ifelse(landCov == 2500, 'Native wet',
                                       ifelse(landCov == 2600, 'Native mesic',
                                       ifelse(landCov == 2800, 'Native dry',
                                       ifelse(landCov == 3300, 'Alien wet',
                                       ifelse(landCov == 3400, 'Alien mesic',
                                       ifelse(landCov == 3500, 'Alien dry',
                                       NA))))))
                            )
dat_shrub$landCov <- NULL; gc()
dat_shrub$Cover <- with(dat_shrub, ifelse(variable == 'barecov', 'Bare',
                                   ifelse(variable == 'herbCov', 'Herbaceous',
                                   ifelse(variable == 'woodCov', 'Woody',
                                          NA)))
)
dat_shrub$variable <- NULL; gc()

# plot
ggplot(data = dat_shrub, aes(value, color = Landcover, fill = Landcover)) +
  geom_density(alpha = 0.3) +
  facet_grid(rows = vars(Cover), scales = 'free') +
  labs(x = '% of pixel', y = 'Pixel density', title = 'Shrubland') +
  theme(text = element_text(size = 15)) +
  scale_fill_manual(values = c('brown4', 'brown3', 'brown1',
                               'royalblue4', 'royalblue3', 'royalblue1')) +
  scale_color_manual(values = c('brown4', 'brown3', 'brown1',
                               'royalblue4', 'royalblue3', 'royalblue1'))
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Misc/distributions of woody herbaceous and bare by landcover/02b shrubland distribution.png',
       dpi = 300, height = 8, width = 8)




##### K-S tests #####

# Kolmogorov-Smirnov tests to test distribution similarity

# grassland tests - matrix of test statistics
dat_Grass_ksStatistic <- matrix(NA,
                      nrow = length(unique(dat_grass$Cover)),
                      ncol = length(unique(dat_grass$Landcover))-1)
rownames(dat_Grass_ksStatistic) <- unique(dat_grass$Cover)  # rows are Wet, Mesic, Dry classifications
colnames(dat_Grass_ksStatistic) <- c(unique(dat_grass$Landcover)[-grep('Native', unique(dat_grass$Landcover))])  # columns are Alien grass types, compared to Native grass
for(r in 1:nrow(dat_Grass_ksStatistic)){
  for(c in 1:ncol(dat_Grass_ksStatistic)){
    dat_Grass_ksStatistic[r,c] <-
      ks.test(x = dat_grass[dat_grass$Landcover == 'Native' &
                              dat_grass$Cover == rownames(dat_Grass_ksStatistic)[[r]],
                            'value'],
              y = dat_grass[dat_grass$Landcover == colnames(dat_Grass_ksStatistic)[[c]] &
                              dat_grass$Cover == rownames(dat_Grass_ksStatistic)[[r]],
                            'value'])$statistic
  }
}

# grassland tests - matrix of P-values
dat_Grass_ksPvalue <- matrix(NA,
                                nrow = length(unique(dat_grass$Cover)),
                                ncol = length(unique(dat_grass$Landcover))-1)
rownames(dat_Grass_ksPvalue) <- unique(dat_grass$Cover)  # rows are Wet, Mesic, Dry classifications
colnames(dat_Grass_ksPvalue) <- c(unique(dat_grass$Landcover)[-grep('Native', unique(dat_grass$Landcover))])  # columns are Alien grass types, compared to Native grass
for(r in 1:nrow(dat_Grass_ksPvalue)){
  for(c in 1:ncol(dat_Grass_ksPvalue)){
    dat_Grass_ksPvalue[r,c] <-
      ks.test(x = dat_grass[dat_grass$Landcover == 'Native' &
                              dat_grass$Cover == rownames(dat_Grass_ksPvalue)[[r]],
                            'value'],
              y = dat_grass[dat_grass$Landcover == colnames(dat_Grass_ksPvalue)[[c]] &
                              dat_grass$Cover == rownames(dat_Grass_ksPvalue)[[r]],
                            'value'])$p.value
  }
}

# shrubland tests - matrix of test statistics
dat_Shrub_ksStatistic <- array(NA,
                               dim = c(length(unique(dat_shrub$Landcover)[grep('Alien',  unique(dat_shrub$Landcover))]),  # first dimension of array is all Alien shrubs
                                       length(unique(dat_shrub$Landcover)[grep('Native', unique(dat_shrub$Landcover))]),  # second dimension is all Native shrubs
                                       length(unique(dat_shrub$Cover))))  # third dimension is bare, herbaceous, woody
dimnames(dat_Shrub_ksStatistic) <-
  list(unique(dat_shrub$Landcover)[grep('Alien',  unique(dat_shrub$Landcover))],
       unique(dat_shrub$Landcover)[grep('Native', unique(dat_shrub$Landcover))],
       unique(dat_shrub$Cover))

for(dim3 in 1:dim(dat_Shrub_ksStatistic)[[3]]){
  mat <- dat_Shrub_ksStatistic[,,dim3]
  for(r in 1:nrow(mat)){
    for(c in 1:ncol(mat)){
      mat[r,c] <-
        ks.test(x = dat_shrub[dat_shrub$Cover == dimnames(dat_Shrub_ksStatistic)[[3]][[dim3]] &  # subset native data by bare-herbaceous-woody
                                dat_shrub$Landcover == colnames(dat_Shrub_ksStatistic[,,dim3])[[c]],  # subset native data by wet-mesic-dry
                              'value'],
                y = dat_shrub[dat_shrub$Cover == dimnames(dat_Shrub_ksStatistic)[[3]][[dim3]] &  # subset alien data by bare-herbaceous-woody
                                dat_shrub$Landcover == rownames(dat_Shrub_ksStatistic[,,dim3])[[r]],  # subset alien data by wet-mesic-dry
                              'value'])$statistic
      
      dat_Shrub_ksStatistic[,,dim3] <- mat
    }
  }
}
rm(mat, r, c, dim3)

# shrubland tests - matrix of P-values
dat_Shrub_ksPvalue <- array(NA,
                               dim = c(length(unique(dat_shrub$Landcover)[grep('Alien',  unique(dat_shrub$Landcover))]),  # first dimension of array is all Alien shrubs
                                       length(unique(dat_shrub$Landcover)[grep('Native', unique(dat_shrub$Landcover))]),  # second dimension is all Native shrubs
                                       length(unique(dat_shrub$Cover))))  # third dimension is bare, herbaceous, woody
dimnames(dat_Shrub_ksPvalue) <-
  list(unique(dat_shrub$Landcover)[grep('Alien',  unique(dat_shrub$Landcover))],
       unique(dat_shrub$Landcover)[grep('Native', unique(dat_shrub$Landcover))],
       unique(dat_shrub$Cover))

for(dim3 in 1:dim(dat_Shrub_ksPvalue)[[3]]){
  mat <- dat_Shrub_ksPvalue[,,dim3]
  for(r in 1:nrow(mat)){
    for(c in 1:ncol(mat)){
      mat[r,c] <-
        ks.test(x = dat_shrub[dat_shrub$Cover == dimnames(dat_Shrub_ksPvalue)[[3]][[dim3]] &  # subset native data by bare-herbaceous-woody
                                dat_shrub$Landcover == colnames(dat_Shrub_ksPvalue[,,dim3])[[c]],  # subset native data by wet-mesic-dry
                              'value'],
                y = dat_shrub[dat_shrub$Cover == dimnames(dat_Shrub_ksPvalue)[[3]][[dim3]] &  # subset alien data by bare-herbaceous-woody
                                dat_shrub$Landcover == rownames(dat_Shrub_ksPvalue[,,dim3])[[r]],  # subset alien data by wet-mesic-dry
                              'value'])$p.value
      
      dat_Shrub_ksPvalue[,,dim3] <- mat
    }
  }
}
rm(mat, r, c, dim3)

