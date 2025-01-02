library(terra); library(gstat); library(ggplot2); library(sf)
theme_set(theme_gray(base_size = 14))



##### data setup #####

# load spatial data and change from utm to lat/lon
moku <- read_sf('D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Moku-shp/Moku-ridge-to-reef', 'moku_ridge_to_reef_dar')
moku <- st_transform(moku, crs = '+proj=longlat +zone=4 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')

# load all vars data
dat <- as.data.frame(readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/DWS/Land_cover/Data/Raw/AllVars_MeanAnnVals.rds"))

# convert dat to spatial
dat_sp <- st_as_sf(dat, coords = c('POINT_X', 'POINT_Y'), crs = crs(moku))

# # find which moku each point in dat is located
# dat_moku <- over(dat_sp, moku[1:3])
# dat_moku <- st_intersection(dat_sp, moku)
# 
# # add moku data to dat
# dat <- cbind(dat, dat_moku)
# 
# # split data by moku
# dat_by_moku <- split(dat, dat$Moku_ID)
# 
# # summarize number of pixels per moku and clean up environment
# summary(sapply(dat_by_moku, nrow))
# rm(dat_moku, moku); gc()
# 
# # number of pixels of each landcover per moku
# summary(sapply(dat_by_moku, function(t){nrow(t[t$LC ==  8,])}))
# summary(sapply(dat_by_moku, function(t){nrow(t[t$LC == 10,])}))
# summary(sapply(dat_by_moku, function(t){nrow(t[t$LC == 13,])}))
# summary(sapply(dat_by_moku, function(t){nrow(t[t$LC == 32,])}))
# summary(sapply(dat_by_moku, function(t){nrow(t[t$LC == 34,])}))
# 
# 
# 
# 
# ##### spatial autocorrelation anaylsis #####
# 
# # # moran's I: ALL VARS SPATIALLY-CORELATED
# moran_i_AET  <- moranfast::moranfast(dat$AET,  dat$POINT_X, dat$POINT_Y)
# moran_i_LAI  <- moranfast::moranfast(dat$LAI,  dat$POINT_X, dat$POINT_Y)
# moran_i_SM   <- moranfast::moranfast(dat$SM,   dat$POINT_X, dat$POINT_Y)
# moran_i_U    <- moranfast::moranfast(dat$U,    dat$POINT_X, dat$POINT_Y)
# moran_i_T    <- moranfast::moranfast(dat$T,    dat$POINT_X, dat$POINT_Y)
# moran_i_Rnet <- moranfast::moranfast(dat$Rnet, dat$POINT_X, dat$POINT_Y)
# 
# # variograms
# variogram_AET        <- variogram(AET~1,               data=dat, locations= ~POINT_X+POINT_Y)
# variogram_LAI        <- variogram(LAI~1,               data=dat, locations= ~POINT_X+POINT_Y)
# variogram_SM         <- variogram(SM~1,                data=dat, locations= ~POINT_X+POINT_Y)
# variogram_U          <- variogram(U~1,                 data=dat, locations= ~POINT_X+POINT_Y)
# variogram_T          <- variogram(T~1,                 data=dat, locations= ~POINT_X+POINT_Y)
# variogram_Rnet       <- variogram(Rnet~1,              data=dat, locations= ~POINT_X+POINT_Y)
# variogram_regression <- variogram(AET~LAI+SM+U+T+Rnet, data=dat, locations= ~POINT_X+POINT_Y, cutoff = 0.01)
# ggplot(data=variogram_regression, aes(x=dist, y=gamma)) +
#   geom_point() + geom_smooth(se=FALSE) +
#   labs(x='Distance', y='Gamma')
# # CUTOFFS THAT MAY BE REASONABLE:
#   # 0.25
# 
# ggplot(data=variogram_AET, aes(x=dist, y=gamma)) +
#   geom_point() + geom_smooth(se=FALSE) +
#   labs(x='Distance', y='Gamma')
# ggplot(data=variogram_LAI, aes(x=dist, y=gamma)) +
#   geom_point() + geom_smooth(se=FALSE) +
#   labs(x='Distance', y='Gamma')
# ggplot(data=variogram_SM, aes(x=dist, y=gamma)) +
#   geom_point() + geom_smooth(se=FALSE) +
#   labs(x='Distance', y='Gamma')
# ggplot(data=variogram_U, aes(x=dist, y=gamma)) +
#   geom_point() + geom_smooth(se=FALSE) +
#   labs(x='Distance', y='Gamma')
# ggplot(data=variogram_T, aes(x=dist, y=gamma)) +
#   geom_point() + geom_smooth(se=FALSE) +
#   labs(x='Distance', y='Gamma')
# ggplot(data=variogram_Rnet, aes(x=dist, y=gamma)) +
#   geom_point() + geom_smooth(se=FALSE) +
#   labs(x='Distance', y='Gamma')
# ggplot(data=variogram_regression, aes(x=dist, y=gamma)) +
#   geom_point() + geom_smooth(se=FALSE) +
#   labs(x='Distance', y='Gamma')
# 
# variogram_combinedData <- rbind(variogram_AET, variogram_LAI, variogram_SM, variogram_U, variogram_T, variogram_Rnet, variogram_regression)
# variogram_combinedData$model <- rep(c('AET', 'LAI', 'SM', 'U', 'T', 'Rnet', 'Model'), each = nrow(variogram_AET))
# variogram_combinedData$model <- factor(variogram_combinedData$model, levels = c('AET', 'LAI', 'SM', 'U', 'T', 'Rnet', 'Model'))
# 
# ggplot(data=variogram_combinedData, aes(x=dist, y=gamma)) +
#   geom_point() + geom_smooth(se=FALSE) +
#   labs(x='Distance', y='Gamma') +
#   facet_wrap(.~model, scales = 'free')




# save data
save.image(file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/01_regression_data_setup.Rdata')
