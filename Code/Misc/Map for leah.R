
# This script creates a map of fallow land.

library(raster)
library(ggplot2)
library(ggsn)

# set directory
dir_data <- "H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Fallow raster/"
dir_output <- "H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fallow/"




##### load data #####

# load raster
ras_fallow <-
  raster(paste0(dir_data, "land_use_fallow1.tif"))




##### format data for plotting #####

# convert to data.frame
dat_fallow <- as.data.frame(ras_fallow, xy = TRUE)

# combine all the categories besides fallow and pasture into one category (other active agriculture)
vec_codesToKeep <- c(1, 100)  # (Pasture, Fallow)
dat_fallow$land_use_fallow1[!(dat_fallow$land_use_fallow1 %in% vec_codesToKeep) &
                              !is.na(dat_fallow$land_use_fallow1)] <- 1000
gc()

# export new raster
ras_fallow_aggregatedCodes <- ras_fallow
values(ras_fallow_aggregatedCodes) <- dat_fallow$land_use_fallow1
gc()
writeRaster(ras_fallow_aggregatedCodes,
            filename = paste0(dir_output, 'ras_fallowCombinedCodes.tif'),
            overwrite = TRUE)

# remove missing rows for plotting
dat_fallow <- dat_fallow[!is.na(dat_fallow$land_use_fallow1),]
gc()




##### plot #####

# load background raster
ras_lc <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/mhi_land_cover_names.tif")

# match CRS of background to fallow raster
ras_lc <- projectRaster(ras_lc, ras_fallow)

# convert background to data.frame
dat_lc <- as.data.frame(ras_lc, xy = TRUE)
colnames(dat_lc) <- c('long', 'lat', 'value')
dat_lc <- dat_lc[dat_lc$value != 65535,]
rm(ras_lc); gc()

# create plotting categories
dat_fallow$land_use_label <-
  with(dat_fallow,
       ifelse(land_use_fallow1 == 1000, 'Other active agriculture',
              ifelse(land_use_fallow1 == vec_codesToKeep[[1]], 'Pasture',
                     ifelse(land_use_fallow1 == vec_codesToKeep[[2]], 'Fallow', NA)
                     )
              )
       )
dat_fallow$land_use_label <-
  factor(dat_fallow$land_use_label,
         levels = c('Fallow', 'Pasture', 'Other active agriculture'))

# plot
ggplot() +
  geom_raster(data = dat_lc, aes(x = long, y = lat), fill = 'lightgray') +
  geom_raster(data = dat_fallow, aes(x = x, y = y, fill = land_use_label)) +
  coord_equal() +
  scale_fill_viridis_d(direction = -1) +
  labs(fill = 'Land use') +
  north(data = dat_lc, location = 'topright', symbol = 9) +
  scalebar(data = dat_lc, location = 'bottomleft', transform = FALSE,
           dist = 30, dist_unit = 'km', st.dist = 0.02, st.size = 3, height = 0.01) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        text = element_text(size = 15))
ggsave(filename = paste0(dir_output, 'map_fallow.png'),
       dpi = 300, width = 18, height = 9)
