
# This script plots examples of the fractional landcover maps from Matty.

library(raster)
library(ggplot2)
library(ggsn)
library(viridis)
library(scales)

# load rasters
ras_wood2016 <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages/",
                "HI_FracLC_wood_2016.tif"))

# create dummy raster
ras_wood2016Dummy <- ras_wood2016
ras_wood2016Dummy[ras_wood2016Dummy >= 0.40 & !is.na(ras_wood2016Dummy)] <- 1
ras_wood2016Dummy[ras_wood2016Dummy <  0.40 & !is.na(ras_wood2016Dummy)] <- 0
gc()

# convert data for plotting
spdf_wood2016      <- as(ras_wood2016,      "SpatialPixelsDataFrame")
spdf_wood2016Dummy <- as(ras_wood2016Dummy, "SpatialPixelsDataFrame")
gc()
df_wood2016      <- as.data.frame(spdf_wood2016)
df_wood2016Dummy <- as.data.frame(spdf_wood2016Dummy)
gc()

# format data.frames
df_wood2016$long <- df_wood2016$x
df_wood2016$lat  <- df_wood2016$y
df_wood2016$x <- df_wood2016$y <- NULL
df_wood2016Dummy$forestDominant <-
  ifelse(df_wood2016Dummy$layer == 1, 'Yes',
         ifelse(df_wood2016Dummy$layer == 0, 'No', NA))
df_wood2016Dummy$forestDominant <-
  factor(df_wood2016Dummy$forestDominant,
         levels = c('Yes', 'No'))
df_wood2016Dummy$long <- df_wood2016Dummy$x
df_wood2016Dummy$lat  <- df_wood2016Dummy$y
df_wood2016Dummy$x <- df_wood2016Dummy$y <- NULL
gc()

# plot dummy raster
ggplot(data = df_wood2016Dummy) +
  geom_raster(aes(x = long, y = lat, fill = forestDominant)) +
  scale_fill_viridis_d(direction = -1, name = 'Forest dominant') +
  coord_equal() +
  scalebar(data = df_wood2016Dummy,
           location = "bottomleft", transform = TRUE,
           dist = 100, dist_unit = 'km', st.dist = 0.04) +
  north(data = df_wood2016Dummy, location = 'topright', symbol = 9) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        legend.position = 'bottom', text = element_text(size = 14))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/',
                         '12-02-01 forestdominant dummy 2016.png'),
       dpi = 300, width = 10, height = 6,units = 'in')

# plot actual wood cover
ggplot(data = df_wood2016) +
  geom_raster(aes(x = long, y = lat, fill = HI_FracLC_wood_2016)) +
  scale_fill_viridis_c(name = 'Forest cover proportion', option = 'H') +
  coord_equal() +
  scalebar(data = df_wood2016,
           location = "bottomleft", transform = TRUE,
           dist = 100, dist_unit = 'km', st.dist = 0.04) +
  north(data = df_wood2016, location = 'topright', symbol = 9) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        legend.position = 'bottom', text = element_text(size = 14),
        legend.key.width = unit(1, units = 'in'))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/',
                         '12-02-01 forest frac cover 2016.png'),
       dpi = 300, width = 10, height = 6,units = 'in')
