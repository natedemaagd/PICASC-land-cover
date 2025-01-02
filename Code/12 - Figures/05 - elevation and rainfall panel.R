
# This script plots elevation and wind patterns.

library(terra)
library(sf)
library(ggplot2)
library(ggsn)
library(cowplot)




##### load data #####

# load elevation raster
ras_elevation <-
  rast(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/elevation_hawaii_100m/",
                "elslhii0100a.tif"))

# load coastlines
sf_coast <-
  read_sf(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii coastlines/",
                 "coast_n83.shp"))




##### elevation map #####

# transform sf_coast to match elevation data
sf_coast_transform <-
  st_transform(sf_coast, crs = crs(ras_elevation))

# convert elevation raster to data.frame
dat_elevation <-
  as.data.frame(ras_elevation, xy = TRUE)
colnames(dat_elevation) <- c('long', 'lat', 'elevation_m')
gc()

# change elevation values
dat_elevation$elevation_m2 <-
  scales::rescale(x = dat_elevation$elevation_m, to = c(0, 4207/1000))

# remove ocean pixels
dat_elevation <- dat_elevation[dat_elevation$elevation_m != 2,]
gc()

# initial raster plot
p_elevation <-
  ggplot() +
  geom_raster(data = dat_elevation,
              aes(x = long, y = lat, fill = elevation_m2)) +
  coord_equal() +
  scale_fill_gradientn(colors = terrain.colors(10),
                       name = 'Elevation (1000s m)') +
  ggsn::north(data = dat_elevation, location = 'topright', symbol = 9) +
  ggsn::scalebar(data = dat_elevation,
                 location = "bottomleft", transform = FALSE,
                 dist = 100, dist_unit = 'km', st.dist = 0.04) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        text = element_text(size = 20), legend.position = c(0.2, 0.3),
        legend.direction = 'horizontal')

# # mask ocean pixels with blue
# dat_ocean <- dat_elevation[dat_elevation$elevation_m <= 2,]; gc()
# p <- p +
#   geom_rast(data = dat_ocean,
#               aes(x = x, y = y, fill = elevation_m),
#               fill = 'white', show.legend = FALSE)

# add shoreline polygons, north arrow, and scalebar
p_elevation <- p_elevation +
  geom_sf(data = sf_coast_transform,
          fill = 'transparent',  color = 'black')

plot(p); gc()

ggsave(plot = p_elevation,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/',
                         '12-05-01 elevation map.png'),
       dpi = 300)




##### elevation and rainfall panel #####

# load rainfall raster
ras_rainfall <- rast("H:/My Drive/Projects/Data/rainfall_atlas_ann_rainfall_inches/Hawaii-State/rfgrid_inches_state_ann.txt")
ras_rainfall <- project(ras_rainfall, ras_elevation, threads = TRUE)

# convert to data.frames and convert from inches to mm
dat_rainfall <- as.data.frame(ras_rainfall, xy = TRUE)
dat_rainfall$rain_mm <- dat_rainfall$rfgrid_inches_state_ann * 25.4

# melt data
dat_panel <-
  data.frame(long = c(dat_rainfall$x, dat_elevation$long),
             lat  = c(dat_rainfall$y, dat_elevation$lat),
             value = c(dat_rainfall$rain_mm, dat_elevation$elevation_m2),
             var = c(rep('rain', times = nrow(dat_rainfall)), rep('elevation', times = nrow(dat_elevation))))

# plot elevation
p_elevation <-
  ggplot(data = dat_panel[dat_panel$var == 'elevation',],
         aes(x = long, y = lat, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(option = 'H',
                       limits = c(min(dat_panel[dat_panel$var == 'elevation', 'value']),
                                  max(dat_panel[dat_panel$var == 'elevation', 'value'])),
                       breaks = c(min(dat_panel[dat_panel$var == 'elevation', 'value']),
                                  (min(dat_panel[dat_panel$var == 'elevation', 'value']) +
                                     max(dat_panel[dat_panel$var == 'elevation', 'value']))/2,
                                  max(dat_panel[dat_panel$var == 'elevation', 'value'])),
                       labels = round(c(min(dat_panel[dat_panel$var == 'elevation', 'value']),
                                        (min(dat_panel[dat_panel$var == 'elevation', 'value']) +
                                           max(dat_panel[dat_panel$var == 'elevation', 'value']))/2,
                                        max(dat_panel[dat_panel$var == 'elevation', 'value'])),
                                      1)) +
  labs(fill = 'Elevation\n(1000 m)') +
  # scalebar(data = dat_panel[dat_panel$var == 'elevation',],
  #          dist_unit = 'km', dist = 100, transform = FALSE,
  #          location = 'bottomleft', st.dist = 0.05) +
  north(data = dat_panel[dat_panel$var == 'elevation',],
        location = 'topright', symbol = 9) +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        text = element_text(size = 14))

# plot rain
p_rain <-
  ggplot(data = dat_panel[dat_panel$var == 'rain',],
         aes(x = long, y = lat, fill = value)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_c(option = 'H', direction = -1,
                       limits = c(min(dat_panel[dat_panel$var == 'rain', 'value']),
                                  max(dat_panel[dat_panel$var == 'rain', 'value'])),
                       breaks = c(min(dat_panel[dat_panel$var == 'rain', 'value']),
                                  (min(dat_panel[dat_panel$var == 'rain', 'value']) +
                                   max(dat_panel[dat_panel$var == 'rain', 'value']))/2,
                                  max(dat_panel[dat_panel$var == 'rain', 'value'])),
                       labels = round(c(min(dat_panel[dat_panel$var == 'rain', 'value']),
                                        (min(dat_panel[dat_panel$var == 'rain', 'value']) +
                                           max(dat_panel[dat_panel$var == 'rain', 'value']))/2,
                                        max(dat_panel[dat_panel$var == 'rain', 'value'])))) +
  labs(fill = 'Mean rainfall\n(mm/yr)') +
  scalebar(data = dat_panel[dat_panel$var == 'rain',],
           dist_unit = 'km', dist = 100, transform = FALSE,
           location = 'bottomleft', st.dist = 0.05) +
  # north(data = dat_panel[dat_panel$var == 'rain',],
  #       location = 'topright', symbol = 9) +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        text = element_text(size = 14))

# create panel
p_panel <- plot_grid(p_elevation, p_rain, ncol = 1, align = "hv")
ggsave(p_panel,
       file = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/05 - elevation and rain panel.png',
       dpi = 300)
