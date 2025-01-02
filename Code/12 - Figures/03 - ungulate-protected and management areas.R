
# This script plots the ungulate-protected areas, and current and future
# management areas.

library(sf)
library(raster)
library(ggplot2)
library(ggsn)




##### load data #####

# load Hawaii coastlines for base layer
sf_coastlines <- read_sf(paste0("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Shapefiles/Hawaii island outlines/All islands coastline/",
                                "coast_n83.shp"))

# load ungulate shapefile
sf_ungulateProtection <-
  read_sf(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Priority areas/Shapefiles/UngulateUnit/",
                 "AllUngulateUnit_Sept2019.shp"))
sf_ungulateProtection$type <-
  ifelse(sf_ungulateProtection$FenceStat %in% c('Fenced', 'Compliant'), 'Management unit - fenced', 'Management unit - proposed')
sf_ungulateProtection <- sf_ungulateProtection[c('type', 'geometry')]

# merge national parks to ungulate shapefile
sf_np <- 
  read_sf("H:/My Drive/Projects/Data/Shapefiles/NPS_Land_Resources_Division_Boundary_and_Tract_Data_Service/nps_boundary.shp")
sf_np <- st_transform(sf_np, crs(sf_ungulateProtection))
sf_np <- sf_np[sf_np$UNIT_NAME %in% c('Hawai\'i Volcanoes National Park', 'Haleakala National Park'),]
sf_np$type <- 'National park outside management area'
sf_np <- sf_np[c('type', 'geometry')]
sf_combined <- rbind(sf_np, sf_ungulateProtection)




##### plot #####

ggplot() +
  geom_sf(data = sf_coastlines) +
  geom_sf(data = sf_combined,
          aes(fill = type, color = type)) +
  geom_sf(data = sf_coastlines,
          fill = 'transparent', color = 'black') +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  scalebar(data = sf_coastlines, transform = FALSE, dist = 100, dist_unit = 'km',
           location = 'bottomleft') +
  north(data = sf_coastlines, symbol = 9) +
  labs(fill = NULL, color = NULL) +
  theme(text = element_text(size = 14), panel.background = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = 'bottom')

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/',
                         '12-03-01 Mgmt units and natl parks map.png'),
       dpi = 300)

