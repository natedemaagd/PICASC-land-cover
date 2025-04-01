
# This script combines the figures from 01 and 03 in this directory.

library(ggplot2)
library(ggsn)
library(terra)
library(dplyr)
library(sf)
library(RColorBrewer)




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

# load landcover raster
ras_landcover <-
  rast(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
                "mhi_land_cover_names.tif"))

# match sf crs to raster
sf_combined <- st_transform(sf_combined, crs(ras_landcover))

# convert to data.frame
dat_landcover <- as.data.frame(ras_landcover, xy = TRUE, na.rm = TRUE); gc()
colnames(dat_landcover) <- c('x', 'y', 'lc')
gc()




##### create simplified landcover classification #####

# load meta categories and rename columns
dat_landcover_names_meta <- 
  readxl::read_xlsx(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/",
                           "Landcover_MetaCategories_GM.xlsx"))
colnames(dat_landcover_names_meta)[colnames(dat_landcover_names_meta) ==
                                     'CAH_AG_COMBINED'] <-
  'lc'
dat_landcover_names_meta <- dat_landcover_names_meta[c('lc', 'Meta_Category')]

# merge landcover raster names with meta category names
dat_landcover <-
  left_join(dat_landcover, dat_landcover_names_meta, 'lc')

# fill in NAs
dat_landcover[dat_landcover$lc == 'Lava Flow' & !is.na(dat_landcover$lc), 'Meta_Category'] <- 'Lava Flow'
dat_landcover[dat_landcover$lc == 'Cultivated agriculture, Tropical Fruits (~orchards)' & !is.na(dat_landcover$lc), 'Meta_Category'] <- 'Agriculture'
dat_landcover[dat_landcover$lc == 'Cultivated agriculture, Taro (wet)' & !is.na(dat_landcover$lc), 'Meta_Category'] <- 'Agriculture'
dat_landcover[dat_landcover$lc == 'Pasture, Plantation forest' & !is.na(dat_landcover$lc), 'Meta_Category'] <- 'Other'
dat_landcover[dat_landcover$lc == 'Pasture, Mixed native-alien mesic shrubs and grass' & !is.na(dat_landcover$lc), 'Meta_Category'] <- 'Other'
dat_landcover[dat_landcover$lc == 'Cultivated agriculture, Sugarcane' & !is.na(dat_landcover$lc), 'Meta_Category'] <- 'Agriculture'
dat_landcover[dat_landcover$lc == 'Cultivated agriculture, Dairy (intensive pasture)' & !is.na(dat_landcover$lc), 'Meta_Category'] <- 'Agriculture'
gc()




##### create even simpler landcover classification #####

# keep only native/alien forest, shrub, grass, and moisture zone classifications
dat_landcover$Meta_Category2 <- dat_landcover$Meta_Category
dat_landcover$Meta_Category2[!(dat_landcover$Meta_Category %in%
                                 c("Wet Native Forest",    "Mesic Native Forest",    "Dry Native Forest",
                                   "Wet Native Shrubland", "Mesic Native Shrubland", "Dry Native Shrubland",
                                   "Wet Native Grassland", "Mesic Native Grassland", "Dry Native Grassland",
                                   "Wet Alien Forest",    "Mesic Alien Forest",    "Dry Alien Forest",
                                   "Wet Alien Shrubland", "Mesic Alien Shrubland", "Dry Alien Shrubland",
                                   "Wet Alien Grassland", "Mesic Alien Grassland", "Dry Alien Grassland")) &
                               !is.na(dat_landcover$Meta_Category)] <-
  'Other'
dat_landcover <- dat_landcover[complete.cases(dat_landcover),]
gc()
dat_landcover$Meta_Category2 <-
  factor(dat_landcover$Meta_Category2,
         levels = c(c("Wet Native Forest",    "Mesic Native Forest", "Dry Native Forest",
                      "Wet Native Shrubland", "Mesic Native Shrubland", "Dry Native Shrubland",
                      "Wet Native Grassland", "Mesic Native Grassland", "Dry Native Grassland",
                      "Wet Alien Forest",    "Mesic Alien Forest",    "Dry Alien Forest",
                      "Wet Alien Shrubland", "Mesic Alien Shrubland", "Dry Alien Shrubland",
                      "Wet Alien Grassland", "Mesic Alien Grassland", "Dry Alien Grassland",
                      "Other")))

# create simple native/alien categories
dat_landcover$Meta_Category3 <-
  ifelse(dat_landcover$Meta_Category2 %in% c("Wet Native Forest", "Mesic Native Forest", "Dry Native Forest"), 'Native forest',
  ifelse(dat_landcover$Meta_Category2 %in% c("Wet Alien Forest", "Mesic Alien Forest", "Dry Alien Forest"), 'Non-native forest',
  ifelse(dat_landcover$Meta_Category2 %in% c("Wet Alien Grassland", "Mesic Alien Grassland", "Dry Alien Grassland"), 'Non-native grass', 'Other')))
dat_landcover$Meta_Category3 <-
  factor(dat_landcover$Meta_Category3,
         levels = c('Non-native forest', 'Non-native grass', 'Native forest', 'Other'))

# # convert to raster
# ras_landcoverSimplified <-
#   rasterFromXYZ(dat_landcover[c('x', 'y', 'Meta_Category2')],
#                 crs = crs(ras_landcover))
gc()

# rename coords
colnames(dat_landcover)[colnames(dat_landcover) == 'x'] <- 'long'
colnames(dat_landcover)[colnames(dat_landcover) == 'y'] <- 'lat'
gc()




##### what percent of land is native vs non-native? #####

table(dat_landcover$Meta_Category3)
2533962/nrow(dat_landcover) * 100  # non-native forest: 13.64624%
3085468/nrow(dat_landcover) * 100  # non-native grass: 16.61629%
3806647/nrow(dat_landcover) * 100  # native forest: 20.50008%




##### create plot #####

# # color scale
# vec_colors <-
#   c('green3',  'green2',  'green1',  # native forest
#     'blue4',   'blue2',   'skyblue', # native shrub
#     'purple2',            # native grass (mesic only)
#     'red3',    'red2',    'red1',    # alien forest
#     'orange3', 'orange2', 'orange1', # alien shrub
#     'yellow3', 'yellow2', 'yellow1', # alien grass
#     'gray')                          # other

# plot
p <- ggplot() +
  geom_raster(data = dat_landcover,
              aes(x = long, y = lat, fill = Meta_Category3)) +
  scale_fill_manual(values = c('#7A0403', '#FABA39', '#4662D7', 'lightgray')) +
  geom_sf(data = sf_combined,
          color = 'white', fill = 'transparent', aes(linetype = type),
          linewidth = 1) +
  geom_sf(data = sf_combined,
          color = 'black', fill = 'transparent', aes(linetype = type),
          linewidth = 0.5) +
  ggsn::north(data = dat_landcover, location = 'topright', symbol = 9) +
  ggsn::scalebar(data = dat_landcover,
                 location = "bottomleft", transform = FALSE,
                 dist = 100, dist_unit = 'km', st.dist = 0.04) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), panel.background = element_blank(),
        text = element_text(size = 14),
        legend.title = element_blank(), legend.position = 'bottom',
        legend.box = "vertical")
ggsave(p,
       filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/",
                         "06 - combined simp landcover and mgmt areas.png"),
       width = 10, height = 8, units = 'in')
ggsave(p,
       filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/",
                         "06 - combined simp landcover and mgmt areas.pdf"),
       width = 10, height = 8, units = 'in')

gc()