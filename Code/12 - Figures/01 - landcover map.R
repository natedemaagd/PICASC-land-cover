
# This script creates a map of the MHI landcover raster

library(ggplot2)
library(ggsn)
library(raster)
library(terra)
library(dplyr)
library(RColorBrewer)

# load landcover raster
ras_landcover <-
  raster(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/MHI landcover raster/",
                "mhi_land_cover_names.tif"))

# convert to data.frame
dat_landcover <- as.data.frame(ras_landcover, xy = TRUE); gc()
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

# # convert to raster
# ras_landcoverSimplified <-
#   rasterFromXYZ(dat_landcover[c('x', 'y', 'Meta_Category2')],
#                 crs = crs(ras_landcover))
gc()

# rename coords
colnames(dat_landcover)[colnames(dat_landcover) == 'x'] <- 'long'
colnames(dat_landcover)[colnames(dat_landcover) == 'y'] <- 'lat'
gc()




##### create plot #####

# color scale
vec_colors <-
  c('green3',  'green2',  'green1',  # native forest
    'blue4',   'blue2',   'skyblue', # native shrub
               'purple2',            # native grass (mesic only)
    'red3',    'red2',    'red1',    # alien forest
    'orange3', 'orange2', 'orange1', # alien shrub
    'yellow3', 'yellow2', 'yellow1', # alien grass
    'gray')                          # other

# plot
ggplot(data = dat_landcover,
       aes(x = long, y = lat, fill = Meta_Category2)) +
  geom_raster() +
  coord_equal() +
  scale_fill_manual(values = vec_colors) +
  ggsn::north(data = dat_landcover, location = 'topright', symbol = 9) +
  ggsn::scalebar(data = dat_landcover,
           location = "bottomleft", transform = FALSE,
           dist = 100, dist_unit = 'km', st.dist = 0.04) +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), panel.background = element_blank(),
        text = element_text(size = 14),
        legend.title = element_blank(), legend.position = 'bottom')
ggsave(filename = paste0("H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/",
                         "12-01-01 landcover map.png"),
       width = 10, height = 8, units = 'in')

gc()
