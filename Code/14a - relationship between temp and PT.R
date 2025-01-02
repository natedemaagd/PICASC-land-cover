
# This script looks at the relationship between Priestley-Taylor ET and temperature

library(terra); library(ggplot2)




##### load data #####

# PT and temp rasters
rast_pt <- rast("H:/My Drive/Projects/Data/rainfall_atlas_PriestlyTaylorPotentialET/pr0_mm_ann/w001001.adf")
rast_temp <- rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/2021_HI_FIre_Model_Data - PROTECT/tair_ann/HI_EVAP_mean_annual_temp_statewide_250m.tif")




##### format data #####

# convert to data.frames
dat_pt <- as.data.frame(rast_pt, xy = TRUE, na.rm = FALSE)
dat_temp <- as.data.frame(rast_temp, xy = FALSE, na.rm = FALSE)
gc()

# rename columns
colnames(dat_pt) <- c('x', 'y', 'PT_mm')
colnames(dat_temp) <- 'temp_C'

# combine data
dat <- cbind(dat_pt, dat_temp)
dat <- dat[complete.cases(dat),]
rm(dat_pt, dat_temp, rast_pt, rast_temp)
gc()




##### analyze #####

# plot to see relationship
ggplot(data = dat,
       aes(x = temp_C, y = PT_mm)) +
  geom_hex() +
  scale_fill_viridis_c() +
  labs(x = 'Mean annual temperature (C)', y = 'Mean annual Priestley-Taylor ET (mm)',
       fill = 'Pixel count') +
  theme(text = element_text(size = 14))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14a - relationship between temp and PT.png'),
       dpi = 300, width = 10, height = 5)
 