
# This script plots maps of rainfall changes due to climate change

library(terra); library(ggplot2); library(ggsn)




##### load and format data #####

# dynamical downscale data
ras_rain_dynBaseline <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_baseline.tif")
ras_rain_dynRcp45Iprc <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_future_projection_rcp45_IPRC.tif")
ras_rain_dynRcp85Iprc <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_future_projection_rcp85_IPRC.tif")
ras_rain_dynRcp85Ncar <-
  rast("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/hawaii climate change - dynamical downscale/annual_precip_future_projection_rcp85_NCAR.tif")

# rasters are in mm/month. Convert to mm/year
ras_rain_dynBaseline <- ras_rain_dynBaseline * 12
ras_rain_dynRcp45Iprc <- ras_rain_dynRcp45Iprc * 12
ras_rain_dynRcp85Iprc <- ras_rain_dynRcp85Iprc * 12
ras_rain_dynRcp85Ncar <- ras_rain_dynRcp85Ncar * 12

# get change in mm/year
ras_rain_dynRcp45Iprc_diff <- ras_rain_dynRcp45Iprc - ras_rain_dynBaseline
ras_rain_dynRcp85Iprc_diff <- ras_rain_dynRcp85Iprc - ras_rain_dynBaseline
ras_rain_dynRcp85Ncar_diff <- ras_rain_dynRcp85Ncar - ras_rain_dynBaseline

# get pct change
ras_rain_dynRcp45Iprc_pctChange <- (ras_rain_dynRcp45Iprc - ras_rain_dynBaseline) / ras_rain_dynBaseline * 100
ras_rain_dynRcp85Iprc_pctChange <- (ras_rain_dynRcp85Iprc - ras_rain_dynBaseline) / ras_rain_dynBaseline * 100
ras_rain_dynRcp85Ncar_pctChange <- (ras_rain_dynRcp85Ncar - ras_rain_dynBaseline) / ras_rain_dynBaseline * 100

rm(ras_rain_dynBaseline, ras_rain_dynRcp45Iprc, ras_rain_dynRcp85Iprc, ras_rain_dynRcp85Ncar)
gc()




##### convert rasters to data.frames #####

# difference rasters
dat_Rcp45Iprc_diff <- as.data.frame(ras_rain_dynRcp45Iprc_diff, na.rm = TRUE, xy = TRUE)
colnames(dat_Rcp45Iprc_diff) <- c('long', 'lat', 'mm_delta')
dat_Rcp45Iprc_diff$model <- 'IPRC RCP 4.5'
dat_Rcp85Iprc_diff <- as.data.frame(ras_rain_dynRcp85Iprc_diff, na.rm = TRUE, xy = TRUE)
colnames(dat_Rcp85Iprc_diff) <- c('long', 'lat', 'mm_delta')
dat_Rcp85Iprc_diff$model <- 'IPRC RCP 8.5'
dat_Rcp85Ncar_diff <- as.data.frame(ras_rain_dynRcp85Ncar_diff, na.rm = TRUE, xy = TRUE)
colnames(dat_Rcp85Ncar_diff) <- c('long', 'lat', 'mm_delta')
dat_Rcp85Ncar_diff$model <- 'NCAR RCP 8.5'
dat_diff <- rbind(dat_Rcp45Iprc_diff, dat_Rcp85Iprc_diff, dat_Rcp85Ncar_diff)
rm(dat_Rcp45Iprc_diff, dat_Rcp85Iprc_diff, dat_Rcp85Ncar_diff,
   ras_rain_dynRcp45Iprc_diff, ras_rain_dynRcp85Iprc_diff, ras_rain_dynRcp85Ncar_diff)
gc()

# percent change rasters
dat_Rcp45Iprc_pctChange <- as.data.frame(ras_rain_dynRcp45Iprc_pctChange, na.rm = TRUE, xy = TRUE)
colnames(dat_Rcp45Iprc_pctChange) <- c('long', 'lat', 'mm_delta')
dat_Rcp45Iprc_pctChange$model <- 'IPRC RCP 4.5'
dat_Rcp85Iprc_pctChange <- as.data.frame(ras_rain_dynRcp85Iprc_pctChange, na.rm = TRUE, xy = TRUE)
colnames(dat_Rcp85Iprc_pctChange) <- c('long', 'lat', 'mm_delta')
dat_Rcp85Iprc_pctChange$model <- 'IPRC RCP 8.5'
dat_Rcp85Ncar_pctChange <- as.data.frame(ras_rain_dynRcp85Ncar_pctChange, na.rm = TRUE, xy = TRUE)
colnames(dat_Rcp85Ncar_pctChange) <- c('long', 'lat', 'mm_delta')
dat_Rcp85Ncar_pctChange$model <- 'NCAR RCP 8.5'
dat_pctChange <- rbind(dat_Rcp45Iprc_pctChange, dat_Rcp85Iprc_pctChange, dat_Rcp85Ncar_pctChange)
rm(dat_Rcp45Iprc_pctChange, dat_Rcp85Iprc_pctChange, dat_Rcp85Ncar_pctChange,
   ras_rain_dynRcp45Iprc_pctChange, ras_rain_dynRcp85Iprc_pctChange, ras_rain_dynRcp85Ncar_pctChange)
gc()




##### plot #####

# difference
p_diff <- ggplot(data = dat_diff,
       aes(x = long, y = lat, fill = mm_delta)) +
  geom_raster() +
  coord_equal() +
  facet_wrap(vars(model), nrow = 3) +
  scale_fill_viridis_b(option = 'H', direction = -1) +
  labs(fill = 'Change in mean rainfall\n(mm/yr)') +
  scalebar(data = dat_diff, dist_unit = 'km', dist = 100, transform = TRUE,
           facet.var = 'model', facet.lev = 'NCAR RCP 8.5',
           location = 'bottomleft', st.dist = 0.05) +
  north(data = dat_diff, location = 'topright', symbol = 9) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), text = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
ggsave(p_diff,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14b - change in rainfall under climate change mm per yr.png'),
       dpi = 300, height = 8, width = 6)

# percent change
p_pctChange <- ggplot(data = dat_pctChange,
                 aes(x = long, y = lat, fill = mm_delta)) +
  geom_raster() +
  coord_equal() +
  facet_wrap(vars(model), nrow = 3) +
  scale_fill_viridis_b(option = 'H', direction = -1, end = 0.8) +
  labs(fill = '% change in mean\nannual rainfall') +
  scalebar(data = dat_pctChange, dist_unit = 'km', dist = 100, transform = TRUE,
           facet.var = 'model', facet.lev = 'NCAR RCP 8.5',
           location = 'bottomleft', st.dist = 0.05) +
  north(data = dat_pctChange, location = 'topright', symbol = 9) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), text = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
ggsave(p_pctChange,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14b - change in rainfall under climate change pct change.png'),
       dpi = 300, height = 8, width = 6)
