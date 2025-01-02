
# This script plots the Climate Atlas of Hawaii rasters used to model ET.

library(raster)
library(ggplot2)
library(ggsn)
library(cowplot)




##### load data #####

# load AllVars data
dat <-
  readRDS(paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11a - Tom G rasters resampled/",
                 "ras_data_combined_df.rds"))
gc()




##### melt required data #####

# keep only vars needed to plot (AET, rainfall, PT)
dat2 <-
  dat[c('x', 'y', 'AET', 'rainfall', 'PT')]
gc()

rm(dat); gc()

# melt data
dat <-
  with(dat2,
       data.frame(x = x, y = y,
                  value = c(AET, rainfall, PT),
                  var = rep(c('AET', 'rain', 'PT'),
                            each = nrow(dat2))
       )
  )

gc()
rm(dat2); gc()

# # convert to factor to plot in specific order
# dat$var <-
#   factor(dat$var,
#          levels = c('AET', 'rain', 'PT'),
#          labels = c('Actual ET (mm/year)',
#                     'Rainfall (mm/yr)', 'Priestly-Taylor ET (mm)'))
# gc()

# change coordinate variable names for plotting
colnames(dat)[colnames(dat) == 'x'] <- 'long'
colnames(dat)[colnames(dat) == 'y'] <- 'lat'
gc()

# remove NAs
dat <- dat[complete.cases(dat),]
gc()

# convert to m/year
dat$val <- dat$val / 1000




##### plot faceted figure #####

# p <-
#   ggplot(data = dat,
#          aes(x = long, y = lat, fill = value)) +
#   geom_raster() +
#   coord_equal() +
#   facet_grid(rows = vars(var),
#              switch = 'y') +
#   scalebar(data = dat, location = "bottomleft", transform = FALSE,
#            dist = 100, dist_unit = 'km', st.dist = 0.04,
#            facet.var = 'var',
#            facet.lev = 'Priestly-Taylor ET (mm)') +
#   #north(data = dat, location = 'topright', symbol = 9) +
#   scale_fill_viridis_c(option = 'H', name = 'mm/year') +
#   theme(axis.title = element_blank(), axis.text = element_blank(),
#         axis.ticks = element_blank(), panel.background = element_blank(),
#         legend.position = 'bottom')
# gc()
# 
# png(file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/',
#                   '12-04-01 climate maps.png'),
#     width = 6, height = 10, units = 'in', res = 300)
# north2(p, symbol = 9, x = 0.95, y = 0.95)
# dev.off()




##### plot separately then stitch together #####

# split data
dat_split <- split(dat, f = dat$var)
rm(dat); gc()

# AET plot
p_aet <-
  ggplot(data = dat_split$AET,
         aes(x = long, y = lat, fill = val)) +
  geom_raster() +
  coord_equal() +
  labs(title = 'Actual evapotranspiration') +
  # scalebar(data = dat, location = "bottomleft", transform = FALSE,
  #          dist = 100, dist_unit = 'km', st.dist = 0.04,
  #          facet.var = 'var',
  #          facet.lev = 'Priestly-Taylor ET (mm)') +
  north(data = dat_split$AET, location = 'topright', symbol = 9) +
  scale_fill_viridis_c(option = 'H', name = 'm/year',
                       breaks = c(min(dat_split$AET$val),
                                  (min(dat_split$AET$val) + max(dat_split$AET$val)) / 2,
                                  max(dat_split$AET$val)),
                       labels = round(c(min(dat_split$AET$val),
                                        (min(dat_split$AET$val) + max(dat_split$AET$val)) / 2,
                                        max(dat_split$AET$val)),
                                      digits = 1)
                       ) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        legend.position = 'bottom', text = element_text(size = 14))
gc()

# Rainfall plot
p_rain <-
  ggplot(data = dat_split$rain,
         aes(x = long, y = lat, fill = val)) +
  geom_raster() +
  coord_equal() +
  labs(title = 'Rainfall') +
  # scalebar(data = dat, location = "bottomleft", transform = FALSE,
  #          dist = 100, dist_unit = 'km', st.dist = 0.04) +
  # north(data = dat_split$AET, location = 'topright', symbol = 9) +
  scale_fill_viridis_c(option = 'H', name = 'm/year',
                       breaks = c(min(dat_split$rain$val),
                                  (min(dat_split$rain$val) + max(dat_split$rain$val)) / 2,
                                  max(dat_split$rain$val)),
                       labels = round(c(min(dat_split$rain$val),
                                        (min(dat_split$rain$val) + max(dat_split$rain$val)) / 2,
                                        max(dat_split$rain$val)),
                                      digits = 1)) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        legend.position = 'bottom', text = element_text(size = 14))
gc()

# PT plot
p_pt <-
  ggplot(data = dat_split$PT,
         aes(x = long, y = lat, fill = val)) +
  geom_raster() +
  coord_equal() +
  labs(title = 'Priestly-Taylor evapotranspiration') +
  scalebar(data = dat_split$PT, location = "bottomleft", transform = FALSE,
           dist = 100, dist_unit = 'km', st.dist = 0.04) +
  #north(data = dat_split$AET, location = 'topright', symbol = 9) +
  scale_fill_viridis_c(option = 'H', name = 'm/year',
                       breaks = c(min(dat_split$PT$val),
                                  (min(dat_split$PT$val) + max(dat_split$PT$val)) / 2,
                                  max(dat_split$PT$val)),
                       labels = round(c(min(dat_split$PT$val),
                                        (min(dat_split$PT$val) + max(dat_split$PT$val)) / 2,
                                        max(dat_split$PT$val)),
                                      digits = 1)) +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        legend.position = 'bottom', text = element_text(size = 14))

plot_grid(p_aet, p_rain, p_pt, nrow = 3)

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/',
                         '12-04-01 climate atlas maps.png'),
       dpi = 300, height = 12, width = 7)

gc()




##### figures alternative - binned instead of continuous #####

# AET plot
p_aet <-
  ggplot(data = dat_split$AET,
         aes(x = long, y = lat, fill = val)) +
  geom_raster() +
  coord_equal() +
  labs(title = 'Actual evapotranspiration') +
  # scalebar(data = dat, location = "bottomleft", transform = FALSE,
  #          dist = 100, dist_unit = 'km', st.dist = 0.04,
  #          facet.var = 'var',
  #          facet.lev = 'Priestly-Taylor ET (mm)') +
  north(data = dat_split$AET, location = 'topright', symbol = 9) +
  scale_fill_viridis_b(option = 'H', name = 'm/year') +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        legend.position = 'bottom', text = element_text(size = 14))
gc()

# Rainfall plot
p_rain <-
  ggplot(data = dat_split$rain,
         aes(x = long, y = lat, fill = val)) +
  geom_raster() +
  coord_equal() +
  labs(title = 'Rainfall') +
  # scalebar(data = dat, location = "bottomleft", transform = FALSE,
  #          dist = 100, dist_unit = 'km', st.dist = 0.04) +
  # north(data = dat_split$AET, location = 'topright', symbol = 9) +
  scale_fill_viridis_b(option = 'H', name = 'm/year') +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        legend.position = 'bottom', text = element_text(size = 14))
gc()

# PT plot
p_pt <-
  ggplot(data = dat_split$PT,
         aes(x = long, y = lat, fill = val)) +
  geom_raster() +
  coord_equal() +
  labs(title = 'Priestly-Taylor evapotranspiration') +
  scalebar(data = dat_split$PT, location = "bottomleft", transform = FALSE,
           dist = 100, dist_unit = 'km', st.dist = 0.04) +
  #north(data = dat_split$AET, location = 'topright', symbol = 9) +
  scale_fill_viridis_b(option = 'H', name = 'm/year') +
  theme(axis.title = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), panel.background = element_blank(),
        legend.position = 'bottom', text = element_text(size = 14))

plot_grid(p_aet, p_rain, p_pt, nrow = 3)

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/12 figures/',
                         '12-04-01 climate atlas maps - binned.png'),
       dpi = 300, height = 12, width = 7)

gc()
