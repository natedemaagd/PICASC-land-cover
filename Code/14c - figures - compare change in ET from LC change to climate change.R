
# This script compares the change in ET from climate change to the change in ET
# from the change in land cover

library(sf); library(terra); library(ggplot2); library(ggsn)




##### load data #####

# load climate change ET data
dat_ETCC <- readRDS("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/12a - ET under all scenarios.rds")

# load land cover change ET data
list_rast_LCchange_names <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11d - change in AET',
             pattern = 'mm per year', full.names = TRUE)
list_rast_LCchange <-
  lapply(list_rast_LCchange_names, rast)
names(list_rast_LCchange_names) <-
  c('best 2070', 'best 2100', 'middle 2070', 'middle 2100', 'worst 2070', 'worst 2100')




##### format data #####

# match CRSs
list_rast_LCchange <-
  lapply(list_rast_LCchange,
         function(r) project(r, '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs',
                             threads = TRUE))
gc()




##### climate plots - individual #####

# RCP 4.5 IPRC
dat_plot <-
  data.frame(long = dat_ETCC$x,
             lat = dat_ETCC$y,
             ET_delta = dat_ETCC$AET_dynRcp45Iprc - dat_ETCC$AET_dynBaseline)
vec_breaks <- quantile(dat_plot$ET_delta, probs = seq(0, 1, 0.2), na.rm = TRUE)
vec_breaks[which.min(abs(vec_breaks))] <- 0
dat_plot$ET_delta_bin <-
  cut(dat_plot$ET_delta, breaks = vec_breaks, include.lowest = TRUE)
dat_plot <- dat_plot[!is.na(dat_plot$ET_delta_bin),]
gc()
p <- ggplot(data = dat_plot,
       aes(x = long, y = lat, fill = ET_delta_bin)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_d(option = 'H', direction = -1) +
  scalebar(data = dat_plot, dist_unit = 'km', dist = 100, transform = FALSE,
           location = 'bottomleft', st.dist = 0.03) +
  north(data = dat_plot, location = 'topright', symbol = 9) +
  labs(fill = 'Change in ET\n(mm/yr)') +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        text = element_text(size = 14))
ggsave(p,
       filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/14c - change in ET - IPRC RCP 4.5.png',
       dpi = 300, height = 6, width = 9)
rm(p, vec_breaks, dat_plot)
gc()

# RCP 8.5 IPRC
dat_plot <-
  data.frame(long = dat_ETCC$x,
             lat = dat_ETCC$y,
             ET_delta = dat_ETCC$AET_dynRcp85Iprc - dat_ETCC$AET_dynBaseline)
vec_breaks <- quantile(dat_plot$ET_delta, probs = seq(0, 1, 0.2), na.rm = TRUE)
vec_breaks[which.min(abs(vec_breaks))] <- 0
dat_plot$ET_delta_bin <-
  cut(dat_plot$ET_delta, breaks = vec_breaks, include.lowest = TRUE)
dat_plot <- dat_plot[!is.na(dat_plot$ET_delta_bin),]
gc()
p <- ggplot(data = dat_plot,
            aes(x = long, y = lat, fill = ET_delta_bin)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_d(option = 'H', direction = -1) +
  scalebar(data = dat_plot, dist_unit = 'km', dist = 100, transform = FALSE,
           location = 'bottomleft', st.dist = 0.03) +
  north(data = dat_plot, location = 'topright', symbol = 9) +
  labs(fill = 'Change in ET\n(mm/yr)') +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        text = element_text(size = 14))
ggsave(p,
       filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/14c - change in ET - IPRC RCP 8.5.png',
       dpi = 300, height = 6, width = 9)
rm(p, vec_breaks, dat_plot)
gc()

# RCP 8.5 NCAR
dat_plot <-
  data.frame(long = dat_ETCC$x,
             lat = dat_ETCC$y,
             ET_delta = dat_ETCC$AET_dynRcp85Ncar - dat_ETCC$AET_dynBaseline)
vec_breaks <- quantile(dat_plot$ET_delta, probs = seq(0, 1, 0.2), na.rm = TRUE)
vec_breaks[which.min(abs(vec_breaks))] <- 0
dat_plot$ET_delta_bin <-
  cut(dat_plot$ET_delta, breaks = vec_breaks, include.lowest = TRUE)
dat_plot <- dat_plot[!is.na(dat_plot$ET_delta_bin),]
gc()
p <- ggplot(data = dat_plot,
            aes(x = long, y = lat, fill = ET_delta_bin)) +
  geom_raster() +
  coord_equal() +
  scale_fill_viridis_d(option = 'H', direction = -1) +
  scalebar(data = dat_plot, dist_unit = 'km', dist = 100, transform = FALSE,
           location = 'bottomleft', st.dist = 0.03) +
  north(data = dat_plot, location = 'topright', symbol = 9) +
  labs(fill = 'Change in ET\n(mm/yr)') +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(panel.background = element_blank(), axis.title = element_blank(),
        axis.ticks = element_blank(), axis.text = element_blank(),
        text = element_text(size = 14))
ggsave(p,
       filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/14c - change in ET - NCAR RCP 8.5.png',
       dpi = 300, height = 6, width = 9)
rm(p, vec_breaks, dat_plot)
gc()




##### climate plots - panel #####

# melt data
dat_plot <-
  data.frame(long = dat_ETCC$x, lat = dat_ETCC$y,
             value = c(dat_ETCC$AET_dynRcp45Iprc - dat_ETCC$AET_dynBaseline,
                       dat_ETCC$AET_dynRcp85Iprc - dat_ETCC$AET_dynBaseline,
                       dat_ETCC$AET_dynRcp85Ncar - dat_ETCC$AET_dynBaseline),
             model = rep(c('IPRC RCP 4.5', 'IPRC RCP 8.5', 'NCAR RCP 8.5'),
                         each = nrow(dat_ETCC)))
dat_plot$model <-
  factor(dat_plot$model,
         levels = c('IPRC RCP 4.5', 'IPRC RCP 8.5', 'NCAR RCP 8.5'))
vec_breaks <- quantile(dat_plot$value, probs = seq(0, 1, 0.2), na.rm = TRUE)
vec_breaks[which.min(abs(vec_breaks))] <- 0
dat_plot$value_bin <-
  cut(dat_plot$value, breaks = vec_breaks, include.lowest = TRUE)
dat_plot <- dat_plot[!is.na(dat_plot$value_bin),]
gc()


# plot
p <- ggplot(data = dat_plot,
            aes(x = long, y = lat, fill = value_bin)) +
  geom_raster() +
  coord_equal() +
  facet_wrap(vars(model), nrow = 3) +
  scale_fill_viridis_d(option = 'H', direction = -1) +
  labs(fill = 'Change in ET\n(mm/yr)') +
  guides(fill = guide_legend(reverse = TRUE)) +
  scalebar(data = dat_plot, dist_unit = 'km', dist = 100, transform = FALSE,
           facet.var = 'model', facet.lev = 'NCAR RCP 8.5',
           location = 'bottomleft', st.dist = 0.05) +
  north(data = dat_plot, location = 'topright', symbol = 9) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), text = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
ggsave(p,
       filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/14c - change in ET - all models panel.png',
       dpi = 300, height = 10, width = 6)
rm(p, vec_breaks, dat_plot)
gc()
