
# This script plots maps of the change in ET due to the changing land cover

library(terra); library(sf); library(ggplot2); library(viridis); library(ggsn)




##### load data #####

# load ET rasters
list_rast_et <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11d - change in AET',
             pattern = 'mm per year', full.names = TRUE)
list_rast_et_names <- strsplit(list_rast_et, ' - ')
list_rast_et_names <- sapply(list_rast_et_names, function(x) substr(x[[3]], 14, nchar(x)))
list_rast_et <- lapply(list_rast_et, rast)
names(list_rast_et) <- list_rast_et_names
rm(list_rast_et_names)

# aggregate rasters
list_rast_et <- lapply(list_rast_et, aggregate, fact = 2, cores = length(list_rast_et))



##### format data for plotting #####

# convert to data.frames
list_dat_et <- lapply(list_rast_et, as.data.frame, xy = TRUE, na.rm = TRUE)
names(list_dat_et) <-  names(list_rast_et)
rm(list_rast_et); gc()

# change column names, add model name
for(i in 1:length(list_dat_et)){
  colnames(list_dat_et[[i]]) <- c('long', 'lat', 'et_delta')
  list_dat_et[[i]]$model <- names(list_dat_et)[[i]]
  print(i)
  gc()
}
rm(i)

# melt data
dat_et <- do.call(rbind, list_dat_et)
rm(list_dat_et)
gc()

# assign levels to model
dat_et$model <-
  factor(dat_et$model,
         levels = c('worst case 2070', 'worst case 2100', 'middle case 2070', 'middle case 2100', 'best case 2070', 'best case 2100'),
         labels = c('Scenario 1 - 2070', 'Scenario 1 - 2100', 'Scenario 2 - 2070', 'Scenario 2 - 2100', 'Scenario 3 - 2070', 'Scenario 3 - 2100')
         )
gc()

# create bins
dat_temp <- dat_et[dat_et$et_delta > 1 | dat_et$et_delta < -1,]; gc()
vec_breaks <- quantile(dat_temp$et_delta, probs = seq(0, 1, 0.2), na.rm = TRUE)
vec_breaks[which.min(abs(vec_breaks))] <- 0
dat_et$et_delta_bin <- cut(dat_et$et_delta, breaks = vec_breaks, include.lowest = TRUE)
rm(dat_temp)




##### create plot #####
dat_et0 <- dat_et[dat_et$et_delta == 0,]; gc()
p <- ggplot() +
  geom_raster(data = dat_et, aes(x = long, y = lat, fill = et_delta_bin)) +
  coord_equal() +
  facet_wrap(vars(model), nrow = 3) +
  scale_fill_viridis_d(option = 'H', direction = -1) +
  labs(fill = 'Change in ET\n(mm/yr)') +
  guides(fill = guide_legend(reverse = TRUE)) +
  scalebar(data = dat_et, dist_unit = 'km', dist = 100, transform = FALSE,
           facet.var = 'model', facet.lev = 'Scenario 3 - 2070',
           location = 'bottomleft', st.dist = 0.05) +
  north(data = dat_et, location = 'topright', symbol = 9) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), text = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
p <-
  p +
  geom_raster(data = dat_et0, aes(x = long, y = lat), fill = 'lightgray')
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - change in ET under spread models mm per yr.png'),
       dpi = 300, height = 9, width = 9)
