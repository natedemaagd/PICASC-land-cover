
# This script plots maps of the change in ET due to the changing land cover

library(terra); library(sf); library(ggplot2); library(viridis); library(ggsn)
library(tidyverse); library(ggrepel); #library(ggnewscale)




##### load data #####

# load ET rasters
list_rast_et <-
  list(rast_baseline = rast('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted pre-invasion.tif'),
       rast_best2070 = rast('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted best case 2070.tif'),
       rast_best2100 = rast('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted best case 2100.tif'),
       rast_middle2070 = rast('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted middle case 2070.tif'),
       rast_middle2100 = rast('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted middle case 2100.tif'),
       rast_worst2070 = rast('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted worst case 2070.tif'),
       rast_worst2100 = rast('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/11 water yield/11c - ras AET predicted worst case 2100.tif'))

# create water yield raster (rain - ET)
rast_rain <- rast('H:/My Drive/Projects/Data/rainfall_atlas_ann_rainfall_inches/Hawaii-State/rfgrid_inches_state_ann.txt')
rast_aet <- rast('H:/My Drive/Projects/Data/rainfall_atlas_AET/aet_mm_ann.txt')
rast_rain <- rast_rain * 25.4  # convert inches rainfall to mm
rast_wyTotal <- rast_rain - rast_aet
rast_wyTotal <- project(rast_wyTotal, list_rast_et$rast_worst2070, threads = TRUE, method = 'cubic')
rm(rast_rain, rast_aet); gc()

# get required differences and percent changes
list_rast_diffET <-
  list(rast_worst2070_diff = list_rast_et$rast_worst2070 - list_rast_et$rast_baseline,
       rast_worst2100_diff = list_rast_et$rast_worst2100 - list_rast_et$rast_baseline,
       rast_bestworst2070_diff = list_rast_et$rast_best2070 - list_rast_et$rast_worst2070,
       rast_bestworst2100_diff = list_rast_et$rast_best2100 - list_rast_et$rast_worst2100,
       rast_midworst2070_diff = list_rast_et$rast_middle2070 - list_rast_et$rast_worst2070,
       rast_midworst2100_diff = list_rast_et$rast_middle2100 - list_rast_et$rast_worst2100)
# list_rast_pctChangeET <-
#   list(rast_worst2070_diff = (list_rast_et$rast_worst2070 - list_rast_et$rast_baseline) / list_rast_et$rast_baseline * 100,
#        rast_worst2100_diff = (list_rast_et$rast_worst2100 - list_rast_et$rast_baseline) / list_rast_et$rast_baseline * 100,
#        rast_bestworst2070_diff = (list_rast_et$rast_best2070 - list_rast_et$rast_worst2070) / list_rast_et$rast_worst2070 * 100,
#        rast_bestworst2100_diff = (list_rast_et$rast_best2100 - list_rast_et$rast_worst2100) / list_rast_et$rast_worst2100 * 100,
#        rast_midworst2070_diff = (list_rast_et$rast_middle2070 - list_rast_et$rast_worst2070 / list_rast_et$rast_worst2070 * 100),
#        rast_midworst2100_diff = (list_rast_et$rast_middle2100 - list_rast_et$rast_worst2100 / list_rast_et$rast_worst2100 * 100))
gc()

# # aggregate for plotting
# list_rast_diffET <- lapply(list_rast_diffET, aggregate, fact = 2, cores = length(list_rast_diffET))
# gc()

# define scenarios
vec_scenarios <- c('worst2070',     'worst2100',
                   'bestworst2070', 'bestworst2100',
                   'midworst2070',  'midworst2100')
vec_scenarios_pctChange <-
  c('baseline',
    'best2070',   'best2100',
    'middle2070', 'middle2100',
    'worst2070',  'worst2100')

# load aquifer unit shapefile
sf_aquifer <- read_sf('H:/My Drive/Projects/Data/Shapefiles/DLNR_Aquifers/DLNR_Aquifers.shp')
sf_aquifer <- st_transform(sf_aquifer, crs(list_rast_et$rast_baseline))




##### format data for plotting #####

# convert to data.frames
list_dat <- lapply(list_rast_diffET, as.data.frame, xy = TRUE, na.rm = TRUE)
names(list_dat) <- names(list_rast_diffET)
gc()

# column names
for(i in 1:length(list_dat)){
  colnames(list_dat[[i]]) <- c('long', 'lat', 'et_delta')
  list_dat[[i]]$model <- names(list_dat)[[i]]
}
rm(i)

# melt data
dat_worstBaseline <- rbind(list_dat$rast_worst2070_diff, list_dat$rast_worst2100_diff)
list_dat <- list_dat[3:length(list_dat)]
dat_scenarios <- do.call(rbind, list_dat)
rm(list_dat); gc()

# convert from ET to water yield
dat_worstBaseline$wy_delta <- (dat_worstBaseline$et_delta * 30000 * 30000) * (0.000001 / 365) * -1  # (cubic mm) to (liters per day) * -1
dat_scenarios$wy_delta     <- (dat_scenarios$et_delta     * 30000 * 30000) * (0.000001 / 365) * -1

# create labels
dat_worstBaseline$model_lab <-
  factor(dat_worstBaseline$model,
         levels = c('rast_worst2070_diff', 'rast_worst2100_diff'),
         labels = c('(a) Delta, no protection 2070', '(b) Delta, no protection 2100'))
dat_scenarios$model_lab <-
  factor(dat_scenarios$model,
         levels = c('rast_midworst2070_diff', 'rast_midworst2100_diff', 'rast_bestworst2070_diff', 'rast_bestworst2100_diff'),
         labels = c('(a) Delta, targeted\nprotection 2070', '(b) Delta, targeted\nprotection 2100', '(c) Delta, targeted prot.\nand rest. 2070', '(d) Delta, targeted prot.\nand rest. 2100'))
gc()

# create water yield bins
dat_temp <- rbind(dat_scenarios, dat_worstBaseline)
dat_temp <- dat_temp[dat_temp$et_delta > 1 | dat_temp$et_delta < -1,]; gc()
vec_breaks <- quantile(dat_temp$wy_delta, probs = seq(0, 1, 0.2), na.rm = TRUE)
vec_breaks[which.min(abs(vec_breaks))] <- 0
dat_scenarios$wy_delta_bin <- cut(dat_scenarios$wy_delta, breaks = vec_breaks, include.lowest = TRUE)
dat_worstBaseline$wy_delta_bin <- cut(dat_worstBaseline$wy_delta, breaks = vec_breaks, include.lowest = TRUE)
rm(dat_temp, vec_breaks); gc()




##### create plot - compare worst case to baseline #####

# get pixels where yield didn't change
dat_worstBaseline_0 <- dat_worstBaseline[dat_worstBaseline$wy_delta == 0,]; gc()

# create initial plot
p <- ggplot() +
  geom_raster(data = dat_worstBaseline, aes(x = long, y = lat, fill = wy_delta_bin)) +
  coord_equal() +
  facet_wrap(vars(model_lab), ncol = 1) +
  scale_fill_viridis_d(option = 'H', direction = -1) +
  labs(fill = 'Change in water\nyield (L/day)') +
  guides(fill = guide_legend(reverse = TRUE)) +
  scalebar(data = dat_worstBaseline, dist_unit = 'km', dist = 100, transform = FALSE,
           facet.var = 'model_lab', facet.lev = '(b) Delta, no protection 2100',
           location = 'bottomleft', st.dist = 0.05) +
  north(data = dat_worstBaseline, location = 'topright', symbol = 9) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), text = element_text(size = 14),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))

# gray out pixels with 0 change
p <- p +
  geom_raster(data = dat_worstBaseline_0,
              aes(x = long, y = lat), fill = 'lightgray')
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - change in ET under spread models mm per yr - worst case compared to baseline.png'),
       dpi = 300, height = 9, width = 9)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - change in ET under spread models mm per yr - worst case compared to baseline.pdf'),
       dpi = 300, height = 9, width = 9)

rm(dat_worstBaseline, dat_worstBaseline_0, p)
gc()




##### create plot - compare middle and best cases with worst case #####

# get pixels where yield didn't change
dat_scenarios_0 <- dat_scenarios[dat_scenarios$wy_delta == 0,]; gc()

# create initial plot
p <- ggplot() +
  geom_raster(data = dat_scenarios, aes(x = long, y = lat, fill = wy_delta_bin)) +
  coord_equal() +
  facet_wrap(vars(model_lab), nrow = 2) +
  scale_fill_viridis_d(option = 'H', direction = -1) +
  labs(fill = 'Change in water\nyield (L/day)') +
  guides(fill = guide_legend(reverse = TRUE)) +
  scalebar(data = dat_scenarios, dist_unit = 'km', dist = 100, transform = FALSE,
           facet.var = 'model_lab', facet.lev = '(c) Delta, targeted prot.\nand rest. 2070',
           location = 'bottomleft', st.dist = 0.05) +
  north(data = dat_scenarios, location = 'topright', symbol = 9) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
        panel.background = element_blank(), text = element_text(size = 14),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))

# gray out pixels with 0 change
p <- p +
  geom_raster(data = dat_scenarios_0,
              aes(x = long, y = lat), fill = 'lightgray')
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - change in ET under spread models mm per yr - scenarios compared to worst case.png'),
       dpi = 300, height = 9, width = 9)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - change in ET under spread models mm per yr - scenarios compared to worst case.pdf'),
       dpi = 300, height = 9, width = 9)

rm(dat_scenarios, dat_scenarios_0, p)
gc()




##### PART 2: aggregate data to aquifer units #####

# clear environment if needed
rm(dat_scenarios, dat_scenarios_0,
   dat_worstBaseline, dat_worstBaseline_0, p)
gc()

# convert ET differences to water yield differences (L per day)
list_rast_diffwy <- lapply(list_rast_diffET, function(r){
  (r * 30000 * 30000) * (0.000001 / 365) * -1
  })
rm(list_rast_diffET); gc()

# split ET (levels) rasters by aquifer
list_rast_et_byaquifer <- list()
for(l in 1:length(list_rast_et)){
  list_rast_et_byaquifer[[l]] <-
    lapply(1:nrow(sf_aquifer), function(i) {
      mask(crop(list_rast_et[[l]], sf_aquifer[i,]), sf_aquifer[i,])
    })
}
names(list_rast_et_byaquifer) <- vec_scenarios
rm(l, list_rast_et)
gc()

# split water yield difference data by aquifer
list_rast_diffwy_byaquifer <- list()
for(l in 1:length(list_rast_diffwy)){
  list_rast_diffwy_byaquifer[[l]] <-
    lapply(1:nrow(sf_aquifer), function(i) {
      mask(crop(list_rast_diffwy[[l]], sf_aquifer[i,]), sf_aquifer[i,])
    })
}
names(list_rast_diffwy_byaquifer) <- vec_scenarios
rm(l, list_rast_diffwy)
gc()

# convert ET (levels) splits to data.frames
list_dat_et_byaquifer <- list()
for(l in 1:length(list_rast_et_byaquifer)){
  list_dat_et_byaquifer[[l]] <- list()
  for(r in 1:length(list_rast_et_byaquifer[[l]])){
    list_dat_et_byaquifer[[l]][[r]] <-
      as.data.frame(list_rast_et_byaquifer[[l]][[r]], xy = TRUE)
  }
}
names(list_dat_et_byaquifer) <- vec_scenarios_pctChange
rm(l, r)

# convert water yield splits to data.frames
list_dat_diffwy_byaquifer <- list()
for(l in 1:length(list_rast_diffwy_byaquifer)){
  list_dat_diffwy_byaquifer[[l]] <- list()
  for(r in 1:length(list_rast_diffwy_byaquifer[[l]])){
    list_dat_diffwy_byaquifer[[l]][[r]] <-
      as.data.frame(list_rast_diffwy_byaquifer[[l]][[r]], xy = TRUE)
  }
}
names(list_dat_diffwy_byaquifer) <- vec_scenarios
rm(l, r)

# change column names
for(l in 1:length(list_dat_et_byaquifer)){
  for(r in 1:length(list_dat_et_byaquifer[[l]])){
    colnames(list_dat_et_byaquifer[[l]][[r]]) <- c('x', 'y', 'et')
  }
}
for(l in 1:length(list_dat_diffwy_byaquifer)){
  for(r in 1:length(list_dat_diffwy_byaquifer[[l]])){
    colnames(list_dat_diffwy_byaquifer[[l]][[r]]) <- c('x', 'y', 'lpd')
  }
}

# add scenario and aquifer IDs - ET (levels)
for(l in 1:length(list_dat_et_byaquifer)){
  for(r in 1:length(list_dat_et_byaquifer[[l]])){
    if(nrow(list_dat_et_byaquifer[[l]][[r]]) == 0){
      list_dat_et_byaquifer[[l]][[r]] <-
        data.frame(x = NA, y = NA, lpd = 0)
    }
    list_dat_et_byaquifer[[l]][[r]]$scenario <- vec_scenarios_pctChange[[l]]
    list_dat_et_byaquifer[[l]][[r]]$objectid <- sf_aquifer$objectid[[r]]
  }
}
rm(l, r, list_rast_et_byaquifer)
gc()

# add scenario and aquifer IDs - water yield diff
for(l in 1:length(list_dat_diffwy_byaquifer)){
  for(r in 1:length(list_dat_diffwy_byaquifer[[l]])){
    if(nrow(list_dat_diffwy_byaquifer[[l]][[r]]) == 0){
      list_dat_diffwy_byaquifer[[l]][[r]] <-
        data.frame(x = NA, y = NA, lpd = 0)
    }
    list_dat_diffwy_byaquifer[[l]][[r]]$scenario <- vec_scenarios[[l]]
    list_dat_diffwy_byaquifer[[l]][[r]]$objectid <- sf_aquifer$objectid[[r]]
  }
}
rm(l, r, list_rast_diffwy_byaquifer)
gc()

# add percent change water yield to shapefile
list_dat_et_byaquifer2 <- list()
for(i in 1:length(list_dat_et_byaquifer)){
  list_dat_et_byaquifer2[[i]] <- list()
  for(j in 1:length(list_dat_et_byaquifer[[i]])){
    list_dat_et_byaquifer2[[i]][[j]] <-
      list_dat_et_byaquifer[[i]][[j]][c('objectid', 'et')]
  }
}
names(list_dat_et_byaquifer2) <- vec_scenarios_pctChange
list_dat_et_byaquifer2 <- lapply(list_dat_et_byaquifer2, function(l) do.call(rbind, l))
gc()
list_dat_et_byaquifer2 <-
  lapply(list_dat_et_byaquifer2, function(df){
    as.data.frame(aggregate(df$et, list(df$objectid), sum, na.rm = TRUE))
  })
for(i in 1:length(list_dat_et_byaquifer2)){
  colnames(list_dat_et_byaquifer2[[i]]) <- c('objectid', paste0(names(list_dat_et_byaquifer2)[[i]], '_etmm'))
}
dat_etbyaquifer2 <- 
  list_dat_et_byaquifer2 %>% reduce(left_join, by = 'objectid')
rm(list_dat_et_byaquifer2, i, j)
dat_etbyaquifer2$pctChange_worstBaseline2070 <- (dat_etbyaquifer2$worst2070_etmm - dat_etbyaquifer2$baseline_etmm) / dat_etbyaquifer2$baseline_etmm * 100 * -1
dat_etbyaquifer2$pctChange_worstBaseline2100 <- (dat_etbyaquifer2$worst2100_etmm - dat_etbyaquifer2$baseline_etmm) / dat_etbyaquifer2$baseline_etmm * 100 * -1
dat_etbyaquifer2$pctchange_middleBaseline2070 <- (dat_etbyaquifer2$middle2070_etmm - dat_etbyaquifer2$baseline_etmm) / dat_etbyaquifer2$baseline_etmm * 100 * -1
dat_etbyaquifer2$pctchange_middleBaseline2100 <- (dat_etbyaquifer2$middle2100_etmm - dat_etbyaquifer2$baseline_etmm) / dat_etbyaquifer2$baseline_etmm * 100 * -1
dat_etbyaquifer2$pctchange_bestBaseline2070 <- (dat_etbyaquifer2$best2070_etmm - dat_etbyaquifer2$baseline_etmm) / dat_etbyaquifer2$baseline_etmm * 100 * -1
dat_etbyaquifer2$pctchange_bestBaseline2100 <- (dat_etbyaquifer2$best2100_etmm - dat_etbyaquifer2$baseline_etmm) / dat_etbyaquifer2$baseline_etmm * 100 * -1
dat_etbyaquifer2$pctchange_middleworst2070 <- (dat_etbyaquifer2$middle2070_etmm - dat_etbyaquifer2$worst2070_etmm) / dat_etbyaquifer2$worst2070_etmm * 100 * -1
dat_etbyaquifer2$pctchange_middleworst2100 <- (dat_etbyaquifer2$middle2100_etmm - dat_etbyaquifer2$worst2100_etmm) / dat_etbyaquifer2$worst2100_etmm * 100 * -1
dat_etbyaquifer2$pctchange_bestworst2070 <- (dat_etbyaquifer2$best2070_etmm - dat_etbyaquifer2$worst2070_etmm) / dat_etbyaquifer2$worst2070_etmm * 100 * -1
dat_etbyaquifer2$pctchange_bestworst2100 <- (dat_etbyaquifer2$best2100_etmm - dat_etbyaquifer2$worst2100_etmm) / dat_etbyaquifer2$worst2100_etmm * 100 * -1
dat_etbyaquifer2$pctdiff_middleworst2070 <- (dat_etbyaquifer2$pctchange_middleBaseline2070 - dat_etbyaquifer2$pctChange_worstBaseline2070)
dat_etbyaquifer2$pctdiff_middleworst2100 <- (dat_etbyaquifer2$pctchange_middleBaseline2100 - dat_etbyaquifer2$pctChange_worstBaseline2100)
dat_etbyaquifer2$pctdiff_bestworst2070 <- (dat_etbyaquifer2$pctchange_bestBaseline2070 - dat_etbyaquifer2$pctChange_worstBaseline2070)
dat_etbyaquifer2$pctdiff_bestworst2100 <- (dat_etbyaquifer2$pctchange_bestBaseline2100 - dat_etbyaquifer2$pctChange_worstBaseline2100)
dat_etbyaquifer2$baseline_etmm <-
  dat_etbyaquifer2$best2070_etmm <-
  dat_etbyaquifer2$best2100_etmm <-
  dat_etbyaquifer2$middle2070_etmm <-
  dat_etbyaquifer2$middle2100_etmm <-
  dat_etbyaquifer2$worst2070_etmm <-
  dat_etbyaquifer2$worst2100_etmm <- NULL
gc()

# recombine into single data.frame
dat_cumwy <- do.call(rbind, unlist(list_dat_diffwy_byaquifer, recursive = FALSE))
rm(list_dat_diffwy_byaquifer, list_dat_et_byaquifer)
gc()

# aggregate data
dat_cumwy_aggregated <-
  aggregate(dat_cumwy$lpd,
            list(dat_cumwy$scenario, dat_cumwy$objectid),
            sum, na.rm = TRUE)
colnames(dat_cumwy_aggregated) <-
  c('scenario', 'objectid', 'lpd')
rm(dat_cumwy); gc()

# merge with spatial data
sf_aquifer <- left_join(sf_aquifer, dat_cumwy_aggregated, 'objectid')
sf_aquifer <- left_join(sf_aquifer, dat_etbyaquifer2, 'objectid')
rm(dat_cumwy_aggregated, dat_etbyaquifer2); gc()

# create plotting variables
sf_aquifer$year <- substr(sf_aquifer$scenario, nchar(sf_aquifer$scenario)-3, nchar(sf_aquifer$scenario))
sf_aquifer$scenario <- substr(sf_aquifer$scenario, 1, nchar(sf_aquifer$scenario)-4)
sf_aquifer$scenario <-
  factor(sf_aquifer$scenario,
         levels = c('worst', 'midworst', 'bestworst'),
         labels = c('No protection', 'Targeted protection vs.\nno protection', 'Targeted protection\nand restoration vs.\nno protection'))
sf_aquifer$mlpd <- sf_aquifer$lpd / 1e6
sf_aquifer$thousandLpdPerHectare <- sf_aquifer$lpd / sf_aquifer$st_areasha / 1000
st_write(sf_aquifer, 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/14/14d - water yield summarized by aquifer unit full.gpkg',
         delete_dsn = TRUE)




#### plot aquifer-level results: percent change worst vs baseline #####

# create data
plotdat <-
  st_sf(pctchange = c(sf_aquifer$pctChange_worstBaseline2070, sf_aquifer$pctChange_worstBaseline2100),
        year = rep(c('2070', '2100'), each = nrow(sf_aquifer)),
        geometry = c(sf_aquifer$geometry, sf_aquifer$geometry))
st_write(plotdat, 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/14/14d - water yield summarized by aquifer unit.gpkg',
         delete_dsn = TRUE)

sf_aquifer <- st_read('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/14/14d - water yield summarized by aquifer unit full.gpkg')
plotdat <- st_read('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/14/14d - water yield summarized by aquifer unit.gpkg')

# bin percent changes and create labels
vec_bins <- quantile(plotdat$pctchange, probs = seq(0, 1, 1/5))
vec_bins[which.min(abs(vec_bins))] <- 0
vec_bins <- round(vec_bins)
plotdat$pctchange_bin <- cut(plotdat$pctchange, breaks = vec_bins, include.lowest = TRUE)
plotdat$pctchange_bin <-
  factor(plotdat$pctchange_bin,
         labels = c(paste0('< ', vec_bins[[2]]),
                    paste0('[', vec_bins[[2]], ', ', vec_bins[[3]], ')'),
                    paste0('[', vec_bins[[3]], ', ', vec_bins[[4]], ')'),
                    paste0('[', vec_bins[[4]], ', ', vec_bins[[5]], ']'),
                    paste0('> ', vec_bins[[5]])))
plotdat$pctchange_bin <- factor(plotdat$pctchange_bin, levels = rev(levels(plotdat$pctchange_bin)))

# generate and save plot
p <-
  ggplot(data = plotdat) +
  geom_sf(aes(fill = pctchange_bin)) +
  # geom_label_repel(aes(label = round(pctchange, 2),
  #                      geometry = geometry,
  #                      color = pctchange),
  #                  stat = sf_coordinates,
  #                  min.segment.length = 0.1, max.time = 5, alpha = 0.8) +
  facet_wrap(~year, strip.position = 'top', nrow = 2) +
  scale_fill_manual(values = viridis(9, option = 'turbo')[c(2, 6:9)]) +
  scale_color_viridis_d(option = 'turbo') +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5, text_cex = 1) +
  ggspatial::annotation_north_arrow(location = 'tr') +
  # north(data = plotdat, location = 'topright', symbol = 9) +
  # scalebar(data = plotdat,
  #          location = 'bottomleft', transform = FALSE,
  #          dist_unit = 'km', dist = 100, st.dist = 0.03,
  #          facet.var = 'year',
  #          facet.lev = '2100') +
  labs(fill = '% change\nwater yield') +
  theme(text = element_text(size = 14),
        panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - pct change in water yield under spread models - worst case compared to baseline by aquifer.png'),
       dpi = 300, height = 9, width = 9)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - pct change in water yield under spread models - worst case compared to baseline by aquifer.pdf'),
       dpi = 300, height = 9, width = 9)

rm(p, plotdat, vec_bins); gc()




##### plot aquifer-level results: percent change middle vs. worst #####

# create data
plotdat_mid <-
  st_sf(pctchange = c(sf_aquifer$pctchange_middleworst2070, sf_aquifer$pctchange_middleworst2100),
        year = rep(c('2070', '2100'), each = nrow(sf_aquifer)),
        geometry = c(sf_aquifer$geom, sf_aquifer$geom),
        scenario = 'Targeted protection\nvs. no protection')
plotdat_best <-
  st_sf(pctchange = c(sf_aquifer$pctchange_bestworst2070, sf_aquifer$pctchange_bestworst2100),
        year = rep(c('2070', '2100'), each = nrow(sf_aquifer)),
        geometry = c(sf_aquifer$geom, sf_aquifer$geom),
        scenario = 'Targeted protection\nand restoration\nvs. no protection')
plotdat <- rbind(plotdat_mid, plotdat_best)
plotdat$scenario <-
  factor(plotdat$scenario,
         levels = c('Targeted protection\nvs. no protection',
                    'Targeted protection\nand restoration\nvs. no protection'))
rm(plotdat_best, plotdat_mid)

# bin percent changes and create labels
# vec_bins <- quantile(plotdat$pctchange, probs = seq(0, 1, 1/5))
# vec_bins[which.min(abs(vec_bins))] <- 0
vec_bins <- c(-Inf, -0.01, 0.01, 1, 2, Inf)
plotdat$pctchange_bin <- cut(plotdat$pctchange, breaks = vec_bins, include.lowest = TRUE)
plotdat$pctchange_bin <-
  factor(plotdat$pctchange_bin,
         labels = c('< 0',
                    '0',
                    '(0, 1]',
                    '(1, 2]',
                    '> 2'))
plotdat$pctchange_bin <- factor(plotdat$pctchange_bin, levels = rev(levels(plotdat$pctchange_bin)))

# dummy for aquifer units that changed
plotdat$changeDummy <-
  ifelse(plotdat$pctchange > 0, 'Positive change',
  ifelse(plotdat$pctchange < 0, 'Negative change',
         'No change'))
plotdat$changeDummy <-
  factor(plotdat$changeDummy,
         levels = c('Positive change', 'Negative change', 'No change'))

# generate and save plot - targeted protection vs. no protection
p <-
  ggplot() +
  geom_sf(data = plotdat[plotdat$scenario == 'Targeted protection\nvs. no protection',],
          aes(fill = pctchange_bin)) +
  # geom_label_repel(aes(label = round(pctchange, 2),
  #                      geometry = geometry,
  #                      color = pctchange),
  #                  stat = sf_coordinates,
  #                  min.segment.length = 0.1, max.time = 5, alpha = 0.8) +
  #facet_grid(rows = vars(year), cols = vars(scenario)) +
  facet_wrap(~year, strip.position = 'top', nrow = 2) +
  #scale_fill_manual(values = viridis(7, option = 'turbo')[2:6]) +
  scale_fill_manual(values = c(viridis(7, option = 'turbo')[2:4], 'lightgray', viridis(7, option = 'turbo')[[6]])) +
  north(data = plotdat, location = 'topright', symbol = 9) +
  scalebar(data = plotdat,
           location = 'bottomleft', transform = FALSE,
           dist_unit = 'km', dist = 100, st.dist = 0.03,
           facet.var = c('year', 'scenario'),
           facet.lev = c('2100', 'Targeted protection\nvs. no protection')) +
  labs(fill = '% difference\nwater yield') +
  theme(text = element_text(size = 14),
        panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - pct change in water yield under spread models - middle compared to worst by aquifer.png'),
       dpi = 300, height = 9, width = 9)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - pct change in water yield under spread models - middle compared to worst by aquifer.pdf'),
       dpi = 300, height = 9, width = 9)




##### plot aquifer-level results: percent difference middle vs. worst #####

# create data
plotdat_mid <-
  st_sf(pctdiff = c(sf_aquifer$pctdiff_middleworst2070, sf_aquifer$pctdiff_middleworst2100),
        year = rep(c('2070', '2100'), each = nrow(sf_aquifer)),
        geometry = c(sf_aquifer$geometry, sf_aquifer$geometry),
        scenario = 'Targeted protection\nvs. no protection')
plotdat_best <-
  st_sf(pctdiff = c(sf_aquifer$pctdiff_bestworst2070, sf_aquifer$pctdiff_bestworst2100),
        year = rep(c('2070', '2100'), each = nrow(sf_aquifer)),
        geometry = c(sf_aquifer$geometry, sf_aquifer$geometry),
        scenario = 'Targeted protection\nand restoration\nvs. no protection')
plotdat <- rbind(plotdat_mid, plotdat_best)
plotdat$scenario <-
  factor(plotdat$scenario,
         levels = c('Targeted protection\nvs. no protection',
                    'Targeted protection\nand restoration\nvs. no protection'))
rm(plotdat_best, plotdat_mid)

# bin percent changes and create labels
# vec_bins <- quantile(plotdat$pctdiff, probs = seq(0, 1, 1/5))
# vec_bins[which.min(abs(vec_bins))] <- 0
vec_bins <- c(-Inf, -0.1, 0.1, 1, 2, Inf)
plotdat$pctdiff_bin <- cut(plotdat$pctdiff, breaks = vec_bins, include.lowest = TRUE)
plotdat$pctdiff_bin <-
  factor(plotdat$pctdiff_bin,
         labels = c('< 0',
                    '0',
                    '(0, 1]',
                    '(1, 2]',
                    '> 2'))
plotdat$pctdiff_bin <- factor(plotdat$pctdiff_bin, levels = rev(levels(plotdat$pctdiff_bin)))

# dummy for aquifer units that changed
plotdat$changeDummy <-
  ifelse(plotdat$pctdiff > 0, 'Positive change',
         ifelse(plotdat$pctdiff < 0, 'Negative change',
                'No change'))
plotdat$changeDummy <-
  factor(plotdat$changeDummy,
         levels = c('Positive change', 'Negative change', 'No change'))

# generate and save plot - targeted protection vs. no protection
p <-
  ggplot() +
  geom_sf(data = plotdat[plotdat$scenario == 'Targeted protection\nvs. no protection',],
          aes(fill = pctdiff_bin)) +
  # geom_label_repel(aes(label = round(pctdiff, 2),
  #                      geometry = geometry,
  #                      color = pctdiff),
  #                  stat = sf_coordinates,
  #                  min.segment.length = 0.1, max.time = 5, alpha = 0.8) +
  #facet_grid(rows = vars(year), cols = vars(scenario)) +
  facet_wrap(~year, strip.position = 'top', nrow = 2) +
  #scale_fill_manual(values = viridis(7, option = 'turbo')[2:6]) +
  scale_fill_manual(values = c(viridis(7, option = 'turbo')[2:4], 'lightgray', viridis(7, option = 'turbo')[[6]])) +
  north(data = plotdat, location = 'topright', symbol = 9) +
  scalebar(data = plotdat,
           location = 'bottomleft', transform = FALSE,
           dist_unit = 'km', dist = 100, st.dist = 0.03,
           facet.var = c('year', 'scenario'),
           facet.lev = c('2100', 'Targeted protection\nvs. no protection')) +
  labs(fill = '% difference\nwater yield') +
  theme(text = element_text(size = 14),
        panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - pct difference in water yield under spread models - middle compared to worst by aquifer.png'),
       dpi = 300, height = 9, width = 9)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - pct difference in water yield under spread models - middle compared to worst by aquifer.pdf'),
       dpi = 300, height = 9, width = 9)




##### plot aquifer-level results: percent change best vs. worst #####

# create data
plotdat_mid <-
  st_sf(pctchange = c(sf_aquifer$pctchange_middleworst2070, sf_aquifer$pctchange_middleworst2100),
        year = rep(c('2070', '2100'), each = nrow(sf_aquifer)),
        geometry = c(sf_aquifer$geom, sf_aquifer$geom),
        scenario = 'Targeted protection\nvs. no protection')
plotdat_best <-
  st_sf(pctchange = c(sf_aquifer$pctchange_bestworst2070, sf_aquifer$pctchange_bestworst2100),
        year = rep(c('2070', '2100'), each = nrow(sf_aquifer)),
        geometry = c(sf_aquifer$geom, sf_aquifer$geom),
        scenario = 'Targeted protection\nand restoration\nvs. no protection')
plotdat <- rbind(plotdat_mid, plotdat_best)
plotdat$scenario <-
  factor(plotdat$scenario,
         levels = c('Targeted protection\nvs. no protection',
                    'Targeted protection\nand restoration\nvs. no protection'))
rm(plotdat_best, plotdat_mid)

# bin percent changes and create labels
# vec_bins <- quantile(plotdat$pctchange, probs = seq(0, 1, 1/5))
# vec_bins[which.min(abs(vec_bins))] <- 0
vec_bins <- c(-Inf, -0.01, 0.01, 1, 2, Inf)
plotdat$pctchange_bin <- cut(plotdat$pctchange, breaks = vec_bins, include.lowest = TRUE)
plotdat$pctchange_bin <-
  factor(plotdat$pctchange_bin,
         labels = c('< 0',
                    '0',
                    '(0, 1]',
                    '(1, 2]',
                    '> 2'))
plotdat$pctchange_bin <- factor(plotdat$pctchange_bin, levels = rev(levels(plotdat$pctchange_bin)))

# dummy for aquifer units that changed
plotdat$changeDummy <-
  ifelse(plotdat$pctchange > 0, 'Positive change',
         ifelse(plotdat$pctchange < 0, 'Negative change',
                'No change'))
plotdat$changeDummy <-
  factor(plotdat$changeDummy,
         levels = c('Positive change', 'Negative change', 'No change'))

# find aquifer units whose difference is different from comparison with middle case above
plotdat_mid <- plotdat[plotdat$scenario == 'Targeted protection\nvs. no protection',]
plotdat_best <- plotdat[plotdat$scenario == 'Targeted protection\nand restoration\nvs. no protection',]
plotdat_best$differentFromMid <- ifelse(plotdat_best$pctchange_bin == plotdat_mid$pctchange_bin, 'No', 'Yes')

# generate and save plot - targeted protection vs. no protection
p <-
  ggplot() +
  geom_sf(data = plotdat_best,
          aes(fill = pctchange_bin, color = differentFromMid, linewidth = differentFromMid)) +
  # geom_label_repel(aes(label = round(pctchange, 2),
  #                      geometry = geometry,
  #                      color = pctchange),
  #                  stat = sf_coordinates,
  #                  min.segment.length = 0.1, max.time = 5, alpha = 0.8) +
  #facet_grid(rows = vars(year), cols = vars(scenario)) +
  facet_wrap(~year, strip.position = 'top', nrow = 2) +
  #scale_fill_manual(values = viridis(7, option = 'turbo')[2:6]) +
  scale_fill_manual(values = c(viridis(7, option = 'turbo')[2:4], 'lightgray', viridis(7, option = 'turbo')[[6]])) +
  scale_color_manual(values = c('#58595B', 'black'), guide = 'none') +
  scale_linewidth_manual(values = c(0.5, 1.2), guide = 'none') +
  north(data = plotdat, location = 'topright', symbol = 9) +
  scalebar(data = plotdat,
           location = 'bottomleft', transform = FALSE,
           dist_unit = 'km', dist = 100, st.dist = 0.03,
           facet.var = c('year', 'scenario'),
           facet.lev = c('2100', 'Targeted protection\nvs. no protection')) +
  labs(fill = '% difference\nwater yield') +
  theme(text = element_text(size = 14),
        panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - pct change in water yield under spread models - best compared to worst by aquifer.png'),
       dpi = 300, height = 9, width = 9)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - pct change in water yield under spread models - best compared to worst by aquifer.pdf'),
       dpi = 300, height = 9, width = 9)




##### plot aquifer-level results: percent difference best vs. worst #####

# create data
plotdat_mid <-
  st_sf(pctchange = c(sf_aquifer$pctdiff_middleworst2070, sf_aquifer$pctdiff_middleworst2100),
        year = rep(c('2070', '2100'), each = nrow(sf_aquifer)),
        geometry = c(sf_aquifer$geometry, sf_aquifer$geometry),
        scenario = 'Targeted protection\nvs. no protection')
plotdat_best <-
  st_sf(pctchange = c(sf_aquifer$pctdiff_bestworst2070, sf_aquifer$pctdiff_bestworst2100),
        year = rep(c('2070', '2100'), each = nrow(sf_aquifer)),
        geometry = c(sf_aquifer$geometry, sf_aquifer$geometry),
        scenario = 'Targeted protection\nand restoration\nvs. no protection')
plotdat <- rbind(plotdat_mid, plotdat_best)
plotdat$scenario <-
  factor(plotdat$scenario,
         levels = c('Targeted protection\nvs. no protection',
                    'Targeted protection\nand restoration\nvs. no protection'))
rm(plotdat_best, plotdat_mid)

# bin percent diffs and create labels
# vec_bins <- quantile(plotdat$pctchange, probs = seq(0, 1, 1/5))
# vec_bins[which.min(abs(vec_bins))] <- 0
vec_bins <- c(-Inf, -0.1, 0.1, 1, 2, Inf)
plotdat$pctchange_bin <- cut(plotdat$pctchange, breaks = vec_bins, include.lowest = TRUE)
plotdat$pctchange_bin <-
  factor(plotdat$pctchange_bin,
         labels = c('< 0',
                    '0',
                    '(0, 1]',
                    '(1, 2]',
                    '> 2'))
plotdat$pctchange_bin <- factor(plotdat$pctchange_bin, levels = rev(levels(plotdat$pctchange_bin)))

# dummy for aquifer units that changed
plotdat$changeDummy <-
  ifelse(plotdat$pctchange > 0, 'Positive change',
         ifelse(plotdat$pctchange < 0, 'Negative change',
                'No change'))
plotdat$changeDummy <-
  factor(plotdat$changeDummy,
         levels = c('Positive change', 'Negative change', 'No change'))

# find aquifer units whose difference is different from comparison with middle case above
plotdat_mid <- plotdat[plotdat$scenario == 'Targeted protection\nvs. no protection',]
plotdat_best <- plotdat[plotdat$scenario == 'Targeted protection\nand restoration\nvs. no protection',]
plotdat_best$differentFromMid <- ifelse(plotdat_best$pctchange_bin == plotdat_mid$pctchange_bin, 'No', 'Yes')

# generate and save plot - targeted protection vs. no protection
p <-
  ggplot() +
  geom_sf(data = plotdat_best,
          aes(fill = pctchange_bin, color = differentFromMid, linewidth = differentFromMid)) +
  # geom_label_repel(aes(label = round(pctchange, 2),
  #                      geometry = geometry,
  #                      color = pctchange),
  #                  stat = sf_coordinates,
  #                  min.segment.length = 0.1, max.time = 5, alpha = 0.8) +
  #facet_grid(rows = vars(year), cols = vars(scenario)) +
  facet_wrap(~year, strip.position = 'top', nrow = 2) +
  #scale_fill_manual(values = viridis(7, option = 'turbo')[2:6]) +
  scale_fill_manual(values = c(viridis(7, option = 'turbo')[2:4], 'lightgray', viridis(7, option = 'turbo')[[6]])) +
  scale_color_manual(values = c('#58595B', 'black'), guide = 'none') +
  scale_linewidth_manual(values = c(0.5, 1.2), guide = 'none') +
  north(data = plotdat, location = 'topright', symbol = 9) +
  scalebar(data = plotdat,
           location = 'bottomleft', transform = FALSE,
           dist_unit = 'km', dist = 100, st.dist = 0.03,
           facet.var = c('year', 'scenario'),
           facet.lev = c('2100', 'Targeted protection\nvs. no protection')) +
  labs(fill = '% difference\nwater yield') +
  theme(text = element_text(size = 14),
        panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - pct difference in water yield under spread models - best compared to worst by aquifer.png'),
       dpi = 300, height = 9, width = 9)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14d - pct difference in water yield under spread models - best compared to worst by aquifer.pdf'),
       dpi = 300, height = 9, width = 9)
