
# This script plots maps of rainfall changes due to climate change

library(terra); library(ggplot2); library(ggsn); library(sf); library(tidyverse)
library(viridis)




##### load and format data #####

# dynamical and statistical downscale data
ras_rain_dynBaseline <-
  rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/12a rasters rainfall/rast_rain_dynBaseline.tif")
ras_rain_dynRcp85 <-
  rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/12a rasters rainfall/rast_rain_dynRcp85.tif")
ras_rain_staBaseline <-
  rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/12a rasters rainfall/rast_rain_staBaseline.tif")
ras_rain_staRcp85 <-
  rast("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/12a rasters rainfall/rast_rain_staRcp85.tif")

# load aquifer unit shapefile
sf_aquifer <- read_sf('H:/My Drive/Projects/Data/Shapefiles/DLNR_Aquifers/DLNR_Aquifers.shp')
sf_aquifer <- st_transform(sf_aquifer, crs(ras_rain_dynBaseline))

# combine rasters into list
list_rast <- list(ras_rain_dynBaseline, ras_rain_dynRcp85, ras_rain_staBaseline, ras_rain_staRcp85)
names(list_rast) <- c('dynBaseline', 'dynRcp85', 'staBaseline', 'staRcp85')

rm(ras_rain_dynBaseline, ras_rain_dynRcp85, ras_rain_staBaseline, ras_rain_staRcp85)
gc()




##### format data #####

# split rasters by aquifer
list_rast_byaquifer <- list()
for(l in 1:length(list_rast)){
  list_rast_byaquifer[[l]] <-
    lapply(1:nrow(sf_aquifer), function(i) {
      mask(crop(list_rast[[l]], sf_aquifer[i,]), sf_aquifer[i,])
    })
}
names(list_rast_byaquifer) <- names(list_rast)
rm(l); gc()

# convert split rasters to data.frames
list_dat_byaquifer <- list()
for(l in 1:length(list_rast_byaquifer)){
  list_dat_byaquifer[[l]] <- list()
  for(r in 1:length(list_rast_byaquifer[[l]])){
    list_dat_byaquifer[[l]][[r]] <-
      as.data.frame(list_rast_byaquifer[[l]][[r]], xy = TRUE)
  }
}
names(list_dat_byaquifer) <- names(list_rast_byaquifer)
rm(l, r); gc()

# change column names
for(l in 1:length(list_dat_byaquifer)){
  for(r in 1:length(list_dat_byaquifer[[l]])){
    colnames(list_dat_byaquifer[[l]][[r]]) <- c('x', 'y', 'rainmm')
  }
}
rm(l, r)

# add scenario and aquifer IDs
for(l in 1:length(list_dat_byaquifer)){
  for(r in 1:length(list_dat_byaquifer[[l]])){
    if(nrow(list_dat_byaquifer[[l]][[r]]) == 0){
      list_dat_byaquifer[[l]][[r]] <-
        data.frame(x = NA, y = NA, wy = 0)
    }
    list_dat_byaquifer[[l]][[r]]$scenario <- names(list_dat_byaquifer)[[l]]
    list_dat_byaquifer[[l]][[r]]$objectid <- sf_aquifer$objectid[[r]]
  }
}
rm(l, r, list_rast_byaquifer, list_rast)
gc()

# remove x and y variables
for(i in 1:length(list_dat_byaquifer)){
  for(j in 1:length(list_dat_byaquifer[[i]])){
    list_dat_byaquifer[[i]][[j]]$x <- list_dat_byaquifer[[i]][[j]]$y <- NULL
  }
}
rm(i, j); gc()

# combine list of data.frames into data.frames
dat_byaquifer <- do.call(rbind, unlist(list_dat_byaquifer, recursive = FALSE))
rm(list_dat_byaquifer); gc()

# aggregate  by scenario and aquifer unit
dat_byaquifer_agg <-
  as.data.frame(aggregate(dat_byaquifer$rainmm,
                          list(dat_byaquifer$scenario, dat_byaquifer$objectid),
                          sum, na.rm = TRUE))
colnames(dat_byaquifer_agg) <- c('scenario', 'objectid', 'mm')
rm(dat_byaquifer); gc()

# long to wide
dat_byaquifer_agg   <- reshape(dat_byaquifer_agg, idvar = 'objectid', timevar = 'scenario', direction = 'wide')
gc()

# merge to spatial data
sf_aquifer <- left_join(sf_aquifer, dat_byaquifer_agg, 'objectid')

rm(dat_byaquifer_agg); gc()




##### calculate percent change in water yield under each scenario #####

# overall % change in water yield: ((rain2 - rain1) / rain1) * 100

sf_aquifer$pctChangeRainfall_dynRcp85 = with(sf_aquifer, ((mm.dynRcp85 - mm.dynBaseline) / mm.dynBaseline) * 100)
sf_aquifer$pctChangeRainfall_staRcp85 = with(sf_aquifer, ((mm.staRcp85 - mm.staBaseline) / mm.staBaseline) * 100)




##### plots #####

# melt data
plotdat <- st_sf(scenario = rep(c('Dynamical DS RCP 8.5', 'Statistical DS RCP 8.5'), each = nrow(sf_aquifer)),
                 objectid = sf_aquifer$objectid,
                 pctChange = c(sf_aquifer$pctChangeRainfall_dynRcp85, sf_aquifer$pctChangeRainfall_staRcp85),
                 geometry = sf_aquifer$geometry,
                 crs = crs(sf_aquifer))

# create breaks
plotdat_breaks <- c(quantile(plotdat$pctChange[plotdat$pctChange < 0], probs = seq(0, 1, 1/5)), Inf)
plotdat_breaks[which.min(abs(plotdat_breaks))] <- 0
plotdat$pctChange_bin <- cut(plotdat$pctChange, plotdat_breaks, include.lowest = TRUE)

# convert breaks to factor
plotdat_breaks <- round(plotdat_breaks)
plotdat$pctChange_bin <-
  factor(plotdat$pctChange_bin,
         labels = c(paste0('< ', plotdat_breaks[[2]]),
                    paste0('[', plotdat_breaks[[2]], ', ', plotdat_breaks[[3]], ')'),
                    paste0('[', plotdat_breaks[[3]], ', ', plotdat_breaks[[4]], ')'),
                    paste0('[', plotdat_breaks[[4]], ', ', plotdat_breaks[[5]], ')'),
                    paste0('[', plotdat_breaks[[5]], ', ', plotdat_breaks[[6]], ']'),
                    paste0('> ', plotdat_breaks[[6]])))
plotdat$pctChange_bin <- factor(plotdat$pctChange_bin, levels = rev(levels(plotdat$pctChange_bin)))

# create and save plot
p <-
  ggplot(data = plotdat) +
  geom_sf(aes(fill = pctChange_bin)) +
  # geom_label_repel(aes(label = round(pctChange, 2),
  #                      geometry = geometry,
  #                      color = pctChange),
  #                  stat = sf_coordinates,
  #                  min.segment.length = 0.1, max.time = 5, alpha = 0.8) +
  facet_wrap(~scenario, strip.position = 'top', nrow = length(unique(plotdat$scenario))) +
  scale_fill_manual(values = viridis(8, option = 'turbo')[2:7]) +
  scale_color_manual(values = viridis(8, option = 'turbo')[2:7]) +
  north(data = plotdat, location = 'topright', symbol = 9) +
  scalebar(data = plotdat,
           location = 'bottomleft', transform = FALSE,
           dist_unit = 'km', dist = 100, st.dist = 0.03,
           facet.var = 'scenario',
           facet.lev = 'Statistical DS RCP 8.5') +
  labs(fill = '% change\nrainfall') +
  theme(text = element_text(size = 14),
        panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14b - pct change in rainfall under climate models.png'),
       dpi = 300, height = 12, width = 12)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14b - pct change in rainfall under climate models.pdf'),
       dpi = 300, height = 12, width = 12)