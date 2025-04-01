
# This script compares the change in WY from climate change to the change in WY
# from the change in land cover

library(sf); library(terra); library(ggplot2); library(ggsn); library(tidyverse)




##### load data #####

# load rasters
list_rastersWY_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/12 climate change/12a rasters water yield', full.names = TRUE, pattern = '.tif')
list_rastersWY <- lapply(list_rastersWY_filenames, rast)
list_rastersWY_filenames <- strsplit(list_rastersWY_filenames, '/')
list_rastersWY_filenames <- sapply(list_rastersWY_filenames, function(x) substr(x[[length(x)]], 6, nchar(x[[length(x)]])-4))
names(list_rastersWY) <- list_rastersWY_filenames
rm(list_rastersWY_filenames)

# load aquifer unit shapefile
sf_aquifer <- read_sf('H:/My Drive/Projects/Data/Shapefiles/DLNR_Aquifers/DLNR_Aquifers.shp')
sf_aquifer <- st_transform(sf_aquifer, crs(list_rastersWY$WYdynBaseline))




##### format data #####

# split rasters by aquifer
list_rastersWY_byaquifer <- list()
for(l in 1:length(list_rastersWY)){
  list_rastersWY_byaquifer[[l]] <-
    lapply(1:nrow(sf_aquifer), function(i) {
      mask(crop(list_rastersWY[[l]], sf_aquifer[i,]), sf_aquifer[i,])
    })
}
names(list_rastersWY_byaquifer) <- names(list_rastersWY)
rm(l); gc()

# convert split rasters to data.frames
list_datWY_byaquifer <- list()
for(l in 1:length(list_rastersWY_byaquifer)){
  list_datWY_byaquifer[[l]] <- list()
  for(r in 1:length(list_rastersWY_byaquifer[[l]])){
    list_datWY_byaquifer[[l]][[r]] <-
      as.data.frame(list_rastersWY_byaquifer[[l]][[r]], xy = TRUE)
  }
}
names(list_datWY_byaquifer) <- names(list_rastersWY_byaquifer)
rm(l, r); gc()

# change column names
for(l in 1:length(list_datWY_byaquifer)){
  for(r in 1:length(list_datWY_byaquifer[[l]])){
    colnames(list_datWY_byaquifer[[l]][[r]]) <- c('x', 'y', 'wy')
  }
}
rm(l, r)

# add scenario and aquifer IDs
for(l in 1:length(list_datWY_byaquifer)){
  for(r in 1:length(list_datWY_byaquifer[[l]])){
    if(nrow(list_datWY_byaquifer[[l]][[r]]) == 0){
      list_datWY_byaquifer[[l]][[r]] <-
        data.frame(x = NA, y = NA, wy = 0)
    }
    list_datWY_byaquifer[[l]][[r]]$scenario <- substr(names(list_datWY_byaquifer), 3, nchar(names(list_datWY_byaquifer)))[[l]]
    list_datWY_byaquifer[[l]][[r]]$objectid <- sf_aquifer$objectid[[r]]
  }
}
rm(l, r, list_rastersWY_byaquifer, list_rastersWY)
gc()

# remove x and y variables
for(i in 1:length(list_datWY_byaquifer)){
  for(j in 1:length(list_datWY_byaquifer[[i]])){
    list_datWY_byaquifer[[i]][[j]]$x <- list_datWY_byaquifer[[i]][[j]]$y <- NULL
  }
}
rm(i, j); gc()

# combine list of data.frames into data.frames
datWY_byaquifer <- do.call(rbind, unlist(list_datWY_byaquifer, recursive = FALSE))
rm(list_datWY_byaquifer); gc()

# aggregate WY by scenario and aquifer unit
datWY_byaquifer_agg <-
  as.data.frame(aggregate(datWY_byaquifer$wy,
                          list(datWY_byaquifer$scenario, datWY_byaquifer$objectid),
                          sum, na.rm = TRUE))
colnames(datWY_byaquifer_agg) <- c('scenario', 'objectid', 'wy')
rm(datWY_byaquifer); gc()

# long to wide
datWY_byaquifer_agg   <- reshape(datWY_byaquifer_agg,   idvar = 'objectid', timevar = 'scenario', direction = 'wide')
gc()

# merge to spatial data
sf_aquifer <- left_join(sf_aquifer, datWY_byaquifer_agg, 'objectid')

rm(datWY_byaquifer_agg); gc()




##### calculate percent change in water yield under each scenario #####

# overall % change in water yield: ((rain2 - ET2) - (rain1 - ET1)) / (rain1 - ET1) * 100

sf_aquifer$pctChangeWy_dynRcp45Iprc = with(sf_aquifer, (wy.dynRcp45Iprc - wy.dynBaseline) / wy.dynBaseline * 100)
sf_aquifer$pctChangeWy_dynRcp85Iprc = with(sf_aquifer, (wy.dynRcp85Iprc - wy.dynBaseline) / wy.dynBaseline * 100)
sf_aquifer$pctChangeWy_dynRcp85Ncar = with(sf_aquifer, (wy.dynRcp85Ncar - wy.dynBaseline) / wy.dynBaseline * 100)




##### plots #####

# melt data
plotdat <- st_sf(scenario = rep(c('IPRC RCP 4.5', 'IPRC RCP 8.5', 'NCAR RCP 8.5'), each = nrow(sf_aquifer)),
                 objectid = sf_aquifer$objectid,
                 pctChange = c(sf_aquifer$pctChangeWy_dynRcp45Iprc, sf_aquifer$pctChangeWy_dynRcp85Iprc, sf_aquifer$pctChangeWy_dynRcp85Ncar),
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
  scale_fill_viridis_d(option = 'turbo') +
  scale_color_viridis_d(option = 'turbo') +
  north(data = plotdat, location = 'topright', symbol = 9) +
  scalebar(data = plotdat,
           location = 'bottomleft', transform = FALSE,
           dist_unit = 'km', dist = 100, st.dist = 0.03,
           facet.var = 'scenario',
           facet.lev = 'NCAR RCP 8.5') +
  labs(fill = '% change\nwater yield') +
  theme(text = element_text(size = 14),
        panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14c - pct change in water yield under climate models.png'),
       dpi = 300, height = 12, width = 12)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '14c - pct change in water yield under climate models.pdf'),
       dpi = 300, height = 12, width = 12)
