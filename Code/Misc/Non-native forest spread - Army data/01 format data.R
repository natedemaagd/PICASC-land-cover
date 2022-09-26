
library(ggplot2); library(reshape2); library(raster); library(doParallel);
library(viridis); library(cowplot); library(grid); library(egg)

registerDoParallel(cores = 10)

# load data and convert to data.table
dat <- read.csv("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Army data/2022_ANRPO_beltplots_percentcover_spprichness_WGS84.csv")
dat$PltMainDate <- as.Date(dat$PltMainDate, format = '%Y-%m-%d')

# split data by location
datByLoc <- split(dat, dat$BltPltCode)

# order observations by date
datByLoc <- lapply(datByLoc, function(df){
  df[order(df$PltMainDate),]
})




##### calculate change in canopy between start and end date #####

# order observations by date within each location
datByLoc <- lapply(datByLoc, function(df){
  df[order(df$PltMainDate),]
})

# get starting canopy and change in canopy for each location
datChangeCanopy <-
  data.frame(length_years =
               sapply(datByLoc, function(df){
                 as.numeric(df[nrow(df), 'PltMainDate'] - 
                              df[1, 'PltMainDate']) / 365   # number of years between first and last observation for location
               }),
             NatCanopy_pctChange = 
               sapply(datByLoc, function(df){
                 df[nrow(df), 'NatCanopy'] - 
                   df[1, 'NatCanopy']
               }),
             XCanopy_pctChange = 
               sapply(datByLoc, function(df){
                 df[nrow(df), 'XCanopy'] - 
                   df[1, 'XCanopy']
               }),
             TotCanopy_pctChange = 
               sapply(datByLoc, function(df){
                 df[nrow(df), 'TotCanopy'] - 
                   df[1, 'TotCanopy']
               }),
             x =
               sapply(datByLoc, function(df){
                 df[1, 'coords.x1']
               }),
             y =
               sapply(datByLoc, function(df){
                 df[1, 'coords.x2']
               }),
             NatCanopyStart =
               sapply(datByLoc, function(df){
                 df[1, 'NatCanopy']
               }),
             XCanopyStart =
               sapply(datByLoc, function(df){
                 df[1, 'XCanopy']
               }),
             TotalCanopyStart =
               sapply(datByLoc, function(df){
                 df[1, 'TotCanopy']
               }))

# change in canopy per year
datChangeCanopy$NatCanopy_pctChangePerYr <-
  with(datChangeCanopy,
       NatCanopy_pctChange / length_years)
datChangeCanopy$XCanopy_pctChangePerYr <-
  with(datChangeCanopy,
       XCanopy_pctChange / length_years)
datChangeCanopy$TotCanopy_pctChangePerYr <-
  with(datChangeCanopy,
       TotCanopy_pctChange / length_years)

# analyze percent change per year
plotdat <-
  melt(datChangeCanopy[c("NatCanopy_pctChangePerYr",
                         "XCanopy_pctChangePerYr",
                         "TotCanopy_pctChangePerYr")])
plotdat$variable <-
  factor(plotdat$variable,
         levels = c("NatCanopy_pctChangePerYr",
                    "XCanopy_pctChangePerYr",
                    "TotCanopy_pctChangePerYr"),
         labels = c(paste0('Native canopy (mean = ',
                           round(mean(datChangeCanopy$NatCanopy_pctChangePerYr,
                                      na.rm = TRUE),
                                 2),
                           '%)'),
                    paste0('Non-native canopy (mean = ',
                           round(mean(datChangeCanopy$XCanopy_pctChangePerYr,
                                      na.rm = TRUE),
                                 2),
                           '%)'),
                    paste0('Total canopy (mean = ',
                           round(mean(datChangeCanopy$TotCanopy_pctChangePerYr,
                                      na.rm = TRUE),
                                 2),
                           '%)')))

ggplot(data = plotdat, aes(x = value, fill = variable, color = variable)) +
  geom_histogram(bins = 40) +
  labs(x = '% change per year', y = 'Number of sites') +
  theme(legend.title = element_blank(),
        legend.position = c(0.775, 0.9),
        text = element_text(size = 11)) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() 
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Army data/pct change canopy per year.png',
       height = 4, width = 6, dpi = 300)




##### merge canopy change with rainfall #####

# load rainfall raster and convert to mm
raster_annRainIn <- raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_ann_rainfall_inches/Hawaii-State/rfgrid_inches_state_ann.txt")
raster_annRainmm <- raster_annRainIn * 25.4

# convert to data.frame
datAnnRainmm <- as.data.frame(raster_annRainmm, xy = TRUE)
colnames(datAnnRainmm) <- c('x', 'y', 'annRain_mm')
datAnnRainmm <- datAnnRainmm[!is.na(datAnnRainmm$annRain_mm),]

# for each Army location, find closest rainfall grid
datChangeCanopy$annRain_mm <-
  foreach(i = 1:nrow(datChangeCanopy), .combine = 'c') %dopar% {
    
    # get coords of army site
    coords_site_i <- c(datChangeCanopy$x[[i]],
                       datChangeCanopy$y[[i]])
    
    # find distance to each rainfall grid
    vec_dist <- sqrt((datAnnRainmm$x - coords_site_i[[1]]) ^ 2 +
                     (datAnnRainmm$y - coords_site_i[[2]]) ^ 2 )
    
    # return annual rainfall of closest grid
    return(datAnnRainmm$annRain_mm[[which.min(vec_dist)]])
    
    gc()
  
  }




##### analyze relationship between canopy change and annual rainfall #####

# linear regression
reg1 <- lm(data = datChangeCanopy,
           formula = XCanopy_pctChangePerYr ~ annRain_mm)
reg2 <- lm(data = datChangeCanopy,
           formula = XCanopy_pctChangePerYr ~ annRain_mm + length_years)

# scatterplot: canopy scpread onto rainfall
p_scatter <- ggplot(data = datChangeCanopy,
                    aes(x = annRain_mm, y = XCanopy_pctChangePerYr,
                        color = length_years)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis(name = 'Years') +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = 'Mean non-native canopy spread (%/yr)') +
  xlim(c(1100, 1900))

p_scatter_legend <- get_legend(p_scatter)  # get legend to plot separately
p_scatter <- p_scatter + theme(text = element_text(size = 20),
                               legend.position = 'none',
                               axis.text.x = element_blank(),
                               axis.ticks.x = element_blank())

# histogram of rainfall distribution
p_hist <- ggplot(data = datChangeCanopy) +
  geom_histogram(aes(x = annRain_mm), bins = 50,
                 alpha = 0.5, color = 'black', fill = 'gray') +
  scale_color_manual(values = NA) +  # dummy legend for alignment with p_scatter
  labs(x = 'Mean rainfall (mm/yr)', y = 'Number of sites') +
  xlim(c(1100, 1900)) +
  theme(text = element_text(size = 20),
        legend.text = element_blank(),
        legend.title = element_blank())

# stack scatterplot and histogram
plot_grid(p_scatter, p_scatter_legend,  p_hist,
          ncol = 2, align = "v", rel_widths = c(1, 0.1),
          axis = 'bl')

ggsave2(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Army data/',
                          'nonnative canopy change as fcn of rainfall.png'),
        dpi = 300, height = 10, width = 10.5)




saveRDS(datChangeCanopy,
        file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/Misc/Non-native forest spread - Army data/01 formatted data.rds')

