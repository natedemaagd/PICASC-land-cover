
library(ggplot2); library(reshape2)

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

# get change in canopy for each location
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
