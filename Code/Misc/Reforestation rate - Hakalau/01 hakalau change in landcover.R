
# this script tracks the change in woody/herbaceous/bare cover in Hakalau Forest Reserve.
# It looks only at the "snapshot" change from 1999 to 2016, with no intermeidate years considered.

library(raster); library(sf); library(ggplot2); library(viridis); library(snow)
rasterOptions(maxmemory = 1e+09)

# load data
sf_hfr <- read_sf("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/fenced regions/Hakalau GIS data/HFR complete.kml")
r_woodList <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Fire/interpolated_yearly_landcover_percentages',
                         pattern = 'wood', full.names = TRUE)
r_woodList <- lapply(r_woodList, raster)
r_wood1999 <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_wood_1999.tif")
r_wood2016 <- raster("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire/00 Statewide landcover rasters resampled/HI_FracLC_wood_2016.tif")

# calculate change in wood cover 1999 to 2016 and subset using HFR fences
r_woodChange <- r_wood2016 - r_wood1999
sf_hfr <- st_zm(sf_hfr)  # coerce to two-dimensional polygons
r_woodChange_hfr <- mask(x = r_woodChange, mask = sf_hfr)
r_wood1999_hfr <- mask(x = r_wood1999, mask = sf_hfr)  # subset start year woody pct
r_wood2016_hfr <- mask(x = r_wood2016, mask = sf_hfr)  # subset end year woody pct
rm(sf_hfr, r_wood1999, r_wood2016, r_woodChange)
gc()

# convert to data.frame
df_woodChange <- as.data.frame(r_woodChange_hfr, xy = TRUE)
rm(r_woodChange_hfr)
gc()
colnames(df_woodChange) <- c('x', 'y', 'change_woody_pct')
df_woodChange <- df_woodChange[!is.na(df_woodChange$change_woody_pct),]
df_woodChange$change_woody_pct <- df_woodChange$change_woody_pct * 100
gc()
df_woodChange$change_woody_pctPerYr <-
  df_woodChange$change_woody_pct / (2016 - 1999)

# add column with initial woody cover
df_initialWoodyPct <- as.data.frame(r_wood1999_hfr)
colnames(df_initialWoodyPct) <- 'initial_woody_pct'
gc()
df_woodChange$initial_woody_pct <-
  df_initialWoodyPct$initial_woody_pct[!is.na(df_initialWoodyPct$initial_woody_pct)] * 100
rm(df_initialWoodyPct)
gc()

# create initial woody pct quintiles
df_woodChange$initial_woody_pct[df_woodChange$initial_woody_pct < 0] <- 0
df_woodChange$initial_woody_pct_quintile <-
  cut(df_woodChange$initial_woody_pct,
      breaks = c(0, 1,
                 quantile(df_woodChange$initial_woody_pct[df_woodChange$initial_woody_pct > 1],
                          length.out = 5)[2:5]),
      include.lowest = TRUE)




##### plot distribution of change in woody, w/ and w/o 0-change pixels #####

# melt data
plotdat <- with(df_woodChange,
                data.frame(value = c(change_woody_pctPerYr,
                                     change_woody_pctPerYr[change_woody_pctPerYr > 0]),
                           panel = c(rep('All pixels',
                                         times = length(change_woody_pctPerYr)),
                                     rep('Pixels with positive change',
                                         times = length(change_woody_pctPerYr[change_woody_pctPerYr > 0]))),
                           initial_woody_pct_quintile = c(initial_woody_pct_quintile,
                                                          initial_woody_pct_quintile[change_woody_pctPerYr > 0])
                           )
                )

# plot distribution of woody change
ggplot(data = plotdat,
       aes(value, fill = initial_woody_pct_quintile)) +
  geom_histogram(bins = 50) +
  scale_fill_viridis_d() +
  facet_grid(rows = vars(panel), scales = 'free_y') +
  labs(x = 'Change in woody cover per year (%)',
       y = 'Number of pixels',
       fill = 'Initial woody\ncover (%)') +
  scale_x_continuous(limits = c(-1, 6)) +
  theme(text = element_text(size = 13))
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/Hakalau landcover change/avg yearly change woody cover Hakalau Forest Reserve.png',
       dpi = 300, height = 5, width = 7)

rm(df_woodChange, plotdat)
gc()




##### how many pixels converted to forest? #####

# define forest threshold (percent landcover)
threshold <- 40
length_years <- 2016 - 1999

# dummy raster of forest - 1999
r_wood1999_hfr <- r_wood1999_hfr * 100
r_wood1999_hfr_dummy <- r_wood1999_hfr
r_wood1999_hfr_dummy[r_wood1999_hfr >= threshold] <- 1
r_wood1999_hfr_dummy[r_wood1999_hfr  < threshold] <- 0

# dummy raster of forest - 2016
r_wood2016_hfr <- r_wood2016_hfr * 100
r_wood2016_hfr_dummy <- r_wood2016_hfr
r_wood2016_hfr_dummy[r_wood2016_hfr >= threshold] <- 1
r_wood2016_hfr_dummy[r_wood2016_hfr  < threshold] <- 0

# find pixels that were converted
r_woodConverted_hfr <- r_wood2016_hfr
r_woodConverted_hfr[r_wood1999_hfr_dummy == 1] <- 0

# change in number of pixels
pixels_forest1999 <- sum(values(r_wood1999_hfr_dummy), na.rm = TRUE)
pixels_forest2016 <- sum(values(r_wood2016_hfr_dummy), na.rm = TRUE)
pct_change_per_year <- (pixels_forest2016 / pixels_forest1999) / length_years * 100
