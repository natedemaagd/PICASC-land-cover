
# this script determines how change in native canopy is related to initial non-native canopy
# Army data convert from native to non-native dominant canopy

library(ggplot2)

# load data
dat <- read.csv("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Army data/2022_ANRPO_beltplots_percentcover_spprichness_WGS84.csv")
dat$PltMainDate <- as.Date(dat$PltMainDate, format = '%Y-%m-%d')

# define canopy thresholds
threshold_native = 50
threshold_nonnative = 50




##### change in native cover as function of proportion non-native canopy #####

# create canopy dummies
dat$dummy_nativeCanopy <- ifelse(dat$NatCanopy > threshold_native, 1, 0)
dat$dummy_nonnativeCanopy <- ifelse(dat$XCanopy > threshold_native, 1, 0)

# split data by location
datByLoc <- split(dat, dat$BltPltCode)

# order observations by date
datByLoc <- lapply(datByLoc, function(df){
  df[order(df$PltMainDate),]
})

# find change in canopy
datChange <-
  data.frame(change_nativeCanopy =
               sapply(datByLoc, function(df){
                 df$NatCanopy[[nrow(df)]] - df$NatCanopy[[1]]
                 }),
             initialNonNatCanopyProportion = 
               sapply(datByLoc, function(df){
                 df$XCanopy[[1]] / df$TotCanopy[[1]]
                 })
             )

# change in canopy per year
datChange$observationLength_yrs <-
  sapply(datByLoc, function(df){
    (df$PltMainDate[[nrow(df)]] - df$PltMainDate[[1]]) / 365
  })
datChange$change_nativeCanopy_perYr <-
  datChange$change_nativeCanopy / datChange$observationLength_yrs

# if proportion initial non-native canopy > 1, set to 1
datChange$initialNonNatCanopyProportion[datChange$initialNonNatCanopyProportion > 1] <- 1




##### plot change in native canopy = f(proportion initial non-native canopy) #####

ggplot(data = datChange,
       aes(x = initialNonNatCanopyProportion,
           y = change_nativeCanopy_perYr,
           color = observationLength_yrs)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = 'gam') +
  scale_color_viridis_c(name = 'Observation length\n(years)') +
  labs(x = 'Initial proportion non-native canopy',
       y = 'Mean annual change in native canopy (%)') +
  theme(text = element_text(size = 17))

ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Army data/',
                         'change in native canopy as fcn of initial non-native canopy.png'),
       height = 6, width = 10, dpi = 300)
