
# this script determines, based on thresholds defined below, the rate at which sites in the
# Army data convert from native to non-native dominant canopy

# load data
dat <- read.csv("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Army data/2022_ANRPO_beltplots_percentcover_spprichness_WGS84.csv")
dat$PltMainDate <- as.Date(dat$PltMainDate, format = '%Y-%m-%d')

# define canopy thresholds
threshold_native = 50
threshold_nonnative = 50




##### find rate at which sites convert from native to non-native dominant #####

# create canopy dummies
dat$dummy_nativeCanopy <- ifelse(dat$NatCanopy > threshold_native, 1, 0)
dat$dummy_nonnativeCanopy <- ifelse(dat$XCanopy > threshold_native, 1, 0)

# split data by location
datByLoc <- split(dat, dat$BltPltCode)

# order observations by date
datByLoc <- lapply(datByLoc, function(df){
  df[order(df$PltMainDate),]
})

# keep only dates and dummy variables
datByLoc <- lapply(datByLoc, function(df){
  df[c('PltMainDate', 'dummy_nativeCanopy', 'dummy_nonnativeCanopy')]
})
