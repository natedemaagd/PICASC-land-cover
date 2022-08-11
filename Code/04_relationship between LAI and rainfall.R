
library(stargazer)

# load data
load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/01_regression_data_setup.Rdata')

# regress LAI onto rainfall w/ landcover interaction
dat$LC_categorical <- as.factor(dat$LC)  # create categorical landcover variable
reg1 <- lm(LAI ~ rain_ann_in*LC_categorical, data = dat)

stargazer(reg1,
          dep.var.labels = 'LAI')

# save regression results
saveRDS(reg1, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/04_LAI_rainfall_regression.rds')
