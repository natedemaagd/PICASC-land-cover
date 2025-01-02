
# This script estimates fire risk under the new land cover scenarios, using 15a
# rasters with new fractional cover

library(terra); library(data.table)




##### load data #####

# regression data
bigextfinal <- fread( "H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire  - PROTECT/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_07_allisl_monthly_fire_250m_samp_wgs84_OVERWRITE.csv")

# regression models
reg_models <- readRDS('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Fire  - PROTECT/2021_09_FIRE_Hawaii_all_isl_EXTRACTION/2021_09_models_and_evals.Rdata')

# fractional land cover

