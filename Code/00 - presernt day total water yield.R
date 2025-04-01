
# This script estimates present-day water yield by subtracting ET from rainfall

library(terra)




##### load and format data #####

# load rasters 
rast_rain <- rast("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_ann_rainfall_inches/Hawaii-State/rfgrid_inches_state_ann.txt")
rast_et <-  rast("C:/Users/nated/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_AET/aet_mm_ann.txt")

# convert rain to mm
rast_rain <- rast_rain * 25.4




##### calculate water yield #####

# calculate water yield
rast_wy <- rast_rain - rast_et

# ensure water yield isn't less than 0
rast_wy[rast_wy < 0] <- 0

# convert wy from mm to L per day (250m resolution * values to get cubic mm, then convert to L, then get daily million)
rast_wy_MLD <- rast_wy * 250000 * 250000 * 0.000001 / 365 / 1e6

# billion L per day: 57.48942 BLD (or 57489.42 MLD)
sum(values(rast_wy_MLD), na.rm = TRUE) * 0.001
