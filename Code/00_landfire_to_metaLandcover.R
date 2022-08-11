library(raster); library(readxl); library(dplyr)

# load landfire landcover data (all vars)
allvars <- as.data.frame(readRDS("D:/OneDrive - hawaii.edu/Documents/Projects/DWS/Land_cover/Data/Raw/AllVars_MeanAnnVals.rds"))

# load landcover comparison data
lc_dat <- as.data.frame(read_xlsx("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Landcover_MetaCategories_GM.xlsx"))

# create landcover code matching table (landfire -> meta category)
lc_matching_table <- lc_dat[c('Land fire #', 'Meta_Category')]
lc_matching_table <- lc_matching_table[!duplicated(lc_matching_table),]
names(lc_matching_table) <- c('LC', 'LC_metaCategory')

# adjust lc_matching categories with more than one landfire code
lc_matching_table[nrow(lc_matching_table)+1,] <- c('8', 'Wet Native Forest')
lc_matching_table[nrow(lc_matching_table)+1,] <- c('10', 'Wet Native Forest')
lc_matching_table[nrow(lc_matching_table)+1,] <- c('18', 'Mesic Native Grassland')
lc_matching_table[nrow(lc_matching_table)+1,] <- c('19', 'Mesic Native Grassland')
lc_matching_table[nrow(lc_matching_table)+1,] <- c('30', 'Other')
lc_matching_table[nrow(lc_matching_table)+1,] <- c('36', 'Other')
lc_matching_table[nrow(lc_matching_table)+1,] <- c('37', 'Other')
lc_matching_table$LC <- as.numeric(as.character(lc_matching_table$LC))
lc_matching_table <- lc_matching_table[complete.cases(lc_matching_table),]

# add meta landcover categories to allvars using matching table. If no meta, consider it to be "Other" - most is 33 (introduced deciduous shrubland)
allvars <- merge(allvars, lc_matching_table, 'LC')
allvars[is.na(allvars$LC_metaCategory), 'LC_metaCategory'] <- 'Other'

# create raster: create a numerical version of meta category, then use to create raster
LC_metaCategory_table <- data.frame(LC_metaCategory = unique(allvars$LC_metaCategory))
LC_metaCategory_table$LC_metaCategory <- LC_metaCategory_table[order(LC_metaCategory_table$LC_metaCategory),]  # alphabetical order
LC_metaCategory_table$LC_metaCategory_code <- 1:nrow(LC_metaCategory_table)
allvars <- left_join(allvars, LC_metaCategory_table, 'LC_metaCategory')

extent <- as.matrix(allvars[c('POINT_X', 'POINT_Y')])
extent <- extent(extent)
raster_metaCategory <- raster(extent, ncol=length(unique(allvars$POINT_Y)), nrow=length(unique(allvars$POINT_X)))
raster_metaCategory <- rasterize(allvars[,c('POINT_X', 'POINT_Y')], raster_metaCategory, allvars[,'LC_metaCategory_code'], fun=max)
crs(raster_metaCategory) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# write raster
writeRaster(raster_metaCategory, filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/00_landfire_to_metaLandcover/allvars_landcover_metaCategory.tif', overwrite = TRUE)
write.csv(LC_metaCategory_table, file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/00_landfire_to_metaLandcover/allvars_landcover_metaCategory_codes.csv')
