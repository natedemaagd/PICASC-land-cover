
# This script compares the results of the two simulations of non-native forest
# spread (07b - unprotected and 07d - protected).

library(ggplot2)
library(raster)
library(viridis)
library(gganimate)

# # load data - old simulation using 250m raster
# dat_unprotected <-
#   readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/',
#                  '07b - yearly forest conversion - unprotected.rds'))
# dat_protected <-
#   readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/',
#                  '07b - yearly forest conversion - protected.rds'))




##### count pixels converted each year #####

# list rasters
list_rasProtected <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07d ungulate protected/protected pixels set to original values',
             pattern = '.tif', full.names = TRUE)
list_rasProtectedDummy <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07d ungulate protected/protected pixels set to -1',
             pattern = '.tif', full.names = TRUE)
list_rasUnprotected <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/landcover spread timelapse yearly rasters/07b unprotected',
             pattern = '.tif', full.names = TRUE)

# # for each raster, count number of non-native forest pixels
# dat <-
#   data.frame(numPixels_nonNativeForest_protected =
#                rep(NA, times = length(list_rasProtected)),
#              numPixels_nonNativeForest_unprotected = NA
#              )
# for(i in 1:length(list_rasProtected)){
# 
#   # load rasters for year i
#   rasProtected <- raster(list_rasProtected[[i]])
#   rasUnprotected <- raster(list_rasUnprotected[[i]])
# 
#   # count the number of non-native forest pixels
#   dat$numPixels_nonNativeForest_protected[[i]] <-
#     length(rasProtected[rasProtected == 0])
#   dat$numPixels_nonNativeForest_unprotected[[i]] <-
#     length(rasUnprotected[rasUnprotected == 0])
#   print(paste0(i, '---', Sys.time()))
#   gc(); removeTmpFiles(h = 2)
# }
# rm(i, rasProtected, rasUnprotected)
# gc()
# 
# saveRDS(dat, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/',
#                            '07e - yearly nonnative forest pixel count data.rds'))

dat <- readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/',
                      '07e - yearly nonnative forest pixel count data.rds'))




##### format data #####

# change from number of pixels to number of pixels converted
dat$numPixelsConverted_nonNativeForest_protected <-
  c(NA, diff(dat$numPixels_nonNativeForest_protected))
dat$numPixelsConverted_nonNativeForest_unprotected <-
  c(NA, diff(dat$numPixels_nonNativeForest_unprotected))

# convert to hectares
dat$numHectaresConverted_nonNativeForest_protected <-
  dat$numPixelsConverted_nonNativeForest_protected * 0.09
dat$numHectaresConverted_nonNativeForest_unprotected <-
  dat$numPixelsConverted_nonNativeForest_unprotected * 0.09

# get cumulative hectares
dat$numHectaresConverted_nonNativeForest_protected_cumulative <- NA
dat$numHectaresConverted_nonNativeForest_unprotected_cumulative <- NA
for(i in 2:nrow(dat)){
  dat$numHectaresConverted_nonNativeForest_protected_cumulative[[i]] <-
    sum(dat$numHectaresConverted_nonNativeForest_protected[1:i], na.rm = TRUE)
  dat$numHectaresConverted_nonNativeForest_unprotected_cumulative[[i]] <-
    sum(dat$numHectaresConverted_nonNativeForest_unprotected[1:i], na.rm = TRUE)
}
rm(i)

# melt data
dat_combined <-
  data.frame(year = rep(1:nrow(dat), times = 4),
             value = c(dat$numHectaresConverted_nonNativeForest_protected,
                       dat$numHectaresConverted_nonNativeForest_unprotected,
                       dat$numHectaresConverted_nonNativeForest_protected_cumulative,
                       dat$numHectaresConverted_nonNativeForest_unprotected_cumulative),
             model = rep(c('Protected', 'Unprotected', 'Protected', 'Unprotected'),
                         each = nrow(dat)),
             group = rep(c('Yearly', 'Cumulative'),
                         each = nrow(dat)*2)
             )
dat_combined$hectares_1000s <- dat_combined$value / 1000
dat_combined$value <- NULL
dat_combined$model <-
  factor(dat_combined$model, levels = c('Unprotected', 'Protected'))


# plot line graphs
ggplot(data = dat_combined,
       aes(x = year, y = hectares_1000s, linetype = model)) +
  geom_line(linewidth = 1.3) +
  facet_grid(rows = vars(group), scales = 'free_y') +
  labs(x = 'Year', y = 'Hectares converted (1000s)', linetype = 'Model') +
  theme(text = element_text(size = 15))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '07e hectares converted native to nonnative forest - ungulate protected vs unprotected.png'),
       dpi = 300, height = 6, width = 10)

rm(dat_combined, dat)
gc()




##### create 250m timelapse #####

# define pixel categories
val_protectedDummy <- -1
# val_susceptible <- c(8, 10, 13)  # Tom G values
val_susceptible <- c(100, 200, 300, 400, 500, 10100, 10200, 10300, 10400, 10500, 10600,  # Tom G 10 equivalent (no code for Tom G 8 in new raster)
                     600, 700, 800, 1500, 10700, 10800, 10900, 11700)  # Tom G 13 equivalent
#val_invasive <- 32
val_invasive <- c(1600, 1700, 1900, 2000, 11900, 12000)  # Tom G 32 equivalent

# define mode function for use with raster aggregation below
Mode <- function(x, na.rm = TRUE) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# # convert each raster to 250m resolution and add to data.frame
# list_dat <- list()
# for(i in 1:length(list_rasProtected)){
#   
#   # load raster i for protected and unprotected
#   ras_protected <- raster(list_rasProtectedDummy[[i]])
#   ras_unprotected <- raster(list_rasUnprotected[[i]])
#   
#   # aggregate to 250m
#   ras_protected <-
#     raster::aggregate(ras_protected, fact = round(250/30),
#                       fun = Mode, expand = FALSE)
#   ras_unprotected <-
#     raster::aggregate(ras_unprotected, fact = round(250/30),
#                       fun = Mode, expand = FALSE)
#   
#   # convert to data.frames
#   ras_protected[ras_protected == 65535] <- NA
#   ras_unprotected[ras_unprotected == 65535] <- NA
#   ras_protected <- as.data.frame(ras_protected, xy = TRUE)
#   ras_protected <- ras_protected[!is.na(ras_protected$layer),]; gc()
#   ras_unprotected <- as.data.frame(ras_unprotected, xy = TRUE)
#   ras_unprotected <- ras_unprotected[!is.na(ras_unprotected$layer),]; gc()
#   
#   # format data
#   ras_protected$model <- 'Protected'
#   ras_protected$year <- i
#   ras_unprotected$model <- 'Unprotected'
#   ras_unprotected$year <- i
#   ras_dat <- rbind(ras_protected, ras_unprotected)
#   ras_dat$landcover <-
#     ifelse(ras_dat$layer == 0, 'Non-native forest',
#     ifelse(ras_dat$layer %in% val_susceptible, 'Native forest',
#     ifelse(ras_dat$layer == -1, 'Protected', 'Other')
#     )
#     )
#   ras_dat$layer <- NULL
#   
#   # add to list and clearn up
#   list_dat[[i]] <- ras_dat
#   removeTmpFiles(h = 1); gc()
#   print(paste0(i, ' --- ', Sys.time()))
#   
# }
# rm(i, ras_protected, ras_unprotected, ras_dat)
# gc()
# saveRDS(list_dat,
#         file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/',
#                       '07e - list_dat for timelapse map.rds'))

list_dat <- readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/',
                           '07e - list_dat for timelapse map.rds'))

# combine data
dat <- do.call(rbind, list_dat)
rm(list_dat); gc()



# plot script
p <- ggplot(data = dat,
       aes(x = x, y = y, fill = landcover)
       ) +
  geom_raster() +
  coord_equal() +
  facet_grid(rows = vars(model)) +
  scale_fill_manual(values = viridis(4)[c(3,4,2,1)]) +
  labs(title = 'Year {frame_time}',
       fill = NULL) +
  transition_time(year) +
  ease_aes('linear') +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), panel.background = element_blank(),
        text = element_text(size = 40))

anim_save(filename = "H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/07e landcover timelapse 30m upscaled to 250m.gif",
          animation = p, width = 1000, height = 1000)
