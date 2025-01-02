
# This script uses the rasters generated in 08b (non-native forest spread and
# reforestation in protected areas combined) to calculate reforestation over time
# and create a combined timelapse.

library(raster)
library(ggplot2)
library(gganimate)
library(viridis)




##### count pixels reforested each year #####

# reforested pixel value
val_reforested <- 1

# list rasters
list_ras <-
  list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/30m rasters/masked',
             pattern = '.tif', full.names = TRUE)

# for each raster, count number of reforested pixels
dat <-
  data.frame(year = 1:length(list_ras),
             numPixels_reforested = NA)

for(i in 1:length(list_ras)){

  # load raster for year i
  ras <- raster(list_ras[[i]])

  # count the number of reforested pixels
  dat$numPixels_reforested[[i]] <-
    length(ras[ras == val_reforested])
  print(paste0(i, '---', Sys.time()))
  gc(); removeTmpFiles(h = 2)
}
rm(i, ras)
gc()

saveRDS(dat, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration',
                           '08c - yearly nonnative forest pixel count data.rds'))

dat <- readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration',
                      '08c - yearly nonnative forest pixel count data.rds'))




##### format data #####

# change from number of pixels to number of pixels converted
dat$numPixels_reforested <-
  c(NA, diff(dat$numPixels_reforested))

# convert to hectares
dat$numPixels_reforested <-
  dat$numPixels_reforested * 0.09

# get cumulative hectares
dat$numPixels_reforested_cumulative <- NA
for(i in 2:nrow(dat)){
  dat$numPixels_reforested_cumulative[[i]] <-
    sum(dat$numPixels_reforested[1:i], na.rm = TRUE)
}
rm(i)

# melt data
dat <- dat[-2,]
dat_combined <-
  data.frame(year = rep(1:nrow(dat), times = 2),
             value = c(dat$numPixels_reforested,
                       dat$numPixels_reforested_cumulative),
             group = rep(c('Yearly', 'Cumulative'),
                         each = nrow(dat))
  )
dat_combined$hectares_1000s <- dat_combined$value / 1000
dat_combined$value <- NULL

# plot line graphs
ggplot(data = dat_combined,
       aes(x = year, y = hectares_1000s)) +
  geom_line(linewidth = 1.3) +
  facet_grid(rows = vars(group), scales = 'free_y') +
  labs(x = 'Year', y = 'Hectares converted (1000s)', linetype = 'Model') +
  theme(text = element_text(size = 15))
ggsave(filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         '08c hectares converted nonnative to native forest within ungulate protected areas.png'),
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
#val_invasive <- c(1600, 1700, 1900, 2000, 11900, 12000)  # Tom G 32 equivalent
val_invasive <- 0

# areas within ungulate-protected regions that are reforested
val_reforested <- 1

# load 250m rasters
list_ras <-
  list.files(path = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/08 statewide landcover spread - forest restoration/rasters/08b - statewide landcover spread - rasters - combined nonnative spread and native reforestation/250m rasters/masked/',
             full.name = TRUE, pattern = '.tif')

# load each raster and convert to data.frame
list_dat <- lapply(list_ras, function(r){
  as.data.frame(raster(r), xy = TRUE)
})
gc(); removeTmpFiles(h = 1)

# convert ocean pixels to NA
list_dat <- lapply(list_dat, function(df){
  df[df$layer != 65535,]
})
gc()

# combine into one data.frame
for(i in 1:length(list_dat)){
  list_dat[[i]]$year <- i
}
dat <- do.call(rbind, list_dat)
rm(list_dat, i, list_ras)
gc()

# create landcover grouping variables
dat$lcCategory <-
  ifelse(dat$layer %in% val_invasive, 'Non-native forest',
  ifelse(dat$layer %in% val_susceptible, 'Existing native forest',
  ifelse(dat$layer %in% val_protectedDummy, 'Ungulate-protected area',
  ifelse(dat$layer %in% val_reforested, 'Native forest in protected area', 'Other'))))
dat$layer <- NULL
gc()
dat$lcCategory <- 
  factor(dat$lcCategory,
         levels = c('Existing native forest', 'Native forest in protected area',
                    'Ungulate-protected area', 'Non-native forest',
                    'Other'))

# plot
p <- ggplot(data = dat,
            aes(x = x, y = y, fill = lcCategory)
) +
  geom_raster() +
  coord_equal() +
  scale_fill_manual(values = viridis(5)[c(3,4,2,5,1)]) +
  labs(title = 'Year {frame_time}',
       fill = NULL) +
  transition_time(year) +
  ease_aes('linear') +
  theme(axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), panel.background = element_blank(),
        text = element_text(size = 40))

anim_save(filename = "H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/08c landcover timelapse 30m upscaled to 250m.gif",
          animation = p, width = 1200, height = 800)
  
