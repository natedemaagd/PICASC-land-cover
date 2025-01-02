
# This script looks at the spread of specific non-nativbe landcovers in ANRPO data.
# Interested in Toona Ciliata/red cedar, Psidium cattelianum/strawberry guava,
# Grevillea robusta/silk oak, and Schinus terebinthifolius/christmas berry


library(ggplot2); library(reshape2); library(viridis); library(cowplot)

# load data
dat <- read.csv("H:/My Drive/Projects/PICASC Land-to-sea/Data/Raw/Water yield/Army data/ANRPO_Belt_transect_alien_canopy_cover_species_matrix.csv")
colnames(dat)[colnames(dat) == '?..BltPltCode'] <- 'BltPltCode'

# keep only species of interest
vec_species <- c('TooCil', 'PsiCat', 'GreRob', 'SchTer')
vec_speciesName <- c('Red cedar', 'Strawberry guava', 'Silk oak', 'Christmas berry')
dat <- dat[c('BltPltCode', 'MU', 'Transect', 'Start', 'PltMainDate', 'VegStrata',
             vec_species)]

# create date variable and standardize to year-month
dat$date <- as.Date(dat$PltMainDate,
                    format = '%d-%h-%y')
dat$date <- as.Date(sub("\\d{2}$", "01", dat$date))

# if species is NA, replace with 0
for(s in 1:length(vec_species)){
  dat[vec_species[[s]]][is.na(dat[vec_species[[s]]])] <- 0
}




##### create slope variables for species spread rates #####

# slope = percent change per year

# split data by site
datBySite <- split(dat, dat$BltPltCode)

# create years passed and slope variable
datBySite <- lapply(datBySite, function(df){
  
  # get dataframe df
  df_site <- df
  
  # for each observation, find how many years have passed since first observation
  df_site$years_passed <- NA
  for(o in 1:nrow(df_site)){
    df_site$years_passed[[o]] <-
      (df_site$date[[o]] - df_site$date[[1]]) / 365  # subtraction of dates yields days
  }
  
  # slope for each species will be the change in canopy cover per year between observations
  df_site[paste0('slope_', vec_species)] <- NA
  for(s in 1:length(vec_species)){
    for(o in 2:nrow(df_site)){
      df_site[o, paste0('slope_', vec_species[[s]])] <-
        (df_site[o, vec_species[[s]]] - df_site[1, vec_species[[s]]]) /
        df_site$years_passed[[o]]
    }
  }
  
  # determine whether slope is positive or negative
  df_site[paste0('slope_', vec_species, '_isPositive')] <- NA
  for(s in 1:length(vec_species)){
    for(o in 2:nrow(df_site)){
      df_site[o, paste0('slope_', vec_species[[s]], '_isPositive')] <-
        df_site[o, paste0('slope_', vec_species[[s]])] > 0
    }
  }
  
  # make row 1 slope same as row 2 since it'll always be NA
  for(s in 1:length(vec_species)){
    for(o in 2:nrow(df_site)){
      df_site[1, paste0('slope_', vec_species[[s]], '_isPositive')] <-
        df_site[2, paste0('slope_', vec_species[[s]], '_isPositive')]
    }
  }
  
  df_site
  
})

# change to a geom_segment-friendly dataset
datBySite2 <- lapply(datBySite, function(df){
  
  # get data.frame for the site
  df_site <- df
  
  # convert to geom_segment-type data
  df_bySpecies <- list()
  for(s in 1:length(vec_species)){
    df_bySpecies[[s]] <-
      as.data.frame(with(df_site,
                         cbind(embed(get(vec_species[[s]]),2),
                               embed(years_passed,2))))
    colnames(df_bySpecies[[s]]) <- paste0(c('yend','y','xend','x'))
    df_bySpecies[[s]]$species <- vec_species[[s]]
    df_bySpecies[[s]]$slope <- with(df_bySpecies[[s]],
                                    ifelse(yend - y > 0 & !is.na(yend - y),
                                           'Positive', 'Non-positive'))
    df_bySpecies[[s]]$slopeVal <- with(df_bySpecies[[s]],
                                       (yend - y)/(xend - x))
  }
  
  # combine back into a single data.frame containing all species
  df_site <- do.call(rbind, df_bySpecies)
  rm(df_bySpecies)
  
  df_site
})

# add site name
for(s in 1:length(datBySite2)){
  datBySite2[[s]]$site <- names(datBySite2)[[s]]
}

# convert back to single data.frame
dat <- do.call(rbind, datBySite)
dat2 <- do.call(rbind, datBySite2)
rm(datBySite, datBySite2, s)
gc()


# plot change in canopy for each species
plots <- list()
for(s in 1:length(vec_species)){
  plots[[s]] <- ggplot(data = dat2[dat2$species == vec_species[[s]],]) +
    geom_segment(aes(x = x, xend = xend,
                     y = y, yend = yend,
                     color = slopeVal,
                     linetype = site),
              alpha = 0.6) +
    # geom_point(aes(x = years_passed,
    #                y = get(vec_species[[s]])),
    #                alpha = 0.2,
    #                color = 'black',
    #            data = dat) +
    scale_linetype_manual(values = rep('solid',
                                       times = length(unique(dat2$site)))) +
    scale_color_gradient2(low = viridis(2)[[2]],
                          mid = 'gray',
                          high = viridis(2)[[1]]) +
    labs(x = 'Years',
         y = '% cover',
         title = vec_speciesName[[s]],
         subtitle = paste0('Mean positive growth = ',
                           round(mean(dat2$slopeVal[dat2$species ==
                                                      vec_species[[s]]][dat2$slopeVal[dat2$species ==
                                                                                        vec_species[[s]]] > 0]),
                                 2),
                           '% per year')) +
    theme(text = element_text(size = 20),
          legend.position = 'none')
}

plot_grid(plotlist = plots, ncol = 2)
ggsave2(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Army data/ANRPO species-specific growth.png',
        height = 7.8, width = 13.2, dpi = 300)




##### convert slopes to spread rate (area/year) #####

# define transect area
area_transect_sqMeters <- 30 * 10

# create data.frame with distribution of positive growth rates by species
list_positiveGrowthBySpecies <-
  list(data.frame(species = 'Red cedar',
                  slope = dat$slope_TooCil[dat$slope_TooCil > 0]),
       data.frame(species = 'Strawberry guava',
                  slope = dat$slope_PsiCat[dat$slope_PsiCat > 0]),
       data.frame(species = 'Silk oak',
                  slope = dat$slope_GreRob[dat$slope_GreRob > 0]),
       data.frame(species = 'Christmas berry',
                  slope = dat$slope_SchTer[dat$slope_SchTer > 0])
  )
dat_positiveGrowthBySpecies <-
  do.call(rbind, list_positiveGrowthBySpecies)
dat_positiveGrowthBySpecies <-
  dat_positiveGrowthBySpecies[!is.na(dat_positiveGrowthBySpecies$slope),]
rm(list_positiveGrowthBySpecies)

# plot distribution of growth rates
p <- ggplot() +
  geom_histogram(data = dat_positiveGrowthBySpecies,
                 aes(x = slope, color = species, fill = species),
                 bins = 30, alpha = 0.8) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(text = element_text(size = 15),
        legend.position = c(0.9, 0.8)) +
  labs(x = 'Increase in canopy cover (% per year)',
       y = 'Number of transects',
       color = NULL, fill = NULL, linetype = NULL)

# add mean and median positive growth rates for each species
dat_positiveGrowthBySpecies_median <-
  aggregate(dat_positiveGrowthBySpecies$slope, list(dat_positiveGrowthBySpecies$species), median)
dat_positiveGrowthBySpecies_mean <-
  aggregate(dat_positiveGrowthBySpecies$slope, list(dat_positiveGrowthBySpecies$species), mean)
dat_positiveGrowthBySpecies_summary <-
  cbind(dat_positiveGrowthBySpecies_mean, dat_positiveGrowthBySpecies_median$x)
colnames(dat_positiveGrowthBySpecies_summary) <-
  c('species', 'Mean', 'Median')
rm(dat_positiveGrowthBySpecies_mean, dat_positiveGrowthBySpecies_median)
dat_positiveGrowthBySpecies_summary <-
  data.frame(species = dat_positiveGrowthBySpecies_summary$species,
             value = c(dat_positiveGrowthBySpecies_summary$Mean, dat_positiveGrowthBySpecies_summary$Median),
             stat = rep(c('Mean', 'Median'), each = length(unique(dat_positiveGrowthBySpecies_summary$species))))
p +
  geom_vline(data = dat_positiveGrowthBySpecies_summary,
             aes(xintercept = value, linetype = stat, color = species),
             linewidth = 1.2)
ggsave(p,
       filename = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/',
                         'Misc - Non-native forest spread Army data - nonnative spread rates.png'),
       dpi = 300)

# convert slope %/yr to area/yr (sq meters/yr)
dat_positiveGrowthBySpecies$sqMetersPerYr <-
  dat_positiveGrowthBySpecies$slope / 100 * area_transect_sqMeters

# plot distribution of spread rate (sq meters per year)
ggplot(data = dat_positiveGrowthBySpecies,
       aes(x = sqMetersPerYr, color = species, fill = species)) +
  geom_histogram(bins = 30) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(text = element_text(size = 15)) +
  labs(x = 'Growth rate (sq. meters per year)',
       y = 'Number of transects',
       color = NULL, fill = NULL)

# how many years to fill entire transect?
dat_positiveGrowthBySpecies$yrs_to_fill_30sqMeters <-
  area_transect_sqMeters / dat_positiveGrowthBySpecies$sqMetersPerYr

# plot distribution of years to fill transect
ggplot(data = dat_positiveGrowthBySpecies,
       aes(x = yrs_to_fill_30sqMeters,
           color = species, fill = species)) +
  geom_histogram(bins = 30) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(text = element_text(size = 15)) +
  labs(x = 'Years to fill 900 sq. meter pixel',
       y = 'Number of transects',
       color = NULL, fill = NULL)
