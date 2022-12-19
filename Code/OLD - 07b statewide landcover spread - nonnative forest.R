
# this script runs the land cover spread simulation for the state of Hawaii




##### setup #####

library(sp)
library(raster)
library(tidyr)
library(rgdal)
library(doParallel)
registerDoParallel(cores = 4)

#Set the label for the files
simulation_label <- "_02%spread_100 years_250m"
#Parameters
#infested_value
infest_val <- 32
#susceptible_values 
suscep <- as.integer(c(8, 10, 13))
#spread_rate 
spredrate <- growrate <- 0.02
#bird_rate 
birdcell <- 0.00
#simulation length (how far into the future will the sim run)
simlength <- 50
#simulation count (how many times to rep sim before calc a realization)
simulation_count <- 1000


#Rasters
LC          <- raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_landcover_types/land_cv_tp.txt") 
AET_mm_101  <- raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_AET/aet_mm_ann.txt")
PT          <- raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_PriestlyTaylorPotentialET/pr0_mm_ann/w001001.adf")
SM          <- raster("D:/OneDrive - hawaii.edu/Documents/Projects/Data/Rasters/rainfall_atlas_AvailSoilMst/sl_mst_ann/w001001.adf")



LAI <- LC
values(LAI) <- 3.3


#Load regression results
reg_results <- readRDS("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions/02_regressions_landcover32_PtSm.rds")
reg_coefs <- coefficients(reg_results)
intercept = reg_coefs[[1]]
beta_PT  = reg_coefs[[2]]
beta_SM   = reg_coefs[[3]]

pred_AET <- overlay(PT, SM,
                    fun = function(pt, sm){
                      intercept + beta_PT*pt + beta_SM*sm
                    })




##### function to simulate one year of spread #####

one_year <- function(
    x, 
    infested_value, 
    susceptible_values, 
    growth_rate = growrate, 
    bird_rate = birdcell, 
    old_aet = NULL, 
    pred_aet = NULL, 
    counter_layer = NULL
) {
  current_cells <- which(values(x) == infested_value)
  new_cells <- ceiling(length(current_cells) * growth_rate)
  
  adjacent_cells <- adjacent(x, which(values(x) == infested_value),
                             directions = 8, pairs = FALSE)
  flippable_cells <- adjacent_cells[values(x)[adjacent_cells] %in% susceptible_values]
  if (length(flippable_cells) == 0) {
    return(list(
      #starting_cell_size = length(current_cells), 
      #new_cells = new_cells, 
      #flipped_cells = flipped_cells,
      #bird_cells = bird_cells,
      #bird_flipped_cells = bird_flips,
      infested_cell_count = sum(values(x) == infested_value, na.rm = TRUE),
      result = x,
      counter_layer = counter_layer,
      new_aet = old_aet
    ))
  }
  flipped_cells <- sample(flippable_cells, min(length(flippable_cells), new_cells))
  
  bird_cells <- sample(
    which(values(x) %in% c(infested_value, susceptible_values)), 
    ceiling(length(current_cells) * bird_rate)
  )
  # bird_cells <- sample(which(values(x) %in% susceptible_values), birds) # definitely flip birds
  bird_flips <- bird_cells[which(values(x)[bird_cells] %in% susceptible_values)]
  
  y <- x
  y[flipped_cells] <- infested_value
  y[bird_flips] <- infested_value
  
  all_flipped_cells <- unique(c(flipped_cells, bird_flips))
  if (!is.null(counter_layer)) {
    counter_layer[all_flipped_cells] <- counter_layer[all_flipped_cells] + 1
  }
  
  old_aet[flipped_cells] <- values(pred_aet)[flipped_cells]
  old_aet[bird_flips] <- values(pred_aet)[bird_flips]
  
  return(list(
    #starting_cell_size = length(current_cells), 
    #new_cells = new_cells, 
    #flipped_cells = flipped_cells,
    #bird_cells = bird_cells,
    #bird_flipped_cells = bird_flips,
    infested_cell_count = sum(values(y) == infested_value, na.rm = TRUE),
    result = y,
    counter_layer = counter_layer,
    new_aet = old_aet
  ))
}




##### functions to run simulation one time #####

# x: initial land cover raster

#
one_simulation <- function(x, periods, list_of_counters, old_aet, pred_aet) {
  # extend all raster extents
  x <- extend(x, extend(old_aet, pred_aet))
  old_aet <- extend(old_aet, x)
  pred_aet <- extend(pred_aet, x)
  
  sum_aet = rep(NA, times = periods)
  infested_cells = rep(NA, times = periods)
  mean_aet = rep(NA, times = periods)
  median_aet = rep(NA, times = periods)
  
  # run one simulation
  for (i in 1:periods) {
    one_year_result <- one_year(x, infested_value = infest_val,
                                susceptible_values = suscep, 
                                growth_rate = growrate, bird_rate = birdcell, 
                                counter_layer = list_of_counters[[i]], 
                                old_aet = old_aet, pred_aet = pred_aet)
    sum_aet[i] <- sum(values(one_year_result$new_aet), na.rm = TRUE)
    infested_cells[i] <- one_year_result$infested_cell_count
    mean_aet[i] <- mean(values(one_year_result$new_aet), na.rm = TRUE)
    median_aet[i] <- median(values(one_year_result$new_aet), na.rm = TRUE)
    list_of_counters[[i]] <- one_year_result$counter_layer
    
    # use values from one_year() result for use in the next iteration
    x <- one_year_result$result
    old_aet = one_year_result$new_aet
  }
  
  return(list(
    sum_aet = sum_aet,
    infested_cells = infested_cells,
    mean_aet = mean_aet,
    median_aet = median_aet,
    list_of_counters = list_of_counters
  ))
}

#
create_counter_list <- function(x, periods) {
  counter_layer <- x
  counter_layer[which(!is.na(values(counter_layer)))] <- 0
  
  list_of_counters = list()
  for (i in 1:periods) {
    list_of_counters <- c(list_of_counters, counter_layer)
  }
  return(list_of_counters)
}

#
list_of_counters <- create_counter_list(LC, periods = simlength)
sum_aet_result <- c()
infested_cell_counts <- c()
mean_aet_result <- c()
median_aet_result <- c()




##### for loop simulation #####

for (i in 1:simulation_count) {
  simulation_result <- one_simulation(LC, periods = simlength,
                                      list_of_counters,
                                      old_aet = AET_mm_101, pred_aet = pred_AET)
  list_of_counters <- simulation_result$list_of_counters
  sum_aet_result <- rbind(sum_aet_result, simulation_result$sum_aet)
  infested_cell_counts <- rbind(infested_cell_counts,
                                simulation_result$infested_cells)
  mean_aet_result <- rbind(mean_aet_result, simulation_result$mean_aet)
  median_aet_result <- rbind(median_aet_result, simulation_result$median_aet)
  print(i)
}

save.image("H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/07 statewide landcover spread/sim_1_to_1000_LC32.RData")




##### yearly prediction #####

list_of_summed_counters = list()
for (i in 1:simlength) {
  list_of_summed_counters <- c(list_of_summed_counters,
                               raster :: calc(stack(list_of_counters[1:i]),
                                              fun = sum))
}

infested_value = infest_val
susceptible_values = suscep
spread_rate = spredrate
bird_rate = birdcell
last_raster <- LC
summary_list_of_rasters <- list(last_raster)
infested_cells <- rep(NA, 100)
for (i in 1:simlength) {
  infested_cell_count <- sum(values(last_raster) == infested_value, na.rm = TRUE)
  adjacent_target_count <- ceiling(infested_cell_count * spread_rate)
  bird_target_count <- ceiling(infested_cell_count * bird_rate)
  
  # spread
  adjacent_cells <- adjacent(last_raster, which(values(last_raster) == infested_value), 
                             directions = 8, pairs = FALSE)
  flippable_cells <- adjacent_cells[values(last_raster)[adjacent_cells] %in% susceptible_values]
  summed_counters_copy <- values(list_of_summed_counters[[i]])
  summed_counters_copy[-flippable_cells] <- 0
  adjacent_targets <- order(summed_counters_copy, decreasing = TRUE)[1:adjacent_target_count]
  last_raster[adjacent_targets[adjacent_targets %in% flippable_cells]] <- infested_value 
  
  # birds
  bird_targets <- sample(which(values(last_raster) %in% c(infested_value, susceptible_values)),
                         bird_target_count)
  last_raster[bird_targets] <- infested_value 
  
  infested_cells[i] <- sum(values(last_raster) == infested_value, na.rm = TRUE)
  
  # save landcover rasters
  writeRaster(last_raster, paste0("H:/My Drive/Projects/PICASC Land-to-sea/Data/Processed/Water yield/07 statewide landcover spread/rasters/LC 32 - 2 pct spread rate - landcover rasters/finalRRaster",
                                  simulation_label, ' year ', i, ".tif"),
              overwrite = TRUE)
  summary_list_of_rasters <- c(summary_list_of_rasters, last_raster)
}


