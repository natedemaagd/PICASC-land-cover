library(nlme); library(lfe); library(foreach); library(doParallel); library(sp); library(ggplot2)
#registerDoParallel(cores = 4)
set.seed(100)

# load data
load('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/01_regression_data_setup.Rdata')
rm(dat_moku, moku)




##### spatial regressions #####

# regression equation
#reg_eqn <- formula(AET ~ PT + SM)
reg_eqn <- formula(AET ~ PT + rain_ann_in)

# # identify coordinates for spatial autocorrelation
# error_corr <- formula( ~ POINT_X + POINT_Y)
# 
# # correlation types
# corr_types <- expression(corLin(  form = error_corr, c(200, 0.1), nugget = TRUE),
#                          corGaus( form = error_corr, c(200, 0.1), nugget = TRUE),
#                          corRatio(form = error_corr, c(200, 0.1), nugget = TRUE),
#                          corSpher(form = error_corr, c(200, 0.1), nugget = TRUE),
#                          corExp(  form = error_corr, c(200, 0.1), nugget = TRUE))

# define landcovers
landcovers <- c(5,6,31,32,33,39,41,42)


# # regression functions - each landcover
# for(i in 1:length(landcovers)){
#     
#     # create list to store data
#     reg_spatial <- list()
#     
#     # if there are more than 6000 pixels of landcover i, sample 6000
#     if(length(dat$LC[dat$LC == landcovers[[i]]]) < 6000){
#       reg_dat <- dat[dat$LC == landcovers[[i]],]
#     } else {
#       reg_dat <- dat[sample(which(dat$LC == landcovers[[i]]), 6000),]
#     }
#     
#     # run regressions for each correlation type
#     for(j in 1:length(corr_types)){
#         
#       # tryCatch will skip correlation structure type if, e.g., convergence fails
#       reg_spatial[[j]] <- tryCatch({
#         
#         # run model with landcover i and correlation structure j
#         gls(data = reg_dat, model = reg_eqn,
#             correlation = formula(corr_types[[j]]),
#             control = glsControl(maxIter = 100, msMaxIter = 100, tolerance = 1e-3))
#         
#       }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
#     }
#     
#     # remove any remaining NULL regressions
#     reg_spatial[sapply(reg_spatial, is.null)] <- NULL
#     
#     # get AICs
#     reg_AICs <- sapply(reg_spatial, AIC)
#     
#     # keep only smallest
#     best_AIC_reg <- which.min(reg_AICs)
#     reg_spatial_final <- reg_spatial[[best_AIC_reg]]
#     
#     # save output
#     saveRDS(reg_spatial_final, file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions/02_regressions_landcover', landcovers[[i]], '_PtSm.rds'))
#     gc()
# 
# }




# ##### analyze regression results - PT + SM #####
# 
# # load regressions
# reg_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions', pattern = 'PtSm')
# reg_data <- lapply(reg_filenames, function(d) readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions/', d)))
# 
# # get landcover code for each regression
# reg_landcoverCode <- gsub('.*-([0-9]+).*', '\\1', substr(reg_filenames, 3, nchar(reg_filenames)))
# reg_landcoverCode <- as.numeric(stringr::str_extract(reg_landcoverCode, "[[:digit:]]+"))
# 
# # get residuals from each LC type
# reg_resids <- lapply(reg_data, function(reg) data.frame(resid = reg$residuals))
# for(i in 1:length(reg_resids)){ reg_resids[[i]]$LC = reg_landcoverCode[[i]]}
# reg_resids <- do.call(rbind, reg_resids)
# 
# # plot residual distributions
# ggplot(data = reg_resids) +
#   geom_histogram(aes(resid, color = as.character(LC), fill = as.character(LC)), alpha = 0.5) +
#   labs(x = 'Residual (mm/yr)', y = 'Number of pixels', fill = 'Landcover', color = 'Landcover') +
#   annotate(geom = 'text', x = 400, y = 12500, label = paste0('Mean = ', round(mean(reg_resids$resid)), ' mm/yr\nMedian = ', round(median(reg_resids$resid)), ' mm/yr')) +
#   theme(text = element_text(size = 15))
# ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/02_regressions_residualsByLandcoverType_PtSmGls.png',
#        height = 6, width = 8, dpi = 300)
# 
# summary(reg_resids$resid)




# ##### analyze regression results - original LAI model #####
# 
# # regression equation
# reg_eqn <- formula(AET ~ LAI + U + T + SM + Rnet)
# 
# 
# # load regressions
# reg_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions', pattern = '.rds')
# reg_filenames <- reg_filenames[grep('PtSm.rds', reg_filenames)]
# reg_data <- lapply(reg_filenames, function(d) readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions/', d)))
# 
# # get landcover code for each regression
# reg_landcoverCode <- gsub('.*-([0-9]+).*', '\\1', substr(reg_filenames, 3, nchar(reg_filenames)))
# reg_landcoverCode <- as.numeric(stringr::str_extract(reg_landcoverCode, "[[:digit:]]+"))
# 
# # get residuals from each LC type
# reg_resids <- lapply(reg_data, function(reg) data.frame(resid = reg$residuals))
# for(i in 1:length(reg_resids)){ reg_resids[[i]]$LC = reg_landcoverCode[[i]]}
# reg_resids <- do.call(rbind, reg_resids)
# 
# # plot residual distributions
# ggplot(data = reg_resids) +
#   geom_histogram(aes(resid, color = as.character(LC), fill = as.character(LC)), alpha = 0.5) +
#   labs(x = 'Residual (mm/yr)', y = 'Number of pixels', fill = 'Landcover', color = 'Landcover') +
#   annotate(geom = 'text', x = 400, y = 12500, label = paste0('Mean = ', round(mean(reg_resids$resid)), ' mm/yr\nMedian = ', round(median(reg_resids$resid)), ' mm/yr')) +
#   theme(text = element_text(size = 15))
# ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/02_regressions_residualsByLandcoverType_originalLaiModel.png',
#        height = 6, width = 8, dpi = 300)
# 
# summary(reg_resids$resid)




##### compare results to regular lm regression  - PT + rain #####

# reset regression equation
reg_eqn_PtRain <- formula(AET ~ PT + rain_ann_in)

# get landcover code for each regression
reg_filenames <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions', pattern = 'PtSm')
reg_landcoverCode <- gsub('.*-([0-9]+).*', '\\1', substr(reg_filenames, 3, nchar(reg_filenames)))
reg_landcoverCode <- as.numeric(stringr::str_extract(reg_landcoverCode, "[[:digit:]]+"))

# run regression for each landcover type
reg_data_lm_PtRain <- list()
for(i in 1:length(reg_landcoverCode)){
  reg_data_lm_PtRain[[i]] <- lm(formula = reg_eqn_PtRain, data = dat[dat$LC == reg_landcoverCode[[i]],])
}

# get residuals from each LC type
reg_resids_lm_PtRain <- lapply(reg_data_lm_PtRain, function(reg) data.frame(resid = reg$residuals))
for(i in 1:length(reg_resids_lm_PtRain)){ reg_resids_lm_PtRain[[i]]$LC = reg_landcoverCode[[i]]}
reg_resids_lm_PtRain <- do.call(rbind, reg_resids_lm_PtRain)

# plot residual distributions
ggplot(data = reg_resids_lm_PtRain) +
  geom_histogram(aes(resid, color = as.character(LC), fill = as.character(LC)), alpha = 0.5, bins = 100) +
  labs(x = 'Residual (mm/yr)', y = 'Number of pixels', fill = 'Landcover', color = 'Landcover') +
  annotate(geom = 'text', x = 400, y = 12500, label = paste0('Mean = ', round(mean(reg_resids_lm_PtRain$resid)), ' mm/yr\nMedian = ', round(median(reg_resids_lm_PtRain$resid)), ' mm/yr')) +
  theme(text = element_text(size = 15))
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/02_regressions_residualsByLandcoverType_lm.png',
       height = 6, width = 8, dpi = 300)

summary(reg_resids_lm_PtRain$resid)

# model fits - PT + rain
vec_modelFits_PtRain <-
  sapply(reg_data_lm_PtRain, function(x){
    summary(x)$r.squared
  })
summary(vec_modelFits_PtRain)

# save regressions
names(reg_data_lm_PtRain) <- reg_landcoverCode
saveRDS(reg_data_lm_PtRain,
        file = 'H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions/02_regressions lm PtRain all landcovers.rds')




##### compare results to regular lm regression  - PT + SM #####

# reset regression equation
reg_eqn_PtSm <- formula(AET ~ PT + SM)

# run regression for each landcover type
reg_data_lm_PtSm <- list()
for(i in 1:length(reg_landcoverCode)){
  reg_data_lm_PtSm[[i]] <- lm(formula = reg_eqn_PtSm, data = dat[dat$LC == reg_landcoverCode[[i]],])
}

# get residuals from each LC type
reg_resids_lm_PtSm <- lapply(reg_data_lm_PtSm, function(reg) data.frame(resid = reg$residuals))
for(i in 1:length(reg_resids_lm_PtSm)){ reg_resids_lm_PtSm[[i]]$LC = reg_landcoverCode[[i]]}
reg_resids_lm_PtSm <- do.call(rbind, reg_resids_lm_PtSm)

# plot residual distributions
ggplot(data = reg_resids_lm_PtSm) +
  geom_histogram(aes(resid, color = as.character(LC), fill = as.character(LC)), alpha = 0.5, bins = 100) +
  labs(x = 'Residual (mm/yr)', y = 'Number of pixels', fill = 'Landcover', color = 'Landcover') +
  annotate(geom = 'text', x = 400, y = 12500, label = paste0('Mean = ', round(mean(reg_resids_lm_PtSm$resid)), ' mm/yr\nMedian = ', round(median(reg_resids_lm_PtSm$resid)), ' mm/yr')) +
  theme(text = element_text(size = 15))
ggsave(filename = 'H:/My Drive/Projects/PICASC Land-to-sea/Figures and tables/Figures/Water yield/02_regressions_residualsByLandcoverType_lm.png',
       height = 6, width = 8, dpi = 300)

summary(reg_resids_lm_PtSm$resid)

# model fits - PT + SM
vec_modelFits_PtSm <-
  sapply(reg_data_lm_PtSm, function(x){
    summary(x)$r.squared
  })
summary(vec_modelFits_PtRain)




##### FINALIZED MODEL #####

# PT + Rain lm fit the data best, so use that

for(i in 1:length(reg_data_lm_PtRain)){
  saveRDS(reg_data_lm_PtRain[[i]],
          file = paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions/lm/02_regressions_landcover', reg_landcoverCode[[i]], '_PtRain.rds'))
}
