library(nlme); library(stargazer)

# load regression results
reg_names <- list.files('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions', pattern = '.rds')
reg_data  <- lapply(reg_names, function(d) readRDS(paste0('H:/My Drive/Projects/PICASC Land-to-sea/Data/Intermediate/Water yield/02_regressions/', d)))

# define regression parameters
reg_eqn <- formula(AET ~ LAI + SM + U + T + Rnet)

stargazer(reg_data,
          # column.labels = c('Montane RF', 'Low dry forest', 'Low mesic forest', 'Montane sub-alp dry forest',
          #                   'Sub-alp mesic forest', 'Introd. wet mesic forest', 'Introd. peren. grassland', 'Lowland RF'),
          title = 'GLS models of each landcover, with corrections for spatial autocorrelation. Landcovers are as follows: (1) montane rainforest; (2) lowland dry forest; (3) lowland mesic forest; (4) montane sub-alpine dry forest; (5) sub-alpine mesic forest; (6) introduced wet mesic forest; (7) introduced perennial grassland; and (8) lowland rainforest.',
          label = 'tab:gls_landcover_models')
