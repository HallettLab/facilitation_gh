
library(shinystan)

launch_shinystan(PrelimFit)


## load models
load(paste0("data_analysis/models/output/m0_models/", date, "/brho_nb_stat_w", i, "_", date, "_soil_comp_adjust_priors.rdata"))
  