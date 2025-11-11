
library(shinystan)

launch_shinystan(PrelimFit)

load(paste0("data_analysis/models/output/m0_models/", date, "/acam_nb_stat_w", i, "_", date, "_soil_comp_final.rdata"))

## load models
load(paste0("data_analysis/models/output/m0_models/", date, "/brho_nb_sig_w", i, "_", date, "_sterilized_no_intra.rdata"))
  