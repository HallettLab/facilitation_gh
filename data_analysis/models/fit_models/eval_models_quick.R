
library(shinystan)

launch_shinystan(PrelimFit)


## load models
load(paste0("data_analysis/models/output/m0_models/", date, "/brho_nb_sig_w", i, "_", date, "_sterilized_no_intra.rdata"))
  