
library(shinystan)

launch_shinystan(PrelimFit)


for(i in rain){
  
  ## load models
  load(paste0("data_analysis/models/output/m0_models/", date, "/brho_nb_stat_w", i, "_", date, "_soil_comp.rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## extract model info
  tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
  
  ## save posterior distributions
  acam_sig_posts[[paste0("acam_w", i)]] <- tmp
  
}
