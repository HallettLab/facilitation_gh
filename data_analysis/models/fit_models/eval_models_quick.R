
library(shinystan)

launch_shinystan(PrelimFit)


for(i in rain){
  
  ## load models
  load(paste0("data_analysis/models/output/intra_sigmoidal/", date, "/acam_nb_sigmoidal_w", i, "_", date, "_nL_INTRA.rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## extract model info
  tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
  
  ## save posterior distributions
  acam_sig_posts[[paste0("acam_w", i)]] <- tmp
  
}
