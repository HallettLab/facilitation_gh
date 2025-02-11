
library(ggpubr)
library(bayesplot)
library(rstan) ## need this for traceplot
library(tidyverse)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load models ####
## BRHO ####
### sigmoidal ####
rain = c(1, 0.75, 0.6)
#microbe = c(0, 1)
date = 20250211
brho_sig_posts = list()

for(i in rain){
#  for(j in microbe) {
    
    ## load non-constrained models
    load(paste0("data_analysis/models/output/sigmoidal/", date, "/brho_nb_sigmoidal_w", i,  date, ".rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## extract model info
    tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
    
    ## save posterior distributions
    brho_sig_posts[[paste0("brho_w", i)]] <- tmp
  
}

sig_posteriors <- data.frame()

for(i in rain){
#  for(j in microbe) {
    
  tmp = as_tibble(do.call("cbind", brho_sig_posts[[paste0("brho_w", i)]])) %>%
    select(disp, lambda, alpha_brho, N_opt, c, alpha_slope, alpha_initial) %>%
    mutate(water = i)
  
  tmp = tmp[10001:20000,]
  
  sig_posteriors = rbind(sig_posteriors, tmp)
  
#  }
}

rm(tmp, PrelimFit)

### static ####
rain = c(1, 0.75, 0.6)
date = 20250204
brho_stat_posts = list()

for(i in rain){
    
    ## load models
    load(paste0("data_analysis/models/output/static/", date, "/brho_nb_static_w", i, "_", date, ".rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## extract model info
    tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
    
    ## save posterior distributions
    brho_stat_posts[[paste0("brho_w", i)]] <- tmp
  
}

stat_posteriors <- data.frame()

for(i in rain){

    tmp = as_tibble(do.call("cbind", brho_stat_posts[[paste0("brho_w", i)]])) %>%
      select(disp, lambda, alpha_brho, alpha_acam) %>%
      mutate(water = i)
    
    tmp = tmp[2500:5000,]
    
    stat_posteriors = rbind(stat_posteriors, tmp)

}

rm(tmp, PrelimFit)

## ACAM ####
### static ####
rain = c(1, 0.75, 0.6)
date = 20250205
acam_stat_posts = list()

for(i in rain){
  
  ## load models
  load(paste0("data_analysis/models/output/static/", date, "/acam_nb_static_w", i, "_", date, ".rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## extract model info
  tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
  
  ## save posterior distributions
  acam_stat_posts[[paste0("acam_w", i)]] <- tmp
  
}

acam_stat_posteriors <- data.frame()

for(i in rain){
  
  tmp = as_tibble(do.call("cbind", acam_stat_posts[[paste0("acam_w", i)]])) %>%
    select(disp, lambda, alpha_brho, alpha_acam) %>%
    mutate(water = i)
  
  tmp = tmp[2500:5000,]
  
  acam_stat_posteriors = rbind(acam_stat_posteriors, tmp)
  
}

rm(tmp, PrelimFit)


### sigmoidal ####
rain = c(1, 0.75, 0.6)
date = 20250211
acam_sig_posts = list()

for(i in rain){
  
  ## load models
  load(paste0("data_analysis/models/output/sigmoidal/", date, "/acam_nb_sigmoidal_w", i, "_", date, ".rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## extract model info
  tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
  
  ## save posterior distributions
  acam_sig_posts[[paste0("acam_w", i)]] <- tmp
  
}

acam_sig_posteriors <- data.frame()

for(i in rain){
  
  tmp = as_tibble(do.call("cbind", acam_sig_posts[[paste0("acam_w", i)]])) %>%
    select(disp, lambda, alpha_acam, N_opt, c, alpha_slope, alpha_initial) %>%
    mutate(water = i)
  
  tmp = tmp[11001:20000,]
  
  acam_sig_posteriors = rbind(acam_sig_posteriors, tmp)
  
}

rm(tmp, PrelimFit)
