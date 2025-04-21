
# Set up ####
library(ggpubr)
library(bayesplot)
library(rstan) ## need this for traceplot
library(tidyverse)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


# ACAM ####
rain = c(1, 0.75, 0.6)
date = 20250420
acam_stat_posts = list()

for(i in rain){
  
  ## load models
  load(paste0("data_analysis/models/output/m0_models/", date, "/acam_nb_stat_w", i, "_", date, "_soil_comp_adjust_priors.rdata"))
  
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
    select(disp, lambda, lambda_dev, alpha_brho, alpha_brho_dev, alpha_acam, alpha_acam_dev) %>%
    mutate(water = i)
  
  tmp = tmp[3001:6000,]
  
  acam_stat_posteriors = rbind(acam_stat_posteriors, tmp)
  
}

rm(tmp, PrelimFit)

# BRHO ####
rain = c(1, 0.75, 0.6)
date = 20250420
brho_stat_posts = list()

for(i in rain){
  
  load(paste0("data_analysis/models/output/m0_models/", date, "/brho_nb_stat_w", i, "_", date, "_soil_comp_adjust_priors.rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## extract model info
  tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
  
  ## save posterior distributions
  brho_stat_posts[[paste0("brho_w", i)]] <- tmp
  
}

brho_stat_posteriors <- data.frame()

for(i in rain){
  
  tmp = as_tibble(do.call("cbind", brho_stat_posts[[paste0("brho_w", i)]])) %>%
    select(disp, lambda, lambda_dev, alpha_brho, alpha_brho_dev, alpha_acam, alpha_acam_dev) %>%
    mutate(water = i)
  
  tmp = tmp[3001:6000,]
  
  brho_stat_posteriors = rbind(brho_stat_posteriors, tmp)
  
}

rm(tmp, PrelimFit)
