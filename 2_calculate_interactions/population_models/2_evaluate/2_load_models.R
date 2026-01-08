## Header ##
## 
## Script Name: Load Models
##
## Purpose: load and save .csv files of model posteriors for easier use in future
## this script can be run once and then not used again unless models are updated.
## 
## Author: Carmen Watkins

# Set Up ####
## load packaages
library(ggpubr)
library(bayesplot)
library(rstan) ## need this for traceplot
library(tidyverse)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load models ####
## BRHO ####
### Static - LIVE ONLY ####
rain = c(1, 0.75, 0.6)
date = 20260107
brho_stat_posts_LO = list()

for(i in rain){
  
  ## load models
  load(paste0("../outputs/posteriors/", date, 
              "/brho_static_m1_w", i, "_", date, ".rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## extract model info
  tmp = rstan::extract(PrelimFit, inc_warmup = FALSE)
  
  ## save posterior distributions
  brho_stat_posts_LO[[paste0("brho_w", i)]] = tmp
  
}

brho_stat_posteriors_LO = data.frame()

for(i in rain){
  
  tmp = as_tibble(do.call("cbind", brho_stat_posts_LO[[paste0("brho_w", i)]])) %>%
    select(disp, lambda, alpha_brho, alpha_acam) %>%
    mutate(water = i)
  
  tmp = tmp[4001:8000,]
  
  brho_stat_posteriors_LO = rbind(brho_stat_posteriors_LO, tmp)
  
}

rm(tmp, PrelimFit)

## save .csv file version of posteriors for easier use later on.
write.csv(brho_stat_posteriors_LO, 
          "../outputs/posteriors/brho_stat_posts_LO_20260107.csv")


### Sigmoidal - LIVE ONLY ####
rain = c(1, 0.75, 0.6)
date = 20260107
brho_sig_posts = list()

for(i in rain){
    
  ## load models
  load(paste0("../outputs/posteriors/", date, 
              "/brho_sigmoidal_m1_w", i, "_", date, ".rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))

  ## extract model info
  tmp = rstan::extract(PrelimFit, inc_warmup = FALSE)
    
  ## save posterior distributions
  brho_sig_posts[[paste0("brho_w", i)]] = tmp
  
}

brho_sig_posteriors = data.frame()

for(i in rain){

  tmp = as_tibble(do.call("cbind", brho_sig_posts[[paste0("brho_w", i)]])) %>%
    select(disp, lambda, alpha_brho, N_opt, c, alpha_slope, alpha_initial) %>%
    mutate(water = i)
  
  ## discard warmup runs
  tmp = tmp[4001:8000,]
  
  brho_sig_posteriors = rbind(brho_sig_posteriors, tmp)
  
}

rm(tmp, PrelimFit)

## save .csv file version of posteriors for easier use later on.
write.csv(brho_sig_posteriors, 
          "../outputs/posteriors/brho_sig_posts_LO_20260107.csv")

### Static - BOTH ####
rain = c(1, 0.75, 0.6)
date = 20260107
brho_stat_posts_B = list()

for(i in rain){
    
    ## load models
  load(paste0("../outputs/posteriors/", date, 
              "/brho_stat_mboth_w", i, "_", date, ".rdata"))
  
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## extract model info
    tmp = rstan::extract(PrelimFit, inc_warmup = FALSE)
    
    ## save posterior distributions
    brho_stat_posts_B[[paste0("brho_w", i)]] = tmp
  
}

brho_stat_posteriors_B = data.frame()

for(i in rain){

    tmp = as_tibble(do.call("cbind", brho_stat_posts_B[[paste0("brho_w", i)]])) %>%
      select(disp, lambda, lambda_dev, alpha_brho, alpha_brho_dev,
             alpha_acam, alpha_acam_dev) %>%
      mutate(water = i)
    
    tmp = tmp[4001:8000,]
    
    brho_stat_posteriors_B = rbind(brho_stat_posteriors_B, tmp)

}

rm(tmp, PrelimFit)

## calculate m0 posterior distributions from m1 & dev parameters
micB = brho_stat_posteriors_B %>%
  mutate(lambda_m0 = lambda + lambda_dev,
         alpha_brho_m0 = alpha_brho +alpha_brho_dev,
         alpha_acam_m0 = alpha_acam + alpha_acam_dev)

## save .csv file version of posteriors for easier use later on.
write.csv(micB, "../outputs/posteriors/brho_stat_posts_B_20260107.csv")

## ACAM ####
### Static - LIVE ONLY ####
rain = c(1, 0.75, 0.6)
date = 20260107
acam_stat_posts_LO = list()

for(i in rain){
  
  ## load models
  load(paste0("../outputs/posteriors/", date, 
              "/acam_static_m1_w", i, "_", date, ".rdata"))
  
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## extract model info
  tmp = rstan::extract(PrelimFit, inc_warmup = FALSE)
  
  ## save posterior distributions
  acam_stat_posts_LO[[paste0("acam_w", i)]] = tmp
  
}

acam_stat_posteriors_LO = data.frame()

for(i in rain){
  
  tmp = as_tibble(do.call("cbind", acam_stat_posts_LO[[paste0("acam_w", i)]])) %>%
    select(disp, lambda, alpha_brho, alpha_acam) %>%
    mutate(water = i)
  
  tmp = tmp[4001:8000,]
  
  acam_stat_posteriors_LO = rbind(acam_stat_posteriors_LO, tmp)
  
}

rm(tmp, PrelimFit)

## save .csv file version of posteriors for easier use later on.
write.csv(acam_stat_posteriors_LO, 
          "../outputs/posteriors/acam_stat_posts_LO_20260107.csv")

### Sigmoidal - LIVE ONLY ####
rain = c(1, 0.75, 0.6)
date = 20260107
acam_sig_posts = list()

for(i in rain){
  
  ## load models
  load(paste0("../outputs/posteriors/", date, 
              "/acam_sigmoidal_m1_w", i, "_", date, ".rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## extract model info
  tmp = rstan::extract(PrelimFit, inc_warmup = FALSE)
  
  ## save posterior distributions
  acam_sig_posts[[paste0("acam_w", i)]] = tmp
  
}

acam_sig_posteriors = data.frame()

for(i in rain){
  
  tmp = as_tibble(do.call("cbind", acam_sig_posts[[paste0("acam_w", i)]])) %>%
    select(disp, lambda, alpha_acam, N_opt, c, alpha_slope, alpha_initial) %>%
    mutate(water = i)
  
  tmp = tmp[4001:8000,]
  
  acam_sig_posteriors = rbind(acam_sig_posteriors, tmp)
  
}

rm(tmp, PrelimFit)

## save .csv file version of posteriors for easier use later on.
write.csv(acam_stat_posteriors_LO, 
          "../outputs/posteriors/acam_sig_posts_LO_20260107.csv")

### Static - BOTH ####
rain = c(1, 0.75, 0.6)
date = 20260108
acam_stat_posts_B = list()

for(i in rain){
  
  ## load models
  load(paste0("../outputs/posteriors/", date, 
              "/acam_stat_mboth_w", i, "_", date, ".rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## extract model info
  tmp = rstan::extract(PrelimFit, inc_warmup = FALSE)
  
  ## save posterior distributions
  acam_stat_posts_B[[paste0("acam_w", i)]] = tmp
  
}

acam_stat_posteriors_B = data.frame()

for(i in rain){
  
  tmp = as_tibble(do.call("cbind", acam_stat_posts_B[[paste0("acam_w", i)]])) %>%
    select(disp, lambda, lambda_dev, alpha_brho, alpha_brho_dev,
           alpha_acam, alpha_acam_dev) %>%
    mutate(water = i)
  
  tmp = tmp[4001:8000,]
  
  acam_stat_posteriors_B = rbind(acam_stat_posteriors_B, tmp)
  
}

rm(tmp, PrelimFit)

## calculate m0 posterior distributions from m1 & dev parameters
micA = acam_stat_posteriors_B %>%
  mutate(lambda_m0 = lambda + lambda_dev,
         alpha_brho_m0 = alpha_brho +alpha_brho_dev,
         alpha_acam_m0 = alpha_acam + alpha_acam_dev)

## save .csv file version of posteriors for easier use later on.
write.csv(micA, "../outputs/posteriors/acam_stat_posts_B_20260108.csv")


