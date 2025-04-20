## Header ##
## 
## Fit ACAM Models
##
## Purpose: run Stan code to fit bayesian models for all 3 water conditions with non-linear INTRAspecific functions
## 
## Author: Carmen Watkins


## ACAM intraspecific models in live vs. sterilized soil

## could do live vs. sterilized as an offset parameter so that it's only changing it when it's different?

## could estimate just nonlinear intraspecific interactions; leave INTER alphas constant? 

# Set up ####
## load packages
library(tidyverse)
library(bayesplot)
library(rstan)
library(here)
library(beepr)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## read in data 
source("data_cleaning/clean_model_dat_for_nL_INTRA.R")

## set seed ####
set.seed(25)

# ACAM ####
## make a list for model output
rainfall = c(1, 0.75, 0.6)
# rainfall = c(0.6, 0.75)
stat.output <- list()
date = 20250419

## need to fix the 0.6 data, there is an NA;
##oh shit, not filtering out the m0's; no wonder the lambda was so low

for(i in rainfall){
  
  ## select data 
  dat = acam.model[acam.model$water == i,] %>%
    filter(!is.na(num.focal.indiv),
           !is.na(seeds.out))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## create vectors of data inputs
  Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER CAP
  N = as.integer(length(Fecundity)) ## number of observations
  N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
  brho <- as.integer(dat$num.bg.indiv) ## background stem # data
  
   trt <- as.integer(dat$soil) ## microbial treatment (binary)
  ## live soil = 0 = the default case. No deviation param calc'ed for this trt
  ## sterilized soil = 1; deviation param will be calc'ed for this trt
  
  ## make a vector of data inputs to model
  data_vec <- c("N", "Fecundity", "N_i", "brho", "trt") # , "trt"
  
  ## set initial values 
  initials1 <- list(lambda=50, alpha_acam = 0.05,  alpha_acam_dev = -0.005, 
                    alpha_brho = -0.06, alpha_brho_dev = -0.006) #N_opt = 1.5,  c = -0.001, #N_opt_dev = 0.1, c_dev = 0.001, 
                     # alpha_initial = -0.01, # alpha_slope_dev = -0.006,
                    #alpha_initial_dev = -0.002, 
  
  initials2 <- list(lambda=60, alpha_acam = 0.01,  alpha_acam_dev = 0.002, 
                    alpha_brho = -0.09, alpha_brho_dev = 0.001)
                    
                    #N_opt = 2, c = -0.05, # N_opt_dev = -0.1, c_dev = 0.006,
                    #alpha_slope = -0.09, alpha_initial = 0.01, # alpha_slope_dev = 0.01, 
                    #alpha_brho = -0.03) # alpha_initial_dev = -0.02, 
  
  initials3 <- list(lambda=65, alpha_acam = -0.01,  alpha_acam_dev = 0.001, 
                    alpha_brho = -0.01, alpha_brho_dev = 0.003)
  
                    #N_opt = 1, c = -0.1, # c_dev = -0.001, N_opt_dev = -0.2, 
                    #alpha_slope = -0.2, alpha_initial = 0.05, # alpha_slope_dev = 0.1, 
                    #alpha_brho = -0.09) # alpha_initial_dev = -0.01, 
  
  initials4 <- list(lambda=45, alpha_acam = 0.02,  alpha_acam_dev = -0.002, 
                    alpha_brho = -0.03, alpha_brho_dev = -0.001)
                    
                    #N_opt = 0.5, c = -0.3, # c_dev = 0.02, N_opt_dev = -0.01, 
                    #alpha_slope = -0.1, alpha_initial = -0.03,  # alpha_slope_dev = 0.005, 
                    #alpha_brho = -0.02) # alpha_initial_dev = 0.001, 
  
  ## put N_opt b/w 0-2 for init values
  ## maybe start with mean of prior distributions as init values
  
  ## chain 3 & 4 worked; others didn't
  
  initialsall<- list(initials1, initials2, initials3, initials4)
  
  ## run the model
  stat.output[[paste0("acam_w", i)]] = stan(file = 'data_analysis/models/fit_models/ACAM_ricker_nb_sigmoidal_nL_INTRA.stan', data = data_vec, init = initialsall, iter = 8000, chains = 4, thin = 2, control = list(adapt_delta = 0.99, max_treedepth = 15))
  
  PrelimFit <- stat.output[[paste0("acam_w", i)]]
  
  ## save model output
  save(PrelimFit, file = paste0("data_analysis/models/output/m0_models/", date, "/acam_nb_stat_w", i, "_", date, "_soil_comp.rdata"))
  
}

hist(rexp(1000, 2))
hist(rnorm(1000, 0, 0.25))
## water 0.6 = no samples
## water 1 = low ESS on c_dev and alpha_slope_dev ; I wonder if varying  these two in tandem is tricky? If one was constrained and the other allowed to vary??
## Rhats were all good


## more iterations would help; are there other tweaks that would help? More specific priors? Some of the ranges were quite large? 
## the initial values should be fairly well targeted?
    ## leaving these alone for now

## maybe the treedepth could go down a notch? 
    ## tried this

## could also consider that the priors on the ACAM model were set up to predict a competitive shape decreasing; perhaps shifting priors to find a shape more similar to initial pos interactions + decreasing to competitive would be better?
    ## changed priors to match BRHO sigmoidal models, just to see what happens
    ## also adjusted variances from 0.5 to 0.25 on a lot of the dev params as 0.5 is high variance for deviation off a param bounded between 0-1


# BRHO ####
## make a list for model output
rainfall = c(1, 0.75, 0.6)
# rainfall = c(0.6, 0.75)
brho.stat.output <- list()
date = 20250419

## need to fix the 0.6 data, there is an NA;
##oh shit, not filtering out the m0's; no wonder the lambda was so low

for(i in rainfall){
  
  ## select data 
  dat = brho.model[brho.model$water == i,] %>%
    filter(!is.na(num.focal.indiv),
           !is.na(seeds.out))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## create vectors of data inputs
  Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER CAP
  N = as.integer(length(Fecundity)) ## number of observations
  N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
  acam <- as.integer(dat$num.bg.indiv) ## background stem # data
  
  trt <- as.integer(dat$soil) ## microbial treatment (binary)
  ## live soil = 0 = the default case. No deviation param calc'ed for this trt
  ## sterilized soil = 1; deviation param will be calc'ed for this trt
  
  ## make a vector of data inputs to model
  data_vec <- c("N", "Fecundity", "N_i", "acam", "trt") # , "trt"
  
  ## set initial values 
  initials1 <- list(lambda=50, alpha_acam = 0.05,  alpha_acam_dev = -0.005, 
                    alpha_brho = -0.06, alpha_brho_dev = -0.006) 
  
  initials2 <- list(lambda=60, alpha_acam = 0.01,  alpha_acam_dev = 0.002, 
                    alpha_brho = -0.09, alpha_brho_dev = 0.001)
 
  initials3 <- list(lambda=65, alpha_acam = -0.01,  alpha_acam_dev = 0.001, 
                    alpha_brho = -0.01, alpha_brho_dev = 0.003)
 
  initials4 <- list(lambda=45, alpha_acam = 0.02,  alpha_acam_dev = -0.002, 
                    alpha_brho = -0.03, alpha_brho_dev = -0.001)
  
  initialsall<- list(initials1, initials2, initials3, initials4)
  
  ## run the model
  brho.stat.output[[paste0("brho_w", i)]] = stan(file = 'data_analysis/models/fit_models/BRHO_ricker_nb_static_soil_comp.stan', data = data_vec, init = initialsall, iter = 8000, chains = 4, thin = 2, control = list(adapt_delta = 0.99, max_treedepth = 15))
  
  PrelimFit <- brho.stat.output[[paste0("brho_w", i)]]
  
  ## save model output
  save(PrelimFit, file = paste0("data_analysis/models/output/m0_models/", date, "/brho_nb_stat_w", i, "_", date, "_soil_comp.rdata"))
  
}





























