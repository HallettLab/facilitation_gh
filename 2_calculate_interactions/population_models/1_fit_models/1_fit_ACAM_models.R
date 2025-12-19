## Header ##
## 
## Fit ACAM Models
##
## Purpose: run Stan code to fit bayesian models for: 
## 1) static models LIVE ONLY
## 2) sigmoidal models LIVE ONLY
## 3) static models, BOTH live and sterilized, with deviation params
## 
## Author: Carmen Watkins

# Notes ####
## Feedback from Lisa
    ## increase the number of iterations to get better alpha_slope estimate
    ## do the two peaks in the data mean anything?

# Set up ####
## load packages
library(tidyverse)
library(bayesplot)
library(rstan)
library(here)

## set rstan settings
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## read in data 
source("1_data_cleaning/clean_model_dat.R")
## remove outputs not needed for modeling 
rm(ainter, aintra, binter, binter_for_AII, bintra)

## set seed ####
set.seed(25)

# Static - LIVE ONLY ####
## make treatment vector
rainfall = c(1, 0.75, 0.6)
date = 20251219 ## set date
stlive.out = list() ## make a list for model output

for(i in rainfall){

    ## select data 
    dat = acam.model[acam.model$water == i,] %>%
      filter(microbe == 1, ## REMOVE m0
             !is.na(num.focal.indiv), ## get rid of any NA's
             !is.na(seeds.out),
             !is.na(num.bg.indiv))
    
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## create vectors of data inputs
    Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER-CAP
    N = as.integer(length(Fecundity)) ## number of observations
    N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
    brho = as.integer(dat$num.bg.indiv) ## background stem # data
    
    ## make a vector of data inputs to model
    data_vec = c("N", "Fecundity", "N_i", "brho")
    
    ## set initial values 
    initials1 = list(lambda=50, alpha_brho = -0.06, alpha_acam = -0.06)
    initials2 = list(lambda=60, alpha_brho = -0.01, alpha_acam = -0.03)
    initials3 = list(lambda=65, alpha_brho = -0.05, alpha_acam = -0.09)
    initials4 = list(lambda=45, alpha_brho = 0.03, alpha_acam = -0.02)
    
    initialsall = list(initials1, initials2, initials3, initials4)
    
    ## run the model
    stlive.out[[paste0("acam_w", i)]] = stan(file = '2_calculate_interactions/population_models/1_fit_models/model_stan_scripts/ACAM_ricker_nb_static.stan', 
                                               data = data_vec, init = initialsall, 
                                               iter = 8000, chains = 4, thin = 2, 
                                               control = list(adapt_delta = 0.9,
                                                              max_treedepth = 18))
    
    PrelimFit = stlive.out[[paste0("acam_w", i)]]
    
    ## save model output
    save(PrelimFit, file = paste0("../outputs/posteriors/static/", date, 
                                  "/acam_static_m1_w", i, "_", date, ".rdata"))

}


# Sigmoidal - LIVE ONLY ####

## make a list for model output
rainfall = c(1, 0.75, 0.6)
siglive.out = list()
date = 20251219

for(i in rainfall){
  
    ## select data 
    dat = acam.model[acam.model$water == i,] %>%
      filter(microbe == 1, ## REMOVE m0
             !is.na(num.focal.indiv), ## get rid of any NA's
             !is.na(seeds.out),
             !is.na(num.bg.indiv))
    
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## create vectors of data inputs
    Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER CAP
    N = as.integer(length(Fecundity)) ## number of observations
    N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
    brho = as.integer(dat$num.bg.indiv) ## background stem # data

    ## make a vector of data inputs to model
    data_vec = c("N", "Fecundity", "N_i", "brho")
  
    ## set initial values 
    initials1 = list(lambda=50, N_opt = 1.5, c = -0.001, alpha_slope = -0.05, 
                     alpha_initial = -0.06, alpha_acam = -0.06)
    initials2 = list(lambda=60, N_opt = 2, c = -0.05, alpha_slope = -0.09, 
                      alpha_initial = -0.01, alpha_acam = -0.03)
    initials3 = list(lambda=65, N_opt = 1, c = -0.1, alpha_slope = -0.2, 
                      alpha_initial = -0.05, alpha_acam = -0.09)
    initials4 = list(lambda=45, N_opt = 0.5, c = -0.3, alpha_slope = -0.01, 
                      alpha_initial = 0.03, alpha_acam = -0.02)
    ## put N_opt b/w 0-2 for init values

    initialsall = list(initials1, initials2, initials3, initials4)
  
    ## run the model
    siglive.out[[paste0("acam_w", i)]] = stan(file = '2_calculate_interactions/population_models/1_fit_models/model_stan_scripts/ACAM_ricker_nb_sigmoidal.stan', 
                                                   data = data_vec, init = initialsall, 
                                                   iter = 8000, chains = 4, thin = 2, 
                                                   control = list(adapt_delta = 0.999, 
                                                                  max_treedepth = 18))
    
    PrelimFit = siglive.out[[paste0("acam_w", i)]]

    ## save model output
    save(PrelimFit, file = paste0("../outputs/posteriors/sigmoidal/", date, 
                                  "/acam_sigmoidal_m1_w", i, "_", date, ".rdata"))
    
}


# Static - BOTH ####
## make a list for model output
rainfall = c(1, 0.75, 0.6)
stboth.output = list()
date = 20251219

for(i in rainfall){
  
  ## select data 
  dat = acam.model[acam.model$water == i,] %>%
    filter(!is.na(num.focal.indiv), ## get rid of any NA's
           !is.na(seeds.out),
           !is.na(num.bg.indiv))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## create vectors of data inputs
  Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER CAP
  N = as.integer(length(Fecundity)) ## number of observations
  N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
  brho = as.integer(dat$num.bg.indiv) ## background stem # data
  trt = as.integer(dat$soil) ## microbial treatment (binary)
  ## live soil = 0 = the default case. No deviation param calc'ed for this trt
  ## sterilized soil = 1; deviation param will be calc'ed for this trt
  
  ## make a vector of data inputs to model
  data_vec = c("N", "Fecundity", "N_i", "brho", "trt")
  
  ## set initial values 
  initials1 = list(lambda=50, lambda_dev = -6, alpha_acam = 0.05, 
                    alpha_acam_dev = -0.005, alpha_brho = -0.06, 
                    alpha_brho_dev = -0.006) 
  
  initials2 = list(lambda=60, lambda_dev = -8, alpha_acam = 0.01,  
                    alpha_acam_dev = 0.002, alpha_brho = -0.09, 
                    alpha_brho_dev = 0.001)
  
  initials3 = list(lambda=65, lambda_dev = -12, alpha_acam = -0.01,  
                    alpha_acam_dev = 0.001,  alpha_brho = -0.01, 
                    alpha_brho_dev = 0.003)
  
  initials4 = list(lambda=45, lambda_dev = -5, alpha_acam = 0.02,  
                    alpha_acam_dev = -0.002, alpha_brho = -0.03, 
                    alpha_brho_dev = -0.001)
  
  initialsall = list(initials1, initials2, initials3, initials4)
  
  ## run the model
  stboth.output[[paste0("acam_w", i)]] = stan(file = '2_calculate_interactions/population_models/1_fit_models/model_stan_scripts/ACAM_ricker_nb_static_soil_comp.stan', 
                                            data = data_vec, init = initialsall, 
                                            iter = 8000, chains = 4, thin = 2, 
                                            control = list(adapt_delta = 0.99,
                                                           max_treedepth = 15))
  
  PrelimFit = stboth.output[[paste0("acam_w", i)]]

  ## save model output
  save(PrelimFit, file = paste0("../outputs/posteriors/static/", date, 
                                "/acam_stat_mboth_w", i, "_", date, ".rdata"))
  
  
}
