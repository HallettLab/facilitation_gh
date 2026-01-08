## Header ##
## 
## Fit BRHO Models
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

## 2/3/25 realization
    ## BRHO sterilized soil models give really odd alpha_brho outputs 
    ## because they do NOT have enough data
    ## should NOT model these data any longer; just fit live soil models.

## Running 4/1/2025
  ## aim to obtain final coefficients; do NOT edit after this point

## Jan 7 2026 model run
  ## these runs take into account the final data
  ## all contaminated samples have been removed at this point
  ## these should be used in publication, unless further developments become necessary

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

## set seed
set.seed(25)

# Static - LIVE ONLY ####
## make treatment vectors
rainfall = c(1, 0.75, 0.6)
date = 20260107
stlive.out = list() ## make a list for model output

for(i in rainfall){
  
    ## select data 
    dat = brho.model[brho.model$water == i,] %>%
      filter(microbe == 1, ## REMOVE m0
             !is.na(num.focal.indiv), ## get rid of any NA's
             !is.na(seeds.out),
             !is.na(num.bg.indiv))
  
    ## print model to keep track of progress during loop
    print(paste0("brho_w", i))
    
    ## create vectors of data inputs
    Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER-CAP
    N = as.integer(length(Fecundity)) ## number of observations
    N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
    acam = as.integer(dat$num.bg.indiv) ## background stem # data
    
    ## make a vector of data inputs to model
    data_vec = c("N", "Fecundity", "N_i", "acam")
    
    ## set initial values 
    initials1 = list(lambda=200, alpha_acam = 0.03, alpha_brho = -0.08)
    initials2 = list(lambda=150, alpha_acam = 0.05, alpha_brho = -0.06)
    initials3 = list(lambda=250, alpha_acam = -0.01, alpha_brho = -0.04)
    initials4 = list(lambda=300, alpha_acam = -0.04, alpha_brho = -0.01)
    
    initialsall = list(initials1, initials2, initials3, initials4)
    
    ## run the model
    stlive.out[[paste0("brho_w", i)]] = stan(file = '2_calculate_interactions/population_models/1_fit_models/model_stan_scripts/BRHO_ricker_nb_static.stan', 
                                             data = data_vec, init = initialsall, 
                                             iter = 8000, chains = 4, thin = 2, 
                                             control = list(adapt_delta = 0.9, 
                                                            max_treedepth = 18))
    
    PrelimFit = stlive.out[[paste0("brho_w", i)]]
    
    ## save model output
    save(PrelimFit, file = paste0("../outputs/posteriors/", date, 
                                  "/brho_static_m1_w", i, "_", date, ".rdata"))
   
}

# Sigmoidal - LIVE ONLY ####

## make a list for model output
rainfall = c(1, 0.75, 0.6)
siglive.out = list()
date = 20260107

for(i in rainfall){
  
    ## select data 
    dat = brho.model[brho.model$water == i,] %>%
      filter(microbe == 1, ## REMOVE m0
             !is.na(num.focal.indiv), ## get rid of any NA's
             !is.na(seeds.out),
             !is.na(num.bg.indiv))
  
    ## print model to keep track of progress during loop
    print(paste0("brho_w", i))
    
    ## create vectors of data inputs
    Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER CAP
    N = as.integer(length(Fecundity)) ## number of observations
    N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
    acam = as.integer(dat$num.bg.indiv) ## background stem # data

    ## make a vector of data inputs to model
    data_vec = c("N", "Fecundity", "N_i", "acam")
  
    ## set initial values 
    initials1 = list(lambda=200, N_opt = 1, c = -0.05, alpha_slope = -0.1, 
                      alpha_initial = 0.03, alpha_brho = -0.08)
    initials2 = list(lambda=150, N_opt = 0.5, c = -0.06, alpha_slope = -0.15, 
                      alpha_initial = 0.05, alpha_brho = -0.06)
    initials3 = list(lambda=250, N_opt = 2, c = -0.02, alpha_slope = -0.25, 
                      alpha_initial = -0.01, alpha_brho = -0.04)
    initials4 = list(lambda=300, N_opt = 1.5, c = -0.03, alpha_slope = -0.2, 
                      alpha_initial = -0.04, alpha_brho = -0.01)
    ## try 0 for N_opt; 
    
    initialsall = list(initials1, initials2, initials3, initials4)
  
    ## run the model
    siglive.out[[paste0("brho_w", i)]] = stan(file = '2_calculate_interactions/population_models/1_fit_models/model_stan_scripts/BRHO_ricker_nb_sigmoidal.stan', 
                                              data = data_vec, init = initialsall, 
                                              iter = 8000, chains = 4, thin = 2, 
                                              control = list(adapt_delta = 0.99, 
                                                             max_treedepth = 18))
    
    ## adjusted iter from 5000 -> 8000 and adapt_delta from 0.9 -> 0.95 on 1/13
    ## running just for m1_w0.6
    
    ## changed from 8000 -> 10000 iterations, just to give model more time to better estimate alpha_slope and c, which it is having trouble with
    
    ## adjusted iter from 10000 -> 15000 on 1/22/25 to see if this provides a better estimate of alpha_slope, which the model was initially giving fairly wide estimates to.
    
    ## the divergence issues went away; still not estimating things quite well for c and alpha_slope; try removing bound of 0 on both parameters
    
    ## max Rhat = 1.06 (not the worst); need to run for more iterations - bump from 5000 to 10000? 
    ## Neff is low in some cases; need to run for more iterations - bump from 5000 to 10000?
    ## 149 divergent transitions- this part is concerning
    ## some chains had errors, consider specifying chains = 1 to debug ; should definitely try this
    ## look at pairs plot to see sampling problems
    
    
    PrelimFit = siglive.out[[paste0("brho_w", i)]]
    
    ## save model output
    save(PrelimFit, file = paste0("../outputs/posteriors/", date, 
                                  "/brho_sigmoidal_m1_w", i, "_", date, ".rdata"))
    
}


# Static - BOTH ####
## make a list for model output
rainfall = c(1, 0.75, 0.6)
stboth.output = list()
date = 20260107

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
  acam = as.integer(dat$num.bg.indiv) ## background stem # data
  trt = as.integer(dat$soil) ## microbial treatment (binary)
  ## live soil = 0 = the default case. No deviation param calc'ed for this trt
  ## sterilized soil = 1; deviation param will be calc'ed for this trt
  
  ## make a vector of data inputs to model
  data_vec = c("N", "Fecundity", "N_i", "acam", "trt")
  
  ## set initial values 
  initials1 = list(lambda=200, lambda_dev = 10, alpha_acam = 0.05,  
                    alpha_acam_dev = -0.005, alpha_brho = -0.06, 
                    alpha_brho_dev = -0.006) 
  
  initials2 = list(lambda=150, lambda_dev = 5, alpha_acam = 0.01,  
                   alpha_acam_dev = 0.002, alpha_brho = -0.09, 
                   alpha_brho_dev = 0.001)
  
  initials3 = list(lambda=250, lambda_dev = -5, alpha_acam = -0.01,  
                    alpha_acam_dev = 0.001, alpha_brho = -0.01,
                    alpha_brho_dev = 0.003)
  
  initials4 = list(lambda=300, lambda_dev = -8, alpha_acam = -0.02,  
                    alpha_acam_dev = -0.002, alpha_brho = -0.03, 
                    alpha_brho_dev = -0.001)
  
  initialsall = list(initials1, initials2, initials3, initials4)
  
  ## run the model
  stboth.output[[paste0("brho_w", i)]] = stan(file = '2_calculate_interactions/population_models/1_fit_models/model_stan_scripts/BRHO_ricker_nb_static_soil_comp.stan', 
                                              data = data_vec, init = initialsall, 
                                              iter = 8000, chains = 4, thin = 2, 
                                              control = list(adapt_delta = 0.99, 
                                                             max_treedepth = 15))
  
  PrelimFit = stboth.output[[paste0("brho_w", i)]]
  
  ## save model output
  save(PrelimFit, file = paste0("../outputs/posteriors/", date, 
                                "/brho_stat_mboth_w", i, "_", date, ".rdata"))
  
}





