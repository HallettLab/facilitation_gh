## Header ##
## 
## Fit BRHO Models
##
## Purpose: run Stan code to fit bayesian models for all 3 water conditions with both static alphas and sigmoidal alphas
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

# Set up ####
## load packages
library(tidyverse)
library(bayesplot)
library(rstan)
library(here)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## read in data 
source("data_cleaning/clean_model_dat.R")

## set seed
set.seed(25)

# Static Fit ####
## make treatment vectors
rainfall = c(1, 0.75, 0.6)
date = 20250401
static.output <- list() ## make a list for model output

for(i in rainfall){
  
    ## select data 
    dat = brho.model[brho.model$water == i,] %>%
      filter(!is.na(num.focal.indiv))
    ## get rid of any NA's
  
    ## print model to keep track of progress during loop
    print(paste0("brho_w", i))
    
    ## create vectors of data inputs
    Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER-CAP
    N = as.integer(length(Fecundity)) ## number of observations
    N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
    acam <- as.integer(dat$num.bg.indiv) ## background stem # data
    
    ## make a vector of data inputs to model
    data_vec <- c("N", "Fecundity", "N_i", "acam")
    
    ## set initial values 
    initials1 <- list(lambda=200, alpha_acam = 0.03, alpha_brho = -0.08)
    initials2 <- list(lambda=150, alpha_acam = 0.05, alpha_brho = -0.06)
    initials3 <- list(lambda=250, alpha_acam = -0.01, alpha_brho = -0.04)
    initials4 <- list(lambda=300, alpha_acam = -0.04, alpha_brho = -0.01)
    
    initialsall<- list(initials1, initials2, initials3, initials4)
    
    ## run the model
    static.output[[paste0("brho_w", i)]] = stan(file = 'data_analysis/models/fit_models/ricker_nb_static.stan', data = data_vec, init = initialsall, iter = 5000, chains = 4, thin = 2, control = list(adapt_delta = 0.9, max_treedepth = 18))
    
    PrelimFit <- static.output[[paste0("brho_w", i)]]
    
    ## save model output
    save(PrelimFit, file = paste0("data_analysis/models/output/static/", date, "/brho_nb_static_w", i, "_", date, "_final.rdata"))
   
}

# Sigmoidal ####

## make a list for model output
rainfall = c(1, 0.75, 0.6)
sigmoidal.output <- list()
date = 20250401

for(i in rainfall){
  
    ## select data 
    dat = brho.model[brho.model$water == i,] %>%
      filter(!is.na(num.focal.indiv))
  
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## create vectors of data inputs
    Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER CAP
    N = as.integer(length(Fecundity)) ## number of observations
    N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
    acam <- as.integer(dat$num.bg.indiv) ## background stem # data

    ## make a vector of data inputs to model
    data_vec <- c("N", "Fecundity", "N_i", "acam")
  
    ## set initial values 
    initials1 <- list(lambda=200, N_opt = 1, c = -0.05, alpha_slope = -0.1, alpha_initial = 0.03, alpha_brho = -0.08)
    initials2 <- list(lambda=150, N_opt = 0.5, c = -0.06, alpha_slope = -0.15, alpha_initial = 0.05, alpha_brho = -0.06)
    initials3 <- list(lambda=250, N_opt = 2, c = -0.02, alpha_slope = -0.25, alpha_initial = -0.01, alpha_brho = -0.04)
    initials4 <- list(lambda=300, N_opt = 1.5, c = -0.03, alpha_slope = -0.2, alpha_initial = -0.04, alpha_brho = -0.01)

    ## try 0 for N_opt; 
    ## cld try means of priors for init values
    
    ## issues with chain 4 initial values; changed lambda from 100->400 and alpha_initial from 0.5 to 0.3
    ## chain 4 initial values having issues again... 
    ## changed c from 0 -> -0.3; see if this helps? 
  
    initialsall<- list(initials1, initials2, initials3, initials4)
  
    ## run the model
    sigmoidal.output[[paste0("brho_w", i)]] = stan(file = 'data_analysis/models/fit_models/ricker_nb_sigmoidal.stan', data = data_vec, init = initialsall, iter = 5000, chains = 4, thin = 2, control = list(adapt_delta = 0.99, max_treedepth = 18))
    
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
    
    
    PrelimFit <- sigmoidal.output[[paste0("brho_w", i)]]
    
    ## save model output
    save(PrelimFit, file = paste0("data_analysis/models/output/sigmoidal/", date, "/brho_nb_sigmoidal_w", i, "_", date, "_final.rdata"))
    
}
