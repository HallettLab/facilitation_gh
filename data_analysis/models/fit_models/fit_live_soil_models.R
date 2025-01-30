## Header ##
## 
## Fit Live Soil Models
##
## Purpose: run Stan code to fit bayesian models for all 3 water conditions and both soil conditions with both static alphas and sigmoidal alphas
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
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## set date
date <- 20250124

## read in data 
source("data_cleaning/clean_model_dat.R")

## set seed
set.seed(25)

## make treatment vectors
rainfall = c(1, 0.75, 0.6)
microbe = c(0,1)

#rainfall = 0.6
#microbe = 1

## make a list for model output
model.output <- list()

# Static Fit ####
for(i in rainfall){
  for(j in microbe){
  
    ## select data 
    dat = brho.model[brho.model$water == i & brho.model$microbe == j,] %>%
      filter(!is.na(num.focal.indiv))
    ## currently ID 79 doesn't have a focal # entered
  
    ## print model to keep track of progress during loop
    print(paste0("m", j, "_w", i))
    
    ## create vectors of data inputs
    Fecundity = as.integer(round(dat$seeds.out)) ## seeds out
    N = as.integer(length(Fecundity)) ## number of observations
    N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
    acam <- as.integer(dat$num.bg.indiv) ## background stem # data
    
    ## make a vector of data inputs to model
    data_vec <- c("N", "Fecundity", "N_i", "acam")
    
    ## set initial values 
    initials1 <- list(lambda=200, alpha_brho = 0.08, alpha_acam = -0.1)
    initials2 <- list(lambda=600, alpha_brho = 0.15, alpha_acam = -0.01)
    initials3 <- list(lambda=800, alpha_brho = 0.25, alpha_acam = 0.1)
    initials4 <- list(lambda=100, alpha_brho = -0.01, alpha_acam = -0.2)
    
    initialsall<- list(initials1, initials2, initials3, initials4)
    
    ## run the model
    model.output[[paste0("brho_m", j, "_w", i)]] = stan(file = 'data_analysis/models/fit_models/ricker_neg_binom_static_alpha.stan', data = data_vec, init = initialsall, iter = 5000, chains = 4, thin = 2, control = list(adapt_delta = 0.9, max_treedepth = 18))
    
    PrelimFit <- model.output[[paste0("brho_m", j, "_w", i)]]
    
    ## save model output
    save(PrelimFit, file = paste0("data_analysis/models/output/static/brho_nb_static_m", j, "_w", i, "_", date, ".rdata"))
    
  }
  
}

# Sigmoidal ####

## make a list for model output
sigmoidal.output <- list()
#rainfall = c(0.6)
#microbe = c(1)

for(i in rainfall){
  for(j in microbe){
  
    ## select data 
    dat = brho.model[brho.model$water == i & brho.model$microbe == j,] %>%
      filter(!is.na(num.focal.indiv))
    ## currently ID 79 doesn't have a focal # entered
  
    ## print model to keep track of progress during loop
    print(paste0("m", j, "_w", i))
    
    ## create vectors of data inputs
    Fecundity = as.integer(round(dat$seeds.out)) ## seeds out
    N = as.integer(length(Fecundity)) ## number of observations
    N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
    acam <- as.integer(dat$num.bg.indiv) ## background stem # data

    ## make a vector of data inputs to model
    data_vec <- c("N", "Fecundity", "N_i", "acam")
  
    ## set initial values 
    initials1 <- list(lambda=200, N_opt = 1, c = -0.5, alpha_slope = -0.8, alpha_initial = 0.1, alpha_brho = 0.08)
    initials2 <- list(lambda=600, N_opt = 2, c = -0.6, alpha_slope = -0.7, alpha_initial = 0.2, alpha_brho = 0.06)
    initials3 <- list(lambda=800, N_opt = 5, c = -0.2, alpha_slope = -0.4, alpha_initial = -0.1, alpha_brho = 0.04)
    initials4 <- list(lambda=400, N_opt = 4, c = -0.3, alpha_slope = -0.2, alpha_initial = 0.3, alpha_brho = 0.01)
    
    ## issues with chain 4 initial values; changed lambda from 100->400 and alpha_initial from 0.5 to 0.3
    ## chain 4 initial values having issues again... 
    ## changed c from 0 -> -0.3; see if this helps? 
  
    initialsall<- list(initials1, initials2, initials3, initials4)
  
    ## run the model
    sigmoidal.output[[paste0("brho_m", j, "_w", i)]] = stan(file = 'data_analysis/models/fit_models/ricker_nb_sigmoidal.stan', data = data_vec, init = initialsall, iter = 20000, warmup = 10000, chains = 4, thin = 2, control = list(adapt_delta = 0.95, max_treedepth = 18))
    
    ## adjusted iter from 5000 -> 8000 and adapt_delta from 0.9 -> 0.95 on 1/13
    ## running just for m1_w0.6
    
    ## changed from 8000 -> 10000 iterations, just to give model more time to better estimate alpha_slope and c, which it is having trouble with
    
    ## adjusted iter from 10000 -> 15000 on 1/22/25 to see if this provides a better estimate of alpha_slope, which the model was initially giving fairly wide estimates to.
    
    ## the divergence issues went away; still not estimating things quite well for c an dalpha_slope; try removing bound of 0 on both parameters
    
    
    
    ## max Rhat = 1.06 (not the worst); need to run for more iterations - bump from 5000 to 10000? 
    ## Neff is low in some cases; need to run for more iterations - bump from 5000 to 10000?
    ## 149 divergent transitions- this part is concerning
    ## some chains had errors, consider specifying chains = 1 to debug ; should definitely try this
    ## look at pairs plot to see sampling problems
    
    
    PrelimFit <- sigmoidal.output[[paste0("brho_m", j, "_w", i)]]
    
    ## save model output
    save(PrelimFit, file = paste0("data_analysis/models/output/sigmoidal/brho_nb_sigmoidal_m", j, "_w", i, "_", date, ".rdata"))
    
  }
}

pairs(PrelimFit, pars = c("lambda", "disp", "alpha_brho", "N_opt", "c", "alpha_slope", "alpha_initial"))

traceplot(PrelimFit, pars = c("lambda", "disp", "alpha_brho", "N_opt", "c", "alpha_slope", "alpha_initial"), inc_warmup = T)

## C parameter really seems to be the one screwing with this in both pairs plot and traceplots
