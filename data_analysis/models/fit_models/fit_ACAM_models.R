## Header ##
## 
## Fit ACAM Models
##
## Purpose: run Stan code to fit bayesian models for all 3 water conditions with both static alphas and sigmoidal alphas
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
library(beepr)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## read in data 
source("data_cleaning/clean_model_dat.R")

## set seed ####
set.seed(25)

# Static Fit ####
## make treatment vector
rainfall = c(1, 0.75, 0.6)
date = 20250401 ## set date
model.output <- list() ## make a list for model output

for(i in rainfall){

    ## select data 
    dat = acam.model[acam.model$water == i,] %>%
      filter(!is.na(num.focal.indiv),
             !is.na(seeds.out),
             !is.na(num.bg.indiv))
    ## get rid of any NA's
  
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## create vectors of data inputs
    Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER-CAP
    N = as.integer(length(Fecundity)) ## number of observations
    N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
    brho <- as.integer(dat$num.bg.indiv) ## background stem # data
    
    ## make a vector of data inputs to model
    data_vec <- c("N", "Fecundity", "N_i", "brho")
    
    ## set initial values 
    ## set initial values 
    initials1 <- list(lambda=50, alpha_brho = -0.06, alpha_acam = -0.06)
    initials2 <- list(lambda=60, alpha_brho = -0.01, alpha_acam = -0.03)
    initials3 <- list(lambda=65, alpha_brho = -0.05, alpha_acam = -0.09)
    initials4 <- list(lambda=45, alpha_brho = 0.03, alpha_acam = -0.02)
    
   # initials1 <- list(lambda=10, alpha_brho = -0.08, alpha_acam = 0.1)
   # initials2 <- list(lambda=150, alpha_brho = -0.15, alpha_acam = -0.01)
   # initials3 <- list(lambda=75, alpha_brho = -0.25, alpha_acam = 0.1)
   # initials4 <- list(lambda=200, alpha_brho = 0.1, alpha_acam = 0.2)
    
    initialsall<- list(initials1, initials2, initials3, initials4)
    
    ## run the model
    model.output[[paste0("acam_w", i)]] = stan(file = 'data_analysis/models/fit_models/ACAM_ricker_nb_static.stan', data = data_vec, init = initialsall, iter = 5000, chains = 4, thin = 2, control = list(adapt_delta = 0.9, max_treedepth = 18))
    
    PrelimFit <- model.output[[paste0("acam_w", i)]]
    
    ## save model output
    save(PrelimFit, file = paste0("data_analysis/models/output/static/", date, "/acam_nb_static_w", i, "_", date, ".rdata"))

}


# Sigmoidal Fit ####

## make a list for model output
rainfall = c(1, 0.75, 0.6)
sigmoidal.output <- list()
date = 20250401

for(i in rainfall){
  
    ## select data 
    dat = acam.model[acam.model$water == i,] %>%
      filter(!is.na(num.focal.indiv))
    
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## create vectors of data inputs
    Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER CAP
    N = as.integer(length(Fecundity)) ## number of observations
    N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
    brho <- as.integer(dat$num.bg.indiv) ## background stem # data

    ## make a vector of data inputs to model
    data_vec <- c("N", "Fecundity", "N_i", "brho")
  
    ## set initial values 
    initials1 <- list(lambda=50, N_opt = 1.5, c = -0.001, alpha_slope = -0.05, alpha_initial = -0.06, alpha_acam = -0.06)
    initials2 <- list(lambda=60, N_opt = 2, c = -0.05, alpha_slope = -0.09, alpha_initial = -0.01, alpha_acam = -0.03)
 
    initials3 <- list(lambda=65, N_opt = 1, c = -0.1, alpha_slope = -0.2, alpha_initial = -0.05, alpha_acam = -0.09)
    initials4 <- list(lambda=45, N_opt = 0.5, c = -0.3, alpha_slope = -0.01, alpha_initial = 0.03, alpha_acam = -0.02)

    ## put N_opt b/w 0-2 for init values
    ## maybe start with mean of prior distributions as init values
    
    ## chain 3 & 4 worked; others didn't
    
    initialsall<- list(initials1, initials2, initials3, initials4)
  
    ## run the model
    sigmoidal.output[[paste0("acam_w", i)]] = stan(file = 'data_analysis/models/fit_models/ACAM_ricker_nb_sigmoidal.stan', data = data_vec, init = initialsall, iter = 5000, chains = 4, thin = 2, control = list(adapt_delta = 0.999, max_treedepth = 18))
    
    PrelimFit <- sigmoidal.output[[paste0("acam_w", i)]]
    
    ## save model output
    save(PrelimFit, file = paste0("data_analysis/models/output/sigmoidal/", date, "/acam_nb_sigmoidal_w", i, "_", date, "_final.rdata"))
    
}



# Exponential Fit ####

## make a list for model output
rainfall = c(1, 0.75, 0.6)
exponential.output <- list()
date = 20250401

for(i in rainfall){
  
  ## select data 
  dat = acam.model[acam.model$water == i,] %>%
    filter(!is.na(num.focal.indiv))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## create vectors of data inputs
  Fecundity = as.integer(round(dat$seeds.out.percap)) ## seeds out PER CAP
  N = as.integer(length(Fecundity)) ## number of observations
  N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
  brho <- as.integer(dat$num.bg.indiv) ## background stem # data
  
  ## make a vector of data inputs to model
  data_vec <- c("N", "Fecundity", "N_i", "brho")
  
  ## set initial values 
  initials1 <- list(lambda=40, N_opt = 3, c = -0.2, alpha_slope = -0.15, alpha_initial = 0.1, alpha_acam = -0.2)
  initials2 <- list(lambda=90, N_opt = 10, c = -0.1, alpha_slope = -0.4, alpha_initial = -0.1, alpha_acam = -0.4)
  
  #initials2 <- list(lambda=35, N_opt = 0, c = -0.5, alpha_slope = -0.35, alpha_initial = -0.2, alpha_acam = -0.1)
  initials3 <- list(lambda=75, N_opt = 5, c = -0.1, alpha_slope = -0.4, alpha_initial = -0.1, alpha_acam = -0.4)
  initials4 <- list(lambda=40, N_opt = 2, c = -0.3, alpha_slope = -0.05, alpha_initial = 0.3, alpha_acam = -0.2)
  
  ## put N_opt b/w 0-2 for init values
  ## maybe start with mean of prior distributions as init values
  
  ## chain 3 & 4 worked; others didn't
  
  initialsall<- list(initials1, initials2, initials3, initials4)
  
  ## run the model
  exponential.output[[paste0("acam_w", i)]] = stan(file = 'data_analysis/models/fit_models/ACAM_ricker_nb_exponential.stan', data = data_vec, init = initialsall, iter = 30000, chains = 4, thin = 2, control = list(adapt_delta = 0.999, max_treedepth = 18))
  
  PrelimFit <- exponential.output[[paste0("acam_w", i)]]
  
  ## save model output
  save(PrelimFit, file = paste0("data_analysis/models/output/exponential/", date, "/acam_nb_exponential_w", i, "_", date, ".rdata"))
  
}
