## Header ##
## 
## Fit Live Soil Models
##
## Purpose: run Stan code to fit bayesian models for all 3 water conditions in the live soil
## 
## Author: Carmen Watkins

# Set up ####
## load packages
library(tidyverse)
library(bayesplot)
library(rstan)
library(here)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## set date
date <- 20250107

## read in data 
source("data_cleaning/clean_model_dat.R")

## set seed
set.seed(14)

## make vector of rainfall treatments
rainfall <- c(1, 0.75, 0.6)

# Static Fit ####
for(i in rainfall){
  
  ## select data 
  dat = brho.model[brho.model$water == i & brho.model$microbe == 1,]
  
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
  
  mfit = stan(file = 'data_analysis/models/fit_models/ricker_neg_binom_static_alpha.stan', 
                                      data = data_vec, init = initialsall, iter = 5000, chains = 4, thin = 2, 
                                      control = list(adapt_delta = 0.9, max_treedepth = 18))
  
  ## save model output
  save(mfit, file = paste0("data_analysis/models/output/brho_ricker_nb_static_alpha_m1_w", i, "_", date, ".rdata"))
  
}




# Sigmoidal ####
for(i in rainfall){

## select data 
dat = brho.model[brho.model$water == i & brho.model$microbe == 1,]

## create vectors of data inputs
Fecundity = as.integer(round(dat$seeds.out)) ## seeds out
N = as.integer(length(Fecundity)) ## number of observations
N_i = as.integer(dat$num.focal.indiv) ## stem # of focal species
acam <- as.integer(dat$num.bg.indiv) ## background stem # data

## make a vector of data inputs to model
data_vec <- c("N", "Fecundity", "N_i", "acam")

## set initial values 
initials1 <- list(lambda=200, N_opt = 1, c = -0.5, alpha_slope = -0.8, alpha_initial = 0.1, alpha_brho = 0.08)
initials2 <- list(lambda=250, N_opt = 2, c = -0.5, alpha_slope = -0.7, alpha_initial = 0.1, alpha_brho = 0.08)
initials3 <- list(lambda=600, N_opt = 5, c = -0.2, alpha_slope = -0.4, alpha_initial = -0.1, alpha_brho = 0.04)
initials4 <- list(lambda=300, N_opt = 4, c = 0, alpha_slope = -0.2, alpha_initial = 0.5, alpha_brho = 0.01)

initialsall<- list(initials1, initials2, initials3, initials4)

brho_ricker_poiss_alpha_sig2 = stan(file = 'data_analysis/models/ricker_poisson_sigmoidal_alpha_v2.stan', 
                                    data = data_vec, init = initialsall, iter = 5000, chains = 4, thin = 2, 
                                    control = list(adapt_delta = 0.9, max_treedepth = 18))

## save model output
save(brho_ricker_poiss_alpha_sig2, file = paste0("data_analysis/models/output/brho_ricker_poiss_sigmoidal_m1_w", i, "_", date, ".rdata"))

}
