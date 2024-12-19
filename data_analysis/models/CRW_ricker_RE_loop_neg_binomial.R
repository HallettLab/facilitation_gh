## Model 

## load packages
library(tidyverse)
library(bayesplot)
library(rstan)
library(here)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## set date
date <- 20241218

#species <- c("ACAM", "BRHO")

#model.output <- list()
#warnings <- list()

#for(i in species){

  dat <- brho.all
  
## create vectors of the various data inputs
  Fecundity <- as.integer(round(dat$seeds.out)) ## seeds out
  #N_blocks <- as.integer(length(unique(dat$block))) ## number of blocks
  #Blocks <- as.integer(dat$block) ## vector of block vals
  N <- as.integer(length(Fecundity)) ## number of observations
  N_i <- as.integer(dat$num.focal.indiv) ## stem # of focal species
  #trt <- as.integer(dat$trt) ## treatment (binary)

## stems data
  acam <- as.integer(dat$num.bg.indiv)
  #brho <- as.integer(dat$num.focal.indiv)
 
## make a vector of data inputs to model

  data_vec <- c("N", "Fecundity", "N_i", #"N_blocks", "Blocks", "trt", 
                "acam")

  
  initials1 <- list(lambda=200, N_opt = 1, c = -0.5, alpha_slope = -0.8, alpha_initial = 0.1, alpha_brho = 0.08)
  
  
  initials2 <- list(lambda=200, N_opt = 1, c = -0.5, alpha_slope = -0.8, alpha_initial = 0.1, alpha_brho = 0.02)
  
  initials3 <- list(lambda=600, N_opt = 5, c = -0.2, alpha_slope = -0.4, alpha_initial = -0.1, alpha_brho = 0.04)
  initials4 <- list(lambda=300, N_opt = 4, c = -0.4, alpha_slope = -0.2, alpha_initial = 0.5, alpha_brho = 0.01)
  
  initialsall<- list(initials1, initials1, initials3, initials4)
  
#  print(i)

## create initials for epsilon and sigma
 # initials <- list(epsilon=rep(1,N_blocks), sigma = 1)
  #initials1<- list(initials, initials, initials)

# Model ####
  brho_ricker_alphaf = stan(file = 'data_analysis/models/ricker_neg_binom_static_alpha.stan', 
                  data = data_vec, iter = 3000, chains = 4, thin = 2, 
                  control = list(adapt_delta = 0.95, max_treedepth = 15)) 

  
  brho_ricker_alpha_sig = stan(file = 'data_analysis/models/ricker_neg_binom_sigmoidal_alpha.stan', 
                            data = data_vec, iter = 3000, chains = 4, thin = 2, 
                            control = list(adapt_delta = 0.95, max_treedepth = 15)) 
  
  
  
## preserve this code; ran the 12/18 sigmoidal model well 
  
  initials1 <- list(lambda=200, N_opt = 1, c = -0.5, alpha_slope = -0.8, alpha_initial = 0.1, alpha_brho = 0.08)
  
  
  #initials2 <- list(lambda=200, N_opt = 1, c = -0.5, alpha_slope = -0.8, alpha_initial = 0.1, alpha_brho = 0.02)
  ## something was wrong with these initials that they kept kicking up an initialization error; likely the math of them did not work out...
  
  initials3 <- list(lambda=600, N_opt = 5, c = -0.2, alpha_slope = -0.4, alpha_initial = -0.1, alpha_brho = 0.04)
  initials4 <- list(lambda=300, N_opt = 4, c = -0.4, alpha_slope = -0.2, alpha_initial = 0.5, alpha_brho = 0.01)
  
  initialsall<- list(initials1, initials1, initials3, initials4)
  
  brho_ricker_poiss_alpha_sig2 = stan(file = 'data_analysis/models/ricker_poisson_sigmoidal_alpha.stan', 
                                     data = data_vec, init = initialsall, iter = 5000, chains = 4, thin = 2, 
                                     control = list(adapt_delta = 0.9, max_treedepth = 18))
  
  
## save model output
  save(brho_ricker_poiss_alpha_sig2, file = paste0("data_analysis/models/output/ricker_poiss_sigmoidal_brho_m1w1_goodfit", date, ".rdata"))

#}
