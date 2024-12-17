## Model 

date <- 20241216

library(tidyverse)
library(bayesplot)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

library(here)

species <- c("ACAM", "BRHO")

model.output <- list()
warnings <- list()

#for(i in species){

  dat <- brho.model
  
## create vectors of the various data inputs
  Fecundity <- as.integer(round(dat$seeds.out)) ## seeds out
  #N_blocks <- as.integer(length(unique(dat$block))) ## number of blocks
  #Blocks <- as.integer(dat$block) ## vector of block vals
  N <- as.integer(length(Fecundity)) ## number of observations
  N_i <- as.integer(dat$num.focal.indiv) ## stem # of focal species
  #trt <- as.integer(dat$trt) ## treatment (binary)

## stems data
  acam <- as.integer(dat$num.bg.indiv)
  brho <- as.integer(dat$num.focal.indiv)
 
## make a vector of data inputs to model

  data_vec <- c("N", "Fecundity", "N_i", #"N_blocks", "Blocks", "trt", 
                "acam")

#  print(i)

## create initials for epsilon and sigma
 # initials <- list(epsilon=rep(1,N_blocks), sigma = 1)
  #initials1<- list(initials, initials, initials)

# Model ####
  brho_ricker_alphaf <- stan(file = 'data_analysis/models/CRW_ricker_RE_neg_binomial.stan', 
                  data = data_vec, iter = 10000, chains = 4, thin = 2, 
                  control = list(adapt_delta = 0.95, max_treedepth = 15)) 

  ## first run of the alpha_f model didn't look good at all; try more iterations
  
  
  #PrelimFit <- model.output[[paste0("ricker_",i)]] 

## save model output
  save(brho_ricker, file = paste0("data_analysis/models/output/ricker_brho_m1w1", date, ".rdata"))

#}
