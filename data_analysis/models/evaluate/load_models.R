
library(ggpubr)
library(bayesplot)
library(rstan) ## need this for traceplot
library(tidyverse)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load models ####
## BRHO ####
### sigmoidal ####
rain = c(1, 0.75, 0.6)
microbe = c(0, 1)
date = 20250124
brho_sig_posts = list()

## create empty df for diagnostics
#sig_diagnostics = data.frame(model.name = NA, Rhat = NA, Neff = NA)

for(i in rain){
  for(j in microbe) {
    
    ## load non-constrained models
    load(paste0("data_analysis/models/output/sigmoidal/brho_nb_sigmoidal_m",j, "_w", i, "_", date, ".rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("m", j, "_w", i))
    
    ## extract model info
    tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
    
    ## save posterior distributions
    brho_sig_posts[[paste0("brho_m", j, "_w", i)]] <- tmp

  }
  
}

sig_posteriors <- data.frame()

for(i in rain){
  for(j in microbe) {
    
  tmp = as_tibble(do.call("cbind", brho_sig_posts[[paste0("brho_m", j, "_w", i)]])) %>%
    select(disp, lambda, alpha_brho, N_opt, c, alpha_slope, alpha_initial) %>%
    mutate(water = i, microbe = j)
  
  tmp = tmp[10001:20000,]
  
  sig_posteriors = rbind(sig_posteriors, tmp)
  
  }
}

rm(tmp, PrelimFit)

### static ####
rain = c(1, 0.75, 0.6)
microbe = c(0, 1)
date = 20250110
brho_stat_posts = list()

## create empty df for diagnostics
#sig_diagnostics = data.frame(model.name = NA, Rhat = NA, Neff = NA)

for(i in rain){
  for(j in microbe) {
    
    ## load non-constrained models
    load(paste0("data_analysis/models/output/static/brho_nb_static_m", j, "_w", i, "_", date, ".rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("m", j, "_w", i))
    
    ## extract model info
    tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
    
    ## save posterior distributions
    brho_stat_posts[[paste0("brho_m", j, "_w", i)]] <- tmp
    
  }
  
}

stat_posteriors <- data.frame()

for(i in rain){
  for(j in microbe) {
    
    tmp = as_tibble(do.call("cbind", brho_stat_posts[[paste0("brho_m", j, "_w", i)]])) %>%
      select(disp, lambda, alpha_brho, alpha_acam) %>%
      mutate(water = i, microbe = j)
    
    tmp = tmp[2500:5000,]
    
    stat_posteriors = rbind(stat_posteriors, tmp)
    
  }
}

rm(tmp, PrelimFit)

