## Header ##
## 
## Script Name: Model Diagnostics
##
## Purpose: calculate Rhat values and check traceplots, and pairs plots for models
## 
## Author: Carmen Watkins

# Set up ####
## load packages
library(ggpubr)
library(bayesplot)

## save output file location
output_loc = "data_analysis/models/evaluate/diagnostics/"

# Load models ####
## sigmoidal ####
load("data_analysis/models/output/brho_ricker_poiss_sigmoidal_m1_w1_20250106.rdata")
w1 = brho_ricker_poiss_alpha_sig2

load("data_analysis/models/output/brho_ricker_poiss_sigmoidal_m1_w0.75_20250106.rdata")
w0.75 = brho_ricker_poiss_alpha_sig2

load("data_analysis/models/output/brho_ricker_poiss_sigmoidal_m1_w0.6_20250106.rdata")
w0.6 = brho_ricker_poiss_alpha_sig2

## static ####
rain = c(1, 0.75, 0.6)
microbe = c(0, 1)
date = 20250110
brho_stat_posts = list()

## create empty df for diagnostics
diagnostics = data.frame(model.name = NA, Rhat = NA, Neff = NA)

for(i in rain){
  for(j in microbe) {
  
    ## load non-constrained models
    load(paste0("data_analysis/models/output/static/brho_nb_static_m",j, "_w", i, "_", date, ".rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("m", j, "_w", i))
    
    ## extract model info
    tmp <- rstan::extract(PrelimFit)
    
    ## save posterior distributions
    brho_stat_posts[[paste0("brho_m", j, "_w", i)]] <- tmp
    
    ## save Rhat & Neff vals
    Rhat = max(summary(PrelimFit)$summary[,"Rhat"],na.rm =T)
    Neff = min(summary(PrelimFit)$summary[,"n_eff"],na.rm = T)
    
    ## put in df
    tmp2 = data.frame(model.name = paste0("brho_m", j, "_w", i), Rhat = Rhat, Neff = Neff)
    
    ## append to main df
    diagnostics = rbind(diagnostics, tmp2)
    
    ## create traceplot for the model
    traceplot(PrelimFit, pars = c("disp", "lambda", "alpha_acam", "alpha_brho"))
    
    ## save traceplot
    ggsave(paste0(output_loc, "static/", date, "/traceplot_mainparams_brho_m", j, "_w", i, ".png"), width = 6, height = 5)
 
  }
  
}

## remove NA
diagnostics = diagnostics %>%
  filter(!is.na(model.name))

## save output
write.csv(diagnostics, paste0(output_loc, "static/", date, "/rhat_neff_brho_nb_stat_", date, ".csv"))
