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
library(rstan) ## need this for traceplot
library(tidyverse)

## save output file location
output_loc = "data_analysis/models/evaluate/diagnostics/"

# BRHO ####
## sigmoidal ####
rain = c(1, 0.75, 0.6)
#microbe = c(0, 1)
date = 20250401
brho_sig_posts = list()

## create empty df for diagnostics
sig_diagnostics = data.frame(model.name = NA, Rhat = NA, Neff = NA)

for(i in rain){
    
    ## load models
    load(paste0("data_analysis/models/output/sigmoidal/", date, "/brho_nb_sigmoidal_w", i, "_", date, "_final.rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## extract model info
    tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
    
    ## save posterior distributions
    #brho_sig_posts[[paste0("brho_m", j, "_w", i)]] <- tmp
    
    print(PrelimFit)
    
    ## save Rhat & Neff vals
    Rhat = max(summary(PrelimFit)$summary[,"Rhat"],na.rm =T)
    Neff = min(summary(PrelimFit)$summary[,"n_eff"],na.rm = T)
    
    ## put in df
    tmp2 = data.frame(model.name = paste0("brho_w", i), Rhat = Rhat, Neff = Neff)
    
    ## append to main df
    sig_diagnostics = rbind(sig_diagnostics, tmp2)
    
    ## create traceplot for the model
    traceplot(PrelimFit, pars = c("disp", "lambda", "alpha_brho", "alpha_initial", "alpha_slope", "c", "N_opt"), inc_warmup = TRUE)
    
    ## save traceplot
    ggsave(paste0(output_loc, "sigmoidal/", date, "/traceplot_mainparams_brho_w", i, "_final.png"), width = 10, height = 8)
    
}

## remove NA
sig_diagnostics = sig_diagnostics %>%
  filter(!is.na(model.name))

## save output
write.csv(sig_diagnostics, paste0(output_loc, "sigmoidal/", date, "/rhat_neff_brho_nb_sigmoidal_", date, "_final.csv"))

## static ####
rain = c(1, 0.75, 0.6)
date = 20250401
brho_stat_posts = list()

## create empty df for diagnostics
stat_diagnostics = data.frame(model.name = NA, Rhat = NA, Neff = NA)

for(i in rain){
  
    ## load models
    load(paste0("data_analysis/models/output/static/", date, "/brho_nb_static_w", i, "_", date, "_final.rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## extract model info
    tmp <- rstan::extract(PrelimFit)
    
    ## save posterior distributions
    brho_stat_posts[[paste0("brho_w", i)]] <- tmp
    
    ## save Rhat & Neff vals
    Rhat = max(summary(PrelimFit)$summary[,"Rhat"],na.rm =T)
    Neff = min(summary(PrelimFit)$summary[,"n_eff"],na.rm = T)
    
    ## put in df
    tmp2 = data.frame(model.name = paste0("brho_w", i), Rhat = Rhat, Neff = Neff)
    
    ## append to main df
    stat_diagnostics = rbind(stat_diagnostics, tmp2)
    
    ## create traceplot for the model
    traceplot(PrelimFit, pars = c("disp", "lambda", "alpha_acam", "alpha_brho"))
    
    ## save traceplot
    ggsave(paste0(output_loc, "static/", date, "/traceplot_mainparams_brho_w", i, ".png"), width = 6, height = 5)
  
}

## remove NA
stat_diagnostics = stat_diagnostics %>%
  filter(!is.na(model.name))

## save output
write.csv(stat_diagnostics, paste0(output_loc, "static/", date, "/rhat_neff_brho_nb_stat_", date, "_final.csv"))

# ACAM ####
## sigmoidal ####
rain = c(1, 0.75, 0.6)
date = 20250401
acam_sig_posts = list()

## create empty df for diagnostics
acam_sig_diagnostics = data.frame(model.name = NA, Rhat = NA, Neff = NA)

for(i in rain){
  
  ## load models
  load(paste0("data_analysis/models/output/sigmoidal/", date, "/acam_nb_sigmoidal_w", i, "_", date, "_final.rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  ## extract model info
  tmp <- rstan::extract(PrelimFit, inc_warmup = FALSE)
  
  print(PrelimFit)
  
  ## save Rhat & Neff vals
  Rhat = max(summary(PrelimFit)$summary[,"Rhat"],na.rm =T)
  Neff = min(summary(PrelimFit)$summary[,"n_eff"],na.rm = T)
  
  ## put in df
  tmp2 = data.frame(model.name = paste0("acam_w", i), Rhat = Rhat, Neff = Neff)
  
  ## append to main df
  acam_sig_diagnostics = rbind(acam_sig_diagnostics, tmp2)
  
  ## create traceplot for the model
  traceplot(PrelimFit, pars = c("disp", "lambda", "alpha_acam", "alpha_initial", "alpha_slope", "c", "N_opt"), inc_warmup = TRUE)
  
  ## save traceplot
  ggsave(paste0(output_loc, "sigmoidal/", date, "/traceplot_mainparams_acam_w", i, ".png"), width = 10, height = 8)
  
}

## remove NA
acam_sig_diagnostics = acam_sig_diagnostics %>%
  filter(!is.na(model.name))

## save output
write.csv(acam_sig_diagnostics, paste0(output_loc, "sigmoidal/", date, "/rhat_neff_acam_nb_sigmoidal_", date, "_final.csv"))

## static ####
rain = c(1, 0.75, 0.6)
date = 20250401
acam_stat_posts = list()

## create empty df for diagnostics
acam_stat_diagnostics = data.frame(model.name = NA, Rhat = NA, Neff = NA)

for(i in rain){
#  for(j in microbe) {
    
    ## load non-constrained models
    load(paste0("data_analysis/models/output/static/", date, "/acam_nb_static_w", i, "_", date, ".rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    ## extract model info
    tmp <- rstan::extract(PrelimFit)
    
    ## save posterior distributions
    acam_stat_posts[[paste0("acam_w", i)]] <- tmp
    
    ## save Rhat & Neff vals
    Rhat = max(summary(PrelimFit)$summary[,"Rhat"],na.rm =T)
    Neff = min(summary(PrelimFit)$summary[,"n_eff"],na.rm = T)
    
    ## put in df
    tmp2 = data.frame(model.name = paste0("acam_w", i), Rhat = Rhat, Neff = Neff)
    
    ## append to main df
    acam_stat_diagnostics = rbind(acam_stat_diagnostics, tmp2)
    
    ## create traceplot for the model
    traceplot(PrelimFit, pars = c("disp", "lambda", "alpha_acam", "alpha_brho"))
    
    ## save traceplot
    ggsave(paste0(output_loc, "static/", date, "/traceplot_mainparams_acam_w", i, ".png"), width = 6, height = 5)
  
}

## remove NA
acam_stat_diagnostics = acam_stat_diagnostics %>%
  filter(!is.na(model.name))

## save output
write.csv(acam_stat_diagnostics, paste0(output_loc, "static/", date, "/rhat_neff_acam_nb_stat_", date, ".csv"))
