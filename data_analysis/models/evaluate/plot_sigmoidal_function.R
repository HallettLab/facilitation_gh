## Header ##
## 
## Script Name: Plot Sigmoidal Function
##
## Purpose: Use medians of posterior distributions to visualize alpha values and fecundity along a density of ACAM
## 
## Author: Code adapted from Lisa Buche

# Set up ####
## load packages
library(tidyverse)

## read in models
source("data_analysis/models/evaluate/load_models.R")

## set fig location
fig_loc = "data_analysis/models/evaluate/plot_alpha_fecundity/"

theme_set(theme_classic())

## create sigmoidal alpha function
alpha_function4  <- function(Amin, Aslopes,c,N,N0){
  
  e = exp(Aslopes*(N-N0)) # c is stretching the graph horizontally 
  a = c*(1-e) #stretching the graph vertically
  d = Amin
  alpha = (a/(1 + e)) + d
  
  return(alpha)
  
}

## create function for fecundity
fec_function = function(lambda, Amin, Aslopes, c, N_acam, N0, N_brho, alpha_brho) {
  
  e = exp(Aslopes*(N_acam-N0)) # c is stretching the graph horizontally 
  a = c*(1-e) #stretching the graph vertically
  d = Amin
  alpha_acam = (a/(1 + e)) + d
  
  fecundity = N_brho*lambda * exp(-(N_brho*alpha_brho) - (N_acam*alpha_acam))
  
  return(fecundity)
  
}

rain = c(1, 0.75, 0.6)
microbe = c(0, 1)
sig_alpha_dat = data.frame(water = NA, microbe = NA, density = NA, alpha = NA)

## use mean of posteriors to get data using functions
for(i in rain) {
  for(j in microbe){
    
    temp = sig_posteriors %>%
      filter(water == i, microbe == j)

    #[10001:20000,]
    ## these seem like they are including warmups...
  
    ## set variables
    Amin = median(temp$alpha_initial)
    Aslopes = median(temp$alpha_slope)
    c = median(temp$c)
    N0 = median(temp$N_opt)
    
    ## run alpha function and save in df
    tmp_alpha = data.frame(water = rep(paste0(i), 81), microbe = rep(paste0(j), 81), density = c(0:80), alpha = alpha_function4(Amin, Aslopes, c, N = c(0:80), N0))
    
    ## append
    sig_alpha_dat = rbind(sig_alpha_dat, tmp_alpha) %>%
      filter(!is.na(water))
    
  }
  
}

# Plot ####
sig_alpha_dat %>%
  mutate(microbe = ifelse(microbe == 0, "Sterilized Soil", "Live Soil")) %>%
ggplot(aes(x=density, y=alpha, fill = water)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 2.5) +
  coord_cartesian(xlim = c(0,30)) +
  facet_wrap(~microbe) +
  xlab("Density") +
  ylab("Alpha value") +
  labs(fill = "Water") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#de8a5a", "#f6edbd", "#008080"))

ggsave(paste0(fig_loc, "sigmoidal/20250124/alpha_v_density_80.png"), width = 7, height = 3.5)


