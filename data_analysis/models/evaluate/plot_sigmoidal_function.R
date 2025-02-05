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

rain = c(1, 0.75, 0.6)
sig_alpha_dat = data.frame(water = NA, density = NA, alpha = NA, fecundity = NA)

## use mean of posteriors to get data using functions
for(i in rain) {
  
    temp = sig_posteriors %>%
      filter(water == i)
    
    ## set variables
    Amin = median(temp$alpha_initial)
    Aslopes = median(temp$alpha_slope)
    c = median(temp$c)
    N0 = median(temp$N_opt)
    lambda = median(temp$lambda)
    
    ## run alpha function and save in df
    tmp_alpha = data.frame(water = rep(paste0(i), 51), density = c(0:50), alpha = alpha_function4(Amin, Aslopes, c, N = c(0:50), N0))
    tmp_alpha2 = tmp_alpha %>%
      mutate(fecundity = lambda*exp(alpha*density))
                           
    ## append
    sig_alpha_dat = rbind(sig_alpha_dat, tmp_alpha2) %>%
      filter(!is.na(water))

}

# Plot ####
alpha = sig_alpha_dat %>%
  mutate(water = ifelse(water == 1, "High", 
                        ifelse(water == 0.75, "Intermediate", "Low"))) %>%
ggplot(aes(x=density, y=alpha, fill = water)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 2.5) +
  coord_cartesian(xlim = c(0,50)) +
  xlab("Density") +
  ylab("Alpha value") +
  labs(fill = "Water") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#008080", "#f6edbd", "#de8a5a"))

fec = sig_alpha_dat %>%
  mutate(water = ifelse(water == 1, "High", 
                        ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  ggplot(aes(x=density, y=fecundity, fill = water)) +
  geom_line() +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 2.5) +
  coord_cartesian(xlim = c(0,50)) +
  xlab("Density") +
  ylab("Fecundity") +
  labs(fill = "Water") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("#008080", "#f6edbd", "#de8a5a"))

ggarrange(alpha, fec, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave(paste0(fig_loc, "sigmoidal/", date, "/alpha_&_fec_v_density_50.png"), width = 8, height = 4)
