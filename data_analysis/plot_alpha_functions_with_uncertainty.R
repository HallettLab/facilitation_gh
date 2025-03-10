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
library(HDInterval)

## read in models
source("data_analysis/models/evaluate/load_models.R")

## set fig location
fig_loc = "data_analysis/models/evaluate/plot_alpha_fecundity/"

theme_set(theme_classic())

# Create Functions ####
## create sigmoidal alpha function
alpha_function4  <- function(Amin, Aslopes,c,N,N0){
  
  e = exp(Aslopes*(N-N0)) # c is stretching the graph horizontally 
  a = c*(1-e) #stretching the graph vertically
  d = Amin
  alpha = (a/(1 + e)) + d
  
  return(alpha)
  
}

# Get data ####
## BRHO ####
### Sigmoidal ####
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

sig_alpha_dat2 = sig_alpha_dat %>%
  mutate(model = "sigmoidal")


### Static ####
rain = c(1, 0.75, 0.6)
stat_alpha_dat = data.frame(water = NA, density = NA, alpha = NA, fecundity = NA)

## use mean of posteriors to get data using functions
for(i in rain) {
  
  temp = stat_posteriors %>%
    filter(water == i)
  
  ## set variables
  alpha_acam = median(temp$alpha_acam)
  #N0 = median(temp$N_opt)
  lambda = median(temp$lambda)
  
  ## run alpha function and save in df
  tmp_alpha = data.frame(water = rep(paste0(i), 51), density = c(0:50), alpha = alpha_acam)
  tmp_alpha2 = tmp_alpha %>%
    mutate(fecundity = lambda*exp(alpha*density))
  
  ## append
  stat_alpha_dat = rbind(stat_alpha_dat, tmp_alpha2) %>%
    filter(!is.na(water))
  
}

stat_alpha_dat2 = stat_alpha_dat %>%
  mutate(model = "static")

### Join ####
brho_dat = rbind(stat_alpha_dat2, sig_alpha_dat2)

## ACAM ####
### Sigmoidal ####
rain = c(1, 0.75, 0.6)
acam_sig_alpha_dat = data.frame(water = NA, density = NA, alpha = NA, fecundity = NA)

## use mean of posteriors to get data using functions
for(i in rain) {
  
  temp = acam_sig_posteriors %>%
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
  acam_sig_alpha_dat = rbind(acam_sig_alpha_dat, tmp_alpha2) %>%
    filter(!is.na(water))
  
}

acam_sig_alpha_dat2 = acam_sig_alpha_dat %>%
  mutate(model = "sigmoidal")

### Static ####
rain = c(1, 0.75, 0.6)
acam_stat_alpha_dat = data.frame(water = NA, density = NA, alpha = NA, fecundity = NA)

## use mean of posteriors to get data using functions
for(i in rain) {
  
  temp = acam_stat_posteriors %>%
    filter(water == i)
  
  ## set variables
  alpha_acam = median(temp$alpha_acam)
  #N0 = median(temp$N_opt)
  lambda = median(temp$lambda)
  
  ## run alpha function and save in df
  tmp_alpha = data.frame(water = rep(paste0(i), 51), density = c(0:50), alpha = alpha_acam)
  tmp_alpha2 = tmp_alpha %>%
    mutate(fecundity = lambda*exp(alpha*density))
  
  ## append
  acam_stat_alpha_dat = rbind(acam_stat_alpha_dat, tmp_alpha2) %>%
    filter(!is.na(water))
  
}

acam_stat_alpha_dat2 = acam_stat_alpha_dat %>%
  mutate(model = "static")

### Join ####
acam_dat = rbind(acam_stat_alpha_dat2, acam_sig_alpha_dat2)

# Plot ####
## BRHO ####

alpha = ggplot(brho_dat, aes(x=density, y=alpha, color = water, linetype = model)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  coord_cartesian(xlim = c(0,50)) +
  xlab("Density") +
  ylab("Alpha value") +
  labs(color = "Water", linetype = "Model") +
  scale_linetype_manual(values = c(1, 4)) +
  theme(text = element_text(size=13))


fec = ggplot(brho_dat, aes(x=density, y=fecundity, color = water, linetype = model)) +
  #geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  coord_cartesian(xlim = c(0,50)) +
  xlab("Density") +
  ylab("Fecundity") +
  labs(color = "Water", linetype = "Model") +
  scale_linetype_manual(values = c(1, 4)) +
  theme(text = element_text(size=13))


ggarrange(alpha, fec, common.legend = TRUE, legend = "bottom", labels = "AUTO")

ggsave("figures/MS_draft2/brho_sig_v_stat.png", width = 8, height = 4)

## ACAM ####
Aalpha = ggplot(acam_dat, aes(x=density, y=alpha, color = water, linetype = model)) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  coord_cartesian(xlim = c(0,50)) +
  xlab("Density") +
  ylab("Alpha value") +
  labs(color = "Water", linetype = "Model") +
  scale_linetype_manual(values = c(1, 4)) +
  theme(text = element_text(size=13))


Afec = ggplot(acam_dat, aes(x=density, y=fecundity, color = water, linetype = model)) +
  #geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  coord_cartesian(xlim = c(0,50)) +
  xlab("Density") +
  ylab("Fecundity") +
  labs(color = "Water", linetype = "Model") +
  scale_linetype_manual(values = c(1, 4)) +
  theme(text = element_text(size=13))


ggarrange(Aalpha, Afec, common.legend = TRUE, legend = "bottom", labels = "AUTO")

#ggsave("figures/MS_draft2/acam_sig_v_stat.png", width = 8, height = 4)
