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

## get equilibrium values
# source("data_analysis/MCT/find_equilibrium.R")
source("data_analysis/MCT/calc_equilibrium_sigmoidal.R")

## set fig location
fig_loc = "figures/Apr2025/"

theme_set(theme_classic())

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Create Functions ####
## sigmoidal igr; also returns alpha_inter
igr_sig_func = function(surv, germ, lambda, alpha_intra, Nt, germ_inter, inter_abund, alpha_0, alpha_slope, N0, c) {
  
  ## Nt = abund of focal
  
  alpha_inter = alpha_0 + ((c*(1 - exp(alpha_slope*(inter_abund - N0))))/(1+exp(alpha_slope*(inter_abund - N0))))
  
  Ntp1 = ((1-germ)*surv*Nt) + (germ * lambda * Nt * exp(alpha_inter*germ_inter*inter_abund))
  
  dat = data.frame(a_ij = alpha_inter, igr = log(Ntp1/Nt))
  
  return(dat)
  
}

## static igr
igr_stat_func = function(surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund) {
  
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(alpha_intra *germ* Nt + alpha_inter*germ_inter*inter_abund)
  
  return(log(Ntp1/Nt))
  
}

# Calc alpha & IGR ####
## Static ####
## create empty df
igr_stat = data.frame(focal = NA, water = NA, post_num = NA, igr = NA, dens = NA, alpha_inter = NA)

## set post draw list from equil df
posts_stat = unique(equil$post_num)

## create treatment vectors
species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

for(i in 1:length(species)) {
  for (j in 1:length(rain)) {
    
    sp = species[i]
    r = rain[j]
    
    ## select data
    adat = acam_stat_posteriors[acam_stat_posteriors$water == r,]
    bdat = brho_stat_posteriors[brho_stat_posteriors$water == r,]
    
    ## set treatment
    if(r == 1) { trt = "C" } 
    else { trt = "D" }
    
    ## loop thru each posterior draw
    for (k in 1:length(posts_stat)) {
      
      p = posts_stat[k]
      
      ## define params
      if (sp == "ACAM") {
        
        lambda_i = adat[p,]$lambda
        alpha_ii = adat[p,]$alpha_acam
        alpha_ij = adat[p,]$alpha_brho
        g_i = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "ACAM",]$surv.mean.p
        g_j = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
        
        N_eq_v = c(0:45)
        
      } else {
        
        lambda_i = bdat[p,]$lambda
        alpha_ii = bdat[p,]$alpha_brho
        alpha_ij = bdat[p,]$alpha_acam
        g_i = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "BRHO",]$surv.mean.p
        g_j = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
        
        N_eq_v = c(0:45)
        
      }
      
      ## calc IGR
      igr_tmp = igr_stat_func(surv = s_i, germ = g_i, lambda = lambda_i, alpha_intra = alpha_ii, Nt = 1, alpha_inter = alpha_ij, germ_inter = g_j, inter_abund = N_eq_v)
      
      ## fill in data 
      tmp = data.frame(focal = sp, water = r, post_num = p, igr = igr_tmp, alpha_inter = alpha_ij, dens = N_eq_v)
      
      ## append
      igr_stat = rbind(igr_stat, tmp)
      
    }
    
  }
  
}

igr_stat = igr_stat %>%
  filter(!is.na(focal)) %>%
  mutate(model = "static")

## Sigmoidal ####
## create empty df
igr_sig = data.frame(focal = NA, water = NA, post_num = NA, alpha_inter = NA, igr = NA, dens = NA, N_star = NA)

## create treatment vectors
species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

for(i in 1:length(species)) {
  for (j in 1:length(rain)) {
    
    ## select species
    sp = species[i]
    ## select water treat
    r = rain[j]
    
    ## select data
    ## want data from 80% hdi
    dat = sigposts80[sigposts80$focal == sp & sigposts80$water == r,]

    ## set treatment
    if(r == 1) { trt = "C" 
    } else { trt = "D" }
    
    ## set post draw list from equil df
    posts_sig = unique(equil_sig[equil_sig$species == sp & equil_sig$water == r,]$post_num)
        ## should be 400 vals
    
    ## loop thru each posterior draw
    for (k in 1:length(posts_sig)) {
      
      p = posts_sig[k]
      
      ## define params
      if (sp == "ACAM") {
        
        g_i = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "ACAM",]$surv.mean.p
        g_j = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
        
      } else {
        
        g_i = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "BRHO",]$surv.mean.p
        g_j = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
       
      }
      
      lambda_i = dat[dat$post_num == p,]$lambda
      alpha_ii = dat[dat$post_num == p,]$alpha_intra
      
      ## alpha_inter params
      n_opt = dat[dat$post_num == p,]$N_opt
      c = dat[dat$post_num == p,]$c
      alpha_slope = dat[dat$post_num == p,]$alpha_slope
      alpha_init = dat[dat$post_num == p,]$alpha_initial
      
      eq = equil_sig[equil_sig$species == sp & equil_sig$water == r & equil_sig$post_num == p,]$n_star
      N_eq_v = c(0:eq)
      
      
      ## calc IGR
      igr_tmp = igr_sig_func(surv = s_i, germ = g_i, lambda = lambda_i, alpha_intra = alpha_ii, Nt = 1, germ_inter = g_j, inter_abund = N_eq_v, alpha_0 = alpha_init, alpha_slope = alpha_slope, N0 = n_opt, c = c)
      
      ## fill in data
      tmp = data.frame(focal = sp, water = r, post_num = p, igr = igr_tmp$igr, alpha_inter = igr_tmp$a_ij, dens = N_eq_v, N_star = eq)
      
      ## append
      igr_sig = rbind(igr_sig, tmp)
      
    }
    
  }
  
}

igr_sig = igr_sig %>%
  filter(!is.na(focal)) %>%
  mutate(model = "sigmoidal")

#write.csv(igr_sig, "data_analysis/MCT/output/igr_sigmoidal_20250428.csv", row.names = F)
