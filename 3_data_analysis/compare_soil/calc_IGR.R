## Header ##
##
## Calculate IGR and alpha over density
## 

# Set up ####
## load packages
library(tidyverse)

## get equilibrium values
# source("data_analysis/MCT/find_equilibrium.R")
source("data_analysis/compare_soil/calc_equilibrium_soil.R")

theme_set(theme_classic())

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

# Create Functions ####
## static igr
igr_stat_func = function(surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund) {
  
  Ntp1 = (1-germ)*surv*Nt + germ*lambda*Nt*exp(alpha_intra *germ* Nt + alpha_inter*germ_inter*inter_abund)
  
  return(log(Ntp1/Nt))
  
}

# Calc alpha & IGR ####
## m0 ####
## create empty df
igr_m0 = data.frame(focal = NA, water = NA, post_num = NA, igr = NA, equil = NA, alpha_inter = NA, lambda = NA)

## set post draw list from equil df
# posts_stat = unique(m0equil$post_num)

## create treatment vectors
species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

for(i in 1:length(species)) {
for (j in 1:length(rain)) {
    
  sp = species[i]
  
  ## focal & resident
  if(sp == "ACAM") { 
    
    focal = "ACAM"  
    resident = "BRHO"
    
  } else { 
    
    focal = "BRHO"  
    resident = "ACAM"}
    
    ## select water treat
    r = rain[j]
    
    ## select data
    ## want data from 80% hdi
    dat = m0statposts80[m0statposts80$focal == focal & m0statposts80$water == r,]
    
    ## set treatment
    if(r == 1) { trt = "C" 
    } else { trt = "D" }
    
    ## set post draw list from equil df
    posts_stat = unique(m0equil[m0equil$species == focal & m0equil$water == r,]$post_num)
    ## should be 400 vals
    
    m0eq = m0equil %>%
      filter(species == resident, water == r)
    
    dat_filt = dat %>%
      filter(post_num %in% posts_stat)
    
    ## loop thru each posterior draw
    for (k in 1:length(dat_filt$post_num)) {
      
      p = posts_stat[k]
      
      n_star = as.integer(m0eq[k,]$n_star)
      #meq = 
      
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
       
      #N_eq_v = c(0:n_star) ## just use one equilibrium value for the moment
      lambda_i = dat_filt[dat_filt$post_num == p,]$lambda
      alpha_ii = dat_filt[dat_filt$post_num == p,]$alpha_intra
      alpha_ij = dat_filt[dat_filt$post_num == p,]$alpha_inter
      
      ## calc IGR
      igr_tmp = igr_stat_func(surv = s_i, germ = g_i, lambda = lambda_i, alpha_intra = alpha_ii, Nt = 1, alpha_inter = alpha_ij, germ_inter = g_j, inter_abund = n_star)
      
      ## fill in data 
      tmp = data.frame(focal = focal, water = r, post_num = p, igr = igr_tmp, alpha_inter = alpha_ij, equil = n_star, lambda = lambda_i)
      
      ## append
      igr_m0 = rbind(igr_m0, tmp)
      
    }
    
  }
  
}

igr_m0 = igr_m0 %>%
  filter(!is.na(focal)) %>%
  mutate(microbe = 0)


## m1 ####
## create empty df
igr_m1 = data.frame(focal = NA, water = NA, post_num = NA, igr = NA, equil = NA, alpha_inter = NA, lambda = NA)

## set post draw list from equil df
# posts_stat = unique(m0equil$post_num)

## create treatment vectors
species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

for(i in 1:length(species)) {
  for (j in 1:length(rain)) {
    
    sp = species[i]
    
    ## focal & resident
    if(sp == "ACAM") { 
      
      focal = "ACAM"  
      resident = "BRHO"
      
    } else { 
      
      focal = "BRHO"  
      resident = "ACAM"}
    
    ## select water treat
    r = rain[j]
    
    ## select data
    ## want data from 80% hdi
    dat = m1statposts80[m1statposts80$focal == focal & m1statposts80$water == r,]
    
    ## set treatment
    if(r == 1) { trt = "C" 
    } else { trt = "D" }
    
    ## set post draw list from equil df
    posts_stat = unique(m1equil[m1equil$species == focal & m1equil$water == r,]$post_num)
    ## should be 400 vals
    
    m1eq = m1equil %>%
      filter(species == resident, water == r)
    
    dat_filt = dat %>%
      filter(post_num %in% posts_stat)
    
    ## loop thru each posterior draw
    for (k in 1:length(dat_filt$post_num)) {
      
      p = posts_stat[k]
      
      n_star = as.integer(m1eq[k,]$n_star)
      #meq = 
      
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
      
      #N_eq_v = c(0:n_star)
      lambda_i = dat_filt[dat_filt$post_num == p,]$lambda
      alpha_ii = dat_filt[dat_filt$post_num == p,]$alpha_intra
      alpha_ij = dat_filt[dat_filt$post_num == p,]$alpha_inter
      
      ## calc IGR
      igr_tmp = igr_stat_func(surv = s_i, germ = g_i, lambda = lambda_i, alpha_intra = alpha_ii, Nt = 1, alpha_inter = alpha_ij, germ_inter = g_j, inter_abund = n_star)
      
      ## fill in data 
      tmp = data.frame(focal = focal, water = r, post_num = p, igr = igr_tmp, alpha_inter = alpha_ij, equil = n_star, lambda = lambda_i)
      
      ## append
      igr_m1 = rbind(igr_m1, tmp)
      
    }
    
  }
  
}

igr_m1 = igr_m1 %>%
  filter(!is.na(focal)) %>%
  mutate(microbe = 1)
