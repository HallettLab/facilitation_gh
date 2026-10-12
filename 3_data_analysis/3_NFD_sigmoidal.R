

# @author: J.W.Spaak
# Example how to compute the ND and FD for a given differential equation setting

# The script is originally written in python, we run python code from within R
# make sure that python is installed on the system used
# This is not part of the install.packages("reticulate") command!


# LB comments:
# 1- keep using the python file for NFD - works better than the R file
# 2- to integrate the sigmoid or exponent, use the model function I wrote below,
# with c,a_slope,N0 equal 0 for the intra, so the diagonal of each parameter should be 0
# maybe you need to change * with %*% because they are matrix - just try and see if the output is a matrix or not ( it should) 
# you can also check manually that the function are correctly working by doing a manuel test with N=10
# you might encounter some computation error when applying the NFD function, but it is basically that easy - so yay
# let me know if you have any difficulties
# 3- in the pars - FD is old on, the pars$'F' is the updated definition - so your first graph is correct
# 4 - for one community, if species have a ND<1, they should be more or less the same
# but is there is facilitation,the ND will be on opposite side of 1 
# because theoretically ND_i = 1-ND_j 

# Set up ####
library(reticulate)
library(tidyverse)
library(HDInterval) ## to take hdi's of posteriors

py_require("scipy") ## needed for scipy import to work below
np <- import("numpy",convert=F)
sp <- import("scipy", convert = F)

#py_eval("pip install scipy")
# loads the relevant python code
# use python code 
#source("/Users/lisabuche/Documents/Projects/Spaak/numerical_NFD.py")

source_python("3_data_analysis/archive/NFD/NFD_definitions-master/numerical_NFD.py")

## germination data
germ = read.csv("data/germination_data.csv")

## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")

## parameter values
#sigposts = read.csv("data/model_posteriors/sig_posts_20250401.csv")
#statposts = read.csv("data/model_posteriors/stat_posts_20250401.csv")

## post draw numbers
source("3_data_analysis/1_calc_equilibrium_sigmoidal.R")

# create the differential equation system
n_spec = 2 # number of species in the system, must be an integer
set.seed(0) # set random seed for reproducability

# Define models ####
## plain ricker ####
ricker_f = function(N, s, g, lam, A) {

  return(log((1-g)*s + (g*lam*exp(A%*%(g*N)))))
  
} # log to transform the discrete time into continuous

## ricker sigmoidal ####
ricker_sigmoid_f = function(N, s, g, lam, a_init, c, a_slope, N0, a_intra){ 
  
  e = exp(a_slope*(N-N0)) # c is stretching the graph horizontally 
  alpha = a_init  + c*((1 - e)/(1 + e)) 
  
  ## alpha[2] = effects on BRHO
  ## alpha[1] = effects on ACAM
  
  # interaction matrix
  A = matrix(data=c(a_intra[1], # Data$AP_a_ii_exp[i], ## fill in INTRA coeffs
                    alpha[2], # Data$AP_a_ji_exp[i],   
                    alpha[1], # Data$AP_a_ij_exp[i], 
                    a_intra[2]), # Data$AP_a_jj_exp[i]), 
             nrow=n_spec, 
             ncol=n_spec)

  return(log((1-g)*s + (g*lam*exp(A%*%(g*N)))))
  
} # log to transform the discrete time into continuous

# Calc NFD ####
## Sigmoidal ####
NDF_sig = as.data.frame(matrix(NA,3*400,10)) ## 3 rows (water treatments); 11 cols
names(NDF_sig) = c("water", "post_num", "NDi","NDj","NOi",
                       "NOj","FDi","FDj",
                       "ci","cj")

water = c(0.6, 0.75, 1)

## Loop to calc NFD
for(i in 1:length(water)){
  
  w = water[i]
  
  if (w == 0.6) {
    trt = "D"
  } else {
    trt = "C"
  }
  
  wdat = sigposts80[sigposts80$water == w,]
  
 # wdat = sigposts80[sigposts80$water == w,] %>%
  #  mutate(new_post_id = rep(1:400, 2))

  ## filter data to get only certain posterior draw numbers
  ##have to do this in a convoluted way 
  ## filtered posteriors for 80% CI for each param; this means that not all
  ## models have every post num any more, so can't just select 400 random draws 
  ## and expect that each of this will have a row - ea sp x water trt has a unique 
  ## but overlapping set of post nums
  ## since we need the params for both species at once we need 400 specific
  ## numbers from each model that will def work; we can get these from equil sig
  ## calculations
  
  posts = equil_sig %>%
    filter(water == w) %>%
    mutate(focal = species) %>%
    select(focal, post_num)
  
  wdat_filt = left_join(posts, wdat, by = c("focal", "post_num")) %>%
    mutate(new_post_id = rep(1:400, 2))
  
  posts = as.numeric(wdat_filt$new_post_id)
  ## selecting using unique led to losing data as between species some post nums
  ## were repeated
  
  for(j in 1:length(posts)) {
    
    p = posts[j]
    
    dat = wdat_filt %>%
      filter(new_post_id == p)
    
    ## viable seedt
    lam = c(
      dat[dat$focal == "ACAM",]$lam, 
      dat[dat$focal == "BRHO",]$lam
      )
    
    ## seed survival
    s = c(
      seedsurv[seedsurv$species == "ACAM",]$surv.mean.p,
      seedsurv[seedsurv$species == "BRHO",]$surv.mean.p
      )
    
    ## germination
    g = c(
      germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ,
      germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
      ) 
    
    ## INTRAspecific competition
    a_intra = c(
      dat[dat$focal == "ACAM",]$alpha_intra, 
      dat[dat$focal == "BRHO",]$alpha_intra
      )
    
    ## define params for sigmoidal function
    a_init = c(
      dat[dat$focal == "ACAM",]$alpha_initial, 
      dat[dat$focal == "BRHO",]$alpha_initial
      )
    
    a_slope = c(
      dat[dat$focal == "ACAM",]$alpha_slope, 
      dat[dat$focal == "BRHO",]$alpha_slope
      )
    c = c(
      dat[dat$focal == "ACAM",]$c, 
      dat[dat$focal == "BRHO",]$c
      )
    N0 = c(
      dat[dat$focal == "ACAM",]$N_opt, 
      dat[dat$focal == "BRHO",]$N_opt
      )
    
    #A = A*-1
    
    # When there is a value equal to NA, the computation of NDF for that line is stopped
    if (any(is.na(c(lam,s,g)))) next  
    if (any(g*lam<=1)) next
    
    ## empirically solve for equilibrium. 
    N_star = (1/(a_intra*g)) * log( (1 - ((1-g)*s )) / (g * lam) )
    
    ## this equation is analytically solving for equil abundance
    
    ## not sure if it matters, but code as an array as above, just in case
    N_star2 = np$array(as.matrix(rbind(N_star, N_star)))
    
    pars0 = list(N_star = N_star2)
    
    # compute relevant parameters with python
    pars = NFD_model(
      ricker_sigmoid_f, 
      as.integer(n_spec),
      args=list(
        s, 
        g,
        lam,
        a_init,
        c,
        a_slope,
        N0, 
        a_intra
        ),
      pars=pars0, 
      from_R = TRUE
      )
    
    ## to help with counting 
    k = i - 1
    
    ## put params back in df
    NDF_sig$water[(j + (400*k))] = w
    NDF_sig$post_num[(j + (400*k))] = p
    
    NDF_sig$NDi[(j + (400*k))] = pars$ND[1]
    NDF_sig$NDj[(j + (400*k))] = pars$ND[2]
    
    NDF_sig$NOi[(j + (400*k))] = pars$NO[1]
    NDF_sig$NOj[(j + (400*k))] = pars$NO[2]
    
    ## OLD version of fitness diffs; don't use
    #NDF_sig$FDi[i] = pars$FD[1]
    #NDF_sig$FDj[i] = pars$FD[2]
    
    # in the pars - FD is old one, the pars$'F' is the updated
    NDF_sig$FDi[(j + (400*k))] = pars$`F`[1]
    NDF_sig$FDj[(j + (400*k))] = pars$`F`[2]
    
    NDF_sig$ci[(j + (400*k))] = pars$c[3]
    NDF_sig$cj[(j + (400*k))] = pars$c[2]
    
  }
  
}

