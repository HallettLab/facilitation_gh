
## Try altering biological model so I am sure what it is doing


# @author: J.W.Spaak
# Example how to compute the ND and FD for a given differential equation setting

# The script is originally written in python, we run python code from within R
# make sure that python is installed on the system used
# This is not part of the install.packages("reticulate") command!

library(reticulate)
library(tidyverse)

np <- import("numpy",convert=F)

# loads the relevant python code
source_python("data_analysis/NFD/NFD_definitions-master/numerical_NFD.py")

## germination data
germ = read.csv("data/germination_data.csv")

## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")

# create the differential equation system
n_spec <- 2 # number of species in the system, must be an integer
set.seed(0) # set random seed for reproduce ability

# Ricker model ####
## f = percap growth rate of species
ricker_f = function(N, s, g, lam, a_intra, a_inter, g_j, N_j) {
  
  #F_i = lmd[1] * exp(-a_brhom[1]*N[1] - a_brhom[2]*N[2])
  #F_j = lmd[2] * exp(-a_acamm[1]*N[2] - a_acamm[2]*N[1])
  #return(as.matrix(c(F_i, F_j)))
  
#  return(log((1-g)*s + (g*lam* exp(A %*% (g*1000*N)))  )) ## why multiplied by 1000??
  
  return(log((1-g)*s + (g*lam* exp(a_intra*g*N + a_inter*g_j*N_j))  )) 
  
} # log to transform the discrete time into continuous


NDF_static <- matrix(NA,3,11) ## 6 rows (2 sp x 3 water); 8 cols
NDF_static <- as.data.frame(NDF_static)
names(NDF_static )<- c("water", "NDi","NDj","NOi",
                       "NOj","FDi","FDj", "FDi_2", "FDj_2",
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
  
  adat = acam_mp[acam_mp$water == w,]
  bdat = brho_mp[brho_mp$water == w,]
  
  ## viable seedt
  #lam = c(adat$lam, bdat$lam)
  lam = adat$lam
  ## seed survival
  #s = c(seedsurv[seedsurv$species == "ACAM",]$surv.mean.p, 
   #     seedsurv[seedsurv$species == "BRHO",]$surv.mean.p)
  
  s_a = seedsurv[seedsurv$species == "ACAM",]$surv.mean.p
  
  ## germination
  #g = c(germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ,
   #     germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ) 
  
  g_a = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
  g_b = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
  
  # interaction matrix
  #A = matrix(data=c(adat$a_aa, # Data$AP_a_ii_exp[i], 
   #                 bdat$a_ba, # Data$AP_a_ji_exp[i], 
    #                adat$a_ab, # Data$AP_a_ij_exp[i], 
     #               bdat$a_bb), # Data$AP_a_jj_exp[i]), 
      #       nrow=n_spec, 
       #      ncol=n_spec)
  
  a_intra = adat$a_aa
  a_inter = adat$a_ab
  
  #A = A*-1

  ## I think this is empirically solving for equilibrium. 
  N_star = ((log(1-(1-g)*s)/(lam*g))/A) # not needed for lotka volterra
  N_star[2,1] <- N_star[1,1]
  N_star[1,2] <- N_star[2,2] # 
  N_star = np$array(N_star) 
  N_star$setflags(write = TRUE)
  pars0 = list(N_star = N_star) # 
  
  #print(N_star)
  # compute relevant parameters with python
  # the parameter `from_R = TRUE` changes data types from R to python
  pars <- NFD_model(ricker_f, n_spec, args=list(s,g,lam,A), from_R = TRUE,
                    pars=pars0)
  
  ## put params back in df
  NDF_static$water[i] = w
  
  NDF_static$NDi[i] <- pars$ND[1]
  NDF_static$NDj[i] <- pars$ND[2]
  
  NDF_static$NOi[i] <- pars$NO[1]
  NDF_static$NOj[i] <- pars$NO[2]
  
  NDF_static$FDi[i] <- pars$FD[1]
  NDF_static$FDj[i] <- pars$FD[2]
  
  NDF_static$FDi_2[i] <- pars$`F`[1]
  NDF_static$FDj_2[i] <- pars$`F`[2]
  
  NDF_static$ci[i] <- pars$c[3]
  NDF_static$cj[i] <- pars$c[2]
  
}


names(NDF_static)

NFD_plot = NDF_static %>%
  select(water, NDi, NDj, FDi, FDj) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness")) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val")

ggplot(NFD_plot, aes(x=Niche, y= Fitness, shape = sp, fill = as.factor(water))) +
  geom_point(aes(fill = as.factor(water)), size = 3.5) +
  theme_classic() +
  coord_cartesian(xlim = c(-0.1, 2), ylim = c(-1, 1)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_vline(xintercept = 1, color = "gray") +
  scale_fill_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  scale_shape_manual(values = c(21, 22)) +
  geom_abline(slope = 1, intercept = 0)
