

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

##mean parameter values
acam_mp = read.csv("acam_mp.csv")
brho_mp = read.csv("brho_mp.csv")

# create the differential equation system
n_spec <- 2 # number of species in the system, must be an integer
set.seed(0) # set random seed for reproduce ability

# Ricker model ####
## f = percap growth rate of species
ricker_f = function(N, s, g, lam, A) {
  
  #F_i = lmd[1] * exp(-a_brhom[1]*N[1] - a_brhom[2]*N[2])
  #F_j = lmd[2] * exp(-a_acamm[1]*N[2] - a_acamm[2]*N[1])
  #return(as.matrix(c(F_i, F_j)))
  
  return(log((1-g)*s + (g*lam*exp(A%*%(g*N))))) ## why multiplied by 1000?? #1000*
  
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
  lam = c(adat$lam, bdat$lam)
  
  ## seed survival
  s = c(seedsurv[seedsurv$species == "ACAM",]$surv.mean.p, 
        seedsurv[seedsurv$species == "BRHO",]$surv.mean.p)
  
  ## germination
  g = c(germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ,
        germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ) 
  
  # interaction matrix
  A = matrix(data=c(adat$a_aa, # Data$AP_a_ii_exp[i], 
                    bdat$a_ba, # Data$AP_a_ji_exp[i], 
                    adat$a_ab, # Data$AP_a_ij_exp[i], 
                    bdat$a_bb), # Data$AP_a_jj_exp[i]), 
             nrow=n_spec, 
             ncol=n_spec)
  
  #A = A*-1
  
  # When there is a value equal to NA, the computation of NDF for that line is stopped
  if (any(is.na(c(lam,s,g,A)))) next  
  if (any(g*lam<=1)) next
  
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

## in this version plot the niche differences from Spaak 2021 as that seems to be the more useful choice??
NDF_static %>%
  select(water, NDi, NDj, FDi_2, FDj_2) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi_2", "FDj_2"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness")) %>%
  select(-type) %>%
  filter(sp == "ACAM") %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%

ggplot(aes(x=Niche, y= Fitness, shape = sp, fill = as.factor(water))) +
  geom_point(aes(fill = as.factor(water)), size = 3.5) +
  theme_classic() +
 # coord_cartesian(xlim = c(-1, 2), ylim = c(-18, 2)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_vline(xintercept = 1, color = "gray") +
  scale_fill_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  scale_shape_manual(values = c(21, 22)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_hline(yintercept = 1, color = "gray")

ggsave("data_analysis/NFD/figures/static_NFD_2021Fdef.png", width = 7, height = 5)

## Spaak & DeLaender 2020 Fitness version
## i can persist when -F < N/(1-N)
## high F implies implies competitive advantage for species i 
NDF_static %>%
  select(water, NDi, NDj, FDi, FDj) %>%
 # filter(sp == "ACAM") %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness")) %>%
  select(-type)  %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  
  mutate(p_temp = Niche / (1 - Niche),
         p2 = -Fitness) %>%
  
  ggplot(aes(x=Niche, y= -Fitness, shape = sp, fill = as.factor(water))) +
  geom_point(aes(fill = as.factor(water)), size = 3.5) +
  theme_classic() +
  #coord_cartesian(xlim = c(-1, 2), ylim = c(-1.1, 1.1)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_vline(xintercept = 1, color = "gray") +
  scale_fill_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  scale_shape_manual(values = c(21, 22)) +
 # geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_hline(yintercept = 1, color = "gray")# +
#  geom_function(fun = Niche_line)


## I think fitness diffs are plotted as (-F) for the Spaak 2020 version of things??

Niche_line = function(n) {
  
  return(n/(1-n))
  
}










A <- matrix(runif(n_spec^2,0,1), n_spec, n_spec) # interaction matrix
diag(A) <- runif(n_spec, 1,2) # to ensure coexistence
mu <- runif(n_spec,1,2) # intrinsic growth rate
test_f <- function(N){
  return(mu - A%*%N)
}



# Ricker model
A = matrix(data = c(0.05, -0.01, 0.02, 0.1), nrow = 2, ncol = 2)

lmd = c(655, 62)

## water 1 to start
acam_mp


lmd = c(brho_mp[brho_mp$water == 0.6,]$lam, acam_mp[acam_mp$water == 0.6,]$lam)

a_brhom = c(-1*brho_mp[brho_mp$water == 0.6,]$a_bb, -1*brho_mp[brho_mp$water == 0.6,]$a_ba)

a_acamm = c(-1*acam_mp[acam_mp$water == 0.6,]$a_aa, -1*acam_mp[acam_mp$water == 0.6,]$a_ab)


# compute relevant parameters with python
# the parameter `from_R = TRUE` changes data types from R to python
pars <- NFD_model(ricker_f, n_spec, from_R = TRUE)
ND <- pars$ND
NO <- pars$NO
FD <- pars$FD
c <- pars$c

# manualy check results for the two species case
# see appendix for proof of correctness
NO_check = sqrt(A[1,2]*A[2,1]/(A[1,1]*A[2,2]))*c(1,1)
ND_check = 1-NO_check
FD_check = 1- rev(mu)/mu*sqrt(c(A[1,2]*A[1,1]/A[2,1]/A[2,2],
                                A[2,1]*A[2,2]/A[1,2]/A[1,1]))
c_check = sqrt(c(A[1,2]*A[2,2]/A[2,1]/A[1,1],
                 A[2,1]*A[1,1]/A[1,2]/A[2,2]))


###############################################################################
# passing additional arguments

n_spec <- 3 # number of species in the system

# Lotka-Volterra model
A <- matrix(runif(n_spec^2,0,1), n_spec, n_spec) # interaction matrix
diag(A) <- runif(n_spec, n_spec, n_spec+1) # to ensure coexistence in the example
mu <- runif(n_spec,1,2) # intrinsic growth rate
# LV model now depends on additional parameters
test_f <- function(N, mu, A){
  return(mu - A%*%N)
}

# we can pass additional arguments to the test function via the args
# argument. Pass an unnamed list (i.e. not list(mu=mu, A = A)), as
# python will not handle them correctly
pars <- NFD_model(test_f, n_spec, args = list(mu, A), from_R = TRUE)

###############################################################################
# passing additional starting estimates
# NFD_model computes automatically ND and FD for any model, however it's based
# on numerical solving, which requires starting estimates.
# the default for starting estimates is 1, so if your equilibrium abundance
# is very high, it will not find it.
n_spec <- 3
mu <- runif(n_spec, 1,2)*1e10

# will result in an error, as we don't help NFD_model
pars <- NFD_model(test_f, n_spec, args = list(mu, A), from_R = TRUE)

# we don't have to pass the equilibrium densities correctly, just valid
# starting estimates, e.g. N_star = 1e10 will suffice in this case
pars <- list(N_star = matrix(1e10, n_spec, n_spec))
pars <- NFD_model(test_f, n_spec, args = list(mu, A),pars = pars,
                  from_R = TRUE)

# N_star and c can be passed as initial starting guesses for c and N_star
# N_star and c must be matrices with shape (n_spec, n_spec)
# We can pass more specific starting guesses for each species absent
# N_star[1,] should be the densities of the community when species 1 is absent
# The value of N_star[1,1] will be ignored, no matter what
