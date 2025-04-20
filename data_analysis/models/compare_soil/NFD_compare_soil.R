

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

np <- import("numpy",convert=F)

# loads the relevant python code
# use python code 
#source("/Users/lisabuche/Documents/Projects/Spaak/numerical_NFD.py")

source_python("data_analysis/NFD/NFD_definitions-master/numerical_NFD.py")

## germination data
germ = read.csv("data/germination_data.csv")

## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")

## parameter values
sigposts = read.csv("data/model_posteriors/sig_posts_20250401.csv")
statposts = read.csv("data/model_posteriors/stat_posts_20250401.csv")

## post draw numbers
## get these from sigmoidal equilibrium vals 
## these will give same posterior draws used in invasion growth rate calcs, etc. 
equil_sig = read.csv("data_analysis/MCT/output/equil_sig.csv")
equil_stat = read.csv("data_analysis/MCT/output/equil_stat.csv")

# create the differential equation system
n_spec = 2 # number of species in the system, must be an integer
set.seed(0) # set random seed for reproduce ability

# Define models ####
## plain ricker ####
ricker_f = function(N, s, g, lam, A) {
  
  return(log((1-g)*s + (g*lam*exp(A%*%(g*N)))))
  
} # log to transform the discrete time into continuous

## ricker exponential ####
ricker_exp_f = function(N, s, g, lam, a_init,c,a_slope,N0){ 
  
  A = a_init + c*(1-exp(a_slope*(N-N0)))
  return(log((1-g)*s + (g*lam*exp(A%*%(g*N)))))
  
} 

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

# Prep Data ####
## Sig ####
### Calc 80% HDI ####
sigposts80 = sigposts %>%
  select(-disp) %>%
  group_by(focal, water) %>%
  filter(lambda >= hdi(lambda, credMass = 0.8)["lower"],
         lambda <= hdi(lambda, credMass = 0.8)["upper"],
         N_opt >= hdi(N_opt, credMass = 0.8)["lower"],
         N_opt <= hdi(N_opt, credMass = 0.8)["upper"],
         c >= hdi(c, credMass = 0.8)["lower"],
         c <= hdi(c, credMass = 0.8)["upper"],
         alpha_slope >= hdi(alpha_slope, credMass = 0.8)["lower"],
         alpha_slope <= hdi(alpha_slope, credMass = 0.8)["upper"],
         alpha_initial >= hdi(alpha_initial, credMass = 0.8)["lower"],
         alpha_initial <= hdi(alpha_initial, credMass = 0.8)["upper"],
         alpha_intra >= hdi(alpha_intra, credMass = 0.8)["lower"],
         alpha_intra <= hdi(alpha_intra, credMass = 0.8)["upper"])

## Stat ####
acam = statposts %>%
  filter(focal == "ACAM") %>%
  mutate(alpha_intra = alpha_acam,
         alpha_inter = alpha_brho) %>%
  select(-alpha_acam, -alpha_brho)

brho = statposts %>%
  filter(focal == "BRHO") %>%
  mutate(alpha_intra = alpha_brho,
         alpha_inter = alpha_acam) %>%
  select(-alpha_acam, -alpha_brho)

stat2 = rbind(acam, brho)

### Calc 80% HDI ####
statposts80 = stat2 %>%
  select(-disp) %>%
  group_by(focal, water) %>%
  filter(lambda >= hdi(lambda, credMass = 0.8)["lower"],
         lambda <= hdi(lambda, credMass = 0.8)["upper"],
         alpha_intra >= hdi(alpha_intra, credMass = 0.8)["lower"],
         alpha_intra <= hdi(alpha_intra, credMass = 0.8)["upper"],
         alpha_inter >= hdi(alpha_inter, credMass = 0.8)["lower"],
         alpha_inter <= hdi(alpha_inter, credMass = 0.8)["upper"])

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
  
  ## filter data to get only certain posterior draw numbers
  ##have to do this in a convoluted way 
  ## filtered posteriors for 80% CI for each param; this means that not all
  ## models have every post num any more, so can't just select 400 random draws 
  ## and expect that each of this will have a row - ea spxwater trt has a unique 
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
  
  posts2 = unique(wdat_filt$new_post_id)
  
  for(j in 1:length(posts2)) {
    
    p = posts2[j]
    
    dat = wdat_filt %>%
      filter(new_post_id == p)
    
    ## viable seedt
    lam = c(dat[dat$focal == "ACAM",]$lam, dat[dat$focal == "BRHO",]$lam)
    
    ## seed survival
    s = c(seedsurv[seedsurv$species == "ACAM",]$surv.mean.p, 
          seedsurv[seedsurv$species == "BRHO",]$surv.mean.p)
    
    ## germination
    g = c(germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ,
          germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ) 
    
    ## INTRAspecific competition
    a_intra = c(dat[dat$focal == "ACAM",]$alpha_intra, dat[dat$focal == "BRHO",]$alpha_intra)
    
    ## define params for sigmoidal function
    a_init = c(dat[dat$focal == "ACAM",]$alpha_initial, dat[dat$focal == "BRHO",]$alpha_initial)
    a_slope = c(dat[dat$focal == "ACAM",]$alpha_slope, dat[dat$focal == "BRHO",]$alpha_slope)
    c = c(dat[dat$focal == "ACAM",]$c, dat[dat$focal == "BRHO",]$c)
    N0 = c(dat[dat$focal == "ACAM",]$N_opt, dat[dat$focal == "BRHO",]$N_opt)
    
    #A = A*-1
    
    # When there is a value equal to NA, the computation of NDF for that line is stopped
    if (any(is.na(c(lam,s,g)))) next  
    if (any(g*lam<=1)) next
    
    ## I think this is empirically solving for equilibrium. 
    ## I don't understand why it goes into an array like this? 
    
    # ``N_star`` : ndarray (shape = (n_spec, n_spec))
    # N_star[i] starting guess for equilibrium density with species `i`
    # absent. N_star[i,i] is set to 0
    
    #  N_star = ((log((1-(1-g)*s)/(lam*g)))/A) # not needed for lotka volterra
    #  N_star[2,1] <- N_star[1,1]
    #  N_star[1,2] <- N_star[2,2] # 
    #  N_star = np$array(N_star) 
    #  N_star$setflags(write = TRUE)
    #  pars0 = list(N_star = N_star) # 
    
    ## re-coding so I can follow where everything is going
    N_star = (1/(a_intra*g)) * log( (1 - ((1-g)*s )) / (g * lam) )
    
    ## this equation is analytically solving for equil abundance
    
    ## not sure if it matters, but code as an array as above, just in case
    N_star2 = np$array(as.matrix(rbind(N_star, N_star)))
    
    pars0 = list(N_star = N_star2)
    
    # compute relevant parameters with python
    # the parameter `from_R = TRUE` changes data types from R to python
    ## from_R is necessary for it to run in R - when this was removed, was getting an error that the output of the function needed to be an array of length n_spec
    pars = NFD_model(ricker_sigmoid_f, as.integer(n_spec), args=list(s, g, lam, a_init, c, a_slope, N0, a_intra), pars=pars0, from_R = TRUE)
    
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

### Plot ####
## in this version plot the niche differences from Spaak 2021 as that seems to be the more useful choice??
# this is the correct one! 
NDF_sig %>%
  select(water, NDi, NDj, FDi, FDj) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness")) %>%
  select(-type) %>%
  #filter(sp == "ACAM") %>%
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
  geom_hline(yintercept = 1, color = "gray") +
  guides(fill = guide_legend("Legend fill", override.aes = list(shape = 21)))

ggsave("data_analysis/NFD/figures/static_NFD_2021Fdef.png", width = 7, height = 5)

# Fig 5 ####
NDF_sig_means = NDF_sig %>% 
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness")) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  group_by(water, sp) %>%
  summarise(mean_niche = mean(Niche), 
            mean_fitness = mean(Fitness)) %>%
  mutate(water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate")))


NDF_sig %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  ggplot(aes(x=Niche, y= Fitness, shape = sp, color = as.factor(water))) +
  geom_point(alpha = 0.25, aes(fill = as.factor(water))) +
  geom_point(data = NDF_sig_means, aes(x=mean_niche, y=mean_fitness, fill = as.factor(water)), size = 4, color = "black") +
  theme_classic() +
  geom_vline(xintercept = 0, color = "gray") +
  geom_vline(xintercept = 1, color = "gray") +
  scale_color_manual(values = c("#70a494", "#f3d0ae","#de8a5a")) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae","#de8a5a")) +
  scale_shape_manual(values = c(21, 22)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_hline(yintercept = 1, color = "gray") +
  guides(fill = guide_legend("Water", override.aes = list(shape = 21))) +
  labs(color = "Water", shape = "Species") +
  xlab("Niche Differences") +
  ylab("Fitness Differences") +
  theme(text = element_text(size=14))

ggsave("figures/Apr2025/Fig5_NFD_sig.png", width = 7, height = 5)

## Static ####
NDF_static = as.data.frame(matrix(NA,3*400,16)) ## 3 rows (water treatments); 11 cols
names(NDF_static) = c("water", "post_num", "NDi","NDj","NOi",
                      "NOj","FDi","FDj",
                      "ci","cj", 
                      "etai", "etaj", 
                      "mui", "muj", 
                      "ri", "rj")

water = c(0.6, 0.75, 1)

for(i in 1:length(water)){
  
  w = water[i]
  
  if (w == 0.6) {
    trt = "D"
  } else {
    trt = "C"
  }
  
  wdat = statposts80[statposts80$water == w,]
  
  ## filter data to get only certain posterior draw numbers
  ##have to do this in a convoluted way 
  ## filtered posteriors for 80% CI for each param; this means that not all
  ## models have every post num any more, so can't just select 400 random draws 
  ## and expect that each of this will have a row - ea spxwater trt has a unique 
  ## but overlapping set of post nums
  ## since we need the params for both species at once we need 400 specific
  ## numbers from each model that will def work; we can get these from equil sig
  ## calculations
  posts = equil_stat %>%
    filter(water == w) %>%
    mutate(focal = species) %>%
    select(focal, post_num)
  
  wdat_filt = left_join(posts, wdat, by = c("focal", "post_num")) %>%
    mutate(new_post_id = rep(1:400, 2))
  
  posts2 = unique(wdat_filt$new_post_id)
  
  for(j in 1:length(posts2)) {
    
    p = posts2[j]
    
    dat = wdat_filt %>%
      filter(new_post_id == p)
    
    ## viable seedt
    lam = c(dat[dat$focal == "ACAM",]$lambda, dat[dat$focal == "BRHO",]$lambda)
    
    ## seed survival
    s = c(seedsurv[seedsurv$species == "ACAM",]$surv.mean.p, 
          seedsurv[seedsurv$species == "BRHO",]$surv.mean.p)
    
    ## germination
    g = c(germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ,
          germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ) 
    
    ## define this within the ricker_sigmoid_f function now
    # interaction matrix
    A = matrix(data=c(dat[dat$focal == "ACAM",]$alpha_intra, # Data$AP_a_ii_exp[i],
                      dat[dat$focal == "BRHO",]$alpha_inter, # Data$AP_a_ji_exp[i],
                      dat[dat$focal == "ACAM",]$alpha_inter, # Data$AP_a_ij_exp[i], 
                      dat[dat$focal == "BRHO",]$alpha_intra), # Data$AP_a_jj_exp[i]), 
               nrow=n_spec, 
               ncol=n_spec)
    
    a_intra = c(dat[dat$focal == "ACAM",]$alpha_intra, dat[dat$focal == "BRHO",]$alpha_intra)
    #A = A*-1
    
    # When there is a value equal to NA, the computation of NDF for that line is stopped
    if (any(is.na(c(lam,s,g)))) next  
    if (any(g*lam<=1)) next
    
    ## recoding so I can follow where everything is going
    ##create a vector of alpha_intra just for this
    
    N_star = (1/(a_intra*g)) * log( (1 - ((1-g)*s )) / (g * lam) )
    ## do we want this as an array? try putting it as just a vector of 2 values and if that doesn't work, change to array! 
    
    N_star2 = np$array(as.matrix(rbind(N_star, N_star)))
    
    pars0 = list(N_star = N_star2)
    #print(N_star)
    # compute relevant parameters with python
    # the parameter `from_R = TRUE` changes data types from R to python
    pars = NFD_model(ricker_f, as.integer(n_spec), args=list(s, g, lam, A), pars=pars0, from_R = TRUE)
    
    ## to help with counting 
    k = i - 1
    
    ## put params back in df
    NDF_static$water[(j + (400*k))] = w
    NDF_static$post_num[(j + (400*k))] = p
    
    NDF_static$NDi[(j + (400*k))] = pars$ND[1]
    NDF_static$NDj[(j + (400*k))] = pars$ND[2]
    
    NDF_static$NOi[(j + (400*k))] = pars$NO[1]
    NDF_static$NOj[(j + (400*k))] = pars$NO[2]
    
    NDF_static$FDi[(j + (400*k))] = pars$`F`[1]
    NDF_static$FDj[(j + (400*k))] = pars$`F`[2]
    
    NDF_static$ci[(j + (400*k))] = pars$c[3]
    NDF_static$cj[(j + (400*k))] = pars$c[2]
    
    NDF_static$etai[(j + (400*k))] = pars$eta[1]
    NDF_static$etaj[(j + (400*k))] = pars$eta[2]
    
    NDF_static$mui[(j + (400*k))] = pars$mu[1]
    NDF_static$muj[(j + (400*k))] = pars$mu[2]
    
    NDF_static$ri[(j + (400*k))] = pars$r_i[1]
    NDF_static$rj[(j + (400*k))] = pars$r_i[2]
    
  }
  
}

# Fig SXX ####
NDF_stat_means = NDF_static %>% 
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  filter(FDj > -75) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness")) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  group_by(water, sp) %>%
  summarise(mean_niche = mean(Niche), 
            mean_fitness = mean(Fitness)) %>%
  mutate(water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) #%>%
# filter(sp == "ACAM")


NDF_static %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  #filter(Fitness > -75) %>%
  #filter(sp == "ACAM") %>%
  
  ggplot(aes(x=Niche, y= Fitness, shape = sp, color = as.factor(water))) +
  geom_point(alpha = 0.25, aes(fill = as.factor(water))) +
  geom_point(data = NDF_stat_means, aes(x=mean_niche, y=mean_fitness, fill = as.factor(water)), size = 4, color = "black") +
  theme_classic() +
  geom_vline(xintercept = 0, color = "gray") +
  geom_vline(xintercept = 1, color = "gray") +
  scale_color_manual(values = c("#70a494", "#f3d0ae","#de8a5a")) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae","#de8a5a")) +
  scale_shape_manual(values = c(21, 22)) +
  #geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_hline(yintercept = 1, color = "gray") +
  guides(fill = guide_legend("Water", override.aes = list(shape = 21))) +
  labs(color = "Water", shape = "Species") +
  xlab("Niche Differences") +
  ylab("Fitness Differences") +
  theme(text = element_text(size=14))

ggsave("figures/Apr2025/Fig5_NFD_sig.png", width = 7, height = 5)


test = NDF_static %>%
  filter(FDj < -10000)


## Plot ####
NDF_static %>%
  select(water, NDi, NDj, FDi, FDj) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness")) %>%
  select(-type) %>%
  #filter(sp == "ACAM") %>%
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
  geom_hline(yintercept = 1, color = "gray") +
  guides(fill = guide_legend("Legend fill", override.aes = list(shape = 21)))




ggplot(NDF_static, aes(x=etaj)) +
  geom_histogram() +
  facet_wrap(~water)

ggplot(NDF_static, aes(x=muj)) +
  geom_histogram() +
  facet_wrap(~water)

ggplot(NDF_static, aes(x=rj)) +
  geom_histogram() +
  facet_wrap(~water)

ggplot(NDF_static, aes(x=ci)) +
  geom_histogram() +
  facet_wrap(~water, scales = "free") 

ggplot(NDF_static, aes(x=cj)) +
  geom_histogram() +
  facet_wrap(~water, scales = "free") 

## cj - don't know which way this conversion factor goes?? But these estimates are much more specific than estimates of ci

## cij converts sp j to i and cji converts sp i to j
##cij = 1/cji


NDF_static %>%
  mutate(test_c = 1/ci) %>%
  
  ggplot(aes(x=test_c, y=cj)) +
  geom_point()
## yep, that works


# Extras ####

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
