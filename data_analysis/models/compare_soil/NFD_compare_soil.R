

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

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

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
#sigposts = read.csv("data/model_posteriors/sig_posts_20250401.csv")
#statposts = read.csv("data/model_posteriors/stat_posts_20250401.csv")

source("data_analysis/models/compare_soil/calc_equilibrium_soil.R") ## load this for the moment as it will give equil vals + formatted posteriors

## post draw numbers
## get these from sigmoidal equilibrium vals 
## these will give same posterior draws used in invasion growth rate calcs, etc. 
#equil_sig = read.csv("data_analysis/MCT/output/equil_sig.csv")
#equil_stat = read.csv("data_analysis/MCT/output/equil_stat.csv")

# create the differential equation system
n_spec = 2 # number of species in the system, must be an integer
set.seed(25) # set random seed for reproduce ability

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

# Calc NFD ####

## m0 ####
NDF_m0 = as.data.frame(matrix(NA,3*400,16)) ## 3 rows (water treatments); 11 cols
names(NDF_m0) = c("water", "post_num", "NDi","NDj","NOi",
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
  
  wdat = m0statposts80[m0statposts80$water == w,]
  
  ## filter data to get only certain posterior draw numbers
  ##have to do this in a convoluted way 
  ## filtered posteriors for 80% CI for each param; this means that not all
  ## models have every post num any more, so can't just select 400 random draws 
  ## and expect that each of this will have a row - ea spxwater trt has a unique 
  ## but overlapping set of post nums
  ## since we need the params for both species at once we need 400 specific
  ## numbers from each model that will def work; we can get these from equil sig
  ## calculations
  posts = m0equil %>%
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
    NDF_m0$water[(j + (400*k))] = w
    NDF_m0$post_num[(j + (400*k))] = p
    
    NDF_m0$NDi[(j + (400*k))] = pars$ND[1]
    NDF_m0$NDj[(j + (400*k))] = pars$ND[2]
    
    NDF_m0$NOi[(j + (400*k))] = pars$NO[1]
    NDF_m0$NOj[(j + (400*k))] = pars$NO[2]
    
    NDF_m0$FDi[(j + (400*k))] = pars$`F`[1]
    NDF_m0$FDj[(j + (400*k))] = pars$`F`[2]
    
    NDF_m0$ci[(j + (400*k))] = pars$c[3]
    NDF_m0$cj[(j + (400*k))] = pars$c[2]
    
    NDF_m0$etai[(j + (400*k))] = pars$eta[1]
    NDF_m0$etaj[(j + (400*k))] = pars$eta[2]
    
    NDF_m0$mui[(j + (400*k))] = pars$mu[1]
    NDF_m0$muj[(j + (400*k))] = pars$mu[2]
    
    NDF_m0$ri[(j + (400*k))] = pars$r_i[1]
    NDF_m0$rj[(j + (400*k))] = pars$r_i[2]
    
  }
  
}


## m1 ####
NDF_m1 = as.data.frame(matrix(NA,3*400,16)) ## 3 rows (water treatments); 11 cols
names(NDF_m1) = c("water", "post_num", "NDi","NDj","NOi",
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
  
  wdat = m1statposts80[m1statposts80$water == w,]
  
  ## filter data to get only certain posterior draw numbers
  ##have to do this in a convoluted way 
  ## filtered posteriors for 80% CI for each param; this means that not all
  ## models have every post num any more, so can't just select 400 random draws 
  ## and expect that each of this will have a row - ea spxwater trt has a unique 
  ## but overlapping set of post nums
  ## since we need the params for both species at once we need 400 specific
  ## numbers from each model that will def work; we can get these from equil sig
  ## calculations
  posts = m1equil %>%
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
    NDF_m1$water[(j + (400*k))] = w
    NDF_m1$post_num[(j + (400*k))] = p
    
    NDF_m1$NDi[(j + (400*k))] = pars$ND[1]
    NDF_m1$NDj[(j + (400*k))] = pars$ND[2]
    
    NDF_m1$NOi[(j + (400*k))] = pars$NO[1]
    NDF_m1$NOj[(j + (400*k))] = pars$NO[2]
    
    NDF_m1$FDi[(j + (400*k))] = pars$`F`[1]
    NDF_m1$FDj[(j + (400*k))] = pars$`F`[2]
    
    NDF_m1$ci[(j + (400*k))] = pars$c[3]
    NDF_m1$cj[(j + (400*k))] = pars$c[2]
    
    NDF_m1$etai[(j + (400*k))] = pars$eta[1]
    NDF_m1$etaj[(j + (400*k))] = pars$eta[2]
    
    NDF_m1$mui[(j + (400*k))] = pars$mu[1]
    NDF_m1$muj[(j + (400*k))] = pars$mu[2]
    
    NDF_m1$ri[(j + (400*k))] = pars$r_i[1]
    NDF_m1$rj[(j + (400*k))] = pars$r_i[2]
    
  }
  
}




















# Plot ####
## Fig SXX ####
NDF_stat_means = NDF_m0 %>% 
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
#  filter(FDj > -75) %>%
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


NDF_m0 %>%
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
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_hline(yintercept = 1, color = "gray") +
  guides(fill = guide_legend("Water", override.aes = list(shape = 21))) +
  labs(color = "Water", shape = "Species") +
  xlab("Niche Differences") +
  ylab("Fitness Differences") +
  theme(text = element_text(size=14)) +
  ggtitle("Sterilized Soil")

ggsave("figures/Apr2025/m0_NFD_stat.png", width = 7, height = 5)



## Fig SXX ####
NDF_stat_meansm1 = NDF_m1 %>% 
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
#filter(sp == "ACAM")


NDF_m1 %>%
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
  geom_point(data = NDF_stat_meansm1, aes(x=mean_niche, y=mean_fitness, fill = as.factor(water)), size = 4, color = "black") +
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
  theme(text = element_text(size=14)) +
  ggtitle("Live Soil")

ggsave("figures/Apr2025/m1_NFD_stat.png", width = 7, height = 5)



# Compare BRHO ND, FD ####
m1brho = NDF_m1 %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  #filter(Fitness > -75) %>%
  filter(sp == "BRHO")  %>%
  mutate(microbe = 1)

m0brho = NDF_m0 %>%
  select(water, NDi, NDj, FDi, FDj, post_num) %>%
  pivot_longer(cols = c("NDi", "NDj", "FDi", "FDj"), values_to = "val", names_to = "type") %>%
  mutate(sp = ifelse(substr(type, start = 3, stop = 3) == "i", "ACAM", "BRHO"),
         metric = ifelse(substr(type, start = 1, stop = 2) == "ND", "Niche", "Fitness"), 
         water = ifelse(water == 0.6, "Low", 
                        ifelse(water == 1, "High", "Intermediate"))) %>%
  select(-type) %>%
  pivot_wider(names_from = "metric", values_from = "val") %>%
  #filter(Fitness > -75) %>%
  filter(sp == "BRHO") %>%
  mutate(microbe = 0)

FD_brho = rbind(m1brho, m0brho)

FD_brho %>%
  group_by(water, microbe) %>%
  summarise(mean_ND = mean(Niche), 
            se_ND = calcSE(Niche),
            mean_FD = mean(Fitness)) %>%
  ggplot(aes(x=as.factor(microbe), y=mean_ND)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin = mean_ND - se_ND, ymax = mean_ND + se_ND), width = 0.25) +
  facet_wrap(~water)

FD_brho %>%
  group_by(water, microbe) %>%
  summarise(mean_ND = mean(Niche), 
            mean_FD = mean(Fitness),
            se_FD = calcSE(Fitness)) %>%
  ggplot(aes(x=as.factor(microbe), y=mean_FD)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin = mean_FD - se_FD, ymax = mean_FD + se_FD), width = 0.25) +
  facet_wrap(~water, scales = "free")






## Explore ####
test = NDF_static %>%
  filter(FDj < -10000)


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
