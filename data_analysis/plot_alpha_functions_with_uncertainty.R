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

source("data_analysis/MCT/find_equilibrium.R")
source("data_analysis/MCT/find_equilibrium_sigmoidal.R")


## set fig location
fig_loc = "data_analysis/models/evaluate/plot_alpha_fecundity/"

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

## get rid of NA's in posteriors - still not sure why these are here? 
acam_sig_posteriors2 = acam_sig_posteriors %>%
  filter(!is.na(lambda))

brho_sig_posteriors2 = brho_sig_posteriors %>%
  filter(!is.na(lambda))

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
igr_sig = data.frame(focal = NA, water = NA, post_num = NA, alpha_inter = NA, igr = NA, dens = NA)

## set post draw list from equil df
posts_sig = unique(equil_sig$post_num)

## create treatment vectors
species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

for(i in 1:length(species)) {
  for (j in 1:length(rain)) {
    
    sp = species[i]
    r = rain[j]
    
    ## select data
    adat = acam_sig_posteriors2[acam_sig_posteriors2$water == r,]
    bdat = brho_sig_posteriors2[brho_sig_posteriors2$water == r,]
    
    ## set treatment
    if(r == 1) { trt = "C" 
    } else { trt = "D" }
    
    ## loop thru each posterior draw
    for (k in 1:length(posts_sig)) {
      
      p = posts_sig[k]
      
      ## define params
      if (sp == "ACAM") {
        
        lambda_i = adat[p,]$lambda
        alpha_ii = adat[p,]$alpha_acam
        ## alpha_ij = adat[p,]$alpha_brho
        
        ## alpha_inter params
        n_opt = adat[p,]$N_opt
        c = adat[p,]$c
        alpha_slope = adat[p,]$alpha_slope
        alpha_init = adat[p,]$alpha_initial
        
        g_i = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "ACAM",]$surv.mean.p
        g_j = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ

        N_eq_v = c(0:45)
        
        
      } else {
        
        lambda_i = bdat[p,]$lambda
        alpha_ii = bdat[p,]$alpha_brho
        ## alpha_ij = bdat[p,]$alpha_acam
        
        n_opt = bdat[p,]$N_opt
        c = bdat[p,]$c
        alpha_slope = bdat[p,]$alpha_slope
        alpha_init = bdat[p,]$alpha_initial
        
        g_i = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "BRHO",]$surv.mean.p
        g_j = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ

        N_eq_v = c(0:45)
        
      }
      
      ## calc IGR
      igr_tmp = igr_sig_func(surv = s_i, germ = g_i, lambda = lambda_i, alpha_intra = alpha_ii, Nt = 1, germ_inter = g_j, inter_abund = N_eq_v, alpha_0 = alpha_init, alpha_slope = alpha_slope, N0 = n_opt, c = c)
      
      ## fill in data
      tmp = data.frame(focal = sp, water = r, post_num = p, igr = igr_tmp$igr, alpha_inter = igr_tmp$a_ij, dens = N_eq_v)
      
      ## append
      igr_sig = rbind(igr_sig, tmp)
      
    }
    
  }
  
}

igr_sig = igr_sig %>%
  filter(!is.na(focal)) %>%
  mutate(model = "sigmoidal")

# Plot ####
## Join ####
names(igr_sig)

names(igr_stat)

igr_both = rbind(igr_sig, igr_stat) %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter))


alphas = igr_both %>%
  filter(!is.na(focal)) %>%
  group_by(model, focal, water, dens) %>%
  
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  
  ungroup() %>%
  
  mutate(focal = fct_relevel(focal, "BRHO", "ACAM")) %>%
  
ggplot(aes(x=dens, y=mean.alpha, group = interaction(model, water), fill = as.factor(water), color = as.factor(water), linetype = model)) +
  geom_ribbon(aes(ymin = mean.alpha - (2*se.alpha), ymax = mean.alpha + (2*se.alpha)), alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  facet_wrap(~focal) +
  scale_fill_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  xlab("Density") +
  ylab("Alpha value") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=13))


igrs = igr_both %>%
  filter(!is.na(focal)) %>%
  group_by(model, focal, water, dens) %>%
  
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  
  ungroup() %>%
  
  mutate(focal = fct_relevel(focal, "BRHO", "ACAM")) %>%
  
  ggplot(aes(x=dens, y=mean.igr, group = interaction(model, water), fill = as.factor(water), color = as.factor(water), linetype = model)) +
  geom_ribbon(aes(ymin = mean.igr - (2*se.igr), ymax = mean.igr + (2*se.igr)), alpha = 0.5) +
  #geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  facet_wrap(~focal, scales = "free") +
  scale_fill_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  xlab("Density") +
  ylab("Invasion Growth Rate") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=13))

ggarrange(alphas, igrs, labels = "AUTO", common.legend = TRUE, ncol = 1, nrow = 2, legend = "bottom")

#ggsave("data_analysis/MCT/figures/alphas_igrs_dens_with0.png", width = 8, height = 7)




igr_both %>%
  filter(!is.na(focal)) %>%
  group_by(model, focal, water, dens) %>%
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  ungroup() %>%
  mutate(focal = fct_relevel(focal, "BRHO", "ACAM")) %>%
  filter(focal == "BRHO") %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  
  ggplot(aes(x=dens, y=mean.alpha, group = interaction(model, water.text), fill = as.factor(water.text), color = as.factor(water.text), linetype = model)) +
  geom_ribbon(aes(ymin = mean.alpha - (3*se.alpha), ymax = mean.alpha + (3*se.alpha)), alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  scale_fill_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  scale_color_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  xlab("Neighbor Density") +
  ylab("Interaction Coefficient") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=15))

#ggsave("data_analysis/MCT/figures/TCD_job_talk/alphas_igrs_dens_modcomp.png", width = 8, height = 5)


## static only first
igr_both %>%
  filter(!is.na(focal)) %>%
  group_by(model, focal, water, dens) %>%
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  ungroup() %>%
  mutate(focal = fct_relevel(focal, "BRHO", "ACAM")) %>%
  filter(focal == "BRHO", model == "static") %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  ggplot(aes(x=dens, y=mean.alpha, group = interaction(model, water.text), fill = as.factor(water.text), color = as.factor(water.text), linetype = model)) +
  geom_ribbon(aes(ymin = mean.alpha - (3*se.alpha), ymax = mean.alpha + (3*se.alpha)), alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
  scale_fill_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  scale_color_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  scale_linetype_manual(values = "dashed") +
  xlab("Neighbor Density") +
  ylab("Interaction Coefficient") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=15))

#ggsave("data_analysis/MCT/figures/TCD_job_talk/alphas_igrs_dens_static.png", width = 7, height = 5)



# Pres Plots ####
igr_both %>%
  filter(!is.na(focal), focal == "BRHO") %>%
  group_by(model, focal, water, dens) %>%
  
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  
  ungroup() %>%
  
#  mutate(focal = fct_relevel(focal, "BRHO", "ACAM")) %>%
  
  ggplot(aes(x=dens, y=mean.alpha, group = interaction(model, water), fill = as.factor(water), color = as.factor(water), linetype = model)) +
  geom_ribbon(aes(ymin = mean.alpha - (2*se.alpha), ymax = mean.alpha + (2*se.alpha)), alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_line(linewidth = 1) +
#  facet_wrap(~focal) +
  scale_fill_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  xlab("Density") +
  ylab("Alpha value") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=13))

ggsave("alpha_v_dens_pres_fig.png", width = 6, height = 4)











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
