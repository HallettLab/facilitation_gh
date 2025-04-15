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
source("data_analysis/MCT/find_equilibrium_sigmoidal.R")

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

# Plot ####
## Join ####
#names(igr_sig)

#names(igr_stat)

#igr_both = rbind(igr_sig, igr_stat) %>%
#  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter))

# Fig 3 ####
## brho alpha ####
brho_alpha = igr_sig %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  group_by(model, focal, water, dens) %>%
  summarise(mean.igr = mean(igr), 
            min.igr = min(igr),
            max.igr = max(igr),
            se.igr = calcSE(igr),
            
            mean.alpha = mean(alpha_inter), 
            min.alpha = min(alpha_inter),
            max.alpha = max(alpha_inter),
            se.alpha = calcSE(alpha_inter)) %>%
  ungroup() %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  filter(focal == "BRHO") %>%
ggplot(aes(x=dens, y=mean.alpha, fill = as.factor(water.text), color = as.factor(water.text))) +
  geom_ribbon(aes(ymin = (mean.alpha - (2*se.alpha)), ymax = (mean.alpha + (2*se.alpha))), alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.5) +
  scale_fill_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  scale_color_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  xlab(" ") +
  ylab("Interspecific Alpha") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=13)) +
  coord_cartesian(ylim = c(-0.06, 0.15))
#+
 # coord_cartesian(xlim = c(0,50)) #, ylim = c(-0.06, 0.15)

## acam alpha ####
acam_alpha = igr_sig %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  group_by(model, focal, water, dens) %>%
  summarise(mean.igr = mean(igr), 
            min.igr = min(igr),
            max.igr = max(igr),
            se.igr = calcSE(igr),
            
            mean.alpha = mean(alpha_inter), 
            min.alpha = min(alpha_inter),
            max.alpha = max(alpha_inter),
            se.alpha = calcSE(alpha_inter)) %>%
  ungroup() %>%
  filter(focal == "ACAM") %>%
  ggplot(aes(x=dens, y=mean.alpha, fill = as.factor(water), color = as.factor(water))) +
  geom_ribbon(aes(ymin = (mean.alpha - (2*se.alpha)), ymax = (mean.alpha + (2*se.alpha))), alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.5) +
  scale_fill_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  xlab(" ") +
  ylab(" ") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=13)) +
  coord_cartesian(ylim = c(-0.06, 0.15))
  #coord_cartesian(xlim = c(0,50)) #+
  #coord_cartesian(xlim = c(0,30), ylim = c(-0.06, 0.15))

## brho igr ####
brho_igr = igr_sig %>%
  filter(focal == "BRHO") %>%
  group_by(model, focal, water, dens) %>%
  
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  
  ggplot(aes(x=dens, y=mean.igr, fill = as.factor(water), color = as.factor(water))) +
  geom_ribbon(aes(ymin = mean.igr - (2*se.igr), ymax = mean.igr + (2*se.igr)), alpha = 0.5) +
  geom_line(linewidth = 0.5) +
  scale_fill_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  xlab("Neighbor Density") +
  ylab("Invasion Growth Rate") +
  labs(fill = "Water", color = "Water") +
  theme(text = element_text(size=13)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(-2,8))

## acam igr ####
acam_igr = igr_sig %>%
  filter(focal == "ACAM") %>%
  group_by(model, focal, water, dens) %>%
  
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  
  ggplot(aes(x=dens, y=mean.igr, fill = as.factor(water), color = as.factor(water))) +
  geom_ribbon(aes(ymin = mean.igr - (2*se.igr), ymax = mean.igr + (2*se.igr)), alpha = 0.5) +
  geom_line(linewidth = 0.5) +
  scale_fill_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  scale_color_manual(values = c("#de8a5a", "#edbb8a", "#70a494")) +
  xlab("Neighbor Density") +
  ylab(" ") +
  labs(fill = "Water", color = "Water") +
  theme(text = element_text(size=13)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(-2,8))

ggarrange(brho_alpha, acam_alpha, brho_igr, acam_igr, labels = "AUTO", common.legend = TRUE, ncol = 2, nrow = 2, legend = "bottom", align = c("v"))
#

#ggsave("figures/Apr2025/Fig3_alphas_igrs_dens_with0.png", width = 5.8, height = 6)



# Fig 5 ####
## create mean df ####
bigr_mean = igr_sig %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
 # filter(focal == "BRHO") %>%
  group_by(model, focal, water, dens) %>%
  
  summarise(mean.igr = mean(igr), 
            se.igr = calcSE(igr),
            mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low")),
         focal = as.factor(focal), 
         focal = fct_relevel(focal, "BRHO", "ACAM"))

## brho alpha 
## alpha
ba = igr_sig %>%
  filter(focal == "BRHO") %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y = alpha_inter,  fill = as.factor(water.text))) +
  # geom_point(alpha = 0.15, size = 0.25, pch = 21, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~focal) +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  ylab("Interspecific Alpha") +
  xlab(" ") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 0.75) +
  guides(fill = guide_legend("Water", override.aes = list(size = 3, alpha = 0.95))) +
  theme(text = element_text(size=15)) #+
 # coord_cartesian(ylim = c(-0.08, 0.2))

## acam alpha
aa = igr_sig %>%
  filter(focal == "ACAM") %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  ggplot(aes(x=dens, y = alpha_inter,  fill = as.factor(water.text))) +
  #geom_point(alpha = 0.15, size = 0.25, pch = 21, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~focal) +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  ylab(" ") +
  xlab(" ") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "ACAM",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "ACAM",], aes(x=dens, y=mean.alpha, group = water.text, color = water.text), linewidth = 0.75) +
  guides(fill = guide_legend("Water", override.aes = list(size = 3, alpha = 0.95))) +
  theme(text = element_text(size=15))# +
  #coord_cartesian(ylim = c(-0.08, 0.2))

## brho igr
bigr = igr_sig %>%
  filter(focal == "BRHO") %>%
  mutate(water.text = ifelse(water == 1, "High",
                 ifelse(water == 0.75, "Intermediate",
                        "Low"))) %>%
  
  ggplot(aes(x=dens, y = igr,  fill = as.factor(water.text))) +
  
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  #geom_point(alpha = 0.15, size = 0.25, pch = 21, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~focal) +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  ylab("Invasion Growth Rate") +
  xlab("Neighbor Density") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.igr, group = interaction(focal, water), color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "BRHO",], aes(x=dens, y=mean.igr, group = interaction(focal, water), color = water.text), linewidth = 0.75) +
  guides(fill = guide_legend("Water", override.aes = list(size = 3, alpha = 0.95))) +
  theme(text = element_text(size=15)) +
  coord_cartesian(ylim = c(-2.5, 8))

#ggsave("test.png", width = 5, height = 3)
aigr = igr_sig %>%
  filter(focal == "ACAM") %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  
  ggplot(aes(x=dens, y = igr,  fill = as.factor(water.text))) +
  
  scale_fill_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  
  #geom_point(alpha = 0.15, size = 0.25, pch = 21, color = "white") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #facet_wrap(~focal) +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  ylab(" ") +
  xlab("Neighbor Density") +
  labs(color = "Water", linetype = "Focal Species") +
  geom_line(data = bigr_mean[bigr_mean$focal == "ACAM",], aes(x=dens, y=mean.igr, group = interaction(focal, water), color = water.text), linewidth = 1.25, color = "black") +
  geom_line(data = bigr_mean[bigr_mean$focal == "ACAM",], aes(x=dens, y=mean.igr, group = interaction(focal, water), color = water.text), linewidth = 0.75) +
  guides(color = guide_legend("Water", override.aes = list(size = 3, alpha = 1, linewidth = 1))) +
  theme(text = element_text(size=15)) +
  coord_cartesian(ylim = c(-2.5, 8))


ggarrange(ba, aa, bigr, aigr, labels = "AUTO", common.legend = T, ncol = 2, nrow = 2, legend = "bottom", align = c("v"))

ggsave("figures/Apr2025/Fig3_alpha_igr_dens.png", width = 7, height = 7)















#ggsave("figures/Apr2025/Fig5_igr_dens4.png", width = 7, height = 3.5)



























# OLD ####
brho_alpha = igr_sig %>%
  mutate(alpha_inter = ifelse(dens == 0, 0, alpha_inter)) %>%
  group_by(model, focal, water, dens) %>%
  summarise(mean.igr = mean(igr), 
            min.igr = min(igr),
            max.igr = max(igr),
            se.igr = calcSE(igr),
            
            mean.alpha = mean(alpha_inter), 
            min.alpha = min(alpha_inter),
            max.alpha = max(alpha_inter),
            se.alpha = calcSE(alpha_inter)) %>%
  ungroup() %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low"))) %>%
  filter(focal == "BRHO") %>%
  ggplot(aes(x=dens, y=mean.alpha, fill = as.factor(water.text), color = as.factor(water.text))) +
  geom_ribbon(aes(ymin = (mean.alpha - (2*se.alpha)), ymax = (mean.alpha + (2*se.alpha))), alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 0.5) +
  scale_fill_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  scale_color_manual(values = c("#70a494", "#edbb8a", "#de8a5a")) +
  xlab(" ") +
  ylab("Interspecific Alpha") +
  labs(fill = "Water", color = "Water", linetype = "Model") +
  theme(text = element_text(size=13)) +
  coord_cartesian(ylim = c(-0.06, 0.15))