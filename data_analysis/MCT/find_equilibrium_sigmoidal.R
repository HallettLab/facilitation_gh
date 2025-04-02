

# Set up ####
library(tidyverse)
library(HDInterval)

theme_set(theme_classic())

## read in models
sigposts = read.csv("data/model_posteriors/sig_posts_20250401.csv")

## germination data
germ = read.csv("data/germination_data.csv")

## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")


## NOTE!

## equilibrium function does not need to change for static or sigmoidal models; 
## since it is one species with itself, the density dependent INTERspecific function does not come into play. 

##keep this as a separate script just for ease of using diff posterior data sets.

##the invasion growth rate will need the full sigmoidal model as the low density invader will experience interspecific competition.

# Calc 80% HDI ####
sig_80 = sigposts %>%
  group_by(focal, water) %>%
  reframe(lambda80 = hdi(lambda, credMass = 0.8)) %>%
  group_by(focal, water) %>%
  mutate(bound = ifelse())

sigposts80 = sigposts %>%
  group_by(focal, water) %>%
  filter(lambda >= hdi(lambda)["lower"],
         lambda <= hdi(lambda)["upper"])

test["lower"]

# Find Equilibrium ####
set.seed(0)

species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

post_list = sample(c(1:7000), 200, replace = FALSE)

## create empty df
equil_sig = matrix(ncol = 8, nrow = 1)
equil_sig = as.data.frame(equil_sig)
names(equil_sig) =c("species", "water", "post_num", "n_star", "lambda", "alpha_ii", "g_i", "s_i")

## run loop
for(i in 1:length(species)) {
  
  sp = species[i]
  
  for (j in 1:length(rain)) {
    
    r = rain[j]
    
    ## select data
    if (sp == "ACAM") {
      
      dat = acam_sig_posteriors2[acam_sig_posteriors2$water == r,]
      
    } else {
      
      dat = brho_sig_posteriors2[brho_sig_posteriors2$water == r,]
      
    }
    
    ## set treatment
    if(r == 1) { trt = "C" } 
    else { trt = "D" }
    
    ## loop thru each posterior draw
    for (k in 1:length(post_list)) {
      
      p = post_list[k]
      
      ## define params
      if (sp == "ACAM") {
        
        alpha_ii = dat[p,]$alpha_acam
        g_i = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "ACAM",]$surv.mean.p
        
      } else {
        
        alpha_ii = dat[p,]$alpha_brho
        g_i = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "BRHO",]$surv.mean.p
        
      }
      
      lambda_i = dat[p,]$lambda
      
      ## solve for equilibrium
      N_star = (1/(alpha_ii*g_i)) * log( (1 - ((1-g_i)*s_i )) / (g_i * lambda_i) )
 
      ## fill in data 
      tmp = data.frame(species = sp, water = r, post_num = p, n_star = N_star, lambda = lambda_i, 
                       alpha_ii = alpha_ii, g_i = g_i, s_i = s_i)
      
      ## append
      equil_sig = rbind(equil_sig, tmp)
      
    }
    
  }
  
}


equil_sig = equil_sig %>%
  filter(!is.na(species))

hist(equil_sig$n_star)

ggplot(equil_sig, aes(x=n_star, color = as.factor(water))) +
  # geom_histogram() +
  geom_density(linewidth = 1) +
  facet_wrap(~species) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  xlab("Equilibrium Values") +
  labs(color = "Water") +
  ylab("Density")

#ggsave("data_analysis/MCT/figures/equilibrium_dens_postdraws_sigmodels.png", width = 7, height = 3)
