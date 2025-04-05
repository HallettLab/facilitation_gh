

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

## this method does not produce even numbers of runs per group, but it should be okay?
## these are also slightly low rep numbers - 700 - 800, which seems odd; things were filtered a lot to get to the 80% hdi!
## continue for now, could consider bumping up the number of iterations in models to have more samples? 
sigposts80_test = sigposts80 %>%
  group_by(focal, water) %>%
  summarise(nreps = n())

# Find Equilibrium ####
set.seed(0)

species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

## create empty df
equil_sig = as.data.frame(matrix(ncol = 8, nrow = 1))
names(equil_sig) = c("species", "water", "post_num", "n_star", "lambda", "alpha_ii", "g_i", "s_i")

## run loop
for(i in 1:length(species)) {
  
  ## select species
  sp = species[i]
  
  for (j in 1:length(rain)) {
    
    ## select rainfall
    r = rain[j]
    
    ## select data
    dat = sigposts80[sigposts80$water == r & sigposts80$focal == sp,]
    
    ## set treatment - for matching germination and seed survival data (which have only two rainfall levels)
    if(r == 1) { trt = "C" 
    } else { trt = "D" }
    
    ## create list of unique posterior draws
    posts = unique(dat$post_num)
    
    ## select 400 random posterior draws - ok to have different draws for each water x focal combination as they were run in separate models.
    ## selected 400 runs to smooth out equil posterior densities - they were a little weirdly bumpy with just 200 or 300 draws
    post_list = sample(posts, 400, replace = FALSE)
    
    ## loop thru each posterior draw
    for (k in 1:length(post_list)) {
      
      p = post_list[k]
      
      ## define params
      if (sp == "ACAM") {
        
        g_i = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "ACAM",]$surv.mean.p
        
      } else {
        
        g_i = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "BRHO",]$surv.mean.p
        
      }
      
      lambda_i = dat[dat$post_num == p,]$lambda
      alpha_ii = dat[dat$post_num == p,]$alpha_intra
      
      ## solve for equilibrium
      N_star = (1/(alpha_ii*g_i)) * log( (1 - ((1-g_i)*s_i )) / (g_i * lambda_i) )
 
      ## fill in data 
      tmp = data.frame(species = sp, water = r, post_num = p, n_star = N_star, 
                       lambda = lambda_i, alpha_ii = alpha_ii, g_i = g_i, s_i = s_i)
      
      ## append
      equil_sig = rbind(equil_sig, tmp)
      
    }
    
  }
  
}


equil_sig = equil_sig %>%
  filter(!is.na(species))

hist(equil_sig$n_star)

ggplot(equil_sig, aes(x=n_star, color = as.factor(water))) +
  geom_density(linewidth = 1) +
  facet_wrap(~species) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  xlab("Equilibrium Values") +
  labs(color = "Water") +
  ylab("Density")

#ggsave("figures/Apr2025/sig_equil_dens.png", width = 7, height = 3)

write.csv(equil_sig, "data_analysis/MCT/output/equil_sig.csv", row.names = F)

