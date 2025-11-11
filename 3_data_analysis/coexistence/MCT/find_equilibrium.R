# Set up ####
library(tidyverse)
library(HDInterval)

theme_set(theme_classic())

## read in models
statposts = read.csv("data/model_posteriors/stat_posts_20250401.csv")

## germination data
germ = read.csv("data/germination_data.csv")

## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")

# Prep data ####
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


## Calc 80% HDI ####
statposts80 = stat2 %>%
  select(-disp) %>%
  group_by(focal, water) %>%
  filter(lambda >= hdi(lambda, credMass = 0.8)["lower"],
         lambda <= hdi(lambda, credMass = 0.8)["upper"],
         alpha_intra >= hdi(alpha_intra, credMass = 0.8)["lower"],
         alpha_intra <= hdi(alpha_intra, credMass = 0.8)["upper"],
         alpha_inter >= hdi(alpha_inter, credMass = 0.8)["lower"],
         alpha_inter <= hdi(alpha_inter, credMass = 0.8)["upper"])


# Find Equilibrium ####
set.seed(0)

species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

## create empty df
equil = as.data.frame(matrix(ncol = 8, nrow = 1))
names(equil) =c("species", "water", "post_num", "n_star", "lambda", "alpha_ii", "g_i", "s_i")


for(i in 1:length(species)) {
  
  ## select species
  sp = species[i]
  
  for (j in 1:length(rain)) {
    
    r = rain[j]
    
    ## select data
    dat = statposts80[statposts80$water == r & statposts80$focal == sp,]
    
    ## set treatment
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
      tmp = data.frame(species = sp, water = r, post_num = p, n_star = N_star, lambda = lambda_i, 
                       alpha_ii = alpha_ii, g_i = g_i, s_i = s_i)

      ## append
      equil = rbind(equil, tmp)
      
    }
    
  }
  
}

equil = equil %>%
  filter(!is.na(species))

## write.csv(equil, "data_analysis/MCT/output/equil_stat.csv", row.names = F)

# Plot ####
ggplot(equil, aes(x=n_star, color = as.factor(water))) +
 # geom_histogram() +
  geom_density(linewidth = 1) +
  facet_wrap(~species) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  xlab("Equilibrium Values") +
  labs(color = "Water") +
  ylab("Density")
  
## ggsave("data_analysis/MCT/figures/equilibrium_dens_postdraws.png", width = 7, height = 3)
