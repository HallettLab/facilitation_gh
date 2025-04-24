
# Set up ####
library(tidyverse)
library(HDInterval)

theme_set(theme_classic())

## read in models
# statposts = read.csv("data/model_posteriors/stat_posts_20250401.csv")
aposts = read.csv("data/model_posteriors/acam_soil_comp_posts_20250420.csv")
bposts = read.csv("data/model_posteriors/brho_soil_comp_posts_20250420.csv")

## germination data
germ = read.csv("data/germination_data.csv")

## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")

# Prep data ####
## don't filter at this step because m1 and m0 still together, don't necessarily want to filter ,0 and m1 together in case it really constrains distribs??
acam = aposts %>%
  mutate(alpha_intra = alpha_acam,
         alpha_inter = alpha_brho,
         alpha_intra_m0 = alpha_acam_m0,
         alpha_inter_m0 = alpha_brho, 
         ## the dev term is a fairly wide distribution, but see how using it goes??
         ## no, too wide, leave as alpha_brho
         
         focal = "ACAM") %>%
  select(-alpha_acam, -alpha_brho, -alpha_acam_m0, -alpha_acam_dev, -alpha_brho_m0, -alpha_brho_dev, -X, -disp)

brho = bposts %>%
  mutate(alpha_intra = alpha_brho,
         alpha_inter = alpha_acam, 
         alpha_intra_m0 = alpha_brho, ## PURPOSELY leaving this as alpha_brho as I don't trust the dev term
         alpha_inter_m0 = alpha_acam_m0,
         focal = "BRHO") %>%
  select(-alpha_acam, -alpha_brho, -alpha_acam_m0, -alpha_acam_dev, -alpha_brho_m0, -alpha_brho_dev, -X, -disp)

stat2 = rbind(acam, brho) %>%
  group_by(focal, water) %>%
  mutate(post_num = c(1:n()))

m0 = stat2 %>%
  select(focal, water, lambda_m0, alpha_intra_m0, alpha_inter_m0, post_num)
names(m0) = c("focal", "water", "lambda", "alpha_intra", "alpha_inter", "post_num")

m1 = stat2 %>%
  select(focal, water, lambda, alpha_intra, alpha_inter, post_num)

## Calc 80% HDI ####
m1statposts80 = m1 %>%
  group_by(focal, water) %>%
  filter(lambda >= hdi(lambda, credMass = 0.8)["lower"],
         lambda <= hdi(lambda, credMass = 0.8)["upper"],
         alpha_intra >= hdi(alpha_intra, credMass = 0.8)["lower"],
         alpha_intra <= hdi(alpha_intra, credMass = 0.8)["upper"],
         alpha_inter >= hdi(alpha_inter, credMass = 0.8)["lower"],
         alpha_inter <= hdi(alpha_inter, credMass = 0.8)["upper"])

m0statposts80 = m0 %>%
  group_by(focal, water) %>%
  filter(lambda >= hdi(lambda, credMass = 0.8)["lower"],
         lambda <= hdi(lambda, credMass = 0.8)["upper"],
         alpha_intra >= hdi(alpha_intra, credMass = 0.8)["lower"],
         alpha_intra <= hdi(alpha_intra, credMass = 0.8)["upper"],
         alpha_inter >= hdi(alpha_inter, credMass = 0.8)["lower"],
         alpha_inter <= hdi(alpha_inter, credMass = 0.8)["upper"])

ggplot(m0statposts80, aes(x=lambda)) +
  geom_density() +
  facet_grid(water~focal, scales = "free")

ggplot(m0statposts80, aes(x=alpha_intra)) +
  geom_density() +
  facet_grid(focal~water)

# Find Equilibrium ####
set.seed(25)

## m1 ####
species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

## create empty df
m1equil = as.data.frame(matrix(ncol = 8, nrow = 1))
names(m1equil) =c("species", "water", "post_num", "n_star", "lambda", "alpha_ii", "g_i", "s_i")


for(i in 1:length(species)) {
  
  ## select species
  sp = species[i]
  
  for (j in 1:length(rain)) {
    
    r = rain[j]
    
    ## select data
    dat = m1statposts80[m1statposts80$water == r & m1statposts80$focal == sp,]
    
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
      m1equil = rbind(m1equil, tmp)
      
    }
    
  }
  
}

m1equil = m1equil %>%
  filter(!is.na(species))

## write.csv(equil, "data_analysis/MCT/output/equil_stat.csv", row.names = F)

# Plot ####
ggplot(m1equil, aes(x=n_star, color = as.factor(water))) +
  # geom_histogram() +
  geom_density(linewidth = 1) +
  facet_wrap(~species) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  xlab("Equilibrium Values") +
  labs(color = "Water") +
  ylab("Density")

## ggsave("data_analysis/MCT/figures/equilibrium_dens_postdraws.png", width = 7, height = 3)

## m0 ####
species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

## create empty df
m0equil = as.data.frame(matrix(ncol = 8, nrow = 1))
names(m0equil) =c("species", "water", "post_num", "n_star", "lambda", "alpha_ii", "g_i", "s_i")


for(i in 1:length(species)) {
  
  ## select species
  sp = species[i]
  
  for (j in 1:length(rain)) {
    
    r = rain[j]
    
    ## select data
    dat = m0statposts80[m0statposts80$water == r & m0statposts80$focal == sp,]
    
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
    #  N_star = (1/(-0.01691412*0.5161111)) * log( (1 - ((1-0.5161111)*0.16 )) / (0.5161111 * 62.50656) )
      
      ## solve for equilibrium
      N_star = (1/(alpha_ii*g_i)) * log( (1 - ((1-g_i)*s_i )) / (g_i * lambda_i) )
      
      ## fill in data 
      tmp = data.frame(species = sp, water = r, post_num = p, n_star = N_star, lambda = lambda_i, 
                       alpha_ii = alpha_ii, g_i = g_i, s_i = s_i)
      
      ## append
      m0equil = rbind(m0equil, tmp)
      
    }
    
  }
  
}

m0equil = m0equil %>%
  filter(!is.na(species))

## write.csv(equil, "data_analysis/MCT/output/equil_stat.csv", row.names = F)

# Plot ####
ggplot(m0equil, aes(x=n_star, color = as.factor(water))) +
  # geom_histogram() +
  geom_density(linewidth = 1) +
  facet_wrap(~species) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  xlab("Equilibrium Values") +
  labs(color = "Water") +
  ylab("Density")
