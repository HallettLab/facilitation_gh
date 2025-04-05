
# Set up ####
library(tidyverse)
theme_set(theme_classic())

## read in models
statposts = read.csv("data/model_posteriors/stat_posts_20250401.csv")

## germination data
germ = read.csv("data/germination_data.csv")

## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")

# Calc 80% HDI ####
statposts80 = statposts %>%
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


# Find Equilibrium ####
set.seed(0)

species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

post_list = sample(c(1:4000), 200, replace = FALSE)

## create empty df
equil = as.data.frame(matrix(ncol = 8, nrow = 1))

names(equil) =c("species", "water", "post_num", "n_star", "lambda", "alpha_ii", "g_i", "s_i")


for(i in 1:length(species)) {
  
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
    
    ## loop thru each posterior draw
    for (k in 1:length(posts)) {
      
      p = posts[k]
      
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
      equil = rbind(equil, tmp)
      
    }
    
  }
  
}

equil = equil %>%
  filter(!is.na(species))

hist(equil$n_star)

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








# N_star = (1/alpha_ii*g_i)*ln ( (1 - (1-g_i)*s_i ) / (g_i * lambda_i) )

# N_star = (1/(-0.0517*0.516)) * log( (1 - ((1-0.516)*0.16 )) / (0.516 * 29) )

# ((1 - 0.51611111) * 0.16) + (0.51611111 * 29 * exp(-0.0517 * 0.51611111 * 104.4))


# brho_mp
# bsurv = 0
# bgerm = 1
# blam = 400
# bintra = -0.0520
# ainter = -0.00603

# agerm = 0.516
# aNeq = 104


# OLD, SAVE ####
## solve for equilibrium graphically

# test_vals = seq(1, 120, by = 100/1000)

# growth = rep(NA, length(test_vals))
# err = 1

# for (i in 1:length(test_vals)) {
  
#   N_eq = test_vals[i]
  
#  growth_val = ((1 - 0.516) * 0.16 ) +  (0.516 * 29 * exp(-0.0517 * 0.516 * N_eq))
  
#  growth[i] = growth_val
  
#  if(abs(growth_val - 1) < err) {
#    err = abs(growth_val - 1)
#    print(paste0("N = ", as.character(N_eq), ", err = ", as.character(err)))
#  }
  
#}

#plot(test_vals, growth)