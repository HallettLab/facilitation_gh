
# Set up ####
library(tidyverse)
theme_set(theme_classic())

## germination data
germ = read.csv("data/germination_data.csv")

## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")


# Find Equilibrium ####
set.seed(0)

species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

post_list = sample(c(1:4000), 200, replace = FALSE)

## create empty df
equil = matrix(ncol = 8, nrow = 1)
equil = as.data.frame(equil)
names(equil) =c("species", "water", "post_num", "n_star", "lambda", "alpha_ii", "g_i", "s_i")


for(i in 1:length(species)) {
  
  sp = species[i]
  
  for (j in 1:length(rain)) {
    
    r = rain[j]
    
    ## select data
    if (sp == "ACAM") {
      
      dat = acam_stat_posteriors[acam_stat_posteriors$water == r,]
      
    } else {
      
      dat = brho_stat_posteriors[brho_stat_posteriors$water == r,]
      
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