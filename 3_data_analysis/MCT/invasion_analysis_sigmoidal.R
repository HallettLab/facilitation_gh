
# Set up ####

## read in model posts + equilibrium values
source("data_analysis/MCT/find_equilibrium_sigmoidal.R")

# Create Functions ####
igr_sig = function(surv, germ, lambda, alpha_intra, Nt, germ_inter, inter_abund, alpha_0, alpha_slope, N0, c) {
  
  ## Nt = abund of focal
  
  alpha_inter = alpha_0 + ((c*(1 - exp(alpha_slope*(inter_abund - N0))))/(1+exp(alpha_slope*(inter_abund - N0))))
  
  Ntp1 = ((1-germ)*surv*Nt) + (germ * lambda * Nt * exp(alpha_inter*germ_inter*inter_abund))
  
  return(log(Ntp1/Nt))
  
}


# Calc IGR ####
## create empty df
igr_sig_dat = data.frame(focal = NA, water = NA, post_num = NA, igr = NA, dens = NA)

species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)

for(i in 1:length(species)) {
  for (j in 1:length(rain)) {
    
    sp = species[i]
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
      
      ## select equil value
      N_eq = equil_sig %>%
        filter(species == sp, water == r, post_num == p) %>%
        select(n_star) %>%
        as.numeric()
      
      ## calc IGR
      igr_tmp = igr_sig(surv = s_i, germ = g_i, lambda = lambda_i, 
                        alpha_intra = alpha_ii, Nt = 1, germ_inter = g_j, 
                        inter_abund = N_eq, alpha_0 = alpha_init, 
                        alpha_slope = alpha_slope, N0 = n_opt, c = c)
      
        ## fill in data
        tmp = data.frame(focal = sp, water = r, post_num = p, igr = igr_tmp, dens = N_eq)
        
        ## append
        igr_sig_dat = rbind(igr_sig_dat, tmp)
        
    }
    
  }
  
}


igr_sig_dat = igr_sig_dat %>%
  filter(!is.na(focal))

write.csv(igr_sig_dat, "data_analysis/MCT/output/igr_sig.csv", row.names = F)

igr_sig_dat %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  
  ggplot(aes(x=as.factor(focal), y=igr, group = interaction(focal, water.text), color = as.factor(water.text))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_jitter(size = 0.5) +
  geom_boxplot() +
  facet_wrap(~water.text) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  ylab("Invasion Growth Rate") +
  xlab("Focal Species") +
  labs(color = "Water") +
  theme(text = element_text(size=14))

# ggsave("figures/Apr2025/Fig4_igr_sig_boxplot.png", width = 7, height = 3)





igr_dat %>%
  group_by(focal, post_num) %>%
ggplot(aes(x=dens, y = igr, group = interaction(water, post_num), color = as.factor(water))) +
  #geom_vline(xintercept = 0, linetype = "dashed") +
 # geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  facet_grid(water~focal) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  ylab("Invasion Growth Rate") +
  xlab("Neighbor Density") +
  labs(color = "Water")

#ggsave("data_analysis/MCT/figures/igr_sigmoidal_models.png", width = 8, height = 6.5)

