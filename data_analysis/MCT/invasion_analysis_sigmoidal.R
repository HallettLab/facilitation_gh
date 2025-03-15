
## read in model data from load_models script - needs to be done manually for the moment.
source("data_analysis/models/evaluate/load_models.R")
source("data_analysis/MCT/find_equilibrium_sigmoidal.R")

## germination data
#germ = read.csv("data/germination_data.csv")

## seed survival data
#seedsurv = read.csv("data/seed_survival_sumdat.csv")


igr_sig = function(surv, germ, lambda, alpha_intra, Nt, germ_inter, inter_abund, alpha_0, alpha_slope, N0, c) {
  
  ## Nt = abund of focal
  
  alpha_inter = alpha_0 + ((c*(1 - exp(alpha_slope*(inter_abund - N0))))/(1+exp(alpha_slope*(inter_abund - N0))))
  
  Ntp1 = ((1-germ)*surv*Nt) + (germ * lambda * Nt * exp(alpha_inter*germ_inter*inter_abund))
  
  return(log(Ntp1/Nt))
  
}


acam_sig_posteriors2 = acam_sig_posteriors %>%
  filter(!is.na(lambda))

brho_sig_posteriors2 = brho_sig_posteriors %>%
  filter(!is.na(lambda))


## create empty df
igr_sig_dat = data.frame(focal = NA, water = NA, post_num = NA, igr = NA, dens = NA)

## set post draw list from equil df
posts = unique(equil_sig$post_num)

## find a diff way of getting posts... 

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
    for (k in 1:length(posts)) {
      
      p = posts[k]
      
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
        
        ## might need to loop over diff equil values
        ## could create as a vector from 1:Neq??
        N_eq = equil_sig %>%
          filter(species == "BRHO", water == r, post_num == p) %>%
          select(n_star) %>%
          as.numeric()
        
       # N_eq_v = c(1:170)
        
        
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
        
        N_eq = equil_sig %>%
          filter(species == "ACAM", water == r, post_num == p) %>%
          select(n_star) %>%
          as.numeric()
       
      #  N_eq_v = c(1:as.integer(N_eq))
        ## leave this commented out for the moment to calc IGR at JUST equilibrium vals
         
      }
      
      ## calc IGR
      igr_tmp = igr_sig(surv = s_i, germ = g_i, lambda = lambda_i, alpha_intra = alpha_ii, Nt = 1, germ_inter = g_j, inter_abund = N_eq, alpha_0 = alpha_init, alpha_slope = alpha_slope, N0 = n_opt, c = c)
        
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
  #group_by(focal, water) %>%
  #summarise(mean_igr = mean(igr), 
  #       se_igr = calcSE(igr)) %>%
  mutate(water.text = ifelse(water == 1, "High", 
                             ifelse(water == 0.75, "Intermediate", "Low"))) %>%
  
  ggplot(aes(x=as.factor(focal), y=igr, group = interaction(focal, water.text), color = as.factor(water.text))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  #geom_density(linewidth = 1) +
  geom_jitter(size = 0.75) +
  geom_boxplot() +
  #geom_errorbar(aes(ymin = mean_igr - 2*se_igr, ymax = mean_igr + 2*se_igr)) +
  facet_wrap(~water.text) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  ylab("Invasion Growth Rate") +
  xlab("Focal Species") +
  labs(color = "Water") +
  ggtitle("Sigmoidal Model")

  #  legend("bottom")

# ggsave("data_analysis/MCT/figures/igr_sigmoidal_models_boxplot.png", width = 7, height = 3)





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

