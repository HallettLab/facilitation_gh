
## read in model data from load_models script - needs to be done manually for the moment.
source("data_analysis/models/evaluate/load_models.R")
source("data_analysis/MCT/find_equilibrium_sigmoidal.R")

## germination data
germ = read.csv("data/germination_data.csv")

## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")


igr = function(surv, germ, lambda, alpha_intra, Nt, germ_inter, inter_abund, alpha_0, alpha_slope, N0, c) {
  
  ## Nt = abund of focal
  
  alpha_inter = alpha_0 + ((c*(1 - exp(alpha_slope*(inter_abund - N0))))/(1+exp(alpha_slope*(inter_abund - N0))))
  
  Ntp1 = ((1-germ)*surv*Nt) + (germ * lambda * Nt * exp(alpha_inter*germ_inter*inter_abund))
  
  return(log(Ntp1/Nt))
  
}


acam_sig_posteriors2 = acam_sig_posteriors %>%
  filter(!is.na(lambda))

brho_sig_posteriors2 = sig_posteriors %>%
  filter(!is.na(lambda))


## create empty df
igr_dat = data.frame(focal = NA, water = NA, post_num = NA, igr = NA, dens = NA)

## set post draw list from equil df
posts = unique(equil$post_num)

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
        N_eq = equil %>%
          filter(species == "BRHO", water == r, post_num == p) %>%
          select(n_star) %>%
          as.numeric()
        
        N_eq_v = c(1:170)
        
        
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
        
        N_eq = equil %>%
          filter(species == "ACAM", water == r, post_num == p) %>%
          select(n_star) %>%
          as.numeric()
       
        N_eq_v = c(1:as.integer(N_eq))
         
      }
      
      ## calc IGR
      igr_tmp = igr(surv = s_i, germ = g_i, lambda = lambda_i, alpha_intra = alpha_ii, Nt = 1, germ_inter = g_j, inter_abund = N_eq_v, alpha_0 = alpha_init, alpha_slope = alpha_slope, N0 = n_opt, c = c)
        
        ## fill in data
        tmp = data.frame(focal = sp, water = r, post_num = p, igr = igr_tmp, dens = N_eq_v)
        
        ## append
        igr_dat = rbind(igr_dat, tmp)
        
    }
    
  }
  
}

ggplot(tmp, aes(x=dens, y=igr)) +
  geom_point()


igr_dat = igr_dat %>%
  filter(!is.na(focal))


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

