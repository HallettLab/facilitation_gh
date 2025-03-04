
## germination data
germ = read.csv("data/germination_data.csv")

## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")


igr = function(surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund) {
  
  alpha_inter = alpha_0 + ((c*(1 - exp(alpha_slope*(Nt - N0))))/(1+exp(alpha_slope*(Nt - N0))))
  
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(alpha_intra *germ* Nt + alpha_inter*germ_inter*inter_abund)
  
  return(log(Ntp1/Nt))
  
}


## create empty df
igr_dat = data.frame(focal = NA, water = NA, post_num = NA, igr = NA)

## set post draw list from equil df
posts = unique(equil$post_num)

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
    for (k in 1:length(posts)) {
      
      p = posts[k]
      
      ## define params
      if (sp == "ACAM") {
        
        lambda_i = adat[p,]$lambda
        alpha_ii = adat[p,]$alpha_acam
        alpha_ij = adat[p,]$alpha_brho
        g_i = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "ACAM",]$surv.mean.p
        g_j = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
        
        N_eq = equil %>%
          filter(species == "BRHO", water == r, post_num == p) %>%
          select(n_star) %>%
          as.numeric()
        
        
      } else {
        
        lambda_i = bdat[p,]$lambda
        alpha_ii = bdat[p,]$alpha_brho
        alpha_ij = bdat[p,]$alpha_acam
        g_i = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
        s_i = seedsurv[seedsurv$species == "BRHO",]$surv.mean.p
        g_j = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
        
        N_eq = equil %>%
          filter(species == "ACAM", water == r, post_num == p) %>%
          select(n_star) %>%
          as.numeric()
        
      }
      
      ## calc IGR
      igr_tmp = igr(surv = s_i, germ = g_i, lambda = lambda_i, alpha_intra = alpha_ii, Nt = 1, alpha_inter = alpha_ij, germ_inter = g_j, inter_abund = N_eq)
      
      ## fill in data 
      tmp = data.frame(focal = sp, water = r, post_num = p, igr = igr_tmp)
      
      ## append
      igr_dat = rbind(igr_dat, tmp)
      
    }
    
  }
  
}

igr_dat = igr_dat %>%
  filter(!is.na(focal))

ggplot(igr_dat, aes(x=igr, color = as.factor(water), linetype = focal)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_density(linewidth = 1) +
  facet_wrap(~water) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  xlab("Invasion Growth Rate") +
  ylab("Density") +
  labs(color = "Water")

ggsave("data_analysis/MCT/figures/igr_static_models.png", width = 6, height = 2.75)

