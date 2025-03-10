
# Set up ####
## first, load models
## still have to do that manually for the moment; eventually will automate it.

## equilibrium data
source("data_analysis/models/evaluate/load_models.R")
source("data_analysis/MCT/find_equilibrium.R")

## germ and seedsurv come along with the find equilibrium script; no need to re-load these
## germination data
#germ = read.csv("data/germination_data.csv")

## seed survival data
#seedsurv = read.csv("data/seed_survival_sumdat.csv")

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## create function to calc IGR
igr = function(surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund) {
  
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(alpha_intra *germ* Nt + alpha_inter*germ_inter*inter_abund)
  
  return(log(Ntp1/Nt))
  
}

# Calc IGR ####
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
        g_j = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
        
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

# Plot ####
ggplot(igr_dat, aes(x=igr, color = as.factor(water), linetype = focal)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_density(linewidth = 1) +
  facet_wrap(~water) +
  scale_color_manual(values = c("#de8a5a", "#f3d0ae", "#70a494")) +
  xlab("Invasion Growth Rate") +
  ylab("Density") +
  labs(color = "Water")

#ggsave("data_analysis/MCT/figures/igr_static_models.png", width = 6, height = 2.75)


igr_dat %>%
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
  labs(color = "Water") #+
#  legend("bottom")

# ggsave("data_analysis/MCT/figures/igr_static_models_boxplot.png", width = 7, height = 3)

