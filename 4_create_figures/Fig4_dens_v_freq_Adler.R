# Set Up ####
## load packages
library(cowplot)
library(ggpubr)


## read in data
### model params with 80% hdi calculated
source("3_data_analysis/create_sigmoidal_alphas_df.R")
### germination data
germ = read.csv("data/germination_data.csv")
### seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")

## start off with just the means
postmeans = sigposts80 %>%
  group_by(focal, water) %>%
  summarise(Mlam = mean(lambda),
            MNopt = mean(N_opt), 
            Mc = mean(c), 
            Ma_slope = mean(alpha_slope),
            Ma_init = mean(alpha_initial),
            Ma_intra = mean(alpha_intra))


equil_mean = equil_sig %>%
  group_by(species, water) %>%
  summarise(mean_equil = mean(n_star))

## Create Function ####
## sigmoidal igr; also returns alpha_inter
ricker_sig_func = function(surv, germ, lambda, alpha_intra, Nt, germ_inter, inter_abund, alpha_0, alpha_slope, N0, c) {
  
  ## Nt = abund of focal
  
  e = exp(alpha_slope*(inter_abund - N0))
  
  alpha_inter = alpha_0 + ((c*(1 - e))/(1 + e))
  
  Ntp1 = ((1-germ)*surv*Nt) + (germ * lambda * Nt * exp(alpha_inter*germ_inter*inter_abund + alpha_intra*germ*Nt))
  
  dat = data.frame(a_ij = alpha_inter, igr = log(Ntp1/Nt))
  
  return(dat)
  
}


#ricker_stat_func = function(surv, germ, lambda, alpha_intra, Nt, germ_inter, inter_abund, alpha_inter) {
  
  ## Nt = abund of focal
  
  #e = exp(alpha_slope*(inter_abund - N0))
  
  #alpha_inter = alpha_0 + ((c*(1 - e))/(1 + e))
  
#  Ntp1 = ((1-germ)*surv*Nt) + (germ * lambda * Nt * exp(alpha_inter*germ_inter*inter_abund + alpha_intra*germ*Nt))
  
#  dat = data.frame(a_ij = alpha_inter, igr = log(Ntp1/Nt))
  
#  return(dat)
  
#}


## Plan Analyses ####
## need to load model poseriors

## need to select arbirary densities 
## maybe 4, 12, 24, 48??

## at each density, need to vary frequency within that density

## then use ricker model to estimate growth rate

## so it will be very similar to hte invasion growth rate model, except that both species will be varying


## need a total density column
## need a relative frequency column - prob 1 for each species


## okay, will need to loop over density
## the model params won't vary by dens - only after alpha ij is calculated
## so we only need to change dens & freq, which we can do pretty easily by looping over just density.

## can set these params outside of the loop as they won't change with dens


dens = c(1,4, 6, 8, 12, 16, 20, 30)
rain = c(1, 0.75, 0.6)
species = c("ACAM", "BRHO")

gr = data.frame(water = NA, focal = NA, dtot = NA, Nt_ACAM = NA, Nt_BRHO = NA, A_freq = NA, B_freq = NA, a_ij = NA, igr = NA, a_ii = NA)

for (i in 1:length(rain)){
  
  r = rain[i]
  
  ## set treatment
  if(r == 1) { trt = "C" 
  } else { trt = "D" }

for(j in 1:length(species)) {
  
  ## select species
  sp = species[j]
  
  ## select params
  lam = postmeans[postmeans$focal == sp & postmeans$water == r,]$Mlam
  s = seedsurv[seedsurv$species == sp,]$surv.mean.p
  g_i = germ[germ$phyto == sp & germ$treatment == trt,]$mean.germ
  a_intra = postmeans[postmeans$focal == sp & postmeans$water == r,]$Ma_intra
  a_init = postmeans[postmeans$focal == sp & postmeans$water == r,]$Ma_init
  a_slope = postmeans[postmeans$focal == sp & postmeans$water == r,]$Ma_slope
  Nopt = postmeans[postmeans$focal == sp & postmeans$water == r,]$MNopt
  c = postmeans[postmeans$focal == sp & postmeans$water == r,]$Mc

  ## set g_j param as other species
  if(sp == "ACAM") {
    g_j = germ[germ$phyto == "BRHO" & germ$treatment == trt,]$mean.germ
  } else {
    g_j = germ[germ$phyto == "ACAM" & germ$treatment == trt,]$mean.germ
  }
  
## loop over density
for(i in 1:length(dens)) {
  
  ## select density
  d = dens[i]
  
  ## create dens/freq df
  tdens = data.frame(Nt_ACAM = c(0:d), Nt_BRHO = rev(c(0:d))) %>%
    mutate(A_freq = Nt_ACAM / (Nt_ACAM + Nt_BRHO),
           B_freq = Nt_BRHO / (Nt_ACAM + Nt_BRHO))
  
  ## select N_i and N_j vals
  if(sp == "ACAM"){
    N_it = tdens$Nt_ACAM
    N_jt = tdens$Nt_BRHO
  } else {
    N_it = tdens$Nt_BRHO
    N_jt = tdens$Nt_ACAM
  }
  
  ## run the function
  gr_dat = ricker_sig_func(surv = s, germ = g_i, lambda = lam, alpha_intra = a_intra, Nt = N_it, germ_inter = g_j, inter_abund = N_jt, alpha_0 = a_init, alpha_slope = a_slope, N0 = Nopt, c = c)
  
  ## add gr dat back to id info
  tgr = cbind(tdens, gr_dat) %>%
    mutate(dtot = d,
           focal = sp, 
           water = r,
           a_ii = a_intra)
  
  ## append
  gr = rbind(gr, tgr) %>%
    filter(!is.na(dtot))

  }
}
  
}
gr_fin = gr %>%
  mutate(self_freq = ifelse(focal == "ACAM", A_freq, B_freq),
         hetero_freq = ifelse(focal == "BRHO", A_freq, B_freq))
  
  
# Fig 5 ####
# Fig 5 Talk ####
cg = gr_fin %>%
  filter(water == 1, !dtot %in% c(1, 100, 130)) %>%
  ggplot(aes(x=self_freq, y = igr, color = as.factor(dtot), linetype = focal)) +
  # facet_wrap(~focal) +
  # geom_point(size = 2) +
  geom_line(linewidth = 1) +
  xlab(" ") +
  ylab(" ") +
  labs(color = "Total Density") +
  scale_color_manual(values = c(#"#d4e3de", 
    "#c5dad3", 
    #"#b7d1c8", 
    "#9bbfb3", 
    "#8db6a9", 
    "#70a494", 
    "#639283", 
    "#51796d", 
    "#273d36", 
    "#1a2b26")) +
  theme(text = element_text(size = 14))  +
  coord_cartesian(ylim = c(0, 6.3)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

bg = gr_fin %>%
  filter(water == 0.75, !dtot %in% c(1, 100, 130)) %>%
  ggplot(aes(x=self_freq, y = igr, color = as.factor(dtot), linetype = focal)) +
  #geom_point(size = 2) +
  geom_line(linewidth = 1) +
  xlab("Self Frequency") +
  ylab(" ") +
  labs(color = "Total Density") +
  scale_color_manual(values = c("#fcf1e7", "#fbecdf", 
                                "#f9e8d7", 
                                "#f7dec6", 
                                "#f4d4b5", 
                                "#f3d0ae", 
                                "#d9b99b")) +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 6.3)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ag = gr_fin %>%
  filter(water == 0.6, !dtot %in% c(1, 100, 130)) %>%
  ggplot(aes(x=self_freq, y = igr, color = as.factor(dtot), linetype = focal)) +
  # geom_point(size = 2) +
  geom_line(linewidth = 1) +
  xlab(" ") +
  ylab("Growth rate") +
  labs(color = "Total Density") +
  scale_color_manual(values = c("#f7d8c8", "#f1c5ad", "#eebb9f", "#ebb192", "#e59e76", "#de8a5a", "#c67a4f")) +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 6.3)) +
  labs(linetype = "Focal") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

ggarrange(ag, bg, cg, ncol = 3, common.legend = T, legend = "right", labels = "auto")

ggsave("../outputs/figures/Autumn_2026/Fig4_dens_freq_adler.png", width = 8, height = 6, bg = "white")

