
# Set Up ####
## Read in Data ####
## read in models
source("data_analysis/MCT/calc_equilibrium_sigmoidal.R")

## reading in this script as it will give posteriors already with 80% hdi calcualted.
## germ adn seed surv data also come along with it.

## sigposts 80 is the df we need

## start off with just the means
postmeans = sigposts80 %>%
  group_by(focal, water) %>%
  summarise(Mlam = mean(lambda),
            MNopt = mean(N_opt), 
            Mc = mean(c), 
            Ma_slope = mean(alpha_slope),
            Ma_init = mean(alpha_initial),
            Ma_intra = mean(alpha_intra))


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


ricker_stat_func = function(surv, germ, lambda, alpha_intra, Nt, germ_inter, inter_abund, alpha_inter) {
  
  ## Nt = abund of focal
  
  #e = exp(alpha_slope*(inter_abund - N0))
  
  #alpha_inter = alpha_0 + ((c*(1 - e))/(1 + e))
  
  Ntp1 = ((1-germ)*surv*Nt) + (germ * lambda * Nt * exp(alpha_inter*germ_inter*inter_abund + alpha_intra*germ*Nt))
  
  dat = data.frame(a_ij = alpha_inter, igr = log(Ntp1/Nt))
  
  return(dat)
  
}


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


dens = c(4, 6, 8, 12, 16, 20, 30, 100, 150)
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
## brho ####
cg = gr_fin %>%
  filter(focal == "BRHO", water == 1) %>%
  ggplot(aes(x=self_freq, y = igr, color = as.factor(dtot))) +
  # facet_wrap(~dtot) +
  geom_point(size = 2) +
  geom_line() +
  xlab(" ") +
  ylab(" ") +
  labs(color = "Total Density") +
  scale_color_manual(values = c("#d4e3de", "#c5dad3", "#b7d1c8", "#9bbfb3", "#8db6a9", "#70a494", "#639283", "black")) +
  theme(text = element_text(size = 14),
        axis.text.x=element_blank()) +
  
  coord_cartesian(ylim = c(4.2, 6.3))

bg = gr_fin %>%
  filter(focal == "BRHO", water == 0.75) %>%
  ggplot(aes(x=self_freq, y = igr, color = as.factor(dtot))) +
  geom_point(size = 2) +
  geom_line() +
  xlab(" ") +
  ylab(" ") +
  labs(color = "Total Density") +
  scale_color_manual(values = c("#fcf1e7", "#fbecdf", "#f9e8d7", "#f7dec6", "#f4d4b5", "#f3d0ae", "#d9b99b")) +
  theme(text = element_text(size = 14),
        axis.text.x=element_blank()) +
  
  coord_cartesian(ylim = c(4.2, 6.3))

ag = gr_fin %>%
  filter(focal == "BRHO", water == 0.6) %>%
  ggplot(aes(x=self_freq, y = igr, color = as.factor(dtot))) +
  geom_point(size = 2) +
  geom_line() +
  xlab(" ") +
  ylab("Grass Growth rate ln(Nt1/Nt)") +
  labs(color = "Total Density") +
  scale_color_manual(values = c("#f7d8c8", "#f1c5ad", "#eebb9f", "#ebb192", "#e59e76", "#de8a5a", "#c67a4f")) +
  theme(text = element_text(size = 14),
        axis.text.x=element_blank()) +
  
  coord_cartesian(ylim = c(4.2, 6.3))

#ggarrange(ag, bg, cg, ncol = 3, nrow = 1, labels = "AUTO", legend = F)

#ggsave("figures/final_diss/Fig5_dens_freq_dep.png", width = 9, height = 3.5)

## create a legend
#ggarrange(ag, bg, cg, ncol = 3, nrow = 1, labels = "AUTO")
#ggsave("figures/final_diss/Fig5_for legend.png", width = 5, height = 3.5)

## acam ####
cl = gr_fin %>%
  filter(focal == "ACAM", water == 1) %>%
  ggplot(aes(x=self_freq, y = igr, color = as.factor(dtot))) +
  # facet_wrap(~dtot) +
  geom_point(size = 2) +
  geom_line() +
  xlab(" ") +
  ylab(" ") +
  labs(color = "Total Density") +
  scale_color_manual(values = c("#d4e3de", "#c5dad3", "#b7d1c8", "#9bbfb3", "#8db6a9", "#70a494", "#639283", "black", "gray")) +
  theme(text = element_text(size = 14)) +
  
  coord_cartesian(ylim = c(1, 4))

bl = gr_fin %>%
  filter(focal == "ACAM", water == 0.75) %>%
  ggplot(aes(x=self_freq, y = igr, color = as.factor(dtot))) +
  geom_point(size = 2) +
  geom_line() +
  xlab(" ") +
  ylab(" ") +
  labs(color = "Total Density") +
  scale_color_manual(values = c("#fcf1e7", "#fbecdf", "#f9e8d7", "#f7dec6", "#f4d4b5", "#f3d0ae", "#d9b99b")) +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(1, 4))

al = gr_fin %>%
  filter(focal == "ACAM", water == 0.6) %>%
  ggplot(aes(x=self_freq, y = igr, color = as.factor(dtot))) +
  geom_point(size = 2) +
  geom_line() +
  xlab(" ") +
  ylab("Legume Growth rate ln(Nt1/Nt)") +
  labs(color = "Total Density") +
  scale_color_manual(values = c("#f7d8c8", "#f1c5ad", "#eebb9f", "#ebb192", "#e59e76", "#de8a5a", "#c67a4f", "black")) +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(1, 4))

ggarrange(ag, bg, cg, al, bl, cl, ncol = 3, nrow = 2, labels = "AUTO", legend = F, align = "v")
ggsave("figures/final_diss/Fig5_bothsp_dens_freq_dep.png", width = 9, height = 7.25)



# Old ####
gr %>%
  filter(focal == "ACAM", water == 1) %>%
  ggplot(aes(x=A_freq, y = igr, group = (dtot), color = as.factor(dtot))) +
  geom_point(size = 2) +
  geom_line()

gr %>%
  filter(focal == "ACAM") %>%
  ggplot(aes(x=A_freq, y = igr, group = (dtot), color = as.factor(dtot))) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~water)
 
gr %>%
  filter(focal == "ACAM", water == 0.6) %>%
  ggplot(aes(x=A_freq, group = (dtot), y=igr )) +
  geom_point() +
  geom_line()

  
  
  
Atest = gr %>%
  filter(focal == "ACAM", dtot == 12, water == 1) 


  
  
  
  
  
  
  

gr %>%
  filter(focal == "ACAM", water == 0.6) %>%
  ggplot(aes(x=a_ij, y = igr, color = as.factor(dtot))) +
  geom_point(size = 2) +
  geom_line() +
  geom_vline(xintercept = gr[gr$focal == "ACAM" & gr$water == 0.6,]$a_ii)

gr %>%
  filter(focal == "ACAM", water == 0.6) %>%
  ggplot(aes(x=a_ii, y = igr, color = as.factor(dtot))) +
  geom_point(size = 2) +
  geom_line() 


gr %>%
  filter(focal == "ACAM", water == 0.6) %>%
  ggplot(aes(x=B_freq, y = igr, color = as.factor(dtot), shape = focal)) +
  geom_point(size = 2) +
  geom_line()


#+
  xlab(" ") +
  ylab("Grass Growth rate ln(Nt1/Nt)") +
  labs(color = "Total Density") +
  scale_color_manual(values = c("#f7d8c8", "#f1c5ad", "#eebb9f", "#ebb192", "#e59e76", "#de8a5a", "#c67a4f")) #+
  #coord_cartesian(ylim = c(4.2, 6.3))














log(6)

exp(6)
exp(5.5)


gr %>%
  filter(focal == "BRHO") %>%
ggplot(aes(x=B_freq, y = igr, color = as.factor(dtot))) +
 # facet_wrap(~dtot) +
  geom_point() +
  geom_line() +
  xlab("BRHO Frequency") +
  ylab("Growth rate") +
  labs(color = "Density") +
  ggtitle("BRHO sig")

ggsave("Adler_freq_dens_dep_BRHO.png", width = 5, height = 3.5)



gr %>%
  filter(focal == "ACAM") %>%
  ggplot(aes(x=A_freq, y = igr, color = as.factor(dtot))) +
  # facet_wrap(~dtot) +
  geom_point() +
  geom_line() +
  xlab("ACAM Frequency") +
  ylab("Growth rate") +
  labs(color = "Density")

ggsave("Adler_freq_dens_dep_ACAM.png", width = 5, height = 3.5)


## test with static coeff ####
dens = c(4, 12, 24, 48, 64)
species = c("ACAM", "BRHO")
stat_gr = data.frame(focal = NA, dtot = NA, Nt_ACAM = NA, Nt_BRHO = NA, A_freq = NA, B_freq = NA, a_ij = NA, igr = NA)

for(j in 1:length(species)) {
  
  ## select species
  sp = species[j]
  
  ## select params
  lam = postmeans[postmeans$focal == sp & postmeans$water == 1,]$Mlam
  s = seedsurv[seedsurv$species == sp,]$surv.mean.p
  g_i = germ[germ$phyto == sp & germ$treatment == "C",]$mean.germ
  a_intra = postmeans[postmeans$focal == sp & postmeans$water == 1,]$Ma_intra
  a_inter = 0.012 ## try as constant
  #a_init = postmeans[postmeans$focal == sp & postmeans$water == 1,]$Ma_init
  #a_slope = postmeans[postmeans$focal == sp & postmeans$water == 1,]$Ma_slope
  #Nopt = postmeans[postmeans$focal == sp & postmeans$water == 1,]$MNopt
  #c = postmeans[postmeans$focal == sp & postmeans$water == 1,]$Mc
  
  ## set g_j param as other species
  if(sp == "ACAM") {
    g_j = germ[germ$phyto == "BRHO" & germ$treatment == "C",]$mean.germ
  } else {
    g_j = germ[germ$phyto == "ACAM" & germ$treatment == "C",]$mean.germ
  }
  
  ## loop over density
  for(i in 1:length(dens)) {
    
    ## select density
    d = dens[i]
    
    ## create dens/freq df
    tdens = data.frame(Nt_ACAM = seq(1:d), Nt_BRHO = rev(seq(1:d))) %>%
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
    gr_dat = ricker_stat_func(surv = s, germ = g_i, lambda = lam, alpha_intra = a_intra, Nt = N_it, germ_inter = g_j, inter_abund = N_jt, alpha_inter = a_inter)
    
    ## add gr dat back to id info
    tgr = cbind(tdens, gr_dat) %>%
      mutate(dtot = d,
             focal = sp)
    
    ## append
    stat_gr = rbind(stat_gr, tgr) %>%
      filter(!is.na(dtot))
    
  }
}

bstat = stat_gr %>%
  filter(focal == "BRHO") %>%
  ggplot(aes(x=B_freq, y = igr, color = as.factor(dtot))) +
  # facet_wrap(~dtot) +
  geom_point() +
  geom_line() +
  xlab("BRHO Frequency") +
  ylab("Growth rate") +
  labs(color = "Density") +
  ggtitle("BRHO stat")

astat = stat_gr %>%
  filter(focal == "ACAM") %>%
  ggplot(aes(x=A_freq, y = igr, color = as.factor(dtot))) +
  # facet_wrap(~dtot) +
  geom_point() +
  geom_line() +
  xlab("ACAM Frequency") +
  ylab("Growth rate") +
  labs(color = "Density")

ggarrange(bsig, bstat, ncol = 2, common.legend = T)



bsigdat = gr %>%
  filter(focal == "BRHO") %>%
  mutate(model = "sig")
bstatdat = stat_gr %>%
  filter(focal == "BRHO") %>%
  mutate(model = "stat")

bdat = rbind(bsigdat, bstatdat)

ggplot(bdat, aes(x=B_freq, y=igr, color = model)) +
  geom_point() +
  geom_line() +
  facet_wrap(~dtot)





