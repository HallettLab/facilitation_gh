## Header ##
## 
## Script Name: Plot sigmoidal interaction coefficients
##
## Purpose: 
##
## This script creates data figure 2 of the paper 
## 
## Author: Carmen Watkins

# Set Up ####
## read in data
acam = read.csv("../outputs/posteriors/acam_sig_posts_LO_20260107.csv")
brho = read.csv("../outputs/posteriors/brho_sig_posts_LO_20260107.csv")

## create functions
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}

## create sigmoidal alpha function
alpha_function4  <- function(Amin, Aslopes,c,N,N0){
  
  e = exp(Aslopes*(N-N0)) # c is stretching the graph horizontally 
  a = c*(1-e) #stretching the graph vertically
  d = Amin
  alpha = (a/(1 + e)) + d
  
  return(alpha)
  
}

# Prep Data ####
acam2 = acam %>%
  select(-X) %>%
  mutate(alpha_intra = alpha_acam, 
         focal = "ACAM") %>%
  select(-alpha_acam) %>%
  group_by(water) %>%
  mutate(post_num = 1:4000)

brho2 = brho %>%
  select(-X) %>%
  mutate(alpha_intra = alpha_brho,
         focal = "BRHO") %>%
  select(-alpha_brho) %>%
  group_by(water) %>%
  mutate(post_num = 1:4000)

sigposts = rbind(acam2, brho2)

## Calc 80% HDI ####
sigposts80 = sigposts %>%
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

## Calculate alpha over density ####
## create an empty df
alpha_sig = data.frame(focal = NA, water = NA, post_num = NA, 
                       alpha_inter = NA, dens = NA)

## create vectors to loop over
species = c("ACAM", "BRHO")
rain = c(0.6, 0.75, 1)
dens = c(0:50)

## run loop
for(i in 1:length(species)) {
  for (j in 1:length(rain)) {
    
    sp = species[i]
    r = rain[j]
    
    ## select data from 80% hdi
    dat = sigposts80[sigposts80$focal == sp & sigposts80$water == r,]
## should be 400 vals
    
    ## create list of unique posterior draws
    posts = unique(dat$post_num)
    
    ## select 400 random posterior draws - ok to have different draws for each water x focal combination as they were run in separate models.
    ## selected 400 runs to smooth out equil posterior densities - they were a little weirdly bumpy with just 200 or 300 draws
    post_list = sample(posts, 200, replace = FALSE)
    
    ## loop thru each posterior draw
    for (k in 1:length(post_list)) {
      
      p = post_list[k]
      
      lambda_i = dat[dat$post_num == p,]$lambda
      alpha_ii = dat[dat$post_num == p,]$alpha_intra
      
      ## alpha_inter params
      n_opt = dat[dat$post_num == p,]$N_opt
      c = dat[dat$post_num == p,]$c
      alpha_slope = dat[dat$post_num == p,]$alpha_slope
      alpha_init = dat[dat$post_num == p,]$alpha_initial
      
      ## calc alpha
      a_tmp = alpha_function4(Amin = alpha_init, Aslopes = alpha_slope,
                              c = c, N = dens, N0 = n_opt)
      
      ## fill in data
      tmp = data.frame(focal = sp, water = r, post_num = p, 
                             alpha_inter = a_tmp, dens = dens)
      
      ## append
      alpha_sig = rbind(alpha_sig, tmp)
      
    }
    
  }
  
}

alpha_sig = alpha_sig %>%
  filter(!is.na(focal))

# Plot ####
## calculate the mean alpha value at each density
alpha_mean = alpha_sig %>%
  group_by(focal, water, dens) %>%
  summarise(mean.alpha = mean(alpha_inter), 
            se.alpha = calcSE(alpha_inter)) %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low")),
         focal = as.factor(focal), 
         focal = fct_relevel(focal, "BRHO", "ACAM"),
         realised_alpha = dens*mean.alpha)


## realised interaction
br_plot = alpha_sig %>%
  filter(focal == "BRHO") %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low")),
         realised_alpha = dens*alpha_inter) %>%
  ggplot(aes(x=dens, y=realised_alpha, color = as.factor(water))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_line(data = alpha_mean[alpha_mean$focal == "BRHO",], 
            aes(x=dens, y=realised_alpha, group = water.text, color = water.text), 
            linewidth = 1.25, color = "black") +
  geom_line(data = alpha_mean[alpha_mean$focal == "BRHO",], 
            aes(x=dens, y=realised_alpha, group = water.text, color = water.text), 
            linewidth = 0.75) +
  ylab("Realised Interspecific Interaction") +
  xlab("Legume Density") +
  labs(color = "Water Level") +
  ggtitle("B. hordeaceus") +
  theme(text = element_text(size=13))

ar_plot = alpha_sig %>%
  filter(focal == "ACAM") %>%
  mutate(water.text = ifelse(water == 1, "High",
                             ifelse(water == 0.75, "Intermediate",
                                    "Low")),
         realised_alpha = dens*alpha_inter) %>%
  ggplot(aes(x=dens, y=realised_alpha, color = as.factor(water))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(alpha = 0.15, aes(color = water.text, group = interaction(water.text, post_num))) +
  scale_color_manual(values = c("#70a494", "#f3d0ae", "#de8a5a")) +
  geom_line(data = alpha_mean[alpha_mean$focal == "ACAM",], 
            aes(x=dens, y=realised_alpha, group = water.text, color = water.text), 
            linewidth = 1.25, color = "black") +
  geom_line(data = alpha_mean[alpha_mean$focal == "ACAM",], 
            aes(x=dens, y=realised_alpha, group = water.text, color = water.text), 
            linewidth = 0.75) +
  ylab(" ") +
  xlab("Grass Density") +
  labs(color = "Water Level") +
  ggtitle("A. americanus") +
  theme(text = element_text(size=13))

ggarrange(br_plot, ar_plot, common.legend = TRUE, legend = "right")

ggsave("realised_interaction_sigmoidal_plots.png", width = 10, height = 4.5, bg = "white")

