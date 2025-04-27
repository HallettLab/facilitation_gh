
library(tidyverse)
# In case ####
## I don't think you'll need it, but here's germ & seed surv data
## germination data
germ = read.csv("data/germination_data.csv")
## seed survival data
seedsurv = read.csv("data/seed_survival_sumdat.csv")

## posteriors
# STATIC ####
acam = read.csv("data/model_posteriors/acam_soil_comp_posts_final_20250424.csv")
brho = read.csv("data/model_posteriors/brho_soil_comp_posts_final_20250424.csv")

a_means = acam %>%
  select(-X, -disp, -lambda_dev, -alpha_brho_dev, -alpha_acam_dev) %>%
  group_by(water) %>%
  summarise_if(is.numeric, list(mean)) %>%
  mutate(focal = "ACAM")

names(a_means) = c("water", "lambda", "alpha_INTER", "alpha_INTRA", "lambda_m0", "alpha_INTER_m0", "alpha_INTRA_m0", "focal")
 
b_means = brho %>%
  select(-X, -disp, -lambda_dev, -alpha_brho_dev, -alpha_acam_dev) %>%
  group_by(water) %>%
  summarise_if(is.numeric, list(mean)) %>%
  mutate(focal = "BRHO")

names(b_means) = c("water", "lambda", "alpha_INTRA", "alpha_INTER", "lambda_m0", "alpha_INTRA_m0", "alpha_INTER_m0", "focal")


## NOTES: 
## lambda, alpha_acam, alpha_brho are in LIVE SOIL
## lambda_m0, alpha_acam_m0, alpha_brho_m0 are in STERILIZED SOIL
## I would recommend NOT using alpha_brho_m0; this had a very large variance from the alpha_brho_dev column. 

statp = rbind(a_means, b_means)
  
write.csv(statp, "stat_posts_live_sterilized_Lauren.csv", row.names = F)

# SIG ####
sigposts = read.csv("data/model_posteriors/sig_posts_20250401.csv")

sig_means = sigposts %>%
  select(-disp, -post_num) %>%
  group_by(focal, water) %>%
  summarise_if(is.numeric, list(mean))

write.csv(sig_means, "sig_posts_live_only_Lauren.csv", row.names = F)


