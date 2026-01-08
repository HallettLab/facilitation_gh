## Header ##
## 
## Clean Posteriors
##
## Purpose: reformat posteriors to prepare for use in analyses
## save a clean .csv file of posteriors to avoid needing to re-run
## every time
## 
## Author: Carmen Watkins


## load models
source("2_calculate_interactions/population_models/2_evaluate/load_models.R")

## save posterior distributions
brho_stat_posts[[paste0("brho_w", i)]] = tmp

## Static - BOTH ####

ggplot(acam_stat_posteriors, aes(x=lambda)) +
  geom_density()

micA = acam_stat_posteriors %>%
  mutate(lambda_m0 = lambda + lambda_dev,
         alpha_brho_m0 = alpha_brho +alpha_brho_dev,
         alpha_acam_m0 = alpha_acam + alpha_acam_dev)

micB = brho_stat_posteriors %>%
  mutate(lambda_m0 = lambda + lambda_dev,
         alpha_brho_m0 = alpha_brho +alpha_brho_dev,
         alpha_acam_m0 = alpha_acam + alpha_acam_dev)

write.csv(micA, "data/model_posteriors/acam_soil_comp_posts_final_20250424.csv")
write.csv(micB, "data/model_posteriors/brho_soil_comp_posts_final_20250424.csv")

ggplot(micA) +
  geom_density(aes(x=lambda, color = "red")) +
  geom_density(aes(x=lambda_m0)) +
  facet_wrap(~water)

ggplot(micA) +
  geom_density(aes(x=alpha_brho, color = "red")) +
  geom_density(aes(x=alpha_brho_m0)) +
  facet_wrap(~water)

ggplot(micA) +
  geom_density(aes(x=alpha_acam, color = "red")) +
  geom_density(aes(x=alpha_acam_m0)) +
  facet_wrap(~water)

hdi(micA$alpha_brho_m0, credMass = 0.8)

ggplot(micB) +
  geom_density(aes(x=alpha_acam, color = "red")) +
  geom_density(aes(x=alpha_acam_m0)) +
  facet_wrap(~water) +
  geom_vline(xintercept = 0, linetype = "dashed")

ggplot(micB) +
  geom_density(aes(x=lambda, color = "red")) +
  geom_density(aes(x=lambda_m0)) +
  facet_wrap(~water)

ggplot(micB) +
  geom_density(aes(x=alpha_brho, color = "red")) +
  geom_density(aes(x=alpha_brho_m0)) +
  facet_wrap(~water)
