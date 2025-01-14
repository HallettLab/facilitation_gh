library(tidyverse)

mc_params = read.csv("data/megacomp_posteriors_20240714_models.csv")

sum_params = mc_params %>%
  select(species, lambda_d, lambda_c, alpha_acam_c, alpha_acam_d, alpha_brho_c, alpha_brho_d) %>%
  filter(species %in% c("BRHO", "ACAM")) %>%
  group_by(species) %>%
  summarise_all(.funs = c("mean", "sd"))

hist(runif(10000, min = -1, max = 0))
hist(rnorm(10000, 0, 0.1))

hist(rcauchy(10000, 0, 1))
## seems centered on 0 and almost all obs are very close to this however, there are a few that are super far out. This could potentially be concerning? Leave for now, but good to know

hist(rnorm(10000, -0.2, 0.2))
hist(rnorm(10000, 0, 0.1))

hist(rcauchy(100, 0, 1), breaks = 30)
hist(rcauchy(50, 0, 1), breaks = 30)


## brho lambda
sum_params[sum_params$species == "BRHO",]$lambda_c_mean
sum_params[sum_params$species == "BRHO",]$lambda_c_sd

sum_params[sum_params$species == "BRHO",]$alpha_acam_c_mean
sum_params[sum_params$species == "BRHO",]$alpha_acam_c_sd

sum_params[sum_params$species == "BRHO",]$alpha_brho_c_mean
sum_params[sum_params$species == "BRHO",]$alpha_brho_c_sd

# Lambda priors
## exponential(0.0009)
hist(rexp(100, rate = 0.0009))


hist(dat$seeds.out)

## lambda prior for BRHO using informed values from mega-comp
hist(rnorm(n=10000, mean = 201, sd = 53))
hist(rnorm(n=10000, mean = 201, sd = 75))

## alpha acam
hist(rnorm(n=10000, mean=-0.09875605, sd = 0.25))
## alpha brho
hist(rnorm(n=10000, mean=0.05728218, sd = 0.25))



hist(rnorm(n=100, mean = 5, sd = 1))

hist(rnorm(n=100, mean = -0.2, sd = 0.2))

hist(rnorm(n=200, mean = 0, sd = 0.1))
