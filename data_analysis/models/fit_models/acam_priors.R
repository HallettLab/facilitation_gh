

mc_posts = read.csv("data/megacomp_posteriors_20240714_models.csv")

acam_info = mc_posts %>%
  filter(species == "ACAM") %>%
  select(lambda_c, alpha_acam_c, alpha_acam_d, alpha_brho_c, alpha_brho_d)

median(acam_info$lambda_c)

## get prior moments
mean(acam_info$lambda_c)
sd(acam_info$lambda_c)

## plot prior distrib
hist(rnorm(10000, 62, 30))

hist(acam_info$lambda_c)

## get alpha_acam prior
mean(acam_info$alpha_acam_c)
sd(acam_info$alpha_acam_c)

## plot alpha acam prior
hist(rnorm(10000, -0.185, 0.078))
hist(rnorm(10000, -0.185, 0.25))

mean(acam_info$alpha_brho_c)
sd(acam_info$alpha_brho_c)

hist(rnorm(10000, 0.024, 0.017))
hist(rnorm(10000, 0.024, 0.25))


hist(acam.model$seeds.out)

hist(ainter$total.biomass.g)

ainter[ainter$unique.ID == 250,]
