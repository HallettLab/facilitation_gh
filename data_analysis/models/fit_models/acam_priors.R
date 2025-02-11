
library(tidyverse)

## read in MC data 
mc_posts = read.csv("data/megacomp_posteriors_20240714_models.csv")

## priors for BRHO model
brho_info = mc_posts %>%
  filter(species == "BRHO") %>%
  select(lambda_c, alpha_acam_c, alpha_acam_d, alpha_brho_c, alpha_brho_d)

## lambda
mean(brho_info$lambda_c)
sd(brho_info$lambda_c)

## alpha_brho
mean(brho_info$alpha_brho_c)
sd(brho_info$alpha_brho_c)

## alpha_acam
mean(brho_info$alpha_acam_c)
sd(brho_info$alpha_acam_c)










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

## Explore Sigmoidal Priors

#// priors
#// lambda & alpha priors all come from mega-comp values
#lambda ~ normal(62, 30);

hist(rnorm(10000, 62, 30))


#alpha_acam ~ normal(-0.185, 0.25);

hist(rnorm(10000, -0.185, 0.25))
plot(density((rnorm(10000, -0.185, 0.25))))

#disp ~ cauchy(0, 1);
#// safer to place prior on disp than on phi (the actual error term)

#alpha_initial ~ normal(0, 0.1);

hist(rnorm(10000, 0, 0.1))
plot(density((rnorm(10000, 0, 0.1))))

#//try flat priors on these parameters, esp since bounding b/w 0-1
#// model had trouble predicting both alpha_slope and c; both of which had a uniform prior put on them. Do they improve with a more specific prior?
 # //alpha_slope ~ uniform(-1, 0);
#//c ~ uniform(-1, 0);

#alpha_slope ~ normal(-0.2, 0.2); // using priors from Lisa's model
 # // I wonder about flatter priors for this?
  #c ~ normal(0, 0.1);

hist(rnorm(10000, -0.2, 0.2))
plot(density((rnorm(10000, -0.2, 0.2))))
  
#  //N_opt ~ normal(0, 5); // not working well
  
 # N_opt ~ exponential(0.2); //try poisson as its positive

hist(rexp(10000, 0.2))
plot(density((rexp(10000, 0.2))))


plot(density((rnorm(10000, 0, 4))))


#// poisson didn't work as th


beep(sound = 3)


