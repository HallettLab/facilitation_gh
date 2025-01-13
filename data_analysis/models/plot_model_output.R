




# Visualize ####
post_w1 <- as.matrix(w1)
post_w0.75 <- as.matrix(w0.75)
post_w0.6 <- as.matrix(w0.6)



post_w1 <- as.matrix(w1stat)
post_w0.75 <- as.matrix(w75stat)
post_w0.6 <- as.matrix(w60stat)

## Static ####
mcmc_areas(post_w1,
           pars = c("F_sim[2]"),
           prob = 0.8) +
  ggtitle("High")

L1 = mcmc_areas(post_w1,
                pars = c("lambda"),
                prob = 0.8) +
  ggtitle("High")

L0.75 = mcmc_areas(post_w0.75,
                   pars = c("lambda"),
                   prob = 0.8) +
  ggtitle("Intermediate")

L0.6 =  mcmc_areas(post_w0.6,
                   pars = c("lambda"),
                   prob = 0.8) +
  ggtitle("Low")

ggarrange(L0.6, L0.75, L1,  ncol = 3, nrow = 1)

ggsave("figures/MS_version1/model_output/lambda_staticmodels_20250109models.png", width = 8, height = 3.5)


ab1 = mcmc_areas(post_w1,
                 pars = c("alpha_brho"),
                 prob = 0.8) +
  ggtitle("High")

ab0.75 = mcmc_areas(post_w0.75,
                    pars = c("alpha_brho"),
                    prob = 0.8) +
  ggtitle("Intermediate")

ab0.6 =  mcmc_areas(post_w0.6,
                    pars = c("alpha_brho"),
                    prob = 0.8) +
  ggtitle("Low")

ggarrange(ab0.6, ab0.75, ab1, ncol = 3, nrow = 1)
ggsave("figures/MS_version1/model_output/alpha_brho_staticmodels_20250109models.png", width = 8, height = 3.5)

aa1 = mcmc_areas(post_w1,
                 pars = c("alpha_acam"),
                 prob = 0.8) +
  ggtitle("High")

aa0.75 = mcmc_areas(post_w0.75,
                    pars = c("alpha_acam"),
                    prob = 0.8) +
  ggtitle("Intermediate")

aa0.6 =  mcmc_areas(post_w0.6,
                    pars = c("alpha_acam"),
                    prob = 0.8) +
  ggtitle("Low")

ggarrange(aa0.6, aa0.75, aa1, ncol = 3, nrow = 1)
ggsave("figures/MS_version1/model_output/alpha_acam_staticmodels_20250109models.png", width = 8, height = 3.5)






## Sigmoidal ####

L1 = mcmc_areas(post_w1,
                pars = c("lambda"),
                prob = 0.8) +
  ggtitle("High")

L0.75 = mcmc_areas(post_w0.75,
                   pars = c("lambda"),
                   prob = 0.8) +
  ggtitle("Intermediate")

L0.6 =  mcmc_areas(post_w0.6,
                   pars = c("lambda"),
                   prob = 0.8) +
  ggtitle("Low")

ggarrange(L0.6, L0.75, L1,  ncol = 3, nrow = 1)

ggsave("figures/MS_version1/model_output/lambda_sigmoidalmodels_20250106models.png", width = 8, height = 3.5)










hist(brho.model$seeds.out/brho.model$num.focal.indiv)


as1 = mcmc_areas(post_w1,
                 pars = c("alpha_slope"),
                 prob = 0.8) +
  ggtitle("High")

as0.75 = mcmc_areas(post_w0.75,
                    pars = c("alpha_slope"),
                    prob = 0.8) +
  ggtitle("Intermediate")

as0.6 =  mcmc_areas(post_w0.6,
                    pars = c("alpha_slope"),
                    prob = 0.8) +
  ggtitle("Low")

ggarrange(as1, as0.75, as0.6, ncol = 3, nrow = 1)
ggsave("figures/MS_version1/model_output/alpha_slope.png", width = 8, height = 3.5)


ai1 = mcmc_areas(post_w1,
                 pars = c("alpha_initial"),
                 prob = 0.8) +
  ggtitle("High")

ai0.75 = mcmc_areas(post_w0.75,
                    pars = c("alpha_initial"),
                    prob = 0.8) +
  ggtitle("Intermediate")

ai0.6 =  mcmc_areas(post_w0.6,
                    pars = c("alpha_initial"),
                    prob = 0.8) +
  ggtitle("Low")

ggarrange(ai1, ai0.75, ai0.6, ncol = 3, nrow = 1)
ggsave("figures/MS_version1/model_output/alpha_initial.png", width = 8, height = 3.5)



N01 = mcmc_areas(post_w1,
                 pars = c("N_opt"),
                 prob = 0.8) +
  ggtitle("High")

N00.75 = mcmc_areas(post_w0.75,
                    pars = c("N_opt"),
                    prob = 0.8) +
  ggtitle("Intermediate")

N00.6 =  mcmc_areas(post_w0.6,
                    pars = c("N_opt"),
                    prob = 0.8) +
  ggtitle("Low")

ggarrange(N01, N00.75, N00.6, ncol = 3, nrow = 1)
ggsave("figures/MS_version1/model_output/N_opt.png", width = 8, height = 3.5)










b = mcmc_areas(posterior,
               pars = c("alpha_brho"),
               prob = 0.8)

c = mcmc_areas(posterior,
               pars = c("alpha_initial"),
               prob = 0.8)

d = mcmc_areas(posterior,
               pars = c("alpha_slope"),
               prob = 0.8, scales = "free") 

e = mcmc_areas(posterior,
               pars = c("N_opt"),
               prob = 0.8) 

f = mcmc_areas(posterior,
               pars = c("c"),
               prob = 0.8)

ggarrange(a, b, c, d,e,f, ncol = 1, nrow = 6)

ggsave("figures/MS_version1/model_posteriors_brhom1w1_ricker_poiss_sigmoidal.png", height = 8, width = 3.5)
