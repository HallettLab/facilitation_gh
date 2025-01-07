library(ggpubr)

load("data_analysis/models/output/brho_ricker_poiss_sigmoidal_m1_w1_20250106.rdata")
w1 = brho_ricker_poiss_alpha_sig2

load("data_analysis/models/output/brho_ricker_poiss_sigmoidal_m1_w0.75_20250106.rdata")
w0.75 = brho_ricker_poiss_alpha_sig2

load("data_analysis/models/output/brho_ricker_poiss_sigmoidal_m1_w0.6_20250106.rdata")
w0.6 = brho_ricker_poiss_alpha_sig2

## eval models 
### Rhat
print(w1)
print(w0.75)
print(w0.6)

### traceplot
traceplot(w1, inc_warmup = TRUE)
traceplot(w0.75)
traceplot(w0.6)

### pairs plos
pairs(w1)
pairs(w0.75)
pairs(w0.6)

## visualize posteriors
post_w1 <- as.matrix(w1)
post_w0.75 <- as.matrix(w0.75)
post_w0.6 <- as.matrix(w0.6)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
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

ggarrange(L1, L0.75, L0.6, ncol = 3, nrow = 1)

ggsave("figures/MS_version1/model_output/lambda.png", width = 8, height = 3.5)


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

ggarrange(ab1, ab0.75, ab0.6, ncol = 3, nrow = 1)
ggsave("figures/MS_version1/model_output/alpha_brho.png", width = 8, height = 3.5)


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

library(ggpubr)
ggarrange(a, b, c, d,e,f, ncol = 1, nrow = 6)

ggsave("figures/MS_version1/model_posteriors_brhom1w1_ricker_poiss_sigmoidal.png", height = 8, width = 3.5)
