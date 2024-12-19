


print(brho_ricker)
print(brho_ricker_poiss_alpha_sig2)


print(brho_ricker_poiss_alpha_sig)

traceplot(brho_ricker_alpha_sig, inc_warmup = TRUE)
traceplot(brho_ricker_alphaf)
traceplot(brho_ricker_poiss_alpha_sig2)
## looks good


pairs(brho_ricker_alphaf)
pairs(brho_ricker_poiss_alpha_sig2)
## correlation between alpha_brho and lambda
## perhaps adding in random effects could help this? 
## first want to try fitting density as a function
pairs(brho_ricker_poiss_alpha_sig)

#fit <- stan_glm(mpg ~ ., data = mtcars)
posterior <- as.matrix(brho_ricker_poiss_alpha_sig2)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
a = mcmc_areas(posterior,
           pars = c("lambda"),
           prob = 0.8)

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
