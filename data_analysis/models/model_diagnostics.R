


print(brho_ricker)
print(brho_ricker_alphaf)
## looks good
traceplot(brho_ricker)
traceplot(brho_ricker_alphaf)
## looks good


pairs(brho_ricker_alphaf)
## correlation between alpha_brho and lambda
## perhaps adding in random effects could help this? 
## first want to try fitting density as a function


#fit <- stan_glm(mpg ~ ., data = mtcars)
posterior <- as.matrix(brho_ricker_alphaf)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("lambda"),
           prob = 0.8) + plot_title

mcmc_areas(posterior,
           pars = c("disp_dev"),
           prob = 0.8) + plot_title

mcmc_areas(posterior,
           pars = c("alpha_acam", "alpha_brho"),
           prob = 0.8) + plot_title

mcmc_areas(posterior,
           pars = c("alpha_brho"),
           prob = 0.8) + plot_title

#ppc_dens_overlay(y = brho_ricker$alpha_acam,
 #                yrep = posterior_predict(brho_ricker, draws = 50))


