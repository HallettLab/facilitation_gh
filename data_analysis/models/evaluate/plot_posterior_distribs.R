## plot posterior distributions

for(i in rainfall) {
  
  for(j in microbe){

post_w0.6 <- as.matrix(PrelimFit)


pdf("data_analysis/models/evaluate/plot_posteriors/sigmoidal/posts_m", j, "_w", i, date, ".pdf")

mcmc_areas(post_w0.6,
                 pars = c("c"),
                 prob = 0.8)
## c doesn't seem to like the bound it is given, it looks like it wants to possibly be greater than 1

mcmc_areas(post_w0.6,
           pars = c("alpha_slope"),
           prob = 0.8)
## this posterior looks particularly bad...

mcmc_areas(post_w0.6,
           pars = c("alpha_initial"),
           prob = 0.8)

mcmc_areas(post_w0.6,
           pars = c("N_opt"),
           prob = 0.8)

mcmc_areas(post_w0.6,
           pars = c("lambda"),
           prob = 0.8)

mcmc_areas(post_w0.6,
           pars = c("disp"),
           prob = 0.8)

  }
}

