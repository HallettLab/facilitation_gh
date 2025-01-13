library(loo)


log_lik <- loo::extract_log_lik(PrelimFit, 
                                parameter_name = "F_sim", 
                                merge_chains = F)
#as of loo v2.0.0 we can optionally provide relative effective sample sizes
# when calling loo, which allows for better estimates of the PSIS effective
# sample sizes and Monte Carlo error
r_eff <- loo::relative_eff(log_lik, cores = 3) 
# preferably use more than 2 cores (as many cores as possible)
# will use value of 'mc.cores' option if cores is not specified
model.loo[[paste0(Code.focal,"_function_",function.int)]] <-  loo::loo(log_lik,
                                                                       threshold=0.7,
                                                                       r_eff = r_eff, 
                                                                       cores = 3)