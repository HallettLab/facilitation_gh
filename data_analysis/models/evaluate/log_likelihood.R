
# Set up ####
library(loo)
library(ggpubr)
library(bayesplot)
library(rstan) ## need this for traceplot
library(tidyverse)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

set.seed(25)

date = 20250124

# Run LOO ####
## Static ####
rain = c(1, 0.75, 0.6)
microbe = c(0, 1)

date_stat = 20250110

stat.loo = list()

for(i in rain){
  for(j in microbe) {
    
    ## load non-constrained models
    load(paste0("data_analysis/models/output/static/brho_nb_static_m",j, "_w", i, "_", date_stat, ".rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("m", j, "_w", i))
    
    
    log_lik <- loo::extract_log_lik(PrelimFit, 
                                    parameter_name = "F_sim", 
                                    merge_chains = F)
    #as of loo v2.0.0 we can optionally provide relative effective sample sizes
    # when calling loo, which allows for better estimates of the PSIS effective
    # sample sizes and Monte Carlo error
    r_eff <- loo::relative_eff(log_lik, cores = 3) 
    # preferably use more than 2 cores (as many cores as possible)
    # will use value of 'mc.cores' option if cores is not specified
    stat.loo[[paste0("brho_m", j, "_w", i, "_stat")]] <-  loo::loo(log_lik, threshold=0.7,r_eff = r_eff, cores = 3)
    
    
  }
  
}

## Sigmoidal ####
rain = c(1, 0.75, 0.6)
microbe = c(0, 1)

model.loo = list()

for(i in rain){
  for(j in microbe) {
    
    ## load non-constrained models
    load(paste0("data_analysis/models/output/sigmoidal/brho_nb_sigmoidal_m",j, "_w", i, "_", date, ".rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("m", j, "_w", i))
    
    
    log_lik <- loo::extract_log_lik(PrelimFit, 
                                    parameter_name = "F_sim", 
                                    merge_chains = F)
    #as of loo v2.0.0 we can optionally provide relative effective sample sizes
    # when calling loo, which allows for better estimates of the PSIS effective
    # sample sizes and Monte Carlo error
    r_eff <- loo::relative_eff(log_lik, cores = 3) 
    # preferably use more than 2 cores (as many cores as possible)
    # will use value of 'mc.cores' option if cores is not specified
    model.loo[[paste0("brho_m", j, "_w", i)]] <-  loo::loo(log_lik, threshold=0.7,r_eff = r_eff, cores = 3)
    
    
  }
  
}

model.loo[["brho_m0_w1"]]
model.loo[["brho_m0_w0.75"]]
model.loo[["brho_m0_w0.6"]] ## 1 very bad pareto k  ## ohh I bet this came from the one very far out point ... 
model.loo[["brho_m1_w1"]]
model.loo[["brho_m1_w0.75"]]
model.loo[["brho_m1_w0.6"]]

# Compare models ####
comp_loo_model <- data.frame()

for(i in rain){
  for(j in microbe) {
    
    ## this part will compare the static vs. the sigmoidal...
    comp <- loo::loo_compare(stat.loo[[paste0("brho_m", j, "_w", i, "_stat")]], 
                                     model.loo[[paste0("brho_m", j, "_w", i)]])
  
    comp_df <- as.data.frame(comp) %>%
      mutate(water = i,
             microbe = j)
      
    comp_loo_model = rbind(comp_loo_model, comp_df)
    
  }
}

comp_loo_model2 <- comp_loo_model %>%
  rownames_to_column(var="model") %>%
  mutate(model_type = ifelse(substr(model, start = 1, stop = 6) == "model1", "static", "sigmoidal"),
         model_date = ifelse(model_type == "static", date_stat, date))

write.csv(comp_loo_model2, "data_analysis/models/evaluate/log_likelihood_output/model_compare_stat0110_sig0124.csv")
