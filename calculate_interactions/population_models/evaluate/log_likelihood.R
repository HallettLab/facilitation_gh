
# Set up ####
library(loo)
library(ggpubr)
library(bayesplot)
library(rstan) ## need this for traceplot
library(tidyverse)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

set.seed(25)

date = 20250401

# Run LOO ####
## BRHO ####
### Static ####
rain = c(1, 0.75, 0.6)
date_stat = 20250401

stat.loo = list()

for(i in rain){

    ## load models
    load(paste0("data_analysis/models/output/static/", date, "/brho_nb_static_w", i, "_", date_stat, "_final.rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    log_lik <- loo::extract_log_lik(PrelimFit, 
                                    parameter_name = "F_sim", 
                                    merge_chains = F)
    #as of loo v2.0.0 we can optionally provide relative effective sample sizes
    # when calling loo, which allows for better estimates of the PSIS effective
    # sample sizes and Monte Carlo error
    r_eff <- loo::relative_eff(log_lik, cores = 3) 
    # preferably use more than 2 cores (as many cores as possible)
    # will use value of 'mc.cores' option if cores is not specified
    stat.loo[[paste0("brho_w", i, "_stat")]] <-  loo::loo(log_lik, threshold=0.7,r_eff = r_eff, cores = 3)
  
}

### Sigmoidal ####
rain = c(1, 0.75, 0.6)
date = 20250401
sig.loo = list()

for(i in rain){

    ## load models
    load(paste0("data_analysis/models/output/sigmoidal/", date, "/brho_nb_sigmoidal_w", i, "_", date, "_final.rdata"))
    
    ## print model to keep track of progress during loop
    print(paste0("w", i))
    
    log_lik <- loo::extract_log_lik(PrelimFit, 
                                    parameter_name = "F_sim", 
                                    merge_chains = F)
    #as of loo v2.0.0 we can optionally provide relative effective sample sizes
    # when calling loo, which allows for better estimates of the PSIS effective
    # sample sizes and Monte Carlo error
    r_eff <- loo::relative_eff(log_lik, cores = 3) 
    # preferably use more than 2 cores (as many cores as possible)
    # will use value of 'mc.cores' option if cores is not specified
    sig.loo[[paste0("brho_w", i)]] <-  loo::loo(log_lik, threshold=0.7,r_eff = r_eff, cores = 3)
  
}

### Compare models ####
comp_loo_model <- data.frame()

for(i in rain){
  
    ## this part will compare the static vs. the sigmoidal...
    comp <- loo::loo_compare(stat.loo[[paste0("brho_w", i, "_stat")]], 
                                     sig.loo[[paste0("brho_w", i)]])
  
    comp_df <- as.data.frame(comp) %>%
      mutate(water = i)
      
    comp_loo_model = rbind(comp_loo_model, comp_df)

}

comp_loo_model2 <- comp_loo_model %>%
  rownames_to_column(var="model") %>%
  mutate(model_type = ifelse(substr(model, start = 1, stop = 6) == "model1", "static", "sigmoidal"),
         model_date = ifelse(model_type == "static", date_stat, date)) %>%
  select(model_type, model_date, water, elpd_diff, se_diff, elpd_loo, se_elpd_loo, p_loo, se_p_loo, looic, se_looic) %>% 
  mutate_if(is.numeric, round, digits = 3)

write.csv(comp_loo_model2, "data_analysis/models/evaluate/log_likelihood_output/model_compare_stat_sig_20250225.csv")


## ACAM ####
### Static ####
rain = c(1, 0.75, 0.6)
date_stat = 20250401

stat.loo = list()

for(i in rain){
  
  ## load models
  load(paste0("data_analysis/models/output/static/", date_stat, "/acam_nb_static_w", i, "_", date_stat, "_final.rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  log_lik <- loo::extract_log_lik(PrelimFit, 
                                  parameter_name = "F_sim", 
                                  merge_chains = F)
  #as of loo v2.0.0 we can optionally provide relative effective sample sizes
  # when calling loo, which allows for better estimates of the PSIS effective
  # sample sizes and Monte Carlo error
  r_eff <- loo::relative_eff(log_lik, cores = 3) 
  # preferably use more than 2 cores (as many cores as possible)
  # will use value of 'mc.cores' option if cores is not specified
  stat.loo[[paste0("acam_w", i, "_stat")]] <-  loo::loo(log_lik, threshold=0.7,r_eff = r_eff, cores = 3)
  
}

## check pareto k
test = stat.loo[["acam_w1_stat"]]$diagnostics$pareto_k
test>0.7
## one high pareto-k value

test2 = stat.loo[["acam_w0.75_stat"]]$diagnostics$pareto_k
test2>0.7
test3 = stat.loo[["acam_w0.6_stat"]]$diagnostics$pareto_k
test3>0.7


### Sigmoidal ####
rain = c(1, 0.75, 0.6)
date = 20250401
sig.loo = list()

for(i in rain){
  
  ## load models
  load(paste0("data_analysis/models/output/sigmoidal/", date, "/acam_nb_sigmoidal_w", i, "_", date, "_final.rdata"))
  
  ## print model to keep track of progress during loop
  print(paste0("w", i))
  
  log_lik <- loo::extract_log_lik(PrelimFit, 
                                  parameter_name = "F_sim", 
                                  merge_chains = F)
  #as of loo v2.0.0 we can optionally provide relative effective sample sizes
  # when calling loo, which allows for better estimates of the PSIS effective
  # sample sizes and Monte Carlo error
  r_eff <- loo::relative_eff(log_lik, cores = 3) 
  # preferably use more than 2 cores (as many cores as possible)
  # will use value of 'mc.cores' option if cores is not specified
  sig.loo[[paste0("acam_w", i)]] <-  loo::loo(log_lik, threshold=0.7,r_eff = r_eff, cores = 3)
  
}

## check pareto-k
testsg = sig.loo[["acam_w1"]]$diagnostics$pareto_k
testsg>0.7
## one high pareto-k value
testsg2 = sig.loo[["acam_w0.75"]]$diagnostics$pareto_k
testsg2>0.7
testsg3 = sig.loo[["acam_w0.6"]]$diagnostics$pareto_k
testsg3>0.7

### Compare models ####
#### stat v sig ####
comp_loo_model_acam <- data.frame()

for(i in rain){
  
  ## this part will compare the static vs. the sigmoidal...
  comp <- loo::loo_compare(stat.loo[[paste0("acam_w", i, "_stat")]], 
                           sig.loo[[paste0("acam_w", i)]])
  
  comp_df <- as.data.frame(comp) %>%
    mutate(water = i)
  
  comp_loo_model_acam = rbind(comp_loo_model_acam, comp_df)
  
}

comp_loo_model_acam2 <- comp_loo_model_acam %>%
  rownames_to_column(var="model") %>%
  mutate(model_type = ifelse(substr(model, start = 1, stop = 6) == "model1", "static", "sigmoidal"),
         model_date = ifelse(model_type == "static", date_stat, date)) %>%
  select(model_type, model_date, water, elpd_diff, se_diff, elpd_loo, se_elpd_loo, p_loo, se_p_loo, looic, se_looic) %>% 
  mutate_if(is.numeric, round, digits = 3)

write.csv(comp_loo_model_acam2, "data_analysis/models/evaluate/log_likelihood_output/acam_model_compare_stat_sig_20250401.csv")
