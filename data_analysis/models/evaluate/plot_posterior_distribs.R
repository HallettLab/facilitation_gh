

ppd_fig_loc = "data_analysis/models/evaluate/plot_posteriors/"

## plot posterior distributions

for(i in rain) {
  
  for(j in microbe){
    
    ## filter out specific treat
    tmp_df = sig_posteriors %>%
      filter(water == i, microbe == j)
    
    ## turn into matrix
    tmp_mat = as.matrix(tmp_df)

    ## save file
    pdf(file = paste0(ppd_fig_loc, "sigmoidal/", date, "/posts_m", j, "_w", i, date, ".pdf"))
    
    ## plot all the posteriors
    print(mcmc_areas(tmp_mat,
                 pars = c("c"),
                 prob = 0.8))

    print(mcmc_areas(tmp_mat,
               pars = c("alpha_slope"),
               prob = 0.8))
    
    print(mcmc_areas(tmp_mat,
               pars = c("alpha_initial"),
               prob = 0.8))
    
    print(mcmc_areas(tmp_mat,
               pars = c("N_opt"),
               prob = 0.8))
    
    print(mcmc_areas(tmp_mat,
               pars = c("lambda"),
               prob = 0.8))
    
    print(mcmc_areas(tmp_mat,
               pars = c("disp"),
               prob = 0.8))
    
    dev.off()

  }
}

