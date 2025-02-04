
# Set up ####
ppd_fig_loc = "data_analysis/models/evaluate/plot_posteriors/"

theme_set(theme_classic())

# BRHO ####
## Sigmoidal ####
### Plot to PDF ####
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

### Alpha BRHO ####
## compare alpha_brho values
a = sig_posteriors %>%
  filter(water == 1, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.08, 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Live soil")
  
b = sig_posteriors %>%
  filter(water == 0.75, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.08, 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Live soil")

c = sig_posteriors %>%
  filter(water == 0.6, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.08, 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Live soil")

d = sig_posteriors %>%
  filter(water == 1, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.08, 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Sterilized soil")

e = sig_posteriors %>%
  filter(water == 0.75, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.08, 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Sterilized soil") 

f = sig_posteriors %>%
  filter(water == 0.6, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8)  + 
  xlim(-0.08, 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Sterilized soil")

ggarrange(a, d, b, e, c, f, ncol = 2, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/alpha_brho_posts_m", j, "_w", i, date, ".png"), width = 6, height = 6)


### Lambda ####
## compare lambda values
a1 = sig_posteriors %>%
  filter(water == 1, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(100, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Live soil")

b1 = sig_posteriors %>%
  filter(water == 0.75, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(100, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Live soil")

c1 = sig_posteriors %>%
  filter(water == 0.6, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(100, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Live soil")

d1 = sig_posteriors %>%
  filter(water == 1, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(100, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Sterilized soil")

e1 = sig_posteriors %>%
  filter(water == 0.75, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(100, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Sterilized soil") 

f1 = sig_posteriors %>%
  filter(water == 0.6, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8)  + 
  xlim(100, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Sterilized soil")

ggarrange(a1, d1, b1, e1, c1, f1, ncol = 2, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/lambda_posts_m", j, "_w", i, date, ".png"), width = 6, height = 6)

### Alpha Initial ####
## compare lambda values
a2 = sig_posteriors %>%
  filter(water == 1, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Live soil")

b2 = sig_posteriors %>%
  filter(water == 0.75, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Live soil")

c2 = sig_posteriors %>%
  filter(water == 0.6, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Live soil")

d2 = sig_posteriors %>%
  filter(water == 1, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Sterilized soil")

e2 = sig_posteriors %>%
  filter(water == 0.75, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Sterilized soil") 

f2 = sig_posteriors %>%
  filter(water == 0.6, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8)  + 
  xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Sterilized soil")

ggarrange(a2, d2, b2, e2, c2, f2, ncol = 2, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/alpha_initial_posts_m", j, "_w", i, date, ".png"), width = 6, height = 6)

### N Optimum ####
## compare lambda values
a3 = sig_posteriors %>%
  filter(water == 1, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
  #xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Live soil")

b3 = sig_posteriors %>%
  filter(water == 0.75, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
 # xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Live soil")

c3 = sig_posteriors %>%
  filter(water == 0.6, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
 # xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Live soil")

d3 = sig_posteriors %>%
  filter(water == 1, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
 # xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Sterilized soil")

e3 = sig_posteriors %>%
  filter(water == 0.75, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
  #xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Sterilized soil") 

f3 = sig_posteriors %>%
  filter(water == 0.6, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8)  + 
  #xlim(-0.05, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Sterilized soil")

ggarrange(a3, d3, b3, e3, c3, f3, ncol = 2, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/N_opt_posts_m", j, "_w", i, date, ".png"), width = 6, height = 6)

## Static ####
### Plot to PDF ####
## plot posterior distributions
for(i in rain) {
  for(j in microbe){
    
    ## filter out specific treat
    tmp_df = stat_posteriors %>%
      filter(water == i, microbe == j)
    
    ## turn into matrix
    tmp_mat = as.matrix(tmp_df)
    
    ## save file
    pdf(file = paste0(ppd_fig_loc, "static/", date, "/posts_m", j, "_w", i, date, ".pdf"))
    
    ## plot all the posteriors
    print(mcmc_areas(tmp_mat,
                     pars = c("alpha_brho"),
                     prob = 0.8))
    
    print(mcmc_areas(tmp_mat,
                     pars = c("alpha_acam"),
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

### Alpha BRHO ####
## compare alpha_brho values
a = stat_posteriors %>%
  filter(water == 1, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.8, 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Live soil")

b = stat_posteriors %>%
  filter(water == 0.75, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.8, 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Live soil")

c = stat_posteriors %>%
  filter(water == 0.6, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.8, 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Live soil")

d = stat_posteriors %>%
  filter(water == 1, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.8, 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Sterilized soil")

e = stat_posteriors %>%
  filter(water == 0.75, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.8, 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Sterilized soil") 

f = stat_posteriors %>%
  filter(water == 0.6, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8)  + 
  xlim(-0.8, 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Sterilized soil")

ggarrange(a, d, b, e, c, f, ncol = 2, nrow = 3)

ggsave(paste0(ppd_fig_loc, "static/", date, "/alpha_brho_posts_m", j, "_w", i, date, ".png"), width = 6, height = 6)


### Lambda ####
## compare lambda values
a1 = stat_posteriors %>%
  filter(water == 1, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(175, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Live soil")

b1 = stat_posteriors %>%
  filter(water == 0.75, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(175, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Live soil")

c1 = stat_posteriors %>%
  filter(water == 0.6, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(175, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Live soil")

d1 = stat_posteriors %>%
  filter(water == 1, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(175, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Sterilized soil")

e1 = stat_posteriors %>%
  filter(water == 0.75, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(175, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Sterilized soil") 

f1 = stat_posteriors %>%
  filter(water == 0.6, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8)  + 
  xlim(175, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Sterilized soil")

ggarrange(a1, d1, b1, e1, c1, f1, ncol = 2, nrow = 3)

ggsave(paste0(ppd_fig_loc, "static/", date, "/lambda_posts_m", j, "_w", i, date, ".png"), width = 6, height = 6)

### Alpha Acam ####
a2 = stat_posteriors %>%
  filter(water == 1, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.05, 0.05) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Live soil")

b2 = stat_posteriors %>%
  filter(water == 0.75, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.05, 0.05) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Live soil")

c2 = stat_posteriors %>%
  filter(water == 0.6, microbe == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.05, 0.05) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Live soil")

d2 = stat_posteriors %>%
  filter(water == 1, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.05, 0.05) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water, Sterilized soil")

e2 = stat_posteriors %>%
  filter(water == 0.75, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.05, 0.05) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water, Sterilized soil") 

f2 = stat_posteriors %>%
  filter(water == 0.6, microbe == 0) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8)  + 
  xlim(-0.05, 0.05) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water, Sterilized soil")

ggarrange(a2, d2, b2, e2, c2, f2, ncol = 2, nrow = 3)

ggsave(paste0(ppd_fig_loc, "static/", date, "/alpha_acam_posts_m", j, "_w", i, date, ".png"), width = 6, height = 6)
