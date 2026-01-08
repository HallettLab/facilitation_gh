
## load models
source("2_calculate_interactions/population_models/2_evaluate/2_load_models.R")

# Set up ####
ppd_fig_loc = "../output/diagnostics/20260107/posterior_distributions"

theme_set(theme_classic())

# BRHO ####
## Sigmoidal ####
### Plot to PDF ####
## plot posterior distributions
for(i in rain) {
    
    ## filter out specific treat
    tmp_df = brho_sig_posteriors %>%
      filter(water == i)
    
    ## turn into matrix
    tmp_mat = as.matrix(tmp_df)

    ## save file
    pdf(file = paste0(ppd_fig_loc, "sigmoidal/", date, "/brho_posts_w", i, date, "_0.pdf"))
    
    ## plot all the posteriors
    print(mcmc_areas(tmp_mat,
                     pars = c("alpha_brho"),
                     prob = 0.8))
    
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

### Alpha BRHO ####
## compare alpha_brho values
a = sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.075, -0.02) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")
  
b = sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.075, -0.02) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c = sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.075, -0.02) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a, b, c, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/alpha_brho_posts_", date, ".png"), width = 4, height = 5)

### Lambda ####
## compare lambda values
a1 = sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(220, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b1 = sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(220, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c1 = sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(220, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a1, b1, c1, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/lambda_posts_", date, ".png"), width = 4, height = 5)

### Alpha Initial ####
## compare lambda values
a2 = sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.01, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b2 = sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.01, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c2 = sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.01, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a2, b2, c2, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/alpha_initial_posts_", date, ".png"), width = 4, height = 5)

### N Optimum ####
a3 = sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
  xlim(0, 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b3 = sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
  xlim(0, 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c3 = sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
  xlim(0, 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a3, b3, c3, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/N_opt_posts_", date, ".png"), width = 4, height = 5)

### Alpha Slope ####
a4 = sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_slope"), prob = 0.8) + 
  xlim(-1, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b4 = sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_slope"), prob = 0.8) + 
  xlim(-1, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c4 = sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_slope"), prob = 0.8) + 
  xlim(-1, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a4, b4, c4, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/alpha_slope_posts_", date, ".png"), width = 4, height = 5)

### C ####
a5 = sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("c"), prob = 0.8) + 
  xlim(-0.3, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b5 = sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("c"), prob = 0.8) + 
  xlim(-0.3, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c5 = sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("c"), prob = 0.8) + 
  xlim(-0.3, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a5, b5, c5, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/c_posts_", date, ".png"), width = 4, height = 5)


## Static ####
### Plot to PDF ####
## plot posterior distributions
for(i in rain) {
    
    ## filter out specific treat
    tmp_df = brho_stat_posteriors %>%
      filter(water == i)
    
    ## turn into matrix
    tmp_mat = as.matrix(tmp_df)
    
    ## save file
    pdf(file = paste0(ppd_fig_loc, "static/", date, "/posts_w", i, "_", date, ".pdf"))
    
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

### Alpha BRHO ####
## compare alpha_brho values
a = stat_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.07, -0.02) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b = stat_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.07, -0.02) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c = stat_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.07, -0.02) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a, b, c, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "static/", date, "/alpha_brho_posts_", date, ".png"), width = 4, height = 5)

### Lambda ####
## compare lambda values
a1 = stat_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(300, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b1 = stat_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(300, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c1 = stat_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(300, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a1, b1, c1, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "static/", date, "/lambda_posts_", date, ".png"), width = 4, height = 5)

### Alpha Acam ####
a2 = stat_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.025, 0.035) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b2 = stat_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.025, 0.035) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c2 = stat_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.025, 0.035) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a2, b2, c2, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "static/", date, "/alpha_acam_posts_", date, ".png"), width = 4, height = 5)

# ACAM ####
## Static ####
### Plot to PDF ####
## plot posterior distributions
for(i in rain) {
  
  ## filter out specific treat
  tmp_df = acam_stat_posteriors %>%
    filter(water == i)
  
  ## turn into matrix
  tmp_mat = as.matrix(tmp_df)
  
  ## save file
  pdf(file = paste0(ppd_fig_loc, "static/", date, "/acam_posts_w", i, date, ".pdf"))
  
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

### Alpha BRHO ####
## compare alpha_brho values
a = acam_stat_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.08, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b = acam_stat_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.08, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c = acam_stat_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.08, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a, b, c, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "static/", date, "/alpha_brho_posts_", date, "_acam_model.png"), width = 4, height = 5)

### Lambda ####
a1 = acam_stat_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(0, 75) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b1 = acam_stat_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(0, 75) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c1 = acam_stat_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(0, 75) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a1, b1, c1, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "static/", date, "/lambda_posts_", date, "_acam_model.png"), width = 4, height = 5)

### Alpha Acam ####
a2 = acam_stat_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.2, -0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b2 = acam_stat_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.2, -0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c2 = acam_stat_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_acam"), prob = 0.8) + 
  xlim(-0.2, -0.1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a2, b2, c2, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "static/", date, "/alpha_acam_posts_", date, "_acam_model.png"), width = 4, height = 5)

## Sigmoidal ####
### Plot to PDF ####
## plot posterior distributions
for(i in rain) {
  
  ## filter out specific treat
  tmp_df = acam_sig_posteriors %>%
    filter(water == i)
  
  ## turn into matrix
  tmp_mat = as.matrix(tmp_df)
  
  ## save file
  pdf(file = paste0(ppd_fig_loc, "sigmoidal/", date, "/acam_posts_w", i, date, ".pdf"))
  
  ## plot all the posteriors
  print(mcmc_areas(tmp_mat,
                   pars = c("alpha_acam"),
                   prob = 0.8))
  
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

### Alpha ACAM ####
## compare alpha_brho values
a = acam_sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.075, -0.02) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b = acam_sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.075, -0.02) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c = acam_sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_brho"), prob = 0.8) + 
  xlim(-0.075, -0.02) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a, b, c, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/alpha_brho_posts_", date, ".png"), width = 4, height = 5)

### Lambda ####
## compare lambda values
a1 = acam_sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(220, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b1 = acam_sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(220, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c1 = acam_sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("lambda"), prob = 0.8) + 
  xlim(220, 600) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a1, b1, c1, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/lambda_posts_", date, ".png"), width = 4, height = 5)

### Alpha Initial ####
## compare lambda values
a2 = acam_sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.01, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b2 = acam_sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.01, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c2 = acam_sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_initial"), prob = 0.8) + 
  xlim(-0.01, 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a2, b2, c2, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/alpha_initial_posts_", date, ".png"), width = 4, height = 5)

### N Optimum ####
a3 = acam_sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
  xlim(0, 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b3 = acam_sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
  xlim(0, 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c3 = acam_sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("N_opt"), prob = 0.8) + 
  xlim(0, 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a3, b3, c3, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/N_opt_posts_", date, ".png"), width = 4, height = 5)

### Alpha Slope ####
a4 = acam_sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_slope"), prob = 0.8) + 
  xlim(-1, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b4 = acam_sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_slope"), prob = 0.8) + 
  xlim(-1, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c4 = acam_sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("alpha_slope"), prob = 0.8) + 
  xlim(-1, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a4, b4, c4, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/alpha_slope_posts_", date, ".png"), width = 4, height = 5)

### C ####
a5 = acam_sig_posteriors %>%
  filter(water == 1) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("c"), prob = 0.8) + 
  xlim(-0.3, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("High water")

b5 = acam_sig_posteriors %>%
  filter(water == 0.75) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("c"), prob = 0.8) + 
  xlim(-0.3, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Int water")

c5 = acam_sig_posteriors %>%
  filter(water == 0.6) %>%
  as.matrix() %>%
  mcmc_areas(pars = c("c"), prob = 0.8) + 
  xlim(-0.3, 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Low water")

ggarrange(a5, b5, c5, ncol = 1, nrow = 3)

ggsave(paste0(ppd_fig_loc, "sigmoidal/", date, "/c_posts_", date, ".png"), width = 4, height = 5)
