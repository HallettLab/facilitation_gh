## Header ##
## 
## Script Name: Plot Model Runs vs. Raw Data
##
## Purpose: predict seed output using the Fhat and phi values from every model run. Plot the distribution of seed output along with raw data for all models.
## 
## Author: Carmen Watkins; Code adapted from Lisa Buche

# Set up ####
## load packages
library(loo)
library(wesanderson)

fig_loc = "data_analysis/models/evaluate/plot_with_data/"
date = 20250110

## plot 
for(i in rain){
  for(j in microbe){
    
    # extract mu and phi
    mu = brho_stat_posts[[paste0("brho_m", j, "_w", i)]]$F_hat
    disp = brho_stat_posts[[paste0("brho_m", j, "_w", i)]]$disp
    phi = (disp^2)^(-1)
    
    # generating posterior predictions
    seed_pred <- matrix(nrow = dim(mu)[1], ncol = dim(mu)[2])
    
    for (r in 1:dim(mu)[1]) {     # for each posterior draw
      for (c in 1:dim(mu)[2]) {    # for each observation 
        # draw from the predicted distribution
        seed_pred[r, c] <- rnbinom(1, mu = mu[r, c], size = phi[r])  
      }
    }
   
    # get maximum density for plot limits
    max.density <- max(c(apply(seed_pred, 1, function(x) {max(density(x)$y)}), 
                         max(density(seed_pred)$y)))
    
    # dev.new(noRStudioGD = T)
    # start a plot with the first draw 
    col2 = wes_palette("FantasticFox1", n = 5)
    
    png(paste0(fig_loc, "static/", date, "/pred_seed_density_m", j, "_w", i, ".png"), width = 5, height = 4, units = "in", res = 250)
    
    plot(density(seed_pred[1, ]), ylim = c(0,max.density), 
                     col = col2,
                     ylab = 'Density',
                     xlab="Seed Output",
                    main = paste0("brho_m", j, "_w", i)) 
    
    for (r in 2:dim(seed_pred)[1]) {
      # add a line for each draw
      lines(density(seed_pred[r, ]), col = col2)
    }
    
    lines(density(brho.model[brho.model$water == i & brho.model$microbe == j,]$seeds.out), col = "black")
    
    dev.off()
    
  }
  
}
