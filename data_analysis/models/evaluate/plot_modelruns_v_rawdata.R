



library(loo)
library(wesanderson)

stan_post_pred_check_all <- function(post.draws,
                                     var_name = 'mu',
                                     stan.data,
                                     main,
                                     col1,
                                     col2,
                                     value.se,
                                     ...) {
  
  # currently using the loo package, can switch to rethinking
  
  
  ggplot(brho.model, aes(x=seeds.out)) +
    geom_density() +
    ggtitle("Seed output data")
  
  
  # phi is the overdispersion parameter for the neg binom model
  # mu is the mean for predicted seed number 
  
  ## extract model parameters
  FinalPosteriors <- rstan::extract(w1stat)
  
  Fec_df <- data.frame(Obs = 1:get(paste0("Fsim_",Code.focal,"_function_",function.int))[["DataVec"]]$N,
                       Fec =get(paste0("Fsim_",Code.focal,"_function_",function.int))[["DataVec"]]$Fecundity)
  
  
  
  
  # extract mu and phi
  mu <- FinalPosteriors[[F_hat]] # matrix with nrow = draws and ncol = observations
  
  ## FinalPosteriors[[F_hat]] ## didn't work
  
  mu = FinalPosteriors$F_hat
  disp <- FinalPosteriors$disp
  phi <- (disp^2)^(-1)
  
  
  # generating posterior predictions
  seed_pred <- matrix(nrow = dim(mu)[1], ncol = dim(mu)[2])
  for (i in 1:dim(mu)[1]) {     # for each posterior draw
    for (j in 1:dim(mu)[2]) {    # for each observation 
      # draw from the predicted distribution
      seed_pred[i, j] <- rnbinom(1, mu = mu[i, j], size = phi[i])  
    }
  }
  
  # get maximum density for plot limits
  max.density <- max(c(apply(seed_pred, 1, function(x) {max(density(x)$y)}), 
                       max(density(stan.data)$y)))
  
  # dev.new(noRStudioGD = T)
  # start a plot with the first draw 
  col2 <- wes_palette("FantasticFox1", n = 5)
  
  ppc.plot <- plot(density(seed_pred[1, ]), ylim = c(0, 0.0009), 
                   col = col2,
                   ylab = 'Seed density',
                   xlab="" ) #,
                   #main = main) 
  for (i in 2:dim(seed_pred)[1]) {
    # add a line for each draw
    ppc.plot <- lines(density(seed_pred[i, ]), col = col2)
  }
  
  
  lines(density(brho.model$seeds.out), col = "black")
  
  print(ppc.plot)
  
  ggplot(brho.model, aes(x=seeds.out)) +
    geom_density() +
    ggtitle("Seed output data")
  
 # ggsave("pred_seed_density.png", width = 5, height = 3)
  
  # add the actual data
  ppc.plot <- lines(density(stan.data), col = col1, lwd = 2)  
  ppc.plot <- text(labels = value.se, 
                   y= 0.04,#max.density - max.density/10,
                   x=50, #max(stan.data)- max(stan.data)/10,
                   cex=1, pos=3,col="black")
  print(ppc.plot)
}