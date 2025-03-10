# run coexistence models for dry versus wet

# Set up ####
## Read in Data ####
source("data_analysis/models/")

## germination data
germ = read.csv("data/germination_data.csv")
seedsurv = read.csv("data/seed_survival_sumdat.csv")

theme_set(theme_classic())

# Equations ####
## 'Static' ####
### equilibrium abundance of resident sp
run.to.equilibrium <- function(surv, germ, lambda, alpha_intra, Nt, alpha_inter, germ_inter, inter_abund) {
  Ntp1 <- (1-germ)*surv*Nt + germ*lambda*Nt*exp(alpha_intra *germ* Nt + alpha_inter*germ_inter*inter_abund)
  return(Ntp1)
}

# Run to Equilibrium ####
## ACAM ####
### Set up loop ####
all_intra <- c("alpha_acam",  
               "alpha_brho") 

options <- length(all_intra)

time <- 300
runs <- 200

N <- array(NA, c(options, runs, time))
N[,,1] <- 100 # start with 100 individuals in every case
## create an array where each of the rows is one of the species-treatment combos arranged in the order of all_intra. 
## Each of the columns is one separate run of the model
## Each of the stacked matrices represents a particular time slice

## create empty dataframes
residents_low <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))
residents_int <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))
residents_high <- as.data.frame(matrix(data = NA, nrow = 200, ncol = 1))

set.seed(42)

loop = names(acam_stat_posts)
loop = "acam_w1"
  
### Loop ####
for(i in 1:length(loop)) {
  
  ## select the species
  datset <- acam_stat_posts[[i]] 
  
  ## set the intraspecific alpha name
  intra <- "alpha_acam"
  
  ## make a vector of the length of the posterior distribution
  post_length <- c(1:4000)
  
  ## get rid of warmup runs by selecting last 4000 runs
  ## get list of all intraspecific alphas
  all_intras <- datset[[intra]][4001:8000]
  
  lambda = datset$lambda[4001:8000]
  
  if (names(acam_stat_posts)[i] == "acam_w1") {
    trt = "C"
  } else {
    trt = "D"
  }
  
  g.A = germ[germ$phyto == "ACAM" & germ$treatment == trt, ]$mean.germ
  s.A = seedsurv[seedsurv$species == "ACAM", ]$surv.mean.p
  
  ## loop thru each time step
  for(t in 1:(time-1)) {
    
    ## randomly sample indices from the length of posterior distrib 200x
    posts <- sample(post_length, runs, replace=TRUE)
    
    ## use these indices to select 200 lambda values
    lam <- lambda[posts]
    
    ## use again to select 200 intra_alpha values
    alpha_intra <- all_intras[posts]
    
    ## for each sp, use the run to equil function to fill one row of data that uses the abundance at time t and outputs the abundance at time t+1
   
    ## loops through 300 time steps; at each time step, 200 new lambda/alpha vals are sampled and used
    
    ## this should then fill out all cols (independent runs) in one row (sp) in all the matrices (time steps) before going to the next row (sp)
    N[i, ,t+1] <- run.to.equilibrium(germ = g.A, 
                                     surv = s.A,
                                     lambda = lam, 
                                     alpha_intra = alpha_intra, 
                                     Nt = N[i, ,t], 
                                     alpha_inter = 0,
                                     germ_inter = 0,
                                     inter_abund = 0) 
    
  }
  
  tmp.df <- data.frame(N[i,,300]) ## selecting time 300, the final run of the run.to.equil function thru time
  
  ## change the column name to be the species
  #names(tmp.df) <- substr(names(ricker_posteriors)[i], 1, 4)
  
  ## append temporary df to the empty df created earlier
  residents_high <-  cbind(residents_high,  tmp.df)
  
}

# remove first column
residents_high <- residents_high[,-1]




## BRHO ####
for(i in 1:length(names(wet))) {
  
  ## select the species
  datset <- wet[[i]] 
  
  ## set the intraspecific alpha name
  intra <- paste0("alpha_", tolower(substr(names(wet)[i], 1, 4)))
  
  ## make a vector of the length of the posterior distribution
  post_length <- length(datset$lambda)
  
  ## get list of all intraspecific alphas
  all_intras <- datset[[intra]]
  
  ## loop thru each time step
  for(t in 1:(time-1)) {
    
    ## randomly sample indices from the length of posterior distrib 200x
    posts <- sample(post_length, runs, replace=TRUE)
    
    ## use these indices to select 200 lambda values
    lambda <- datset$lambda[posts]
    
    ## use again to select 200 intra_alpha values
    alpha_intra <- all_intras[posts]
    
    ## for each sp x treat combo use the run to equil function to fill one row of data that uses the abundance at time t and outputs the abundance at time t+1
    ## as the model loops thru sp x treat combos, more rows of the array are filled out
    N[i, ,t+1] <- run.to.equilibrium(germ = datset$germ, 
                                     surv = datset$surv,
                                     lambda = lambda, 
                                     alpha_intra = alpha_intra, 
                                     Nt = N[i, ,t], 
                                     alpha_inter = 0,
                                     germ_inter = 0,
                                     inter_abund = 0) 
    
  }
  
  tmp.df <- data.frame(N[i,,300])
  
  ## change the column name to be the species
  names(tmp.df) <- substr(names(ricker_posteriors)[i], 1, 4)
  
  ## append temporary df to the empty df created earlier
  residents_wet <-  cbind(residents_wet,  tmp.df)
  
}

# remove first column
residents_wet <- residents_wet[,-1] 

# Check Equil Abundances ####
## change to long format for visualization
residents_wet_long <- residents_wet %>%
  mutate(num = 1:length(PLER)) %>%
  pivot_longer(1:16, names_to = "species", values_to = "equil_abund")

residents_dry_long <- residents_dry %>%
  mutate(num = 1:length(PLER)) %>%
  pivot_longer(1:16, names_to = "species", values_to = "equil_abund")

## visualize
ggplot(residents_wet_long, aes(x=num, y=equil_abund)) +
  geom_point() +
  facet_wrap(~species, scales = "free") +
  xlab("Run Number") + ylab("Equilibrium Abundance") +
  ggtitle("Ambient Conditions, Equilibrium, Nt1 = 100, 12/18/23")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/", date, "/equil_abund_ricker_negbinom_C_Nt100_", date, ".png"), height = 6, width = 10)

ggplot(residents_dry_long, aes(x=num, y=equil_abund)) +
  geom_point() +
  facet_wrap(~species, scales = "free") +
  xlab("Run Number") + ylab("Equilibrium Abundance") +
  ggtitle("Drought Conditions, Equilibrium, Nt1 = 100, 12/18/23")

ggsave(paste0("analyses/classic_MCT/preliminary_equil_abundance/", date, "/equil_abund_ricker_negbinom_D_Nt100", date, ".png"), height = 6, width = 10)
