## Header ##
## 
## Script Name: Plot Sigmoidal Function
##
## Purpose: Use medians of posterior distributions to visualize alpha values and fecundity along a density of ACAM
## 
## Author: Code adapted from Lisa Buche

# Set up ####
## load packages
library(tidyverse)

theme_set(theme_classic())

## create alpha function
alpha_function4_wnegs  <- function(Amin, Aslopes,c,N,N0){
  
  e = exp(Aslopes*(N-N0)) # c is stretching the graph horizontally 
  a = c*(1-e) #stretching the graph vertically
  d = Amin
  alpha = (-a/(1 + e)) - d
  
  return(alpha)
  
}

alpha_function4  <- function(Amin, Aslopes,c,N,N0){
  
  e = exp(Aslopes*(N-N0)) # c is stretching the graph horizontally 
  a = c*(1-e) #stretching the graph vertically
  d = Amin
  alpha = (a/(1 + e)) + d
  
  return(alpha)
  
}

## create function for fecundity
fec_function = function(lambda, Amin, Aslopes, c, N_acam, N0, N_brho, alpha_brho) {
  
  e = exp(Aslopes*(N_acam-N0)) # c is stretching the graph horizontally 
  a = c*(1-e) #stretching the graph vertically
  d = Amin
  alpha_acam = (a/(1 + e)) + d
  
  fecundity = N_brho*lambda * exp(-(N_brho*alpha_brho) - (N_acam*alpha_acam))
  
  return(fecundity)
  
}

fec_function_swapsigns = function(lambda, Amin, Aslopes, c, N_acam, N0, N_brho, alpha_brho) {
  
  e = exp(Aslopes*(N_acam-N0)) # c is stretching the graph horizontally 
  a = c*(1-e) #stretching the graph vertically
  d = Amin
  alpha_acam = (-a/(1 + e)) - d
  
  fecundity = N_brho*lambda * exp(-(N_brho*alpha_brho) + (N_acam*alpha_acam))
  
  return(fecundity)
  
}



for(i in rain) {
  for(j in microbe){
    
    
    
  }
  
}



## high water
postw1df = as.data.frame(post_w1)

Aminw1 = median(postw1df$alpha_initial)
Aslopesw1 = median(postw1df$alpha_slope)
cw1 = median(postw1df$c)
N0w1 = median(postw1df$N_opt)

w1plotdat = data.frame(density = c(0:80), alpha = alpha_function4(Aminw1, Aslopesw1, cw1, N = c(0:80), N0w1), water = "High")


## intermediate water
postw0.75df = as.data.frame(post_w0.75)

Aminw0.75 = median(postw0.75df$alpha_initial)
Aslopesw0.75 = median(postw0.75df$alpha_slope)
cw0.75 = median(postw0.75df$c)
N0w0.75 = median(postw0.75df$N_opt)

w0.75plotdat = data.frame(density = c(0:80), alpha = alpha_function4(Aminw0.75, Aslopesw0.75, cw0.75, N = c(0:80), N0w0.75), water = "Intermediate")

## low water
postw0.6df = as.data.frame(post_w0.6)

Aminw0.6 = median(postw0.6df$alpha_initial)
Aslopesw0.6 = median(postw0.6df$alpha_slope)
cw0.6 = median(postw0.6df$c)
N0w0.6 = median(postw0.6df$N_opt)
lambdaw0.6 = median(postw0.6df$lambda)
alpha_brhow0.6 = median(postw0.6df$alpha_brho)

## as normal
w0.6plotdat = data.frame(density = c(0:80), alpha = alpha_function4(Aminw0.6, Aslopesw0.6, cw0.6, N = c(0:80), N0w0.6), water = "Low")

ggplot(w0.6plotdat, aes(x=density, y=alpha, color = water)) +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  coord_cartesian(xlim = c(0,80)) +
  scale_fill_manual(values = c("#de8a5a")) +
  xlab("Density") +
  ylab("Alpha value") +
  labs(fill = "Water") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Signs normal")

ggsave("data_analysis/models/evaluate/plot_alpha_fecundity/sigmoidal/20250113/alpha_normal_v_density.png", width = 5, height = 3.5)

## with negatives
w0.6plotdat_negs = data.frame(density = c(0:80), alpha = alpha_function4_wnegs(Aminw0.6, Aslopesw0.6, cw0.6, N = c(0:80), N0w0.6), water = "Low")

ggplot(w0.6plotdat_negs, aes(x=density, y=alpha, color = water)) +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  coord_cartesian(xlim = c(0,80)) +
  scale_fill_manual(values = c("#de8a5a")) +
  xlab("Density") +
  ylab("Alpha value") +
  labs(fill = "Water") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("Signs Reversed")

ggsave("data_analysis/models/evaluate/plot_alpha_fecundity/sigmoidal/20250113/alpha_w_negs_v_density.png", width = 5, height = 3.5) 

plotdat = rbind(w1plotdat, w0.75plotdat, w0.6plotdat)


w0.6plotdat = data.frame(density = c(0:70), fecundity = fec_function(lambda = lambdaw0.6, Amin = Aminw0.6, Aslopes = Aslopesw0.6, c = cw0.6, N_acam = c(0:70), N0 = N0w0.6, N_brho = 4, alpha_brho = alpha_brhow0.6), water = "Low")

ggplot(w0.6plotdat, aes(x=density, y=fecundity, color = water)) +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  coord_cartesian(xlim = c(0,70)) +
  scale_fill_manual(values = c("#de8a5a")) +
  xlab("Density") +
  ylab("Fecundity") +
  labs(fill = "Water") +
  geom_hline(yintercept = 0, linetype = "dashed")


w0.6plotdat_signswap = data.frame(density = c(0:70), fecundity = fec_function_swapsigns(lambda = lambdaw0.6, Amin = Aminw0.6, Aslopes = Aslopesw0.6, c = cw0.6, N_acam = c(0:70), N0 = N0w0.6, N_brho = 4, alpha_brho = alpha_brhow0.6), water = "Low")

ggplot(w0.6plotdat_signswap, aes(x=density, y=fecundity, color = water)) +
  geom_point(aes(fill = water), colour = "black", pch = 21, size = 3.5) +
  coord_cartesian(xlim = c(0,70)) +
  scale_fill_manual(values = c("#de8a5a")) +
  xlab("Density") +
  ylab("Fecundity") +
  labs(fill = "Water") +
  geom_hline(yintercept = 0, linetype = "dashed")


