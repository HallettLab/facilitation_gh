// Ricker growth model
// Model as a negative binomial

data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  vector[N] N_i; // population size of species i at time t
  vector[N] acam; // population sizes of interacting species at time t

}

parameters{
 
  // dispersion parameter 
  real<lower=0> disp; 

  // lambda
  real<lower = 0, upper = 10000> lambda;

  // all the alphas
   real alpha_acam; 
   real alpha_brho;

}


model{
  
  // create vector of predictions
  vector[N] F_hat;
  
  // priors
  lambda ~ normal(200, 50);
  alpha_acam ~ normal(-0.09875605, 0.25);
  alpha_brho ~ normal(0.05728218, 0.25);

  // Biological model
  for(i in 1:N){

    F_hat[i] = N_i[i] * (lambda) * exp( -N_i[i] * (alpha_brho) ) * exp( -acam[i] * (alpha_acam) ) ;
    
  }
  
  // calculate the likelihood
 Fecundity ~ neg_binomial_2(F_hat, disp);

}
