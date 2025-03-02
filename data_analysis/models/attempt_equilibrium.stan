// Ricker growth model
// Model as a negative binomial

data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  vector[N] N_i; // population size of species i at time t
  vector[N] acam; // population size of interacting species at time t
  vector[]



}

parameters{
 
  // dispersion parameter 
  real<lower=0> disp; 

  // lambda
  real<lower = 0, upper = 8000> lambda;

  // all the alphas
   real alpha_acam; 
   real alpha_brho;

}

// calc additional parameters based on the sampled param values without affecting the sampling itself
transformed parameters{
  
  // create vector of predictions
  vector<lower=0>[N] F_hat;
  
  // Biological model
  for(i in 1:N){

    F_hat[i] = (lambda) * exp((N_i[i]*(alpha_brho)) + (acam[i]*(alpha_acam)));
   
    // have changed signs so that facil = positive; comp = negative
  }
  
}


model{
  
  // priors
  // lambda & alpha priors all come from mega-comp values
  lambda ~ normal(200, 50);
  alpha_acam ~ normal(0.099, 0.25); 
  // positive since facilitation is expected
  
  alpha_brho ~ normal(-0.057, 0.25); 
  // negative since self-competition is expected
  
  disp ~ cauchy(0, 1);
  // safer to place prior on disp than on phi (the actual error term)
  
  // calculate the likelihood
 Fecundity ~ neg_binomial_2(F_hat, (disp^2)^(-1));

}


// try out Fhat_simulated to use in model comparison
generated quantities {
  
  vector[N] F_sim; 
  
   for(i in 1:N){
    if(F_hat[i] <= 0) break ;
    F_sim[i] = neg_binomial_2_lpmf(Fecundity[i]|F_hat[i],(disp^2)^(-1));
              }
  
  // real neg_binomial_2_lpmf(ints n | reals mu, reals phi) [1]
  // takes integer values (Fecundity), mu, and phi

  
}
