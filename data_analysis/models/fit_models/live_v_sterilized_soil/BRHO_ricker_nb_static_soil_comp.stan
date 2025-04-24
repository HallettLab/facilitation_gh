// BRHO model

// Ricker growth model
// Model as a negative binomial

// update model to incorporate negative signs into alpha coefficients so that a positive coefficient = facilitation and a negative coefficient = competition; 
// changing to this to mirror Lisa's approach and to ensure that no small sign mistakes are made that mess up interpretation.

data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  vector[N] N_i; // population size of species i at time t
  vector[N] acam; // population size of interacting species at time t
  vector[N] trt; // microbial treatment

}

parameters{

  // dispersion parameter 
  real<lower=0> disp; 

  // lambda
  real<lower = 0, upper = 8000> lambda;
  real lambda_dev; // include a deviation with soil microbial condition

   real alpha_brho;
   real alpha_brho_dev;
   
   real alpha_acam;
   real alpha_acam_dev;
}


// calc additional parameters based on the sampled param values without effecting the sampling itself
transformed parameters{
  
  // create vector of predictions
  vector<lower=0>[N] F_hat;
  

  // Biological model
  for(i in 1:N){

    F_hat[i] = (lambda + lambda_dev*trt[i]) * exp( (acam[i]*(alpha_acam + alpha_acam_dev*trt[i])) + (N_i[i]*(alpha_brho + alpha_brho_dev*trt[i]))) ;
    
  }
  
}

model{
  
  // priors
  // lambda & alpha priors all come from mega-comp values
  lambda ~ normal(200, 50);
  lambda_dev ~normal(0,7);
  // the final distrib is barely changing from this; ok to tighten prior
  
  alpha_brho ~ normal(-0.057, 0.25); 
  alpha_brho_dev ~ normal(0, 0.05);
  // negative since competition is expected
  // adjusted the variance on this parameter to avoid such a large value  
  
  disp ~ cauchy(0, 1);
  // safer to place prior on disp than on phi (the actual error term)

  alpha_acam ~ normal(0.099, 0.25); 
  alpha_acam_dev ~ normal(0, 0.25);
  
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
  
}
