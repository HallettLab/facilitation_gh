// Ricker growth model
// Model as a negative binomial

// update model to incorporate negative signs into alpha coefficients so that a positive coefficient = facilitation and a negative coefficient = competition; 
// changing to this to mirror Lisa's approach and to ensure that no small sign mistakes are made that mess up interpretation.

data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  vector[N] N_i; // population size of species i at time t
  vector[N] acam; // population size of interacting species at time t

}

parameters{

  // dispersion parameter 
  real<lower=0> disp; 

  // lambda
  real<lower = 0, upper = 8000> lambda;

  // intraspecific alpha
   real alpha_brho;

  // params for sigmoidal alpha function: 
  real<lower = 0> N_opt; //optimal density of ACAM that maximizes BRHO fecundity
  real<upper = 0> c; //vertical stretching parameter
  // allowing this to go to 1 because it seems like this could aid in helping fit lines that look exponential. Also, if the alpha_slope is near 0, this value doesn't really matter
  // changing bound to just upper = 0, based on what Lisa does in her code
  
  real<lower = -1, upper = 0> alpha_slope; // density-dependent parameter
  // not fully clear why alpha_slope is bounded by -1 - 0; models show it bumping up against the -1 boundary; leave as is for now
  
  // take away upper 0 bound and see how this effects model fit
  
  real alpha_initial; //density-independent effect of j on i

}


// calc additional parameters based on the sampled param values without effecting the sampling itself
transformed parameters{
  
  // create vector of predictions
  vector<lower=0>[N] F_hat;
  vector[N] alpha_acam;

  // Biological model
  for(i in 1:N){
    
    alpha_acam[i] = alpha_initial + ( c*(1 - exp(alpha_slope*(acam[i] - N_opt))) / (1 + exp(alpha_slope * (acam[i] - N_opt))) ) ;


    F_hat[i] = N_i[i] * (lambda) * exp( (N_i[i] * (alpha_brho)) + (acam[i] * alpha_acam[i])) ; //intersp
    
    // the biological model is not correct yet; hvae questions for Lisa on it!
    
    
  }
  
}

model{
  
  // priors
  // lambda & alpha priors all come from mega-comp values
  lambda ~ normal(200, 50);
  alpha_brho ~ normal(0.05728218, 0.25);
  disp ~ cauchy(0, 1);
  // safer to place prior on disp than on phi (the actual error term)

  alpha_initial ~ normal(0, 0.1);
  
  //try flat priors on these parameters, esp since bounding b/w 0-1
  // model had trouble predicting both alpha_slope and c; both of which had a uniform prior put on them. Do they improve with a more specific prior?
  //alpha_slope ~ uniform(-1, 0);
  //c ~ uniform(-1, 0);
  
  alpha_slope ~ normal(-0.2, 0.2); // using priors from Lisa's model
  c ~ normal(0, 0.1);
  
  N_opt ~ normal(5, 1); 
  // N_opt = the optimal density of ACAM that maximizes fecundity of BRHO
  // planted densities 3 and 6 were the highest RII values; choose between this to be the prior estimated N_opt
  
  
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
