// INTRA SPECIFIC MODEL

// LIVE SOIL ONLY

// Ricker growth model
// Model as a negative binomial

// update model to incorporate negative signs into alpha coefficients so that a positive coefficient = facilitation and a negative coefficient = competition; 
// changing to this to mirror Lisa's approach and to ensure that no small sign mistakes are made that mess up interpretation.

data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  vector[N] N_i; // population size of species i at time t
  vector[N] brho; // population size of interacting species at time t
  //vector[N] trt; // microbial treatment

}

parameters{

  // dispersion parameter 
  real<lower=0> disp; 

  // lambda
  real<lower = 0, upper = 8000> lambda;
  //real lambda_dev; // include a deviation with soil microbial condition

  // intERspecific alpha
   real alpha_brho;

  // params for sigmoidal alpha function: 
  real<lower = 0> N_opt; //optimal density of ACAM that maximizes ACAM fecundity
  real<upper = 0> c; //vertical stretching parameter

  real<lower = -1, upper = 0> alpha_slope; // density-dependent parameter
  // not fully clear why alpha_slope is bounded by -1 - 0; models show it bumping up against the -1 boundary; leave as is for now
  
  // take away upper 0 bound and see how this effects model fit
  
  real alpha_initial; //density-independent effect of j on i
  
  // deviation params for sigmoid function params
  //real N_opt_dev;
  //real c_dev;
  //real alpha_slope_dev;
  //real alpha_initial_dev;
  
  // do all of these need to be allowed to vary? Might be tricky to estimate... 
  // give it a try like this at first and see what comes out I guess

}


// calc additional parameters based on the sampled param values without effecting the sampling itself
transformed parameters{
  
  // create vector of predictions
  vector<lower=0>[N] F_hat;
  vector[N] alpha_acam;

  // Biological model
  for(i in 1:N){
    
    alpha_acam[i] = (alpha_initial) + (   ( (c) *(1 - exp((alpha_slope)*(N_i[i] - (N_opt))))) / (1 + exp((alpha_slope ) * (N_i[i] - (N_opt))) )   ) ;


    F_hat[i] = (lambda) * exp( (N_i[i]*(alpha_acam[i])) + (brho[i]*alpha_brho)) ;
    
  }
  
}

model{
  
  // priors
  // lambda & alpha priors all come from mega-comp values
  lambda ~ normal(62, 30);
  
  alpha_brho ~ normal(-0.024, 0.25);
  // positive since self-facilitation is expected
  
  disp ~ cauchy(0, 1);
  // safer to place prior on disp than on phi (the actual error term)

  alpha_initial ~ normal(0.185, 0.25);
  // replace normal(0, 0.2) prior with alpha_brho from mega-comp models
  //alpha_initial_dev ~ normal(0, 0.25);
  
  //alpha_slope ~ normal(0, 0.1);
  alpha_slope ~ normal(-0.2, 0.2); // try this out just to see what happens?
  // want value somewhat near 0; aiming to make the prior a straight line
  //alpha_slope_dev ~ normal(0, 0.25);

  c ~ normal(0, 0.1);
  //c_dev ~ normal(0, 0.25);
  
  N_opt ~ exponential(0.9); 
  // N_opt = the optimal density of ACAM that maximizes fecundity of ACAM
  // try giving this one a wider range to see what it does? 
  
  //N_opt_dev ~ normal(0, 0.25);
  
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
