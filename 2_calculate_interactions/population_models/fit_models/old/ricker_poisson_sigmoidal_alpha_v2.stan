// Ricker growth model
// Model as a poisson
// Incorporate random effects of blocks
// Models both precip treatments together

data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  vector[N] N_i; // population size of species i at time t
  vector[N] acam; // population size of interacting species at time t

}

parameters{

  // lambda
  real<lower = 0, upper = 10000> lambda;
  //real lambda_dev;
  
  // params for sigmoidal alpha function: 
  real<lower = 0> N_opt; //optimal density of ACAM that maximizes BRHO fecundity
  real<upper = 0> c; //vertical stretching parameter
  
  real<lower=-1, upper=0> alpha_slope; // density-dependent parameter (slope) - rate of change in per cap effect of sp j on i
  
  real alpha_initial; //density-independent effect of j on i
  
  real alpha_brho;

}


model{
  
  // create vector of predictions
  vector[N] F_hat;
  vector[N] alpha_acam;
  //vector[N] F_hat2;
  
  // set priors
  lambda ~ normal(201, 53); 
  // mean of lambda for BRHO from mega-comp is 201; sd = 53
  // no moment matching required as using normal distribution currently
  
  
  alpha_brho ~ normal(0.057, 0.011);
  // prior taken from mega-comp posterior distributions

  alpha_initial ~ normal(0, 0.1);
  alpha_slope ~ normal(-0.2, 0.2);
  c ~ normal(0, 0.1);
  
  N_opt ~ normal(5, 1); 
  // N_opt = the optimal density of ACAM that maximizes fecundity of BRHO
  // planted densities 3 and 6 were the highest RII values; choose between this to be the prior estimated N_opt
  
  // Biological model
  for(i in 1:N){
    
    alpha_acam[i] = alpha_initial + ( (c* (1 - exp(alpha_slope * (acam[i] - N_opt)) )) / (1 + exp(alpha_slope * (acam[i] - N_opt))) );

    F_hat[i] = N_i[i] * (lambda) * exp( (-N_i[i] * (alpha_brho))  - (acam[i] * (alpha_acam[i]) )) ;
    
  }
  
  // calculate the likelihood
 Fecundity ~ poisson(F_hat);
  
}

