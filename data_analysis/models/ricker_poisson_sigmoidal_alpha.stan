// Ricker growth model
// Model as a negative binomial
// Incorporate random effects of blocks
// Models both precip treatments together

data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  
  // could consider adding in prior info from mega-comp models!! I think would put as data here??
  // leave for now, but should do eventually!! 
 
  vector[N] N_i; // population size of species i at time t
  //vector[N] trt; // precip treatment
  
  // population sizes of interacting species at time t
  vector[N] acam;

}


parameters{

  // lambda
  real<lower = 0, upper = 10000> lambda;
  //real lambda_dev;
  
  // params for sigmoidal alpha function: 
  real<lower = 0> N_opt; //optimal density of ACAM that maximizes BRHO fecundity
  real<upper = 0> c; //stretching parameter
  
  real<lower=-1, upper=0> alpha_slope; // decay parameter - impact of the addition of one indiv j on the fecundity of i
  real alpha_initial; //initial effect of j on i when Nj is minimal
  
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
  // previous prior on this was pretty stupid (normal(3300, 1))
  // perhaps this contributed to poor model fit
  // planted densities 3 and 6 were the highest RII values; choose between this to be the prior estimated N_opt
  
  
  // Biological model
  for(i in 1:N){
    
    alpha_acam[i] = alpha_initial + ( (c* (1 - exp(alpha_slope * (acam[i] - N_opt)) )) / (1 + exp(alpha_slope * (acam[i] - N_opt))) );

    //F_hat[i] = N_i[i] * (lambda) * exp( -N_i[i] * (alpha_brho) ) * exp( -acam[i] *  ) ;
    
    //F_hat2[i] = F_hat[i]*epsilon[Blocks[i]]; // effect of block 
    
    F_hat[i] = N_i[i] * (lambda) * exp( (-N_i[i] * (alpha_brho))  - (acam[i] * (alpha_acam[i]) )) ;
    
    
  }
  
  // calculate the likelihood
 Fecundity ~ poisson(F_hat);
  // likelihood outside of for-loop
  // could think of this as observation error term
  
}

