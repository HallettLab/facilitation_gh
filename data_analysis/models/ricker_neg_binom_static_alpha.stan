// Ricker growth model
// Model as a negative binomial
// Incorporate random effects of blocks
// Models both precip treatments together

data{
  
  int<lower = 1> N; // number of observations
  int Fecundity[N]; // observed fecundity count at time t+1
  
  // could consider adding in prior info from mega-comp models!! I think would put as data here??
  // leave for now, but should do eventually!! 
  
  // add in random effects
  //int<lower=1> N_blocks; // Number of groups
  //int Blocks[N]; // block column
  
  vector[N] N_i; // population size of species i at time t
  //vector[N] trt; // precip treatment
  
  // population sizes of interacting species at time t
  vector[N] acam;

}


parameters{
  
  // group-level random effects
  //real epsilon[N_blocks]; 
  //real<lower = 0> sigma;
  
  // dispersion deviation parameter 
  real<lower=0> disp_dev; 

  // lambda
  real<lower = 0, upper = 10000> lambda;
  //real lambda_dev;
  
  // params for sigmoidal alpha function: 
  //real<lower = 0> N_opt; //optimal density of ACAM that maximizes BRHO fecundity
  //real<upper = 0> c; //stretching parameter
  
  //real<lower=-1, upper=0> alpha_slope; // decay parameter - impact of the addition of one indiv j on the fecundity of i
  //real alpha_initial; //initial effect of j on i when Nj is minimal
  
  // all the alphas
   real alpha_acam; // shouldn't need alpha_acam while using the function specified by Lisa
   real alpha_brho;
   
   // alpha deviations
   //real alpha_acam_dev;

}


model{
  
  // create vector of predictions
  vector[N] F_hat;
  //vector[N] F_hat2;
  
  // set priors
  //sigma ~ gamma(1, 1);
  //epsilon ~ gamma(sigma, sigma); // prior for group level random effects
  
  lambda ~ exponential(0.0009); // exponential distrib prior
  //lambda_dev ~ normal(0, 100);

  alpha_acam ~ normal(0, 5);
  alpha_brho ~ normal(0, 5);
  //alpha_acam_dev ~ normal(0,5);
  
  //alpha_initial ~ normal(0, 0.1);
  //alpha_slope ~ normal(-0.2, 0.2);
  //c ~ normal(0, 0.1);
  
  //N_opt ~ normal(7200, 1); // max fecundity of BRHO
  
  
  // Biological model
  for(i in 1:N){

    F_hat[i] = N_i[i] * (lambda) * exp( -N_i[i] * (alpha_brho) ) * exp( -acam[i] * (alpha_acam) ) ;
    
    //F_hat2[i] = F_hat[i]*epsilon[Blocks[i]]; // effect of block 
    
  }
  
  // calculate the likelihood
 Fecundity ~ neg_binomial_2(F_hat, disp_dev);
  // likelihood outside of for-loop
  // could think of this as observation error term
  
}

