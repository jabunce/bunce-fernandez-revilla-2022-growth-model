
data {
  int<lower=1> N;                      // number of height observations
  vector<lower=0>[N] age;              // vector of ages for each observation (in years), can be ~0 (conception)
  vector<lower=0>[N] height;           // vector of height observations (in cm)

  real<lower=0> meanA;                 // means and stdevs from PriorPredictJPA
  real<lower=0> meanC1;
  real<lower=0> meanC2;
  real<lower=0> meanC3;  
  real<lower=0> meanD1;
  real<lower=0> meanD2;
  real<lower=0> meanD3;
  //real<lower=0> meanE;  


  real<lower=0> sdA;
  real<lower=0> sdC1;
  real<lower=0> sdC2;
  real<lower=0> sdC3;  
  real<lower=0> sdD1;
  real<lower=0> sdD2;
  real<lower=0> sdD3;
  //real<lower=0> sdE;

  real<lower=0> muLsigma;         // mean measurement error stdev on log scale
}

transformed data {
  vector[N] log_height;

  log_height = log(height);
}


parameters {
  real<lower=0,upper=400> muA;    // mean adult height (in cm)
  real<lower=0> muC1;             // exponents
  real<lower=0> muC2;
  real<lower=0> muC3;  
  real<lower=0> muD1;             // time scale factors
  real<lower=0> muD2; 
  real<lower=0> muD3;
  //real<lower=0> E;                // shift function on age axis

  real<lower=0,upper=0.005> Lsigma;        // variance of the logged normal distribution, upper limit of 0.005 yields 95% of observations within 2*stdev = 1cm of actual height at 100cm
}


model {
  vector[N] mu;                   // mean of un-logged normal distribution

  muA ~ normal(meanA, sdA);
  muC1 ~ normal(meanC1, sdC1);
  muC2 ~ normal(meanC2, sdC2); 
  muC3 ~ normal(meanC3, sdC3);  
  muD1 ~ normal(meanD1, sdD1);        
  muD2 ~ normal(meanD2, sdD2);          
  muD3 ~ normal(meanD3, sdD3);
  //muE ~ normal(meanE, sdE);      


  Lsigma ~ exponential(1/muLsigma);       // where parameter of exponential = lambda = 1/mean.


  for (n in 1:N) {

    // JPA-1 trajectory, undefined at age=0 even though R takes the limit of exp(-1/0)
    mu[n] = 0.012 + muA*exp( -1/( muC1*log( age[n]/muD1 + 1 ) + (age[n]/muD2)^muC2 + (age[n]/muD3)^muC3 ) );             // muA*( 1 - 1/( 1 + ((age[n] + muE)/muD1)^muC1 + ((age[n] + muE)/muD2)^muC2 + ((age[n] + muE)/muD3)^muC3 ) );   // JPA-2 model from Jolicoeur et al. 1992 table 2

  } // for

  log_height ~ normal( log(mu), Lsigma );
}


generated quantities{
  // generated quantities is just so that we can have mu to look at directly instead of having to re-calculate it from the parameter posteriors when we want to plot.
  
  vector[N] mu;                           // mean of un-logged normal distribution

  for (n in 1:N) {

    // mean of un-logged normal likelihood

    mu[n] = 0.012 + muA*exp( -1/( muC1*log( age[n]/muD1 + 1 ) + (age[n]/muD2)^muC2 + (age[n]/muD3)^muC3 ) );      // muA*( 1 - 1/( 1 + ((age[n] + muE)/muD1)^muC1 + ((age[n] + muE)/muD2)^muC2 + ((age[n] + muE)/muD3)^muC3 ) );

  } // for

}


