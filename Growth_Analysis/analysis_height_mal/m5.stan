
data {
  int<lower=1> N;                      // number of height observations
  vector<lower=0>[N] age;              // vector of ages for each observation (in years), can be ~0 (conception)
  vector<lower=0>[N] height;           // vector of height observations (in cm)

  real<lower=0> meanA;                 // means and stdevs from PriorPredictJPA
  real<lower=0> meanD1;
  real<lower=0> meanD2;
  real<lower=0> meanD3;
  real<lower=0> meanC1;
  real<lower=0> meanC2;
  real<lower=0> meanC3;

  real<lower=0> sdA;
  real<lower=0> sdD1;
  real<lower=0> sdD2;
  real<lower=0> sdD3;
  real<lower=0> sdC1;
  real<lower=0> sdC2;
  real<lower=0> sdC3;
}

transformed data {
  vector[N] log_height;

  log_height = log(height);
}


parameters {
  real<lower=0,upper=400> muA;    // mean adult height (in cm)
  real<lower=0> muD1;             // time scale factors
  real<lower=0> muD2; 
  real<lower=0> muD3;     
  real<lower=0> muC1;             // exponents
  real<lower=0> muC2;
  real<lower=0> muC3;

  real<lower=0> sigma;            // variance of the logged normal distribution
}


model {
  vector[N] mu;                   // mean of un-logged normal distribution

  muA ~ normal(meanA, sdA);
  muD1 ~ normal(meanD1, sdD1);        
  muD2 ~ normal(meanD2, sdD2);          
  muD3 ~ normal(meanD3, sdD3);      
  muC1 ~ normal(meanC1, sdC1);
  muC2 ~ normal(meanC2, sdC2); 
  muC3 ~ normal(meanC3, sdC3);

  sigma ~ exponential(100);    // where parameter of exponential = lambda, larger means mean is smaller. This is essentially the measuring instrument error we assume.


  for (n in 1:N) {

    // mu = mean of (un-logged) normal likelihood

    mu[n] = muA*exp( -1/( muC1*log( age[n]/muD1 + 1 ) + (age[n]/muD2)^muC2 + (age[n]/muD3)^muC3 ) ); // JPA-1, Table 2 of Jolicoeur et al. 1992

  } // for

  log_height ~ normal( log(mu), sigma );
}


generated quantities{
  // generated quantities is just so that we can have mu to look at directly instead of having to re-calculate it from the parameter posteriors when we want to plot.
  
  vector[N] mu;                           // mean of un-logged normal distribution

  for (n in 1:N) {

    // mean of un-logged normal likelihood

    mu[n] = muA*exp( -1/( muC1*log( age[n]/muD1 + 1 ) + (age[n]/muD2)^muC2 + (age[n]/muD3)^muC3 ) );

  } // for

}


