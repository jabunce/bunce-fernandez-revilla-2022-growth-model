
data {
  int<lower=1> N;                  // number of observations
  vector<lower=0>[N] age;          // vector of ages for each observation (in years since conception)
  vector<lower=0>[N] height;       // vector of observations (in cm)

  real<upper=0> meanLQ1;           // means from PriorPredict5
  real<upper=0> meanLQ2;
  real<upper=0> meanLQ3;
  real<upper=0> meanLQ4;
  real<upper=0> meanLQ5;
  real<lower=0> meanLK1;
  real<lower=0> meanLK2;
  real<lower=0> meanLK3;
  real<lower=0> meanLK4;
  real<lower=0> meanLK5;
  real<lower=0> meanR1;
  real<lower=0> meanR2;
  real<lower=0> meanR3;
  real<lower=0> meanR4;
  real<lower=0> meanR5;

  real<lower=0> sdLQ1;             // stdevs from PriorPredict5
  real<lower=0> sdLQ2;
  real<lower=0> sdLQ3;
  real<lower=0> sdLQ4;
  real<lower=0> sdLQ5;
  real<lower=0> sdLK1;
  real<lower=0> sdLK2;
  real<lower=0> sdLK3;
  real<lower=0> sdLK4;
  real<lower=0> sdLK5;
  real<lower=0> sdR1;
  real<lower=0> sdR2;
  real<lower=0> sdR3;
  real<lower=0> sdR4;
  real<lower=0> sdR5;
}

transformed data {
  vector[N] log_height;

  log_height = log(height);
}


parameters {
  real<upper=0> muLQ1;            // population mean parameters to estimate
  real<upper=0> muLQ2;
  real<upper=0> muLQ3;
  real<upper=0> muLQ4;
  real<upper=0> muLQ5;
  real<lower=0> muLK1;
  real<lower=0> muLK2;
  real<lower=0> muLK3;
  real<lower=0> muLK4;
  real<lower=0> muLK5;
  real<lower=0> muR1; 
  real<lower=0> muR2; 
  real<lower=0> muR3;
  real<lower=0> muR4; 
  real<lower=0> muR5;

  real<lower=0> sigma;            // variance of the logged normal distribution
}

transformed parameters {
  real<lower=0> muQ1;
  real<lower=0> muQ2;
  real<lower=0> muQ3;
  real<lower=0> muQ4;
  real<lower=0> muQ5;
  real<lower=0> muK1;
  real<lower=0> muK2;
  real<lower=0> muK3;
  real<lower=0> muK4;
  real<lower=0> muK5;
  real<lower=0> muH1; 
  real<lower=0> muH2; 
  real<lower=0> muH3;
  real<lower=0> muH4; 
  real<lower=0> muH5;
  
  muQ1 = exp(muLQ1);
  muQ2 = exp(muLQ2);
  muQ3 = exp(muLQ3);
  muQ4 = exp(muLQ4);
  muQ5 = exp(muLQ5);
  muK1 = exp(muLK1);
  muK2 = exp(muLK2);
  muK3 = exp(muLK3);
  muK4 = exp(muLK4);
  muK5 = exp(muLK5);
  muH1 = muK1/2 + muR1;
  muH2 = muK2/2 + muR2;
  muH3 = muK3/2 + muR3;
  muH4 = muK4/2 + muR4;
  muH5 = muK5/2 + muR5;
}


model {
  vector[N] mu;                      // mean of un-logged normal distribution

  muLQ1 ~ normal(meanLQ1, sdLQ1);        
  muLQ2 ~ normal(meanLQ2, sdLQ2);       
  muLQ3 ~ normal(meanLQ3, sdLQ3);
  muLQ4 ~ normal(meanLQ4, sdLQ4);       
  muLQ5 ~ normal(meanLQ5, sdLQ5);
  muLK1 ~ normal(meanLK1, sdLK1);
  muLK2 ~ normal(meanLK2, sdLK2);
  muLK3 ~ normal(meanLK3, sdLK3);
  muLK4 ~ normal(meanLK4, sdLK4);
  muLK5 ~ normal(meanLK5, sdLK5);          
  muR1 ~ normal(meanR1, sdR1);      
  muR2 ~ normal(meanR2, sdR2);
  muR3 ~ normal(meanR3, sdR3);
  muR4 ~ normal(meanR4, sdR4);
  muR5 ~ normal(meanR5, sdR5); 

  sigma ~ exponential(100);       // where parameter of exponential = lambda, larger means mean is smaller. This is essentially the measuring instrument error we assume.


  for (n in 1:N) {

    // mean of (un-logged) normal likelihood

    mu[n] = ( -1*exp(-1*muK1*muQ1*age[n]/( 1 + 2*muQ1 )) + 2*muH1/muK1 )^(1/muQ1) +
            ( -1*exp(-1*muK2*muQ2*age[n]/( 1 + 2*muQ2 )) + 2*muH2/muK2 )^(1/muQ2) +
            ( -1*exp(-1*muK3*muQ3*age[n]/( 1 + 2*muQ3 )) + 2*muH3/muK3 )^(1/muQ3) +
            ( -1*exp(-1*muK4*muQ4*age[n]/( 1 + 2*muQ4 )) + 2*muH4/muK4 )^(1/muQ4) +
            ( -1*exp(-1*muK5*muQ5*age[n]/( 1 + 2*muQ5 )) + 2*muH5/muK5 )^(1/muQ5);
  } // for

 
log_height ~ normal( log(mu), sigma );

}


generated quantities{
  // generated quantities is just so that we can have mu to look at directly instead of having to re-calculate it from the parameter posteriors when we want to plot.

  vector[N] mu;                           // mean of un-logged normal distribution

  for (n in 1:N) {

    mu[n] = ( -1*exp(-1*muK1*muQ1*age[n]/( 1 + 2*muQ1 )) + 2*muH1/muK1 )^(1/muQ1) +
            ( -1*exp(-1*muK2*muQ2*age[n]/( 1 + 2*muQ2 )) + 2*muH2/muK2 )^(1/muQ2) +
            ( -1*exp(-1*muK3*muQ3*age[n]/( 1 + 2*muQ3 )) + 2*muH3/muK3 )^(1/muQ3) +
            ( -1*exp(-1*muK4*muQ4*age[n]/( 1 + 2*muQ4 )) + 2*muH4/muK4 )^(1/muQ4) +
            ( -1*exp(-1*muK5*muQ5*age[n]/( 1 + 2*muQ5 )) + 2*muH5/muK5 )^(1/muQ5); 

  } // for

}


