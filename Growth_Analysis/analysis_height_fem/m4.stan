

functions {
  real comp_mod( vector muQ, vector muK, vector muH, vector muI, real t ) {

    real mu; // height at time t

    if        (t <= muI[1]) {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]);
    } else if (t <= muI[2]) {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]) +
                   ( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2]);
    } else if (t <= muI[3]) {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]) +
                   ( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2]) +
                   ( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3]);
    } else if (t <= muI[4]) {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]) +
                   ( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2]) +
                   ( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3]) +
                   ( 2*muH[4]/muK[4] * ( 1 - exp(muK[4]*muQ[4]*( muI[3] - t )/( 1 + 2*muQ[4] )) ) )^(1/muQ[4]);
    } else {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]) +
                   ( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2]) +
                   ( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3]) +
                   ( 2*muH[4]/muK[4] * ( 1 - exp(muK[4]*muQ[4]*( muI[3] - t )/( 1 + 2*muQ[4] )) ) )^(1/muQ[4]) +
                   ( 2*muH[4]/muK[5] * ( 1 - exp(muK[5]*muQ[5]*( muI[4] - t )/( 1 + 2*muQ[5] )) ) )^(1/muQ[5]);
    } //else

    return mu;

  } //comp_mod
}



data {
  int<lower=1> N;                 // number of observations
  vector<lower=0>[N] age;         // vector of ages for each observation (in years since conception)
  vector<lower=0>[N] height;      // vector of observations (in cm)

  vector<upper=0>[5] meanLQ;      // means from PriorPredict5
  vector<lower=0>[5] meanLK;
  vector<lower=0>[5] meanR;
  ordered[5] meanLI;              // first muI == 0 for infant, muI[1] is child 1, muI[5] is post-adolescent just to put upper bound on muI[4] adolescent

  vector<lower=0>[5] msdLQ;        // stdevs from PriorPredict5
  vector<lower=0>[5] msdLK;
  vector<lower=0>[5] msdR;
  vector<lower=0>[5] msdLI;

  real<lower=0> muLsigma;         // mean measurement error stdev on log scale

}

transformed data {
  vector[N] log_height;

  log_height = log(height);
}


parameters {
  vector<upper=0>[5] muLQ;                // population mean parameters to estimate
  vector<lower=0,upper=10>[5] muLK;        // upper limit = e^5 = 148
  vector<lower=0,upper=100>[5] muR;
  ordered[5] muLI;                        // first muI == 0 for infant, muI[1] is child 1, muI[5] is post-adolescent just to put upper bound on muI[4] adolescent

  real<lower=0> Lsigma;        // variance of the logged normal distribution, upper limit of 0.005 yields 95% of observations within 2*stdev = 1cm of actual height at 100cm 
}

transformed parameters {

  vector<lower=0,upper=1>[5] muQ;        // population median parameters to estimate
  vector<lower=0>[5] muK;
  vector<lower=0>[5] muH;
  positive_ordered[5] muI;               // first muI == 0, muI[5] is upper limit on age at initialization of last process
  
  muQ = exp(muLQ);
  muK = exp(muLK);
  muH = muK/2 + muR;
  muI = exp(muLI);
}


model {
  vector[N] mu;                      // mean of un-logged normal distribution

  muLQ ~ multi_normal( meanLQ, diag_matrix(msdLQ) );
  muLK ~ multi_normal( meanLK, diag_matrix(msdLK) );
  muR ~ multi_normal( meanR, diag_matrix(msdR) ); 
  muLI ~ multi_normal( meanLI, diag_matrix(msdLI) );

  Lsigma ~ exponential(1/muLsigma);       // where parameter of exponential = lambda = 1/mean.


  for (n in 1:N) {

    mu[n] = comp_mod(muQ, muK, muH, muI, age[n]);

  } // for


  log_height ~ normal( log(mu), Lsigma );              // e^log(mu) = mu = median of height distribution at a given age, Lsigma = stdev of log of height at a given age

}


generated quantities{
  // generated quantities is just so that we can have mu to look at directly instead of having to re-calculate it from the parameter posteriors when we want to plot.

  vector[N] mu;                           // mean of un-logged normal distribution

  for (n in 1:N) {

    mu[n] = comp_mod(muQ, muK, muH, muI, age[n]);

  } // for

}


