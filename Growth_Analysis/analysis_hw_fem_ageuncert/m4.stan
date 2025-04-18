

functions {
  real comp_mod_h( vector muQ, vector muK, vector muH, vector muI, real t ) {

    real mu; // height at time t

    if ( t <= muI[1] ) {
      mu = 0.012 + ( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1]);
    } else if (t < muI[2]) {
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
                   ( 2*muH[5]/muK[5] * ( 1 - exp(muK[5]*muQ[5]*( muI[4] - t )/( 1 + 2*muQ[5] )) ) )^(1/muQ[5]);
    }; //else


    return mu;

  } //comp_mod_h



  real comp_mod_w( vector muQ, vector muK, vector muH, vector muI, real t) {

    real mu;      // weight at time t

    if ( t <= muI[1] ) {
      mu = 1.02e-6 + pi()*( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1] + 2);
    } else if (t <= muI[2]) {
      mu = 1.02e-6 + pi()*( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1] + 2) +
                    pi()*( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2] + 2);
    } else if (t <= muI[3]) {
      mu = 1.02e-6 + pi()*( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1] + 2) +
                    pi()*( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2] + 2) +
                    pi()*( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3] + 2);
    } else if (t <= muI[4]) {
      mu = 1.02e-6 + pi()*( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1] + 2) +
                    pi()*( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2] + 2) +
                    pi()*( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3] + 2) +
                    pi()*( 2*muH[4]/muK[4] * ( 1 - exp(muK[4]*muQ[4]*( muI[3] - t )/( 1 + 2*muQ[4] )) ) )^(1/muQ[4] + 2);    
    } else {
      mu = 1.02e-6 + pi()*( 2*muH[1]/muK[1] * ( 1 - exp(muK[1]*muQ[1]*( 0      - t )/( 1 + 2*muQ[1] )) ) )^(1/muQ[1] + 2) +
                    pi()*( 2*muH[2]/muK[2] * ( 1 - exp(muK[2]*muQ[2]*( muI[1] - t )/( 1 + 2*muQ[2] )) ) )^(1/muQ[2] + 2) +
                    pi()*( 2*muH[3]/muK[3] * ( 1 - exp(muK[3]*muQ[3]*( muI[2] - t )/( 1 + 2*muQ[3] )) ) )^(1/muQ[3] + 2) +
                    pi()*( 2*muH[4]/muK[4] * ( 1 - exp(muK[4]*muQ[4]*( muI[3] - t )/( 1 + 2*muQ[4] )) ) )^(1/muQ[4] + 2) +
                    pi()*( 2*muH[5]/muK[5] * ( 1 - exp(muK[5]*muQ[5]*( muI[4] - t )/( 1 + 2*muQ[5] )) ) )^(1/muQ[5] + 2);
    }; //else


    return mu;

  } //comp_mod_w

}



data {
  int<lower=1> N;                 // number of observations
  vector<lower=0>[N] age;         // vector of ages for each observation (in years since conception)
  vector<lower=0>[N] height;      // vector of observations (in cm)
  vector<lower=0>[N] weight;      // vector of observations (in g)

  vector<upper=1>[5] meanLQ;      // means from PriorPredict5
  vector<lower=0>[5] meanLK;
  vector<lower=0>[5] meanR;
  ordered[5] meanLI;      // first meanI == 0

  vector<lower=0>[5] sdLQ;        // stdevs from PriorPredict5
  vector<lower=0>[5] sdLK;
  vector<lower=0>[5] sdR;
  vector<lower=0>[5] sdLI;

  real<lower=0> muLsigma_h;         // mean measurement error stdev on log scale
  real<lower=0> muLsigma_w;

}

transformed data {
  vector[N] log_height;
  vector[N] log_weight;

  log_height = log(height);
  log_weight = log(weight);
}


parameters {
  vector<upper=0>[5] muLQ;                 // population mean parameters to estimate
  vector<lower=0,upper=10>[5] muLK;
  vector<lower=0,upper=300>[5] muR;
  ordered[5] muLI;                         // first muI == 0
  
  real<lower=0,upper=0.005> Lsigma_h;        // 0.01     variance of the logged normal distribution, upper limit of 0.005 yields 95% of observations within 2*stdev = 1cm of actual height at 100cm, within 0.5kg of actual weight at 50kg
  real<lower=0,upper=0.5> Lsigma_w;           //0.15 0.11, 0.13, 0.15, 0.2, 0.1, 0.8, nothing,

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
  vector[N] mu_h;                      // mean of un-logged normal distribution
  vector[N] mu_w;                      // mean of un-logged normal distribution

  muLQ ~ multi_normal( meanLQ, diag_matrix(sdLQ) );
  muLK ~ multi_normal( meanLK, diag_matrix(sdLK) );
  muR ~ multi_normal( meanR, diag_matrix(sdR) ); 
  muLI ~ multi_normal( meanLI, diag_matrix(sdLI) );

  Lsigma_h ~ exponential(1/muLsigma_h);       // where parameter of exponential = lambda = 1/mean.
  Lsigma_w ~ exponential(1/muLsigma_w);


  for (n in 1:N) {

    mu_h[n] = comp_mod_h(muQ, muK, muH, muI, age[n]);
    mu_w[n] = comp_mod_w(muQ, muK, muH, muI, age[n]);         // + weight_offset[age_bin[n]];

  } // for

log_height ~ normal( log(mu_h), Lsigma_h );
log_weight ~ normal( log(mu_w), Lsigma_w );


}


generated quantities{
  // generated quantities is just so that we can have mu to look at directly instead of having to re-calculate it from the parameter posteriors when we want to plot.

  vector[N] mu_h;                           // mean of un-logged normal distribution
  vector[N] mu_w;


  for (n in 1:N) {

    mu_h[n] = comp_mod_h(muQ, muK, muH, muI, age[n]);
    mu_w[n] = comp_mod_w(muQ, muK, muH, muI, age[n]);        

  } // for

}


