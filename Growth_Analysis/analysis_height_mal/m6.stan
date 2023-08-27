
data {
  int<lower=1> N;                       // number of height observations
  int<lower=0> E;                       // number of ethnic groups
  vector<lower=0>[N] age;                 // vector of ages for each observation (in years)
  vector<lower=0>[N] height;              // vector of height observations (in cm)
  array[N] int<lower=0,upper=E> ethnicity;    // vector of ethnicities for each observation: 1=ber, 2=mat

  // mean posteriors from Table 4 of Jolicoeur et al. 1992
  real<lower=0> muA;                // mean adult height (in cm)
  real<lower=0> muD1;               // time scale factors
  real<lower=0> muD2; 
  real<lower=0> muD3;     
  real<lower=0> muC1;               // exponents
  real<lower=0> muC2;
  real<lower=0> muC3;

  // sd's from PriorPredictJPA
  real<lower=0> psdA;
  real<lower=0> psdD1;
  real<lower=0> psdD2; 
  real<lower=0> psdD3;     
  real<lower=0> psdC1;
  real<lower=0> psdC2;
  real<lower=0> psdC3;
}


transformed data {
  vector[N] log_height;

  log_height = log(height);
}


parameters {
  vector[E] ethA;        // mean adult height (in cm), ethnicity offsets to parameters
  vector[E] ethD1;
  vector[E] ethD2; 
  vector[E] ethD3;     
  vector[E] ethC1;
  vector[E] ethC2;
  vector[E] ethC3;

  real<lower=0> sigma;          // log variance of the normal distribution (exponentiate to get variance on non-logged scale)
}


model {
  vector[N] mu;                   // mean of un-logged normal distribution

  vector[N] A;                         // temporary holders for linear sub-model for each parameter
  vector[N] D1;
  vector[N] D2;
  vector[N] D3;
  vector[N] C1;
  vector[N] C2;
  vector[N] C3;


  sigma ~ exponential(100);    // where parameter of exponential = lambda, larger means mean is smaller. This is essentially the measuring instrument error we assume.

  //ethA ~ normal(0, 0.00001);    //0.0001          // priors for ethnicity offsets, tighten these priors to keep mu positive to avoid error during sampling: log(mu)=NaN 
  //ethD1 ~ normal(0, 0.00001);        
  //ethD2 ~ normal(0, 0.00001);          
  //ethD3 ~ normal(0, 0.00001);      
  //ethC1 ~ normal(0, 0.00001);
  //ethC2 ~ normal(0, 0.00001); 
  //ethC3 ~ normal(0, 0.00001);

  ethA ~ normal(0, psdA);              // priors for ethnicity offsets, tighten these priors to keep mu positive to avoid error during sampling: log(mu)=NaN 
  ethD1 ~ normal(0, psdD1);        
  ethD2 ~ normal(0, psdD2);          
  ethD3 ~ normal(0, psdD3);      
  ethC1 ~ normal(0, psdC1);
  ethC2 ~ normal(0, psdC2); 
  ethC3 ~ normal(0, psdC3);

  for (n in 1:N) {

    A[n] = muA + ethA[ethnicity[n]];       // linear submodels for each parameter
    D1[n] = muD1 + ethD1[ethnicity[n]];
    D2[n] = muD2 + ethD2[ethnicity[n]];
    D3[n] = muD3 + ethD3[ethnicity[n]];
    C1[n] = muC1 + ethC1[ethnicity[n]];
    C2[n] = muC2 + ethC2[ethnicity[n]];
    C3[n] = muC3 + ethC3[ethnicity[n]];


    // mu = mean of (un-logged) normal likelihood
    mu[n] = A[n]*exp( -1/( C1[n]*log( age[n]/D1[n] + 1 ) + (age[n]/D2[n])^C2[n] + (age[n]/D3[n])^C3[n] ) ); // JPA-1, Table 2 of Jolicoeur et al. 1992
    //mu[n] = A[n]*( 1 - 1/( 1 + (age[n]/D1[n])^C1[n] + (age[n]/D2[n])^C2[n] + (age[n]/D3[n])^C3[n] ) );   // equation 3 of Jolicoeur et al. 1988


  } // for

  //print("mu[1] =", mu[1]);
  //print("ethA[ethnicity[n]] =", ethA[ethnicity[1]]);
  //print("age[1] =", age[1]);
  //print("muA =", muA);
  //print("muD1 =", muD1);
  //print("muD2 =", muD2);
  //print("muD3 =", muD3);
  //print("muC1 =", muC1);
  //print("muC2 =", muC2);
  //print("muC3 =", muC3);


  log_height ~ normal( log(mu), sigma );

}


generated quantities{

  vector[N] mu;                           // mean of un-logged normal distribution

  vector[N] A;                         // temporary holders for linear sub-model for each parameter
  vector[N] D1;
  vector[N] D2;
  vector[N] D3;
  vector[N] C1;
  vector[N] C2;
  vector[N] C3;

  real berA;                         // parameters for mean Berkeley
  real berD1;
  real berD2;
  real berD3;
  real berC1;
  real berC2;
  real berC3;

  real matA;                         // parameters for mean Matsigenka
  real matD1;
  real matD2;
  real matD3;
  real matC1;
  real matC2;
  real matC3;

  berA = muA + ethA[1];
  berD1 = muD1 + ethD1[1];
  berD2 = muD2 + ethD2[1];
  berD3 = muD3 + ethD3[1];
  berC1 = muC1 + ethC1[1];
  berC2 = muC2 + ethC2[1];
  berC3 = muC3 + ethC3[1];

  matA = muA + ethA[2];
  matD1 = muD1 + ethD1[2];
  matD2 = muD2 + ethD2[2];
  matD3 = muD3 + ethD3[2];
  matC1 = muC1 + ethC1[2];
  matC2 = muC2 + ethC2[2];
  matC3 = muC3 + ethC3[2];


  for (n in 1:N) {

    A[n] = muA + ethA[ethnicity[n]];       // linear submodels for each parameter
    D1[n] = muD1 + ethD1[ethnicity[n]];
    D2[n] = muD2 + ethD2[ethnicity[n]];
    D3[n] = muD3 + ethD3[ethnicity[n]];
    C1[n] = muC1 + ethC1[ethnicity[n]];
    C2[n] = muC2 + ethC2[ethnicity[n]];
    C3[n] = muC3 + ethC3[ethnicity[n]];


    // mu = mean of un-logged normal likelihood
    mu[n] = A[n]*exp( -1/( C1[n]*log( age[n]/D1[n] + 1 ) + (age[n]/D2[n])^C2[n] + (age[n]/D3[n])^C3[n] ) ); // JPA-1, Table 2 of Jolicoeur et al. 1992
    //mu[n] = A[n]*( 1 - 1/( 1 + (age[n]/D1[n])^C1[n] + (age[n]/D2[n])^C2[n] + (age[n]/D3[n])^C3[n] ) );   // equation 3 of Jolicoeur et al. 1988

  } // for

}


