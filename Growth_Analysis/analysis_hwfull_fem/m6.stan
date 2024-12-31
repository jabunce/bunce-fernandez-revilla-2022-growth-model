

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


    //if (mu < 0) { mu = 0.012; };

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


    //if (mu < 0) { mu = 3.6e-9; };

    return mu;

  } //comp_mod_w

}



data {
  int<lower=0> N;                           // number of observations
  int<lower=0> J;                           // number of people measured
  int<lower=0> E;                           // number of ethnic groups
  array[N] int<lower=0> ID;                 // vector of person IDs for each measurement
  vector<lower=0>[N] age;                   // vector of ages for each observation (in years since conception)
  vector<lower=0>[N] height;                // vector of observations (in cm)
  vector<lower=0>[N] weight;                // vector of observations (in g)
  array[N] int<lower=0,upper=E> ethnicity;  // vector of ethnicities for each observation: 1=Ber, 2=Mat
  array[J] int<lower=1,upper=E> ethIndiv;   // vector of ethnicities for each individual j: 1=Ber, 2=Mat

  vector[20] meansL;                  // means from PriorPredictIndivStdev

  vector<lower=0>[20] gexpL;          // rate (=1/mean) for exponential hyperprior on group-level offset stdev (from PriorPredictIndivStdev)
  vector<lower=0>[20] iexpL;          // mean and stdev for hyperprior on indiv-level offset stdev
  vector<lower=0>[20] ihsdL;

  real<lower=0> CholEtaG;             // eta parameter for lkj_corr_cholesky (from PriorPredictIndivStdev)
  real<lower=0> CholEtaI;

  real<lower=0> muLsigma_h;           // mean measurement error stdev on log scale
  real<lower=0> muLsigma_w;
}

transformed data {
  vector[20] zeros = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]';         // column vector of 0s, note transpose at the end to turn into row vectors
  vector[20] ones =  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]';         // column vector of 1s
  vector[N] log_height;                            
  vector[N] log_weight;

  log_height = log(height);
  log_weight = log(weight);
}

parameters {
  array[E] vector[20] zGrp;                         // ethnic group offset z-scores to each parameter: E-array of column 15-vectors, one vector for each ethnicity
  vector<lower=0>[20] offsetGroupStdevs;            // stdevs for group-mean offsets to the overall mean (across groups) trajectory for the group, for each of the 20 parameters
  cholesky_factor_corr[20] L_G;                     // cholesky factor 20x20 matrix, for correlation matrix for mean offsets

  array[J] vector[20] zInd;                         // individual offset z-scores to each parameter: J-array of column 20-vectors, one vector for each individual
  array[E] vector<lower=0>[20] offsetIndivStdevs;   // stdevs for mean offsets to the mean group trajectory for each individual, for each of the 20 parameters: E-array of 20-vectors, one vector for each ethnic group
  array[E] cholesky_factor_corr[20] L_I;            // E-array of cholesky factor 20x20 matrices, for correlation matrix among mean (across individuals within ethnic groups) parameter offsets

  real<lower=0,upper=0.005> Lsigma_h;        // variance of the logged normal distribution, upper limit of 0.005 yields 95% of observations within 2*stdev = 1cm of actual height at 100cm, within 0.5kg of actual weight at 50kg
  real<lower=0,upper=0.01> Lsigma_w;       //0.008 0.005 0.01 0.03 0.01 0.05 0.005
}

transformed parameters {
  
  matrix[E,20] GrpOffset;            // matrix of random offsets for each ethnic group to each of the 20 paramters, rows = groups, cols = parameters
  matrix[J,20] IndOffset;            // matrix of random offsets for each individual to each of the 20 paramters, rows = indivs, cols = parameters
                                     // Stan manual pg 150-151, Rethinking pg 409

  for (e in 1:E) {
    GrpOffset[e] = (diag_pre_multiply(offsetGroupStdevs, L_G) * zGrp[e])';                           //create cholesky factor for cov matrix and multiply by vector of offset z-scores, then transpose. This makes an 20-vector for each ethnic group e
  } // for e


  for (j in 1:J) {
    IndOffset[j] = (diag_pre_multiply(offsetIndivStdevs[ethIndiv[j]], L_I[ethIndiv[j]]) * zInd[j])';  //create cholesky factor for group-specific cov matrix and multiply by vector of offset z-scores, then transpose. This makes a 20-vector for each individual j
  } // for j

}


model {
  vector[N] mu_h;                      // mean of un-logged normal distribution
  vector[N] mu_w;

  array[J] vector[5] Q;                               // temporary holders for linear sub-model for each parameter
  array[J] vector[5] K;
  array[J] vector[5] H;
  array[J] vector[5] I;

  Lsigma_h ~ exponential(1/muLsigma_h);       // where parameter of exponential = lambda = 1/mean.
  Lsigma_w ~ exponential(1/muLsigma_w);

  zGrp ~ multi_normal(zeros, diag_matrix(ones));      // E-array of group offset z-scores, sample 20 at a time from a normal
  zInd ~ multi_normal(zeros, diag_matrix(ones));      // J-array of individual offset z-scores

  offsetGroupStdevs ~ normal(gexpL,0.0001); //0.0005 0.001 0.00001 exponential(1/gexpL);           // hyper-priors for stdevs of group offsets to parameters, on log scale, mean of exponential(beta) = 1/beta
  L_G ~ lkj_corr_cholesky(CholEtaG);                  // cholesky factor 20x20 matrix, lower the eta to allow more extreme correlations, Rethinking pg 394

  for (eth in 1:E) {
    offsetIndivStdevs[eth] ~ normal(iexpL,ihsdL);           // E-array of hyper-priors for stdevs of individual offsets to parameters
    L_I[eth] ~ lkj_corr_cholesky(CholEtaI);                 // E-array of cholesky factor 20x20 matrices
  } // for e


  for (n in 1:N) {

    for (z in 1:5) {
      Q[ID[n],z] = exp( meansL[z+0] + GrpOffset[ethnicity[n],z+0] + IndOffset[ID[n],z+0] );
      K[ID[n],z] = exp( meansL[z+5] + GrpOffset[ethnicity[n],z+5] + IndOffset[ID[n],z+5] );
      H[ID[n],z] = K[ID[n],z]/2 + meansL[z+10] + GrpOffset[ethnicity[n],z+10] + IndOffset[ID[n],z+10];
      I[ID[n],z] = exp( meansL[z+15] + GrpOffset[ethnicity[n],z+15] + IndOffset[ID[n],z+15] );
    } // for z

    mu_h[n] = comp_mod_h( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], age[n] );
    mu_w[n] = comp_mod_w( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], age[n] );

  } // for n


  log_height ~ normal( log(mu_h), Lsigma_h );
  log_weight ~ normal( log(mu_w), Lsigma_w ); 

}


generated quantities{

  vector[N] mu_h;                      // mean of un-logged normal distribution
  vector[N] mu_w;

  vector[N] log_lik;                 // for WAIC

  matrix[20,20] G_cor_mat;           // 20x20 correlation matrix for parameters across individuals
  matrix[20,20] G_cov_mat;           // 20x20 variance-covariance matrix for parameters across individuals
  array[E] matrix[20,20] I_cor_mat;  
  array[E] matrix[20,20] I_cov_mat; 

  array[J] vector[5] Q;             // temporary holders for linear sub-model for each parameter
  array[J] vector[5] K;
  array[J] vector[5] H;
  array[J] vector[5] I;

  array[E] vector[5] mQ;            // just group offsets: E=1 US, E=2 Matsigenka
  array[E] vector[5] mK;
  array[E] vector[5] mH;
  array[E] vector[5] mI;


  for (n in 1:N) {

    for (z in 1:5) {
      mQ[ethnicity[n],z] = exp( meansL[z+0] + GrpOffset[ethnicity[n],z+0] );
      mK[ethnicity[n],z] = exp( meansL[z+5] + GrpOffset[ethnicity[n],z+5] );
      mH[ethnicity[n],z] = mK[ethnicity[n],z]/2 + meansL[z+10] + GrpOffset[ethnicity[n],z+10];
      mI[ethnicity[n],z] = exp( meansL[z+15] + GrpOffset[ethnicity[n],z+15] );

      Q[ID[n],z] = exp( meansL[z+0] + GrpOffset[ethnicity[n],z+0] + IndOffset[ID[n],z+0] );
      K[ID[n],z] = exp( meansL[z+5] + GrpOffset[ethnicity[n],z+5] + IndOffset[ID[n],z+5] );
      H[ID[n],z] = K[ID[n],z]/2 + meansL[z+10] + GrpOffset[ethnicity[n],z+10] + IndOffset[ID[n],z+10];
      I[ID[n],z] = exp( meansL[z+15] + GrpOffset[ethnicity[n],z+15] + IndOffset[ID[n],z+15] );
    } // for z

    mu_h[n] = comp_mod_h( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], age[n] );
    mu_w[n] = comp_mod_w( Q[ID[n]], K[ID[n]], H[ID[n]], I[ID[n]], age[n] );

    // log_lik[n] = normal_lpdf( log_height[n] | log(mu[n]), Lsigma );      // for WAIC function

  } // for n


  G_cor_mat = L_G * L_G';                                    //reconstruct the correlation matrix to look at in output

  G_cov_mat = diag_pre_multiply(offsetGroupStdevs, L_G) * 
              diag_pre_multiply(offsetGroupStdevs, L_G)';    //construct cov matrix from cholesky factors of cov matrix to look at in output


  for (eth in 1:E) {

    I_cor_mat[eth] = L_I[eth] * L_I[eth]'; 

    I_cov_mat[eth] = diag_pre_multiply(offsetIndivStdevs[eth], L_I[eth]) * 
                     diag_pre_multiply(offsetIndivStdevs[eth], L_I[eth])';
  
  } // for eth
  
  
}

