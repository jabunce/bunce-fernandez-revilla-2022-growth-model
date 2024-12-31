
data {
  int<lower=0> N;                           // number of observations
  int<lower=0> J;                           // number of people measured
  int<lower=0> E;                           // number of ethnic groups
  array[N] int<lower=0> ID;                 // vector of person IDs for each measurement
  vector<lower=0>[N] age;                   // vector of ages for each observation (in years since conception)
  vector<lower=0>[N] height;                // vector of observations (in cm)
  array[N] int<lower=0,upper=E> ethnicity;  // vector of ethnicities for each observation: 1=Ber, 2=Mat
  array[J] int<lower=1,upper=E> ethIndiv;   // vector of ethnicities for each individual j: 1=Ber, 2=Mat

  // means from PriorPredictGroupStdevJPA
  real<lower=0> Ahat;
  real<lower=0> C1hat;   
  real<lower=0> C2hat;
  real<lower=0> C3hat;
  real<lower=0> D1hat;   
  real<lower=0> D2hat;
  real<lower=0> D3hat;
  

  // 1/rate (=mean) for exponential prior on group-level offset stdev (from PriorPredictGroupStdevJPA)
  real<lower=0> pgsA;
  real<lower=0> pgsC1;      
  real<lower=0> pgsC2;
  real<lower=0> pgsC3;
  real<lower=0> pgsD1;      
  real<lower=0> pgsD2;
  real<lower=0> pgsD3;


  // mean for normal prior on indiv-level offset stdev (from PriorPredictGroupStdevJPA)
  real<lower=0> pimA;
  real<lower=0> pimC1;      
  real<lower=0> pimC2;
  real<lower=0> pimC3;
  real<lower=0> pimD1;      
  real<lower=0> pimD2;
  real<lower=0> pimD3;


  // stdev for normal prior on indiv-level offset stdev (from PriorPredictGroupStdevJPA)
  real<lower=0> pisA;
  real<lower=0> pisC1;      
  real<lower=0> pisC2;
  real<lower=0> pisC3;
  real<lower=0> pisD1;      
  real<lower=0> pisD2;
  real<lower=0> pisD3;


  // eta parameter for lkj_corr_cholesky (from PriorPredictGroupStdevJPA)
  real<lower=0> pCholEtaG;
  real<lower=0> pCholEtaI;

  real<lower=0> muLsigma;         // mean measurement error stdev on log scale
}

transformed data {
  vector[7] zeros = [0,0,0,0,0,0,0]';       // column vector of 0s, note transpose at the end to turn into row vectors
  vector[7] ones = [1,1,1,1,1,1,1]';        // column vector of 1s
  vector[N] log_height;                            

  log_height = log(height);
}

parameters {
  array[E] vector[7] zGrp;                        // ethnic group offset z-scores to each parameter: E-array of column 7-vectors, one vector for each ethnicity
  vector<lower=0>[7] offsetGroupStdevs;           // stdevs for mean (across indivs) offsets to the overall mean (across groups) trajectory for each group, for each of the 7 parameters
  cholesky_factor_corr[7] L_G;                    // cholesky factor 7x7 matrices, for correlation matrix among mean (across ethnicities) parameter offsets

  array[J] vector[7] zInd;                        // individual offset z-scores to each parameter: J-array of column 7-vectors, one vector for each individual
  array[E] vector<lower=0>[7] offsetIndivStdevs;  // stdevs for mean offsets to the mean group trajectory for each individual, for each of the 7 parameters: E-array of 7-vectors, one vector for each ethnic group
  array[E] cholesky_factor_corr[7] L_I;           // E-array of cholesky factor 7x7 matrices, for correlation matrix among mean (across individuals within ethnic groups) parameter offsets

  real<lower=0,upper=0.005> Lsigma;               // variance of the logged normal distribution, upper limit of 0.005 yields 95% of observations within 2*stdev = 1cm of actual height at 100cm
}

transformed parameters {
  
  matrix[E,7] GrpOffset;            // matrix of random offsets for each ethnic group to each of the 7 paramters, rows = groups, cols = parameters
  matrix[J,7] IndOffset;            // matrix of random offsets for each individual to each of the 7 paramters, rows = indivs, cols = parameters
                                    // Stan manual pg 150-151, Rethinking pg 409

  for (e in 1:E) {
    GrpOffset[e] = (diag_pre_multiply(offsetGroupStdevs, L_G) * zGrp[e])';                           //create cholesky factor for cov matrix and multiply by vector of offset z-scores, then transpose. This makes an 9-vector for each ethnic group e
  } // for e


  for (j in 1:J) {
    IndOffset[j] = (diag_pre_multiply(offsetIndivStdevs[ethIndiv[j]], L_I[ethIndiv[j]]) * zInd[j])';  //create cholesky factor for group-specific cov matrix and multiply by vector of offset z-scores, then transpose. This makes an 9-vector for each individual j
  } // for j

}


model {
  vector[N] mu;                      // mean of un-logged normal distribution

  vector[J] A;                      // temporary holders for linear sub-model for each parameter
  vector[J] C1;
  vector[J] C2;
  vector[J] C3;
  vector[J] D1;
  vector[J] D2;
  vector[J] D3;

  Lsigma ~ exponential(1/muLsigma);                   // parameter of exponential = 1/mean


  zGrp ~ multi_normal(zeros, diag_matrix(ones));                // E-array of group offset z-scores, sample 7 at a time from a normal, no covariance among parameters

  offsetGroupStdevs[1] ~ normal(pgsA,4);         //exponential(1/pgsA);           //exponential(pgexpA);                 // hyper-priors for stdevs of ethnicity offsets to parameters, mean of exponential(beta) = 1/beta
  offsetGroupStdevs[2] ~ exponential(1/pgsC1);          //exponential(pgexpC1);
  offsetGroupStdevs[3] ~ exponential(1/pgsC2);          //exponential(pgexpC2);
  offsetGroupStdevs[4] ~ exponential(1/pgsC3);          //exponential(pgexpC3);
  offsetGroupStdevs[5] ~ exponential(1/pgsD1);          //exponential(pgexpD1);
  offsetGroupStdevs[6] ~ exponential(1/pgsD2);          //exponential(pgexpD2);
  offsetGroupStdevs[7] ~ exponential(1/pgsD3);          //exponential(pgexpD3);

  L_G ~ lkj_corr_cholesky(pCholEtaG);                          //cholesky factor 9x9 matrix, lower the eta to allow more extreme correlations, Rethinking pg 394
  


  zInd ~ multi_normal(zeros, diag_matrix(ones));               // J-array of individual offset z-scores, sample 9 at a time from a normal

  for (eth in 1:E) {

    offsetIndivStdevs[eth,1] ~ normal(pimA,pisA);          //exponential(1/pimA);           //normal(pimA,pisA);     //exponential(1/pimA);           //              // hyper-priors for stdevs of individual offsets to parameters
    offsetIndivStdevs[eth,2] ~ normal(pimC1,pisC1);         //exponential(1/pimC1);          //   //exponential(1/pimC1);          //
    offsetIndivStdevs[eth,3] ~ normal(pimC2,pisC2);         //exponential(1/pimC2);          //normal(pimC2,pisC2);   //exponential(1/pimC2);          //
    offsetIndivStdevs[eth,4] ~ normal(pimC3,pisC3);         //exponential(1/pimC3);          //normal(pimC3,pisC3);   //exponential(1/pimC3);          //
    offsetIndivStdevs[eth,5] ~ normal(pimD1,pisD1);         //exponential(1/pimD1);          //normal(pimD1,pisD1);   //exponential(1/pimD1);          //
    offsetIndivStdevs[eth,6] ~ normal(pimD2,pisD2);         //exponential(1/pimD2);          //normal(pimD2,pisD2);   //exponential(1/pimD2);          //
    offsetIndivStdevs[eth,7] ~ normal(pimD3,pisD3);         //exponential(1/pimD3);          //normal(pimD3,pisD3);   //exponential(1/pimD3);          //


    L_I[eth] ~ lkj_corr_cholesky(pCholEtaI);                   //arrays of cholesky factor 9x9 matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  
  } // for eth



  for (n in 1:N) {
    
    A[ID[n]]  = Ahat  + GrpOffset[ethnicity[n],1] + IndOffset[ID[n],1];   // linear submodels for each parameter
    C1[ID[n]] = C1hat + GrpOffset[ethnicity[n],2] + IndOffset[ID[n],2];
    C2[ID[n]] = C2hat + GrpOffset[ethnicity[n],3] + IndOffset[ID[n],3];
    C3[ID[n]] = C3hat + GrpOffset[ethnicity[n],4] + IndOffset[ID[n],4];
    D1[ID[n]] = D1hat + GrpOffset[ethnicity[n],5] + IndOffset[ID[n],5];
    D2[ID[n]] = D2hat + GrpOffset[ethnicity[n],6] + IndOffset[ID[n],6];
    D3[ID[n]] = D3hat + GrpOffset[ethnicity[n],7] + IndOffset[ID[n],7];


    mu[n] = A[ID[n]]*exp( -1/( C1[ID[n]]*log( age[n]/D1[ID[n]] + 1 ) + (age[n]/D2[ID[n]])^C2[ID[n]] + (age[n]/D3[ID[n]])^C3[ID[n]] ) );

  } // for

 
  log_height ~ normal( log(mu), Lsigma );
}


generated quantities{

  vector[N] mu;                      // mean of un-logged normal distribution

  vector[N] log_lik;                 // for WAIC

  array[E] matrix[7,7] I_cor_mat;    // E-array of 7x7 correlation matrices for parameters within ethnic group
  array[E] matrix[7,7] I_cov_mat;    // E-array of 7x7 variance-covariance matrices for parameters within ethnic group
  matrix[7,7] G_cor_mat;             // 7x7 correlation matrix for parameters across ethnic groups
  matrix[7,7] G_cov_mat;             // 7x7 variance-covariance matrix for parameters across ethnic groups

  vector[J] A;                       // temporary holders for linear sub-model for each parameter
  vector[J] C1;
  vector[J] C2;
  vector[J] C3;
  vector[J] D1; 
  vector[J] D2;
  vector[J] D3;


  real berA;                         // parameters for mean Berkeley
  real berC1;
  real berC2;
  real berC3;
  real berD1;
  real berD2;
  real berD3;


  real matA;                         // parameters for mean Matsigenka
  real matC1;
  real matC2;
  real matC3;
  real matD1;
  real matD2;
  real matD3;



  berA = Ahat + GrpOffset[1,1];          // GrpOffset[group, parameter] 
  berC1 = C1hat + GrpOffset[1,2];
  berC2 = C2hat + GrpOffset[1,3];
  berC3 = C3hat + GrpOffset[1,4];
  berD1 = D1hat + GrpOffset[1,5];
  berD2 = D2hat + GrpOffset[1,6];
  berD3 = D3hat + GrpOffset[1,7];


  matA = Ahat + GrpOffset[2,1];
  matC1 = C1hat + GrpOffset[2,2];
  matC2 = C2hat + GrpOffset[2,3];
  matC3 = C3hat + GrpOffset[2,4]; 
  matD1 = D1hat + GrpOffset[2,5];
  matD2 = D2hat + GrpOffset[2,6];
  matD3 = D3hat + GrpOffset[2,7];



  for (n in 1:N) {

    A[ID[n]]  = Ahat  + GrpOffset[ethnicity[n],1] + IndOffset[ID[n],1];   // linear submodels for each parameter
    C1[ID[n]] = C1hat + GrpOffset[ethnicity[n],2] + IndOffset[ID[n],2];
    C2[ID[n]] = C2hat + GrpOffset[ethnicity[n],3] + IndOffset[ID[n],3];
    C3[ID[n]] = C3hat + GrpOffset[ethnicity[n],4] + IndOffset[ID[n],4];
    D1[ID[n]] = D1hat + GrpOffset[ethnicity[n],5] + IndOffset[ID[n],5];
    D2[ID[n]] = D2hat + GrpOffset[ethnicity[n],6] + IndOffset[ID[n],6];
    D3[ID[n]] = D3hat + GrpOffset[ethnicity[n],7] + IndOffset[ID[n],7];


    mu[n] = A[ID[n]]*exp( -1/( C1[ID[n]]*log( age[n]/D1[ID[n]] + 1 ) + (age[n]/D2[ID[n]])^C2[ID[n]] + (age[n]/D3[ID[n]])^C3[ID[n]] ) );

    log_lik[n] = normal_lpdf( log_height[n] | log(mu[n]), Lsigma );       // for WAIC function

  } // for n


  for (eth in 1:E) {

    I_cor_mat[eth] = L_I[eth] * L_I[eth]';                                    //reconstruct the correlation matrix to look at in output

    I_cov_mat[eth] = diag_pre_multiply(offsetIndivStdevs[eth], L_I[eth]) * 
                     diag_pre_multiply(offsetIndivStdevs[eth], L_I[eth])';    //construct cov matrix from cholesky factors of cov matrix to look at in output
  
  } // for eth

  G_cor_mat = L_G * L_G';                                                     //reconstruct the correlation matrix to look at in output

  G_cov_mat = diag_pre_multiply(offsetGroupStdevs, L_G) * 
              diag_pre_multiply(offsetGroupStdevs, L_G)';                     //construct cov matrix from cholesky factors of cov matrix to look at in output
  
}


