
data {
  int<lower=0> N;                           // number of observations
  int<lower=0> J;                           // number of people measured
  int<lower=0> E;                           // number of ethnic groups
  array[N] int<lower=0> ID;                 // vector of person IDs for each measurement
  vector<lower=0>[N] age;                   // vector of ages for each observation (in years since conception)
  vector<lower=0>[N] height;                // vector of observations (in cm)
  array[N] int<lower=0,upper=E> ethnicity;  // vector of ethnicities for each observation: 1=Ber, 2=Mat
  array[J] int<lower=1,upper=E> ethIndiv;   // vector of ethnicities for each individual j: 1=Ber, 2=Mat

  // mean posteriors from m2
  real<upper=0> LQ1hat;
  real<upper=0> LQ2hat;   
  real<upper=0> LQ3hat;
  real<lower=0> LK1hat;
  real<lower=0> LK2hat;   
  real<lower=0> LK3hat;
  real<lower=0> R1hat;
  real<lower=0> R2hat;   
  real<lower=0> R3hat;

  // rate (=1/mean) for exponential prior on group-level offset stdev (from PriorPredictGroupStdev)
  real<lower=0> pgexpLQ1;
  real<lower=0> pgexpLQ2;      
  real<lower=0> pgexpLQ3;
  real<lower=0> pgexpLK1;
  real<lower=0> pgexpLK2;      
  real<lower=0> pgexpLK3;
  real<lower=0> pgexpR1;
  real<lower=0> pgexpR2;      
  real<lower=0> pgexpR3;

  // mean for normal prior on indiv-level offset stdev (from PriorPredictGroupStdev)
  real<lower=0> pimLQ1;
  real<lower=0> pimLQ2;      
  real<lower=0> pimLQ3;
  real<lower=0> pimLK1;
  real<lower=0> pimLK2;      
  real<lower=0> pimLK3;
  real<lower=0> pimR1;
  real<lower=0> pimR2;      
  real<lower=0> pimR3;

  // stdev for normal prior on indiv-level offset stdev (from PriorPredictGroupStdev)
  real<lower=0> pisLQ1;
  real<lower=0> pisLQ2;      
  real<lower=0> pisLQ3;
  real<lower=0> pisLK1;
  real<lower=0> pisLK2;      
  real<lower=0> pisLK3;
  real<lower=0> pisR1;
  real<lower=0> pisR2;      
  real<lower=0> pisR3;

  // eta parameter for lkj_corr_cholesky (from PriorPredictGroupStdev)
  real<lower=0> pCholEtaU;
  real<lower=0> pCholEtaR;
}

transformed data {
  vector[9] zeros = [0,0,0,0,0,0,0,0,0]';       // column vector of 0s, note transpose at the end to turn into row vectors
  vector[9] ones = [1,1,1,1,1,1,1,1,1]';        // column vector of 1s
  vector[N] log_height;                            

  log_height = log(height);
}

parameters {
  real<lower=0> sigma;                            // stdev of the logged normal distribution

  array[E] vector[9] zGrp;                        // ethnic group offset z-scores to each parameter: E-array of column 9-vectors, one vector for each ethnicity
  vector<lower=0>[9] offsetGroupStdevs;           // stdevs for mean (across indivs) offsets to the overall mean (across groups) trajectory for each group, for each of the 9 parameters
  cholesky_factor_corr[9] L_U;                    // cholesky factor 9x9 matrices, for correlation matrix among mean (across ethnicities) parameter offsets

  array[J] vector[9] zInd;                        // individual offset z-scores to each parameter: J-array of column 9-vectors, one vector for each individual
  array[E] vector<lower=0>[9] offsetIndivStdevs;  // stdevs for mean offsets to the mean group trajectory for each individual, for each of the 9 parameters: E-array of 9-vectors, one vector for each ethnic group
  array[E] cholesky_factor_corr[9] L_R;           // E-array of cholesky factor 9x9 matrices, for correlation matrix among mean (across individuals within ethnic groups) parameter offsets
}

transformed parameters {
  
  matrix[E,9] GrpOffset;            // matrix of random offsets for each ethnic group to each of the 9 paramters, rows = groups, cols = parameters
  matrix[J,9] IndOffset;            // matrix of random offsets for each individual to each of the 9 paramters, rows = indivs, cols = parameters
                                    // Stan manual pg 150-151, Rethinking pg 409

  for (e in 1:E) {
    GrpOffset[e] = (diag_pre_multiply(offsetGroupStdevs, L_U) * zGrp[e])';                           //create cholesky factor for cov matrix and multiply by vector of offset z-scores, then transpose. This makes an 9-vector for each ethnic group e
  } // for e


  for (j in 1:J) {
    IndOffset[j] = (diag_pre_multiply(offsetIndivStdevs[ethIndiv[j]], L_R[ethIndiv[j]]) * zInd[j])';  //create cholesky factor for group-specific cov matrix and multiply by vector of offset z-scores, then transpose. This makes an 9-vector for each individual j
  } // for j

}


model {
  vector[N] mu;                      // mean of un-logged normal distribution

  vector[N] Q1;                      // temporary holders for linear sub-model for each parameter
  vector[N] Q2;
  vector[N] Q3;
  vector[N] K1;
  vector[N] K2;
  vector[N] K3;
  vector[N] H1;
  vector[N] H2;
  vector[N] H3;


  sigma ~ exponential(200);   // where parameter of exponential = lambda, larger means mean is smaller. This is essentially the measuring instrument error we assume.


  zGrp ~ multi_normal(zeros, diag_matrix(ones));                // E-array of group offset z-scores, sample 9 at a time from a normal, no covariance among parameters

  offsetGroupStdevs[1] ~ exponential(pgexpLQ1);                 // hyper-priors for stdevs of ethnicity offsets to parameters, mean of exponential(beta) = 1/beta
  offsetGroupStdevs[2] ~ exponential(pgexpLQ2);
  offsetGroupStdevs[3] ~ exponential(pgexpLQ3);
  offsetGroupStdevs[4] ~ exponential(pgexpLK1);
  offsetGroupStdevs[5] ~ exponential(pgexpLK2);
  offsetGroupStdevs[6] ~ exponential(pgexpLK3);
  offsetGroupStdevs[7] ~ exponential(pgexpR1);
  offsetGroupStdevs[8] ~ exponential(pgexpR2);
  offsetGroupStdevs[9] ~ exponential(pgexpR3);

  L_U ~ lkj_corr_cholesky(pCholEtaU);                          //cholesky factor 9x9 matrix, lower the eta to allow more extreme correlations, Rethinking pg 394
  


  zInd ~ multi_normal(zeros, diag_matrix(ones));               // J-array of individual offset z-scores, sample 9 at a time from a normal

  for (eth in 1:E) {

    offsetIndivStdevs[eth,1] ~ normal(pimLQ1,pisLQ1);          // hyper-priors for stdevs of individual offsets to parameters
    offsetIndivStdevs[eth,2] ~ normal(pimLQ2,pisLQ2);
    offsetIndivStdevs[eth,3] ~ normal(pimLQ3,pisLQ3);
    offsetIndivStdevs[eth,4] ~ normal(pimLK1,pisLK1);
    offsetIndivStdevs[eth,5] ~ normal(pimLK2,pisLK2);
    offsetIndivStdevs[eth,6] ~ normal(pimLK3,pisLK3);
    offsetIndivStdevs[eth,7] ~ normal(pimR1,pisR1);
    offsetIndivStdevs[eth,8] ~ normal(pimR2,pisR2);
    offsetIndivStdevs[eth,9] ~ normal(pimR3,pisR3);

    L_R[eth] ~ lkj_corr_cholesky(pCholEtaR);                   //arrays of cholesky factor 9x9 matrices, lower the eta to allow more extreme correlations, Rethinking pg 394
  
  } // for eth



  for (n in 1:N) {
    
    Q1[n] = exp( LQ1hat + GrpOffset[ethnicity[n],1] + IndOffset[ID[n],1] );   // linear submodels for each parameter
    Q2[n] = exp( LQ2hat + GrpOffset[ethnicity[n],2] + IndOffset[ID[n],2] );
    Q3[n] = exp( LQ3hat + GrpOffset[ethnicity[n],3] + IndOffset[ID[n],3] );
    K1[n] = exp( LK1hat + GrpOffset[ethnicity[n],4] + IndOffset[ID[n],4] );
    K2[n] = exp( LK2hat + GrpOffset[ethnicity[n],5] + IndOffset[ID[n],5] );
    K3[n] = exp( LK3hat + GrpOffset[ethnicity[n],6] + IndOffset[ID[n],6] );
    H1[n] = K1[n]/2 + R1hat + GrpOffset[ethnicity[n],7] + IndOffset[ID[n],7];
    H2[n] = K2[n]/2 + R2hat + GrpOffset[ethnicity[n],8] + IndOffset[ID[n],8];
    H3[n] = K3[n]/2 + R3hat + GrpOffset[ethnicity[n],9] + IndOffset[ID[n],9];

    mu[n] = ( -1*exp(-1*K1[n]*Q1[n]*age[n]/( 1 + 2*Q1[n] )) + 2*H1[n]/K1[n] )^(1/Q1[n]) +
            ( -1*exp(-1*K2[n]*Q2[n]*age[n]/( 1 + 2*Q2[n] )) + 2*H2[n]/K2[n] )^(1/Q2[n]) +
            ( -1*exp(-1*K3[n]*Q3[n]*age[n]/( 1 + 2*Q3[n] )) + 2*H3[n]/K3[n] )^(1/Q3[n]);

  } // for

 
  log_height ~ normal( log(mu), sigma );
}


generated quantities{

  vector[N] mu;                      // mean of un-logged normal distribution

  vector[N] log_lik;                 // for WAIC

  array[E] matrix[9,9] R_cor_mat;    // E-array of 9x9 correlation matrices for parameters within ethnic group
  array[E] matrix[9,9] R_cov_mat;    // E-array of 9x9 variance-covariance matrices for parameters within ethnic group
  matrix[9,9] U_cor_mat;             // 9x9 correlation matrix for parameters across ethnic groups
  matrix[9,9] U_cov_mat;             // 9x9 variance-covariance matrix for parameters across ethnic groups

  vector[N] Q1;                      // temporary holders for linear sub-model for each parameter
  vector[N] Q2;
  vector[N] Q3;
  vector[N] K1;
  vector[N] K2;
  vector[N] K3;
  vector[N] H1;
  vector[N] H2;
  vector[N] H3;


  real berLQ1;                         // parameters for mean Berkeley
  real berLQ2;
  real berLQ3;
  real berLK1;
  real berLK2;
  real berLK3;
  real berR1;
  real berR2;
  real berR3;

  real matLQ1;                         // parameters for mean Matsigenka
  real matLQ2;
  real matLQ3;
  real matLK1;
  real matLK2;
  real matLK3;
  real matR1;
  real matR2;
  real matR3;


  berLQ1 = LQ1hat + GrpOffset[1,1];          // GrpOffset[group, parameter] 
  berLQ2 = LQ2hat + GrpOffset[1,2];
  berLQ3 = LQ3hat + GrpOffset[1,3];
  berLK1 = LK1hat + GrpOffset[1,4];
  berLK2 = LK2hat + GrpOffset[1,5];
  berLK3 = LK3hat + GrpOffset[1,6];
  berR1 = R1hat + GrpOffset[1,7];
  berR2 = R2hat + GrpOffset[1,8];
  berR3 = R3hat + GrpOffset[1,9];

  matLQ1 = LQ1hat + GrpOffset[2,1];
  matLQ2 = LQ2hat + GrpOffset[2,2];
  matLQ3 = LQ3hat + GrpOffset[2,3];
  matLK1 = LK1hat + GrpOffset[2,4];
  matLK2 = LK2hat + GrpOffset[2,5];
  matLK3 = LK3hat + GrpOffset[2,6];
  matR1 = R1hat + GrpOffset[2,7];
  matR2 = R2hat + GrpOffset[2,8];
  matR3 = R3hat + GrpOffset[2,9];


  for (n in 1:N) {

    Q1[n] = exp( LQ1hat + GrpOffset[ethnicity[n],1] + IndOffset[ID[n],1] );   // linear submodels for each parameter
    Q2[n] = exp( LQ2hat + GrpOffset[ethnicity[n],2] + IndOffset[ID[n],2] );
    Q3[n] = exp( LQ3hat + GrpOffset[ethnicity[n],3] + IndOffset[ID[n],3] );
    K1[n] = exp( LK1hat + GrpOffset[ethnicity[n],4] + IndOffset[ID[n],4] );
    K2[n] = exp( LK2hat + GrpOffset[ethnicity[n],5] + IndOffset[ID[n],5] );
    K3[n] = exp( LK3hat + GrpOffset[ethnicity[n],6] + IndOffset[ID[n],6] );
    H1[n] = K1[n]/2 + R1hat + GrpOffset[ethnicity[n],7] + IndOffset[ID[n],7];
    H2[n] = K2[n]/2 + R2hat + GrpOffset[ethnicity[n],8] + IndOffset[ID[n],8];
    H3[n] = K3[n]/2 + R3hat + GrpOffset[ethnicity[n],9] + IndOffset[ID[n],9];

    mu[n] = ( -1*exp(-1*K1[n]*Q1[n]*age[n]/( 1 + 2*Q1[n] )) + 2*H1[n]/K1[n] )^(1/Q1[n]) +
            ( -1*exp(-1*K2[n]*Q2[n]*age[n]/( 1 + 2*Q2[n] )) + 2*H2[n]/K2[n] )^(1/Q2[n]) +
            ( -1*exp(-1*K3[n]*Q3[n]*age[n]/( 1 + 2*Q3[n] )) + 2*H3[n]/K3[n] )^(1/Q3[n]);

    log_lik[n] = normal_lpdf( log_height[n] | log(mu[n]), sigma );       // for WAIC function

  } // for n


  for (eth in 1:E) {

    R_cor_mat[eth] = L_R[eth] * L_R[eth]';                                    //reconstruct the correlation matrix to look at in output

    R_cov_mat[eth] = diag_pre_multiply(offsetIndivStdevs[eth], L_R[eth]) * 
                     diag_pre_multiply(offsetIndivStdevs[eth], L_R[eth])';    //construct cov matrix from cholesky factors of cov matrix to look at in output
  
  } // for eth

  U_cor_mat = L_U * L_U';                                                     //reconstruct the correlation matrix to look at in output

  U_cov_mat = diag_pre_multiply(offsetGroupStdevs, L_U) * 
              diag_pre_multiply(offsetGroupStdevs, L_U)';                     //construct cov matrix from cholesky factors of cov matrix to look at in output
  
}


