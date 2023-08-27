
data {
  int<lower=0> N;                     // number of height observations
  int<lower=0> J;                     // number of people measured
  array[N] int<lower=0> ID;           // vector of person IDs for each height measurement
  vector<lower=0>[N] age;             // vector of ages for each observation (in years since conception)
  vector<lower=0>[N] height;          // vector of height observations (in cm)

  // mean posteriors from m1
  real<upper=0> LQ1hat;
  real<upper=0> LQ2hat;   
  real<upper=0> LQ3hat;
  real<lower=0> LK1hat;
  real<lower=0> LK2hat;   
  real<lower=0> LK3hat;
  real<lower=0> R1hat;
  real<lower=0> R2hat;   
  real<lower=0> R3hat;

  // prior stdev for group-level mean offset (from PriorPredictIndivStdev)
  real<lower=0> pgsdLQ1;
  real<lower=0> pgsdLQ2;        
  real<lower=0> pgsdLQ3;
  real<lower=0> pgsdLK1;
  real<lower=0> pgsdLK2;        
  real<lower=0> pgsdLK3; 
  real<lower=0> pgsdR1;
  real<lower=0> pgsdR2;        
  real<lower=0> pgsdR3; 

  // rate (=1/mean) for exponential prior on indiv-level offset (from PriorPredictIndivStdev)
  real<lower=0> piexpLQ1;
  real<lower=0> piexpLQ2;      
  real<lower=0> piexpLQ3;
  real<lower=0> piexpLK1;
  real<lower=0> piexpLK2;      
  real<lower=0> piexpLK3;
  real<lower=0> piexpR1;
  real<lower=0> piexpR2;      
  real<lower=0> piexpR3;

  // eta parameter for lkj_corr_cholesky (from PriorPredictIndivStdev)
  real<lower=0> pCholEta;
}

transformed data {
  vector[9] zeros = [0,0,0,0,0,0,0,0,0]';         // column vector of 0s, note transpose at the end to turn into row vectors
  vector[9] ones = [1,1,1,1,1,1,1,1,1]';          // column vector of 1s
  vector[N] log_height;                            

  log_height = log(height);

  // stdevs for mean group-level offsets to parameters
  vector[9] offsetStdevs = [pgsdLQ1,pgsdLQ2,pgsdLQ3,pgsdLK1,pgsdLK2,pgsdLK3,pgsdR1,pgsdR2,pgsdR3]';
}

parameters {
  real<lower=0> sigma;                  // stdev of the logged normal distribution

  array[J] vector[9] zInd;              // individual offset z-scores to each parameter: 9-vectors, one vector for each individual
  vector[9] muInd;                      // mean (across individuals) offsets to each of the 9 parameters 
  vector<lower=0>[9] sigmaInd;          // stdevs for mean (across individuals) offsets to each of the 9 parameters
  cholesky_factor_corr[9] L_U;          // cholesky factor 9x9 matrices, for correlation matrix among mean (across individuals) parameter offsets
}

transformed parameters {
  matrix[J,9] off_Ind;              // matrix of random offsets for each individual to each of the 9 paramters, rows = indivs, cols = parameters
                                    // Stan manual pg 150-151, Rethinking pg 409

  for (j in 1:J) {
    off_Ind[j] = (diag_pre_multiply(sigmaInd, L_U) * zInd[j])';                           //create cholesky factor for cov matrix and multiply by vector of offset z-scores, then transpose.
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

  sigma ~ exponential(100);   // where parameter of exponential = lambda, larger means mean is smaller. This is essentially the measuring instrument error we assume.

  zInd ~ multi_normal(zeros, diag_matrix(ones));                    // individual offset z-scores, sample 9 at a time from a normal
  muInd ~ multi_normal(zeros, diag_matrix(offsetStdevs));           // 9-vector of mean (across individuals) offsets

  sigmaInd[1] ~ exponential(piexpLQ1);                         // hyper-priors for stdevs of ethnicity offsets to parameters, mean of exponential(beta) = 1/beta
  sigmaInd[2] ~ exponential(piexpLQ2);
  sigmaInd[3] ~ exponential(piexpLQ3);
  sigmaInd[4] ~ exponential(piexpLK1);
  sigmaInd[5] ~ exponential(piexpLK2);
  sigmaInd[6] ~ exponential(piexpLK3);
  sigmaInd[7] ~ exponential(piexpR1);
  sigmaInd[8] ~ exponential(piexpR2);
  sigmaInd[9] ~ exponential(piexpR3);

  L_U ~ lkj_corr_cholesky(pCholEta);                           //cholesky factor 9x9 matrix, lower the eta to allow more extreme correlations, Rethinking pg 394
  

  for (n in 1:N) {

    Q1[n] = exp(LQ1hat + muInd[1] + off_Ind[ID[n],1]);   // linear submodels for each parameter
    Q2[n] = exp(LQ2hat + muInd[2] + off_Ind[ID[n],2]);
    Q3[n] = exp(LQ3hat + muInd[3] + off_Ind[ID[n],3]);
    K1[n] = exp(LK1hat + muInd[4] + off_Ind[ID[n],4]);
    K2[n] = exp(LK2hat + muInd[5] + off_Ind[ID[n],5]);
    K3[n] = exp(LK3hat + muInd[6] + off_Ind[ID[n],6]);
    H1[n] = K1[n]/2 + R1hat + muInd[7] + off_Ind[ID[n],7];
    H2[n] = K2[n]/2 + R2hat + muInd[8] + off_Ind[ID[n],8];
    H3[n] = K3[n]/2 + R3hat + muInd[9] + off_Ind[ID[n],9];

    mu[n] = ( -1*exp(-1*K1[n]*Q1[n]*age[n]/( 1 + 2*Q1[n] )) + 2*H1[n]/K1[n] )^(1/Q1[n]) +
            ( -1*exp(-1*K2[n]*Q2[n]*age[n]/( 1 + 2*Q2[n] )) + 2*H2[n]/K2[n] )^(1/Q2[n]) +
            ( -1*exp(-1*K3[n]*Q3[n]*age[n]/( 1 + 2*Q3[n] )) + 2*H3[n]/K3[n] )^(1/Q3[n]); 

  } // for

  log_height ~ normal( log(mu), sigma );

}


generated quantities{

  vector[N] mu;                      // mean of un-logged normal distribution

  vector[N] log_lik;                 // for WAIC

  matrix[9,9] U_cor_mat;             // 9x9 correlation matrix for parameters across individuals
  matrix[9,9] U_cov_mat;             // 9x9 variance-covariance matrix for parameters across individuals

  vector[N] Q1;                        // temporary holders for linear sub-model for each parameter
  vector[N] Q2;
  vector[N] Q3;
  vector[N] K1;
  vector[N] K2;
  vector[N] K3;
  vector[N] H1;
  vector[N] H2;
  vector[N] H3;

  real mLQ1;                         // parameters for group mean trajectory
  real mLQ2;
  real mLQ3;
  real mLK1;
  real mLK2;
  real mLK3;
  real mR1;
  real mR2;
  real mR3;

  mLQ1 = LQ1hat + muInd[1]; 
  mLQ2 = LQ2hat + muInd[2];
  mLQ3 = LQ3hat + muInd[3];
  mLK1 = LK1hat + muInd[4];
  mLK2 = LK2hat + muInd[5];
  mLK3 = LK3hat + muInd[6];
  mR1 = R1hat + muInd[7];
  mR2 = R2hat + muInd[8];
  mR3 = R3hat + muInd[9];


  for (n in 1:N) {

    Q1[n] = exp(LQ1hat + muInd[1] + off_Ind[ID[n],1]);   // linear submodels for each parameter
    Q2[n] = exp(LQ2hat + muInd[2] + off_Ind[ID[n],2]);
    Q3[n] = exp(LQ3hat + muInd[3] + off_Ind[ID[n],3]);
    K1[n] = exp(LK1hat + muInd[4] + off_Ind[ID[n],4]);
    K2[n] = exp(LK2hat + muInd[5] + off_Ind[ID[n],5]);
    K3[n] = exp(LK3hat + muInd[6] + off_Ind[ID[n],6]);
    H1[n] = K1[n]/2 + R1hat + muInd[7] + off_Ind[ID[n],7];
    H2[n] = K2[n]/2 + R2hat + muInd[8] + off_Ind[ID[n],8];
    H3[n] = K3[n]/2 + R3hat + muInd[9] + off_Ind[ID[n],9];

    mu[n] = ( -1*exp(-1*K1[n]*Q1[n]*age[n]/( 1 + 2*Q1[n] )) + 2*H1[n]/K1[n] )^(1/Q1[n]) +
            ( -1*exp(-1*K2[n]*Q2[n]*age[n]/( 1 + 2*Q2[n] )) + 2*H2[n]/K2[n] )^(1/Q2[n]) +
            ( -1*exp(-1*K3[n]*Q3[n]*age[n]/( 1 + 2*Q3[n] )) + 2*H3[n]/K3[n] )^(1/Q3[n]); 

    log_lik[n] = normal_lpdf( log_height[n] | log(mu[n]), sigma );       // for WAIC function

  } // for n


  U_cor_mat = L_U * L_U';                                                 //reconstruct the correlation matrix to look at in output

  U_cov_mat = diag_pre_multiply(sigmaInd, L_U) * 
              diag_pre_multiply(sigmaInd, L_U)';                          //construct cov matrix from cholesky factors of cov matrix to look at in output
  
}


