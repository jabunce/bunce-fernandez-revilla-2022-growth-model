############################# STAN analysis ##############################################################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())



#load models and data and starting conditions

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models


N <- nrow(Com.mal)                      # number of observations
J <- length(unique(Com.mal$ID))         # number of people measured
E <- length(unique(Com.mal$Ethnicity))  # number of ethnic groups
EthID <- Com.mal[match(unique(Com.mal$ID), Com.mal$ID),c("ID","Ethnicity")] # ethnicity for each ID

# data list for fitting Berkeley and Matsigenka data
data_list3 <- list(
  N = nrow(Com.mal),                      # number of observations
  J = length(unique(Com.mal$ID)),         # number of people measured
  E = length(unique(Com.mal$Ethnicity)),  # number of ethnic groups
  ID = Com.mal$ID,                        # vector of individual IDs for each height observation
  age = Com.mal$TotAge,                   # vector of ages for each observation (in years since conception)
  weight = Com.mal$Weight,                # vector of observations (in kg)
  ethnicity = Com.mal$Ethnicity,          # vector of ethnicity for each observation (1=Ber, 2=Mat)
  ethIndiv = EthID[,2],                   # vector of ethnicity for each person (1=Ber, 2=Mat)

  # means of posterior estimates from m2
  LQ1hat = mean(post2$mLQ1),
  LQ2hat = mean(post2$mLQ2),         
  LQ3hat = mean(post2$mLQ3), 
  LK1hat = mean(post2$mLK1),      
  LK2hat = mean(post2$mLK2), 
  LK3hat = mean(post2$mLK3), 
  R1hat = mean(post2$mR1), 
  R2hat = mean(post2$mR2), 
  R3hat = mean(post2$mR3),  

  # rate (=1/mean) for exponential prior on group-level offset stdev (from PriorPredictGroupStdev)
  pgexpLQ1 = gexpLQ1,
  pgexpLQ2 = gexpLQ2,        
  pgexpLQ3 = gexpLQ3,
  pgexpLK1 = gexpLK1,      
  pgexpLK2 = gexpLK2,
  pgexpLK3 = gexpLK3,
  pgexpR1 = gexpR1,      
  pgexpR2 = gexpR2,
  pgexpR3 = gexpR3,

  # mean for normal prior on indiv-level offset stdev (from PriorPredictGroupStdev)
  pimLQ1 = imLQ1,
  pimLQ2 = imLQ2,      
  pimLQ3 = imLQ3,
  pimLK1 = imLK1,      
  pimLK2 = imLK2,
  pimLK3 = imLK3,
  pimR1 = imR1,      
  pimR2 = imR2,
  pimR3 = imR3,

  # stdev for normal prior on indiv-level offset stdev (from PriorPredictGroupStdev)
  pisLQ1 = isLQ1, # 0.05
  pisLQ2 = isLQ2, # 0.05      
  pisLQ3 = isLQ3, # 0.1
  pisLK1 = isLK1, # 0.05      
  pisLK2 = isLK2, # 0.05
  pisLK3 = isLK3, # 0.1
  pisR1 = isR1, # 0.05     
  pisR2 = isR2, # 0.05
  pisR3 = isR3, # 0.1

  # eta parameter for lkj_corr_cholesky
  pCholEtaU = CholEtaU,
  pCholEtaR = CholEtaR 
)

# starting values
start_list3 <- list(
  sigma = 0.01,                                   # stdev for the logged normal likelihood

  OvAllOffset = rep( 0, times=9 ),                # mean offset to each parameter
  
  zGrp = matrix(0, nrow=E, ncol=9),               # ethnicity offset z-scores to each parameter: E-array of column 9-vectors, one vector for each ethnicity
  offsetGroupStdevs = rep( 1, times=9 ),          # mean offset stdevs
  L_U = array(data=0, dim=c(9,9)),                # cholesky factor 9x9 matrix, for correlation matrix for mean offsets

  zInd = matrix(0, nrow=J, ncol=9),               # individual offset z-scores to each parameter: J-array of column 9-vectors, one vector for each individual
  offsetIndivStdevs = array(data=1, dim=c(E,9)),  # offset stdevs: E-array of 9-vectors, one vector for each ethnicity
  L_R = array(data=0, dim=c(E,9,9))               # E-array of cholesky factor 9x9 matrices, for correlation matrix for offsets
)


############################### Run Stan models

####### m3 composite model using Berkeley and Matsigenka data, random effects for groups and covarying random effects for individuals

# samps <- 1000
# num_chains <- 2

model <- cmdstan_model(model_file[3])

m3 <- model$sample(
        seed=1,
        data=data_list3, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list3), num_chains),
        max_treedepth=12, #default treedepth is 10
        adapt_delta=0.99
      )

m3$save_object(file = "m3_fit.RDS") #save stan output so you don't have re-run model
#m3 <- readRDS("m3_fit.RDS")


# look at posterior estimates
# m3$print(max_rows=100, c(
#          "lp__",
#          "offsetGroupStdevs",
#          "offsetIndivStdevs",
#          "R_cov_mat",
#          "U_cov_mat",
#          #"GrpOffset",
#          #"IndOffset",
#          "sigma"))

print(n=300, as_tibble(
      m3$summary(c("mean", "sd", "rhat", "ess_bulk"),
                 variables=c(
                    "lp__",
                    "GrpOffset",
                    "offsetGroupStdevs",
                    "offsetIndivStdevs",
                    "R_cov_mat",
                    "U_cov_mat",
                    #"IndOffset",
                    "sigma"))
))

post3 <- m3$draws(format = "df", , inc_warmup = FALSE)
str(post3)
saveRDS(post3, "post3.RDS")
#post3 <- readRDS("post3.RDS")
#post3$"GrpOffset[1]"

fitmod <- m3$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

color_scheme_set("mix-blue-red")


#look at all traces, in Plots folder
pdf(file="./Plots/traces_m3.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

    for ( z in 1:9 ){
        print(mcmc_trace(fitmod, pars=paste("offsetGroupStdevs[", z, "]", sep=""), n_warmup = samps/2 ))
    } # for z

    for ( z1 in 1:E ){
        for ( z2 in 1:9 ){
          print(mcmc_trace(fitmod, pars=paste("offsetIndivStdevs[", z1, ",", z2, "]", sep=""), n_warmup = samps/2 ))
        } # for z2
    } # for z1


    for ( z1 in 1:9 ){
      for ( z2 in 1:9 ){
        print(mcmc_trace(fitmod, pars=paste("L_U[", z1, ",", z2, "]", sep=""), n_warmup = samps/2 ))
      } # for z2
    } # for z1


    for ( z1 in 1:E ){
        for ( z2 in 1:9 ){
          for ( z3 in 1:9 ){
            print(mcmc_trace(fitmod, pars=paste("L_R[", z1, ",", z2, ",", z3, "]", sep=""), n_warmup = samps/2 ))
          } # for z3
        } # for z2
    } # for z1

    # for ( z1 in 1:P ){
    #     for ( z2 in 1:9 ){
    #       print(mcmc_trace(fitmod, pars=paste("off_Ind[", z1, z2, "]", sep=""), n_warmup = samps/2 ))
    #     } # for z2
    # } # for z1

    print(mcmc_trace(fitmod, pars="sigma", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

graphics.off()

