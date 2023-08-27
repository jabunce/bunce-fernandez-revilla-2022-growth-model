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
data_list7 <- list(
  N = nrow(Com.mal),                      # number of observations
  J = length(unique(Com.mal$ID)),         # number of people measured
  E = length(unique(Com.mal$Ethnicity)),  # number of ethnic groups
  ID = Com.mal$ID,                        # vector of individual IDs for each height observation
  age = Com.mal$TotAge,                   # vector of ages for each observation (in years since conception)
  height = Com.mal$Height,                # vector of observations (in cm)
  ethnicity = Com.mal$Ethnicity,          # vector of ethnicity for each observation (1=Ber, 2=Mat)
  ethIndiv = EthID[,2],                   # vector of ethnicity for each person (1=Ber, 2=Mat)

  # means from PriorPredictGroupStdevJPA
  Ahat = Ahat, 
  D1hat = D1hat,         
  D2hat = D2hat, 
  D3hat = D3hat,       
  C1hat = C1hat, 
  C2hat = C2hat, 
  C3hat = C3hat, 
  

  # rate (=1/mean) for exponential prior on group-level offset stdev (from PriorPredictGroupStdevJPA)
  pgexpA = gexpA,
  pgexpD1 = gexpD1,        
  pgexpD2 = gexpD2,
  pgexpD3 = gexpD3,      
  pgexpC1 = gexpC1,
  pgexpC2 = gexpC2,
  pgexpC3 = gexpC3,      


  # mean for normal prior on indiv-level offset stdev (from PriorPredictGroupStdev)
  pimA = imA,
  pimD1 = imD1,      
  pimD2 = imD2,
  pimD3 = imD3,      
  pimC1 = imC1,
  pimC2 = imC2,
  pimC3 = imC3,      

  # stdev for normal prior on indiv-level offset stdev (from PriorPredictGroupStdev)
  pisA = isA,
  pisD1 = isD1,       
  pisD2 = isD2,
  pisD3 = isD3,       
  pisC1 = isC1, 
  pisC2 = isC2, 
  pisC3 = isC3,      
 

  # eta parameter for lkj_corr_cholesky
  pCholEtaU = CholEtaU,
  pCholEtaR = CholEtaR 
)

# starting values for fitting composite growth curve function based on Nierop et al. 2016
start_list7 <- list(
  sigma = 0.01,                                   # stdev for the logged normal likelihood

  OvAllOffset = rep( 0, times=7 ),                # mean offset to each parameter
  
  zGrp = matrix(0, nrow=E, ncol=7),               # ethnicity offset z-scores to each parameter: E-array of column 7-vectors, one vector for each ethnicity
  offsetGroupStdevs = rep( 1, times=7 ),          # mean offset stdevs
  L_U = array(data=0, dim=c(7,7)),                # cholesky factor 7x7 matrix, for correlation matrix for mean offsets

  zInd = matrix(0, nrow=J, ncol=7),               # individual offset z-scores to each parameter: J-array of column 7-vectors, one vector for each individual
  offsetIndivStdevs = array(data=1, dim=c(E,7)),  # offset stdevs: E-array of 7-vectors, one vector for each ethnicity
  L_R = array(data=0, dim=c(E,7,7))               # E-array of cholesky factor 7x7 matrices, for correlation matrix for offsets
)


############################### Run Stan models

####### m7 JPA1 model using Berkeley and Matsigenka data, random effects for groups and covarying random effects for individuals

#samps <- 1000
#num_chains <- 2

model <- cmdstan_model(model_file[7])

m7 <- model$sample(
        seed=1,
        data=data_list7, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list7), num_chains),
        max_treedepth=12, #default treedepth is 10
        adapt_delta=0.99
      )

m7$save_object(file = "m7_fit.RDS") #save stan output so you don't have re-run model
#m7 <- readRDS("m7_fit.rds")

# look at posterior estimates
m7$print(max_rows=100, c(
         "lp__",
         "offsetGroupStdevs",
         "offsetIndivStdevs",
         #"R_cov_mat",
         #"U_cov_mat",
         "GrpOffset",
         "IndOffset",
         "sigma"))

post7 <- m7$draws(format = "df", , inc_warmup = FALSE)
str(post7)
#post7$"GrpOffset[1]"

fitmod <- m7$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

fitmodb <- m7$draws(format = "draws_list", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmodb)

color_scheme_set("mix-blue-red")


#look at all traces, in Plots folder
pdf(file="./Plots/traces_m7.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

    for ( z in 1:7 ){
        print(mcmc_trace(fitmod, pars=paste("offsetGroupStdevs[", z, "]", sep=""), n_warmup = samps/2 ))
    } # for z

    for ( z1 in 1:7 ){
      for ( z2 in 1:7 ){
        print(mcmc_trace(fitmod, pars=paste("L_U[", z1, ",", z2, "]", sep=""), n_warmup = samps/2 ))
      } # for z2
    } # for z1


    for ( z1 in 1:E ){
        for ( z2 in 1:7 ){
          nm <- paste("offsetIndivStdevs[", z1, ",", z2, "]", sep="")
          for ( z3 in 1:num_chains ){
            if ( !is.na( fitmodb[[z3]][[nm]][1] ) ) {
              print(mcmc_trace(fitmod, pars=paste("offsetIndivStdevs[", z1, ",", z2, "]", sep=""), n_warmup = samps/2 ))
            } # if
          } # for z3
        } # for z2
    } # for z1


    for ( z1 in 1:E ){
        for ( z2 in 1:7 ){
          for ( z3 in 1:7 ){
            nm <- paste("L_R[", z1, ",", z2, ",", z3, "]", sep="")
            for ( z4 in 1:num_chains ){
              if ( !is.na( fitmodb[[1]][[nm]][1] ) ) {
                print(mcmc_trace(fitmod, pars=paste("L_R[", z1, ",", z2, ",", z3, "]", sep=""), n_warmup = samps/2 ))
              } # if
            } # for z4
          } # for z3
        } # for z2
    } # for z1


    print(mcmc_trace(fitmod, pars="sigma", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

graphics.off()

