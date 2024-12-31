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
data_list6 <- list(
  N = nrow(Com.mal),                      # number of observations
  J = length(unique(Com.mal$ID)),         # number of people measured
  E = length(unique(Com.mal$Ethnicity)),  # number of ethnic groups
  ID = Com.mal$ID,                        # vector of individual IDs for each height observation
  age = Com.mal$TotAge,                   # vector of ages for each observation (in years since conception)
  height = Com.mal$Height,                # vector of observations (in cm)
  ethnicity = Com.mal$Ethnicity,          # vector of ethnicity for each observation (1=Ber, 2=Mat)
  ethIndiv = EthID[,2],                   # vector of ethnicity for each person (1=Ber, 2=Mat)

  # means and stdevs from PriorPredictGroupStdev5
  meansL = c(muLQ1,muLQ2,muLQ3,muLQ4,muLQ5,
             muLK1,muLK2,muLK3,muLK4,muLK5,
             muR1, muR2, muR3, muR4, muR5,
             muLI2,muLI3,muLI4,muLI5,muLIM),

  gexpL = c(gexpLQ1,gexpLQ2,gexpLQ3,gexpLQ4,gexpLQ5,
            gexpLK1,gexpLK2,gexpLK3,gexpLK4,gexpLK5,
            gexpR1, gexpR2, gexpR3, gexpR4, gexpR5,
            gexpLI2,gexpLI3,gexpLI4,gexpLI5,gexpLIM),

  iexpL = c(iexpLQ1,iexpLQ2,iexpLQ3,iexpLQ4,iexpLQ5,
            iexpLK1,iexpLK2,iexpLK3,iexpLK4,iexpLK5,
            iexpR1, iexpR2, iexpR3, iexpR4, iexpR5,
            iexpLI2,iexpLI3,iexpLI4,iexpLI5,iexpLIM),

  ihsdL = c(ihsdLQ1,ihsdLQ2,ihsdLQ3,ihsdLQ4,ihsdLQ5,
            ihsdLK1,ihsdLK2,ihsdLK3,ihsdLK4,ihsdLK5,
            ihsdR1, ihsdR2, ihsdR3, ihsdR4, ihsdR5,
            ihsdLI2,ihsdLI3,ihsdLI4,ihsdLI5,ihsdLIM),

  CholEtaI = CholEtaI,
  CholEtaG = CholEtaG,

  muLsigma = muLsigma

)

# starting values for fitting composite growth curve function
start_list6 <- list(
  zGrp = matrix(0, nrow=E, ncol=20),               # ethnicity offset z-scores to each parameter: E-array of column 20-vectors, one vector for each ethnicity
  offsetGroupStdevs = c(gexpLQ1,gexpLQ2,gexpLQ3,gexpLQ4,gexpLQ5,
                        gexpLK1,gexpLK2,gexpLK3,gexpLK4,gexpLK5,
                        gexpR1,gexpR2,gexpR3,gexpR4,gexpR5,
                        gexpLI2,gexpLI3,gexpLI4,gexpLI5,gexpLIM),   # stdevs for mean (across indivs) offsets to the overall mean (across groups) trajectory for each group, for each of the 20 parameters, on log scale
  L_G = array(data=0, dim=c(20,20)),           # cholesky factor 20x20 matrix, for correlation matrix for mean offsets

  zInd = matrix(0, nrow=J, ncol=20),          # individual offset z-scores to each parameter: J-array of column 20-vectors, one vector for each individual
  offsetIndivStdevs = array( c( c(iexpLQ1,iexpLQ2,iexpLQ3,iexpLQ4,iexpLQ5,
                                  iexpLK1,iexpLK2,iexpLK3,iexpLK4,iexpLK5,
                                  iexpR1,iexpR2,iexpR3,iexpR4,iexpR5,
                                  iexpLI2,iexpLI3,iexpLI4,iexpLI5,iexpLIM),
                                c(iexpLQ1,iexpLQ2,iexpLQ3,iexpLQ4,iexpLQ5,
                                  iexpLK1,iexpLK2,iexpLK3,iexpLK4,iexpLK5,
                                  iexpR1,iexpR2,iexpR3,iexpR4,iexpR5,
                                  iexpLI2,iexpLI3,iexpLI4,iexpLI5,iexpLIM)
                              ), dim=c(2,20)),   # stdevs for mean (across individuals) offsets to the mean group trajectory for each individual, for each of the 20 parameters
  L_I = array(data=0, dim=c(E,20,20)),           # cholesky factor 20x20 matrix, for correlation matrix for mean offsets

  Lsigma = muLsigma # scale for the lognormal likelihood
)


############################### Run Stan models

####### m3 composite model using Berkeley and Matsigenka data, random effects for groups and covarying random effects for individuals

model <- cmdstan_model(model_file[2])

m6 <- model$sample(
        seed=1,
        data=data_list6, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list6), num_chains),
        max_treedepth=15, #default treedepth is 10
        adapt_delta=0.99
      )

m6$save_object(file = "m6_fit.RDS") #save stan output so you don't have re-run model
#m6 <- readRDS("m6_fit.RDS")


print(n=300, as_tibble(
      m6$summary(c("mean", "sd", "rhat", "ess_bulk"),
                 variables=c(
                              "lp__",
                              "Lsigma",
                              "mQ",
                              "mK",
                              "mH",
                              "mI",
                              "GrpOffset",
                              "offsetGroupStdevs",
                              "offsetIndivStdevs"))
))

post6 <- m6$draws(format = "df", , inc_warmup = FALSE)
#str(post6)
saveRDS(post6, "post6.RDS")
#post6 <- readRDS("post6.RDS")
#post6$"GrpOffset[1]"

fitmod <- m6$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
#str(fitmod)

fitmodb <- m6$draws(format = "draws_list", inc_warmup = TRUE) # array format keeps chains separate for plotting
#str(fitmodb)

color_scheme_set("mix-blue-red")


#look at all traces, in Plots folder
pdf(file="./Plots/traces_m6.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

    print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="Lsigma", n_warmup = samps/2))


    for ( z in 1:20 ){
        print(mcmc_trace(fitmod, pars=paste("offsetGroupStdevs[", z, "]", sep=""), n_warmup = samps/2 ))
    } # for z


    for ( z1 in 1:E ){
        for ( z2 in 1:20 ){
          #nm <- paste("offsetIndivStdevs[", z1, ",", z2, "]", sep="")
          #for ( z3 in 1:num_chains ){
            #if ( !is.na( fitmodb[[z3]][[nm]][1] ) ) {
              print(mcmc_trace(fitmod, pars=paste("offsetIndivStdevs[", z1, ",", z2, "]", sep=""), n_warmup = samps/2 ))
            #} # if
          #} # for z3
        } # for z2
    } # for z1



    for ( z1 in 1:20 ){
      for ( z2 in 1:20 ){
        print(mcmc_trace(fitmod, pars=paste("L_G[", z1, ",", z2, "]", sep=""), n_warmup = samps/2 ))
      } # for z2
    } # for z1


    # for ( z1 in 1:E ){
    #     for ( z2 in 1:20 ){
    #       for ( z3 in 1:20 ){
    #         #nm <- paste("L_I[", z1, ",", z2, ",", z3, "]", sep="")
    #         #for ( z4 in 1:num_chains ){                             # plot for each variable for each chain (looks like duplicated plots)
    #          # if ( !is.na( fitmodb[[1]][[nm]][1] ) ) {              # indices are: [chain 1][variable name][first sample]
    #             print(mcmc_trace(fitmod, pars=paste("L_I[", z1, ",", z2, ",", z3, "]", sep=""), n_warmup = samps/2 ))
    #           #} # if
    #         #} # for z4
    #       } # for z3
    #     } # for z2
    # } # for z1




graphics.off()

