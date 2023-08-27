############################# STAN analysis ##############################################################

rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())


#load models and data and starting conditions

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models


# data list for Berkeley 
data_list2 <- list(
  N = nrow(Ber.mal),                                      # number of observations
  J = length(unique(Ber.mal[,"ID"])),                     # number of people measured
  ID = Ber.mal[,"ID"] - ( min( Ber.mal[,"ID"] ) - 1 ),    # vector of individual IDs for each observation, starting at 1
  age = Ber.mal[,"TotAge"],                               # vector of ages for each observation (in years since conception)
  height = Ber.mal[,"Height"],                            # vector of height observations (in cm)
  #weight = Ber.mal[,"Weight"],                            # vector of weight observations (in kg)


  # means of posterior estimates from m1
  LQ1hat = LQ1hat,
  LQ2hat = LQ2hat,       
  LQ3hat = LQ3hat,
  LK1hat = LK1hat,      
  LK2hat = LK2hat,
  LK3hat = LK3hat,
  R1hat = R1hat,
  R2hat = R2hat,
  R3hat = R3hat, 

  # prior stdev for group-level mean offset (from PriorPredictIndivStdev)
  pgsdLQ1 = gsdLQ1,
  pgsdLQ2 = gsdLQ2,        
  pgsdLQ3 = gsdLQ3,
  pgsdLK1 = gsdLK1,     
  pgsdLK2 = gsdLK2,
  pgsdLK3 = gsdLK3,
  pgsdR1 = gsdR1,      
  pgsdR2 = gsdR2,
  pgsdR3 = gsdR3,

  # rate (=1/mean) for exponential prior on indiv-level offset (from PriorPredictIndivStdev)
  piexpLQ1 = iexpLQ1,
  piexpLQ2 = iexpLQ2,      
  piexpLQ3 = iexpLQ3,
  piexpLK1 = iexpLK1,      
  piexpLK2 = iexpLK2,
  piexpLK3 = iexpLK3,
  piexpR1 = iexpR1,      
  piexpR2 = iexpR2,
  piexpR3 = iexpR3,

  # eta parameter for lkj_corr_cholesky
  pCholEta = CholEta
)

N <- nrow(Ber.mal)                                      # number of weight observations
J <- length(unique(Ber.mal[,"ID"]))                     # number of people measured
  


# starting values for Berkeley
start_list2 <- list(
  sigma = 0.01,                           # stdev for the logged normal likelihood

  zInd = matrix(0, nrow=J, ncol=9),       # individual offset z-scores to each parameter: J-array of column 9-vectors, one vector for each individual
  muInd = rep( 0, times=9 ),              # mean offset to each parameter
  sigmaInd = rep( 1, times=9 ),           # mean offset stdevs
  L_U = array(data=0, dim=c(9,9))         # cholesky factor 9x9 matrix, for correlation matrix for mean offsets
)



############################### Run Stan models

####### m2 composite model using Berkeley dataset, covarying random effects for individuals

# samps <- 1000
# num_chains <- 2

model <- cmdstan_model(model_file[2])

m2 <- model$sample(
        seed=1,
        data=data_list2, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list2), num_chains),
        max_treedepth=15, #default treedepth is 10
        adapt_delta=0.99
      )

m2$save_object(file = "m2_fit.RDS") #save stan output so you don't have re-run model
#m2 <- readRDS("m2_fit.RDS")

# look at posterior estimates
m2$print(max_rows=200, c(
         "lp__",
         "muInd",
         "sigmaInd",
         "U_cov_mat",

         #"off_Ind",

         "sigma"))

post2 <- m2$draws(format = "df", , inc_warmup = FALSE)
str(post2)
saveRDS(post2, "post2.RDS")
#post2 <- readRDS("post2.RDS")
#post2$sigmaInd[1]

fitmod <- m2$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)


color_scheme_set("mix-blue-red")


#look at all traces, in Plots folder
pdf(file="./Plots/traces_m2.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

    for ( z in 1:9 ){
        print(mcmc_trace(fitmod, pars=paste("muInd[", z, "]", sep=""), iter1 = samps/2 )) #n_warmup = samps/2 ))
    } # for z

    for ( z in 1:9 ){
        print(mcmc_trace(fitmod, pars=paste("sigmaInd[", z, "]", sep=""), iter1 = samps/2 ))
    } # for z

    for ( z1 in 1:9 ){
      for ( z2 in 1:9 ){
        print(mcmc_trace(fitmod, pars=paste("L_U[", z1, ",", z2, "]", sep=""), iter1 = samps/2 ))
      } # for z2
    } # for z1

    print(mcmc_trace(fitmod, pars="sigma", iter1 = samps/2))

    print(mcmc_trace(fitmod, pars="lp__", iter1 = samps/2))

graphics.off()


