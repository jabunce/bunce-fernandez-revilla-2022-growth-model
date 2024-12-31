############################# STAN analysis ##############################################################

#rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores()) # run chains in parrallel on each core


model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models


####### m4 5-composite model, Berkeley

dat <- Ber.mal
#dat <- Ber.fem[which(Ber.fem$ID < 21 & Ber.fem$TotAge > 7),]

# data list for fitting Berkeley
data_list4 <- list(
  N = nrow(dat),                       # number of observations
  age = dat$TotAge,                    # vector of ages for each observation (in years since conception)
  height = dat$Height,                 # vector of observations (in cm)

  # means and stdevs from PriorPredict5
  meanLQ = c(muLQ1,muLQ2,muLQ3,muLQ4,muLQ5),
  meanLK = c(muLK1,muLK2,muLK3,muLK4,muLK5),
  meanR = c(muR1,muR2,muR3,muR4,muR5),
  meanLI = c(muLI2,muLI3,muLI4,muLI5,muLIM), #max(Ber.fem$TotAge)), # muI1 == 0

  msdLQ = c(sdLQ1,sdLQ2,sdLQ3,sdLQ4,sdLQ5),
  msdLK = c(sdLK1,sdLK2,sdLK3,sdLK4,sdLK5),
  msdR = c(sdR1,sdR2,sdR3,sdR4,sdR5),
  msdLI = c(sdLI2,sdLI3,sdLI4,sdLI5,sdLIM),

  muLsigma = muLsigma

)

# starting values
start_list4 <- list(
  Lsigma = muLsigma, # scale for the lognormal likelihood

  muLQ = c(muLQ1,muLQ2,muLQ3,muLQ4,muLQ5),
  muLK = c(muLK1,muLK2,muLK3,muLK4,muLK5),
  muR = c(muR1,muR2,muR3,muR4,muR5),
  muLI = c(muLI2,muLI3,muLI4,muLI5,muLIM), #max(Ber.fem$TotAge))

  sdLQ = c(sdLQ1,sdLQ2,sdLQ3,sdLQ4,sdLQ5),
  sdLK = c(sdLK1,sdLK2,sdLK3,sdLK4,sdLK5),
  sdR = c(sdR1,sdR2,sdR3,sdR4,sdR5),
  sdLI = c(sdLI2,sdLI3,sdLI4,sdLI5,sdLIM)
 
)



model <- cmdstan_model(model_file[1])


m4 <- model$sample(
        seed=1,
        data=data_list4, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list4), num_chains),
        max_treedepth=12, #default treedepth is 10
        adapt_delta=0.99
      )

m4$save_object(file = "m4_fit.RDS") #save stan output so you don't have re-run model
#m4 <- readRDS("m4_fit.RDS")

# look at posterior estimates
#m4$summary(c("muQ","muK","muH","muI","sigma","lp__"))
#m4$print(max_rows=200, c("muQ","muK","muH","muI","sigma","lp__"))
print(n=100, m4$summary(c("muQ",
                          "muK",
                          "muH",
                          "muI",

                          "Lsigma",
                          "lp__")))

post4 <- m4$draws(format = "df", , inc_warmup = FALSE)
#str(post4)
saveRDS(post4, "post4.RDS")
#post4 <- readRDS("post4.RDS")

fitmod <- m4$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

color_scheme_set("mix-blue-red")

#look at all traces, in Plots folder
pdf(file="./Plots/traces_m4.pdf",
  height=3, width=8)
par(mfrow=c(2,1))
    for ( z in 1:5 ){
        print(mcmc_trace(fitmod, pars=paste("muQ[", z, "]", sep=""), n_warmup = samps/2 ))
    } # for z 

    for ( z in 1:5 ){
        print(mcmc_trace(fitmod, pars=paste("muK[", z, "]", sep=""), n_warmup = samps/2 ))
    } # for z 

    for ( z in 1:5 ){
        print(mcmc_trace(fitmod, pars=paste("muH[", z, "]", sep=""), n_warmup = samps/2 ))
    } # for z 

    for ( z in 1:5 ){
        print(mcmc_trace(fitmod, pars=paste("muI[", z, "]", sep=""), n_warmup = samps/2 ))
    } # for z 



    # for ( z in 1:5 ){
    #     print(mcmc_trace(fitmod, pars=paste("sdLQ[", z, "]", sep=""), n_warmup = samps/2 ))
    # } # for z 

    # for ( z in 1:5 ){
    #     print(mcmc_trace(fitmod, pars=paste("sdLK[", z, "]", sep=""), n_warmup = samps/2 ))
    # } # for z 

    # for ( z in 1:5 ){
    #     print(mcmc_trace(fitmod, pars=paste("sdR[", z, "]", sep=""), n_warmup = samps/2 ))
    # } # for z 

    # for ( z in 1:5 ){
    #     print(mcmc_trace(fitmod, pars=paste("sdLI[", z, "]", sep=""), n_warmup = samps/2 ))
    # } # for z 

    print(mcmc_trace(fitmod, pars="Lsigma", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

graphics.off()

