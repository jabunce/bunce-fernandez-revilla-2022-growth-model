############################# STAN analysis ##############################################################

#rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores()) # run chains in parrallel on each core


model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models


####### m4 5-composite model, Berkeley

# data list for fitting Berkeley
data_list4 <- list(
  N = nrow(Ber.mal),                       # number of observations
  age = Ber.mal$TotAge,                    # vector of ages for each observation (in years since conception)
  height = Ber.mal$Height,                 # vector of observations (in cm)

  # means and stdevs from PriorPredict5
  meanLQ1 = muLQ1,        
  meanLQ2 = muLQ2,
  meanLQ3 = muLQ3,
  meanLQ4 = muLQ4,
  meanLQ5 = muLQ5,
  meanLK1 = muLK1,
  meanLK2 = muLK2,
  meanLK3 = muLK3,
  meanLK4 = muLK4,
  meanLK5 = muLK5,
  meanR1 = muR1,
  meanR2 = muR2,
  meanR3 = muR3,
  meanR4 = muR4,
  meanR5 = muR5,

  sdLQ1 = sdLQ1,
  sdLQ2 = sdLQ2,
  sdLQ3 = sdLQ3,
  sdLQ4 = sdLQ4,
  sdLQ5 = sdLQ5,
  sdLK1 = sdLK1,
  sdLK2 = sdLK2,
  sdLK3 = sdLK3,
  sdLK4 = sdLK4,
  sdLK5 = sdLK5,
  sdR1 = sdR1,
  sdR2 = sdR2,
  sdR3 = sdR3,
  sdR4 = sdR4,
  sdR5 = sdR5
)

# starting values
start_list4 <- list(
  sigma = 0.01, # scale for the un-logged normal likelihood

  muLQ1 = muLQ1, #-1.83
  muLQ2 = muLQ2, #-1.98        
  muLQ3 = muLQ3, #-11.47
  muLQ4 = muLQ4, #-1.98        
  muLQ5 = muLQ5, #-11.47
  muLK1 = muLK1, #2.47      
  muLK2 = muLK2, #0.78
  muLK3 = muLK3, #11.30
  muLK4 = muLK4, #0.78
  muLK5 = muLK5, #11.30
  muR1 = muR1, #6.13      
  muR2 = muR2, #0.88
  muR3 = muR3, #1.26
  muR4 = muR4, #0.88
  muR5 = muR5 #1.26
)


# samps <- 1000
# num_chains <- 2


model <- cmdstan_model(model_file[4])


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
m4$summary(c(
         "muLQ1",
         "muLQ2",
         "muLQ3",
         "muLQ4",
         "muLQ5",
         "muLK1",
         "muLK2",
         "muLK3",
         "muLK4",
         "muLK5",
         "muR1",
         "muR2",
         "muR3",
         "muR4",
         "muR5",
         "sigma",
         "lp__"))

post4 <- m4$draws(format = "df", , inc_warmup = FALSE)
str(post4)
saveRDS(post4, "post4.RDS")


fitmod <- m4$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

color_scheme_set("mix-blue-red")

#look at all traces, in Plots folder
pdf(file="./Plots/traces_m4.pdf",
  height=3, width=8)
par(mfrow=c(2,1))
    # for ( z in 1:3 ){
    #     print(mcmc_trace(fitmod, pars=paste("muLQ[", z, "]", sep=""), n_warmup = samps/2 ))
    # } # for z 
    print(mcmc_trace(fitmod, pars="muLQ1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLQ2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLQ3", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLQ4", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLQ5", n_warmup = samps/2))
    # for ( z in 1:3 ){
    #     print(mcmc_trace(fitmod, pars=paste("muLK[", z, "]", sep=""), n_warmup = samps/2 ))
    # } # for z 
    print(mcmc_trace(fitmod, pars="muLK1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLK2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLK3", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLK4", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLK5", n_warmup = samps/2))
    # for ( z in 1:3 ){
    #     print(mcmc_trace(fitmod, pars=paste("muR[", z, "]", sep=""), n_warmup = samps/2 ))
    # } # for z 
    print(mcmc_trace(fitmod, pars="muR1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muR2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muR3", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muR4", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muR5", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="sigma", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

graphics.off()

