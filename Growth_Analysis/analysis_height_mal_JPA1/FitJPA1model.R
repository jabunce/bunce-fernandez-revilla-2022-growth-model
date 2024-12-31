############################# STAN analysis ##############################################################

#rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores()) # run chains in parrallel on each core


model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models

N <- nrow(Ber.mal)                      # number of height observations
E <- length(unique(Com.mal$Ethnicity))  # number of ethnic groups


# data list for fitting Berkeley
data_list7 <- list(
  N = nrow(Ber.mal[which(Ber.mal$TotAge > 0),]),  #nrow(Ber.mal),                    # number of height observations
  age = Ber.mal[which(Ber.mal$TotAge > 0),"TotAge"],      #Ber.mal$TotAge,                 # vector of ages for each observation (in years since conception)
  height = Ber.mal[which(Ber.mal$TotAge > 0),"Height"],   #Ber.mal$Height,              # vector of height observations (in cm)

  # means and stdevs from PriorPredictJPA
  meanA = muA,              
  meanC1 = muC1,
  meanC2 = muC2, 
  meanC3 = muC3,
  meanD1 = muD1,        
  meanD2 = muD2,          
  meanD3 = muD3,
  #meanE = muE,

  sdA = sdA,
  sdC1 = sdC1,
  sdC2 = sdC2,
  sdC3 = sdC3,
  sdD1 = sdD1,
  sdD2 = sdD2,
  sdD3 = sdD3,
  #sdE = muE,

  muLsigma = muLsigma
)



# starting values for fitting JPA-1 model
start_list7 <- list(
  Lsigma = muLsigma, # scale for the lognormal likelihood

  # from PriorPredictJPA1
  muA = muA,
  muC1 = muC1,
  muC2 = muC2, 
  muC3 = muC3,        
  muD1 = muD1,        
  muD2 = muD2,          
  muD3 = muD3
  #muE = muE      

)




####### m7 JPA-1 model, Berkeley

#samps <- 1000
#num_chains <- 2

model <- cmdstan_model(model_file[1])

m7 <- model$sample(
        seed=1,
        data=data_list7, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list7), num_chains),
        max_treedepth=10, #default treedepth is 10
        adapt_delta=0.99
      )

m7$save_object(file = "m7_fit.RDS") #save stan output so you don't have re-run model
#m7 <- readRDS("m7_fit.RDS")

# look at posterior estimates
m7$summary(c(
         "muA",
         "muC1",
         "muC2",
         "muC3",
         "muD1",
         "muD2",
         "muD3",
         #"muE",
         "Lsigma",
         "lp__"))

post7 <- m7$draws(format = "df", , inc_warmup = FALSE)
#str(post7)
saveRDS(post7, "post7.RDS")
#post7 <- readRDS("post7.RDS")

#post7$muA

# mean(post7$muA)
# mean(post7$muC1)
# mean(post7$muC2)
# mean(post7$muC3)
# mean(post7$muD1)
# mean(post7$muD2)
# mean(post7$muD3)
#mean(post7$muE)



fitmod <- m7$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

color_scheme_set("mix-blue-red")

#look at all traces, in Plots folder
pdf(file="./Plots/traces_m7.pdf",
  height=3, width=8)
par(mfrow=c(2,1))
    print(mcmc_trace(fitmod, pars="muA", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muC1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muC2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muC3", n_warmup = samps/2))    
    print(mcmc_trace(fitmod, pars="muD1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muD2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muD3", n_warmup = samps/2))
    #print(mcmc_trace(fitmod, pars="muE", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="Lsigma", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

graphics.off()



