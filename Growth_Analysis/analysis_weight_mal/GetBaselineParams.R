############################# STAN analysis ##############################################################

#rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores()) # run chains in parrallel on each core



#load models and data and starting conditions

model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models


# data list for fitting Berkeley
data_list1 <- list(
  N = nrow(Ber.mal),                  # number of observations
  age = Ber.mal$TotAge,               # vector of ages for each observation (in years since conception)
  weight = Ber.mal$Weight,            # vector of observations (in kg)

  # means and stdevs from PriorPredictInitial
  meanLQ1 = muLQ1,        
  meanLQ2 = muLQ2,
  meanLQ3 = muLQ3,
  meanLK1 = muLK1,
  meanLK2 = muLK2,
  meanLK3 = muLK3,
  meanR1 = muR1,
  meanR2 = muR2,
  meanR3 = muR3,

  sdLQ1 = sdLQ1,
  sdLQ2 = sdLQ2,
  sdLQ3 = sdLQ3,
  sdLK1 = sdLK1,
  sdLK2 = sdLK2,
  sdLK3 = sdLK3,
  sdR1 = sdR1,
  sdR2 = sdR2,
  sdR3 = sdR3
)



# starting values
start_list1 <- list(
  sigma = 0.01, # scale for the logged normal likelihood

  muLQ1 = muLQ1,
  muLQ2 = muLQ2,        
  muLQ3 = muLQ3,
  muLK1 = muLK1,      
  muLK2 = muLK2,
  muLK3 = muLK3,
  muR1 = muR1,      
  muR2 = muR2,
  muR3 = muR3
)




####### m1 composite model, Berkeley

# samps <- 1000
# num_chains <- 2


model <- cmdstan_model(model_file[1])


m1 <- model$sample(
        seed=1,
        data=data_list1, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list1), num_chains),
        max_treedepth=12, #default treedepth is 10
        adapt_delta=0.99
      )

m1$save_object(file = "m1_fit.RDS") #save stan output so you don't have re-run model
#m1 <- readRDS("m1_fit.RDS")

# look at posterior estimates
m1$summary(c(
         "muLQ1",
         "muLQ2",
         "muLQ3",
         "muLK1",
         "muLK2",
         "muLK3",
         "muR1",
         "muR2",
         "muR3",
         "sigma",
         "lp__"))

post1 <- m1$draws(format = "df", , inc_warmup = FALSE)
saveRDS(post1, "post1.RDS")
#post1 <- readRDS("post1.RDS")
str(post1)
#post1$muLQ1


# mean(post1$muLQ1)
# mean(post1$muLQ2)
# mean(post1$muLQ3)
# mean(post1$muLK1)
# mean(post1$muLK2)
# mean(post1$muLK3)
# mean(post1$muR1)
# mean(post1$muR2)
# mean(post1$muR3)


fitmod <- m1$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

color_scheme_set("mix-blue-red")

#look at all traces, in Plots folder
pdf(file="./Plots/traces_m1.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

    print(mcmc_trace(fitmod, pars="muLQ1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLQ2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLQ3", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="muLK1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLK2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muLK3", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="muR1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muR2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muR3", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="sigma", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

graphics.off()



