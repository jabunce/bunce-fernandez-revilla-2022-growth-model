############################# STAN analysis ##############################################################

#rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores()) # run chains in parrallel on each core


model_file <- list.files(path="./Code/", pattern="*.stan", full.names=TRUE) #list of paths to Stan models

N <- nrow(Ber.mal)                      # number of height observations
E <- length(unique(Com.mal$Ethnicity))  # number of ethnic groups


# data list for fitting Berkeley
data_list5 <- list(
  N = nrow(Ber.mal),                    # number of height observations
  age = Ber.mal$TotAge,                 # vector of ages for each observation (in years since conception)
  height = Ber.mal$Height,              # vector of height observations (in cm)

  # means and stdevs from PriorPredictJPA
  meanA = muA,        
  meanD1 = muD1,        
  meanD2 = muD2,          
  meanD3 = muD3,      
  meanC1 = muC1,
  meanC2 = muC2, 
  meanC3 = muC3,

  sdA = sdA,
  sdD1 = sdD1,
  sdD2 = sdD2,
  sdD3 = sdD3,
  sdC1 = sdC1,
  sdC2 = sdC2,
  sdC3 = sdC3
)


# data list for fitting Matsigenka
data_list6 <- list(
  N = nrow(Mat.mal),                     # number of height observations
  age = Mat.mal$TotAge,                  # vector of ages for each observation (in years since conception)
  height = Mat.mal$Height,               # vector of height observations (in cm)

  # means and stdevs from PriorPredictJPA
  meanA = muA,        
  meanD1 = muD1,        
  meanD2 = muD2,          
  meanD3 = muD3,      
  meanC1 = muC1,
  meanC2 = muC2, 
  meanC3 = muC3,

  sdA = sdA,
  sdD1 = sdD1,
  sdD2 = sdD2,
  sdD3 = sdD3,
  sdC1 = sdC1,
  sdC2 = sdC2,
  sdC3 = sdC3
)


# starting values for fitting JPA-1 model
start_list5 <- list(
  sigma = 0.01, # scale for the logged normal likelihood

  # from PriorPredictJPA
  muA = muA,        
  muD1 = muD1,        
  muD2 = muD2,          
  muD3 = muD3,      
  muC1 = muC1,
  muC2 = muC2, 
  muC3 = muC3

# # used by m3, coefficients for mean ethnicity offsets
#   ethA = rep(0, times=E),        
#   ethD1 = rep(0, times=E),        
#   ethD2 = rep(0, times=E),          
#   ethD3 = rep(0, times=E),      
#   ethC1 = rep(0, times=E),
#   ethC2 = rep(0, times=E), 
#   ethC3 = rep(0, times=E)
)




####### m5 JPA1 model, Berkeley

#samps <- 1000
#num_chains <- 2

model <- cmdstan_model(model_file[5])

m5 <- model$sample(
        seed=1,
        data=data_list5, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list5), num_chains),
        max_treedepth=20, #default treedepth is 10
        adapt_delta=0.99
      )

m5$save_object(file = "m5_fit.RDS") #save stan output so you don't have re-run model
#m5 <- readRDS("m5_fit.RDS")

# look at posterior estimates
m5$summary(c(
         "muA",
         "muD1",
         "muD2",
         "muD3",
         "muC1",
         "muC2",
         "muC3",
         "sigma",
         "lp__"))

post5 <- m5$draws(format = "df", , inc_warmup = FALSE)
str(post5)
saveRDS(post5, "post5.RDS")
#post5 <- readRDS("post5.RDS")

#post5$muA

mean(post5$muA)
mean(post5$muD1)
mean(post5$muD2)
mean(post5$muD3)
mean(post5$muC1)
mean(post5$muC2)
mean(post5$muC3)



fitmod <- m5$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

color_scheme_set("mix-blue-red")

#look at all traces, in Plots folder
pdf(file="./Plots/traces_m5.pdf",
  height=3, width=8)
par(mfrow=c(2,1))
    print(mcmc_trace(fitmod, pars="muA", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muD1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muD2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muD3", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muC1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muC2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muC3", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="sigma", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

graphics.off()




####### m6 JPA1 model, Matsigenka

#samps <- 1000
#num_chains <- 2

model <- cmdstan_model(model_file[5])

m6 <- model$sample(
        seed=1,
        data=data_list6, 
        iter_warmup=samps/2,
        save_warmup = TRUE,
        iter_sampling=samps/2,
        chains=num_chains,
        init=rep(list(start_list5), num_chains),
        max_treedepth=20, #default treedepth is 10
        adapt_delta=0.99
      )

m6$save_object(file = "m6_fit.RDS") #save stan output so you don't have re-run model
#m6 <- readRDS("m6_fit.RDS")

# look at posterior estimates
m6$summary(c(
         "muA",
         "muD1",
         "muD2",
         "muD3",
         "muC1",
         "muC2",
         "muC3",
         "sigma",
         "lp__"))

post6 <- m6$draws(format = "df", , inc_warmup = FALSE)
str(post6)
saveRDS(post6, "post6.RDS")
#post6 <- readRDS("post6.RDS")
#post6$muA

mean(post6$muA)
mean(post6$muD1)
mean(post6$muD2)
mean(post6$muD3)
mean(post6$muC1)
mean(post6$muC2)
mean(post6$muC3)



fitmod <- m6$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
str(fitmod)

color_scheme_set("mix-blue-red")

#look at all traces, in Plots folder
pdf(file="./Plots/traces_m6.pdf",
  height=3, width=8)
par(mfrow=c(2,1))
    print(mcmc_trace(fitmod, pars="muA", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muD1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muD2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muD3", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muC1", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muC2", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="muC3", n_warmup = samps/2))
    print(mcmc_trace(fitmod, pars="sigma", n_warmup = samps/2))

    print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

graphics.off()






# ####### m7 JPA-1 model using data from Berkeley and Matsigenka


# N <- nrow(Com.mal)                      # number of height observations
# E <- length(unique(Com.mal$Ethnicity))  # number of ethnic groups

# # data list for fitting CDC, Tsimane, and Shuar LMS offsets to male CDC LMS
# data_list6 <- list(
#   N = nrow(Com.mal),                      # number of height observations
#   E = length(unique(Com.mal$Ethnicity)),  # number of ethnic groups
#   age = Com.mal$TotAge,                # vector of ages for each observation (in years since conception)
#   height = Com.mal$Height,       # vector of height observations (in cm)
#   ethnicity = Com.mal$Ethnicity,          # vector of ethnicity for each observation (1=CDC, 2=Tsimane, 3=Shuar)

#   # used by m3, from Jolicoeur et al. 1992 Table 2
#   muA = 175.3,        
#   muD1 = 0.155,        
#   muD2 = 8.358,          
#   muD3 = 13.55,      
#   muC1 = 0.458,
#   muC2 = 2.62, 
#   muC3 = 19.55,

#   # sd's from PriorPredictJPA
#   psdA = sdA,        
#   psdD1 = sdD1,        
#   psdD2 = sdD2,          
#   psdD3 = sdD3,      
#   psdC1 = sdC1,
#   psdC2 = sdC2, 
#   psdC3 = sdC3

#   #   # used by m3, means of posterior estimates for m1
#   # muA = mean(post1$muA),        
#   # muD1 = mean(post1$muD1),       
#   # muD2 = mean(post1$muD2),          
#   # muD3 = mean(post1$muD3),      
#   # muC1 = mean(post1$muC1),
#   # muC2 = mean(post1$muC2), 
#   # muC3 = mean(post1$muC3)


#   #   # used by m1, from Jolicoeur et al. 1988 Table 2
#   # muA = 175,        
#   # muD1 = 3.03,        
#   # muD2 = 9.98,          
#   # muD3 = 13.75,      
#   # muC1 = 0.59,
#   # muC2 = 3.63, 
#   # muC3 = 21.84
# )

# start_list7 <- list(
#   sigma = 0.01, # scale for the un-logged normal likelihood

#   ethA = rep(0, times=E),        
#   ethD1 = rep(0, times=E),        
#   ethD2 = rep(0, times=E),          
#   ethD3 = rep(0, times=E),      
#   ethC1 = rep(0, times=E),
#   ethC2 = rep(0, times=E), 
#   ethC3 = rep(0, times=E)
# )


# num_chains = 2
# samps = 1000

# model <- cmdstan_model(model_file[7])

# m3 <- model$sample(
#         seed=1,
#         data=data_list6, 
#         iter_warmup=samps/2,
#         save_warmup = TRUE,
#         iter_sampling=samps/2,
#         chains=num_chains,
#         init=rep(list(start_list7), num_chains),
#         max_treedepth=10, #default treedepth is 10
#         adapt_delta=0.99
#       )

# m3$save_object(file = "m7_fitb.RDS") #save stan output so you don't have re-run model
# #m3 <- readRDS("m3_fitb.rds")

# # look at posterior estimates
# m3$print(max_rows=100, c(
#          "ethA",
#          "ethD1",
#          "ethD2",
#          "ethD3",
#          "ethC1",
#          "ethC2",
#          "ethC3",

#          "berA",
#          "berD1",
#          "berD2",
#          "berD3",
#          "berC1",
#          "berC2",
#          "berC3",

#          "matA",
#          "matD1",
#          "matD2",
#          "matD3",
#          "matC1",
#          "matC2",
#          "matC3",

#          "sigma",
#          "lp__"))

# post7 <- m7$draws(format = "df", , inc_warmup = FALSE)
# str(post7)
# #post7$ethA

# fitmod <- m3$draws(format = "array", inc_warmup = TRUE) # array format keeps chains separate for plotting
# str(fitmod)

# color_scheme_set("mix-blue-red")

# #look at all traces, in Plots folder
# pdf(file="./Plots/traces_m7.pdf",
#   height=3, width=8)
# par(mfrow=c(2,1))
#     for ( z in 1:E ){
#         print(mcmc_trace(fitmod, pars=paste("ethA[", z, "]", sep=""), n_warmup = samps/2 ))
#     } # for z
#     for ( z in 1:E ){
#         print(mcmc_trace(fitmod, pars=paste("ethD1[", z, "]", sep=""), n_warmup = samps/2 ))
#     } # for z 
#     for ( z in 1:E ){
#         print(mcmc_trace(fitmod, pars=paste("ethD2[", z, "]", sep=""), n_warmup = samps/2 ))
#     } # for z 
#     for ( z in 1:E ){
#         print(mcmc_trace(fitmod, pars=paste("ethD3[", z, "]", sep=""), n_warmup = samps/2 ))
#     } # for z 
#     for ( z in 1:E ){
#         print(mcmc_trace(fitmod, pars=paste("ethC1[", z, "]", sep=""), n_warmup = samps/2 ))
#     } # for z 
#     for ( z in 1:E ){
#         print(mcmc_trace(fitmod, pars=paste("ethC2[", z, "]", sep=""), n_warmup = samps/2 ))
#     } # for z 
#     for ( z in 1:E ){
#         print(mcmc_trace(fitmod, pars=paste("ethC3[", z, "]", sep=""), n_warmup = samps/2 ))
#     } # for z 
#     # print(mcmc_trace(fitmod, pars="ethA", n_warmup = samps/2))
#     # print(mcmc_trace(fitmod, pars="ethD1", n_warmup = samps/2))
#     # print(mcmc_trace(fitmod, pars="ethD2", n_warmup = samps/2))
#     # print(mcmc_trace(fitmod, pars="ethD3", n_warmup = samps/2))
#     # print(mcmc_trace(fitmod, pars="ethC1", n_warmup = samps/2))
#     # print(mcmc_trace(fitmod, pars="ethC2", n_warmup = samps/2))
#     # print(mcmc_trace(fitmod, pars="ethC3", n_warmup = samps/2))
#     print(mcmc_trace(fitmod, pars="sigma", n_warmup = samps/2))

#     print(mcmc_trace(fitmod, pars="lp__", n_warmup = samps/2))

# graphics.off()



