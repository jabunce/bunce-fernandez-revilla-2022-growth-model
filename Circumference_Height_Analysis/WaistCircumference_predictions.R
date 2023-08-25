
#Read the data from the csv data file into R:

# waist circumference per height
LMSboys.wh.raw <- read.csv(file="./Data/Sharma2015_LMS_waist_height_boys.csv", header=TRUE)
LMSgirls.wh.raw <- read.csv(file= "./Data/Sharma2015_LMS_waist_height_girls.csv", header = TRUE)

# waist circumference
LMSboys.wc.raw <- read.csv(file="./Data/Sharma2015_LMS_waist_boys.csv", header=TRUE)
LMSgirls.wc.raw <- read.csv(file= "./Data/Sharma2015_LMS_waist_girls.csv", header = TRUE)
meanDutch.wc.raw <- read.csv(file= "./Data/Fredriks2005_waist_circumf_data.csv", header = TRUE)

# CDC height
LMS.ht.raw <- read.csv(file="./Data/statage.csv", header=TRUE)


#Check the variable names and dimensions in the data frame
names(LMSboys.wh.raw)
names(LMSgirls.wh.raw)
dim(LMSboys.wh.raw)
dim(LMSgirls.wh.raw)

names(LMSboys.wc.raw)
names(LMSgirls.wc.raw)
dim(LMSboys.wc.raw)
dim(LMSgirls.wc.raw)

names(LMS.ht.raw)
dim(LMS.ht.raw)

names(meanDutch.wc.raw)
dim(meanDutch.wc.raw)


# delete extra imported columns
LMSboys.wc.raw <- LMSboys.wc.raw[,1:(ncol(LMSboys.wc.raw)-1)]
names(LMSboys.wc.raw)
dim(LMSboys.wc.raw)

meanDutch.wc.raw <- meanDutch.wc.raw[,1:(ncol(meanDutch.wc.raw)-1)]
names(meanDutch.wc.raw)
dim(meanDutch.wc.raw)



######### create dataset with median height and median waist circumference per year

# height: calculate LMS values at time points in-between given time points 
LMSboys.ht.raw <- LMS.ht.raw[which(LMS.ht.raw$Sex==0),]
LMSboys.ht.mod <- c(LMSboys.ht.raw$Agemos[1],LMSboys.ht.raw$M[1])
for ( x in 2:(nrow(LMSboys.ht.raw)-1) ) {

      LMSboys.ht.mod <- rbind( LMSboys.ht.mod, c( LMSboys.ht.raw$Agemos[x+1]-0.5, mean(c(LMSboys.ht.raw$M[x],LMSboys.ht.raw$M[x+1])) ) )
}
LMSboys.ht.mod[nrow(LMSboys.ht.mod),] <- c(LMSboys.ht.raw$Agemos[nrow(LMSboys.ht.raw)], LMSboys.ht.raw$M[nrow(LMSboys.ht.raw)])
colnames(LMSboys.ht.mod) <- c("months","height")
LMSboys.ht.mod <- as.data.frame(LMSboys.ht.mod)
LMSboys.ht.mod$sex <- 0
LMSboys.ht.mod$years <- LMSboys.ht.mod$months/12


LMSgirls.ht.raw <- LMS.ht.raw[which(LMS.ht.raw$Sex==1),]
LMSgirls.ht.mod <- c(LMSgirls.ht.raw$Agemos[1],LMSgirls.ht.raw$M[1])
for ( x in 2:(nrow(LMSgirls.ht.raw)-1) ) {

      LMSgirls.ht.mod <- rbind( LMSgirls.ht.mod, c( LMSgirls.ht.raw$Agemos[x+1]-0.5, mean(c(LMSgirls.ht.raw$M[x],LMSgirls.ht.raw$M[x+1])) ) )
}
LMSgirls.ht.mod[nrow(LMSgirls.ht.mod),] <- c(LMSgirls.ht.raw$Agemos[nrow(LMSgirls.ht.raw)], LMSgirls.ht.raw$M[nrow(LMSgirls.ht.raw)])
colnames(LMSgirls.ht.mod) <- c("months","height")
LMSgirls.ht.mod <- as.data.frame(LMSgirls.ht.mod)
LMSgirls.ht.mod$sex <- 1
LMSgirls.ht.mod$years <- LMSgirls.ht.mod$months/12

LMS.ht.mod <- rbind(LMSboys.ht.mod, LMSgirls.ht.mod)


# waist circumference: cut out time points for which height not measured
LMSboys.wc.mod <- cbind( LMSboys.wc.raw[,c("month","M")], LMSboys.wh.raw[,"M"] ) # combine waist circumference with waist circumference/height
names(LMSboys.wc.mod)[c(ncol(LMSboys.wc.mod)-1,ncol(LMSboys.wc.mod))] <- c("waistcircum","wch")
LMSboys.wc.mod$years <- LMSboys.wc.mod$month/12
LMSboys.wc.mod$sex <- 0
LMSboys.wc.cut <- LMSboys.wc.mod[which(LMSboys.wc.mod$years %in% LMSboys.ht.mod$years),]

LMSgirls.wc.mod <- cbind( LMSgirls.wc.raw[,c("month","M")], LMSgirls.wh.raw[,"M"] ) # combine waist circumference with waist circumference/height
names(LMSgirls.wc.mod)[c(ncol(LMSgirls.wc.mod)-1,ncol(LMSgirls.wc.mod))] <- c("waistcircum","wch")
LMSgirls.wc.mod$years <- LMSgirls.wc.mod$month/12
LMSgirls.wc.mod$sex <- 1
LMSgirls.wc.cut <- LMSgirls.wc.mod[which(LMSgirls.wc.mod$years %in% LMSgirls.ht.mod$years),]

LMS.wc.cut <- rbind(LMSboys.wc.cut, LMSgirls.wc.cut)

# combine: cut out height time points for which head circumference not measured
LMSboys.ht.cut <- LMSboys.ht.mod[which(LMSboys.ht.mod$years %in% LMSboys.wc.cut$years),c("sex","years","height")]
LMSgirls.ht.cut <- LMSgirls.ht.mod[which(LMSgirls.ht.mod$years %in% LMSgirls.wc.cut$years),c("sex","years","height")]
LMS.ht.cut <- rbind(LMSboys.ht.cut, LMSgirls.ht.cut)

LMS.ht.wc <- cbind(LMS.ht.cut,LMS.wc.cut[,c("waistcircum","wch")])



###################### simulate data for waist circumference from LMS

SampAge <- 10 # number of simulated people per age

# boys

#first age
simWaistcirc_boys <- cbind( rep(LMSboys.wc.cut[1, "years"], times=SampAge),
                            sample( size=SampAge,
                                    x=rnorm(SampAge, mean=LMSboys.wc.cut[1,"waistcircum"], sd=0.01),
                                    replace=TRUE
                                   ),
                            rep(LMSboys.ht.cut[1, "height"], times=SampAge)
                           )

# the rest of the ages
for ( t in 2:nrow(LMSboys.wc.cut) ) {
    simWaistcirc_boys <- rbind( simWaistcirc_boys, cbind( rep(LMSboys.wc.cut[t, "years"], times=SampAge),
                                                          sample( size=SampAge,
                                                                  x=rnorm(SampAge, mean=LMSboys.wc.cut[t,"waistcircum"], sd=0.01),
                                                                  replace=TRUE
                                                                 ),
                                                          rep(LMSboys.ht.cut[t, "height"], times=SampAge)
                                                         )
                              )
} #for t

simWaistcirc_boys <- as.data.frame(simWaistcirc_boys)
names(simWaistcirc_boys) <- c("years","waistcirc","height")

#plot(x=simWaistcirc_boys$years, y=simWaistcirc_boys$waistcirc)


# girls

#first age
simWaistcirc_girls <- cbind( rep(LMSgirls.wc.cut[1, "years"], times=SampAge),
                             sample( size=SampAge,
                                     x=rnorm(SampAge, mean=LMSgirls.wc.cut[1,"waistcircum"], sd=0.01),
                                     replace=TRUE
                                    ),
                             rep(LMSgirls.ht.cut[1, "height"], times=SampAge)
                            )

# the rest of the ages
for ( t in 2:nrow(LMSgirls.wc.cut) ) {
    simWaistcirc_girls <- rbind( simWaistcirc_girls, cbind( rep(LMSgirls.wc.cut[t, "years"], times=SampAge),
                                                            sample( size=SampAge,
                                                                    x=rnorm(SampAge, mean=LMSgirls.wc.cut[t,"waistcircum"], sd=0.01),
                                                                    replace=TRUE
                                                                   ),
                                                            rep(LMSgirls.ht.cut[t, "height"], times=SampAge)
                                                           )
                                )
} #for t

simWaistcirc_girls <- as.data.frame(simWaistcirc_girls)
names(simWaistcirc_girls) <- c("years","waistcirc","height")

#plot(x=simWaistcirc_girls$years, y=simWaistcirc_girls$waistcirc)



###################### Stan fitting
rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())

samps <- 4000     # number of mcmc samples
num_chains <- 4   # number of mcmc chains

age_cut_boys <- 16   # age at which predict that waist circum should level off to correspond with height (which it doesn't)
age_cut_girls <- 14

data_list_boys_w <- list(
  N = nrow(simWaistcirc_boys[which(simWaistcirc_boys$years < age_cut_boys),]),                     # number of people (=number of waist circumference measurements)
  hh = simWaistcirc_boys[which(simWaistcirc_boys$years < age_cut_boys),"height"],                   # vector of height for each person
  y = simWaistcirc_boys[which(simWaistcirc_boys$years < age_cut_boys),"waistcirc"],                 # vector of waist circumference for each person
  m = mean(simWaistcirc_boys[which(simWaistcirc_boys$years < age_cut_boys),"waistcirc"])/(2*pi)     # mean waist radius across ages
)

data_list_girls_w <- list(
  N = nrow(simWaistcirc_girls[which(simWaistcirc_girls$years < age_cut_girls),]),                     # number of people (=number of waist circumference measurements)
  hh = simWaistcirc_girls[which(simWaistcirc_girls$years < age_cut_girls),"height"],                   # vector of height for each person
  y = simWaistcirc_girls[which(simWaistcirc_girls$years < age_cut_girls),"waistcirc"],                 # vector of waist circumference for each person
  m = mean(simWaistcirc_girls[which(simWaistcirc_girls$years < age_cut_girls),"waistcirc"])/(2*pi)     # mean waist radius across ages
)

start_list_w <- list(
  c = mean(c(simWaistcirc_boys[which(simWaistcirc_boys$years < age_cut_boys),"waistcirc"],
             simWaistcirc_girls[which(simWaistcirc_girls$years < age_cut_girls),"waistcirc"]))/(2*pi), # constant = r
  p = 1/14,                                                                     # factor of height
  q = 1/2,                                                                      # root of height
  sigma = 1
)


######## r=constant

model_constant_r <- '
data {
  int<lower=1> N;         // number of people=measurements
  vector<lower=0>[N] y;   // observed head circumference for person n
  real<lower=0> m;        // mean observed waist radius
}

parameters {
  real c;                 // constant=r
  real<lower=0> sigma;    // scale of the likelihood
}

model {
  real r;
  real circum;
  
  c ~ normal(m,10);        // centered around mean head radius across age
  sigma ~ exponential(1);  //exponential(beta), where here beta = lambda = 1/mean

  r = c;
  circum = 2*pi()*r;

  y ~ normal(circum,sigma);
}

generated quantities {
  real r;
  real circum;

  r = c;
  circum = 2*pi()*r;
}
'

# fit to boys
m_const_r_boys_w <- stan( model_code=model_constant_r,
                        data=data_list_boys_w,
                        iter=samps,
                        chains=num_chains, 
                        init=rep(list(start_list_w), num_chains),
                        control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                       )

saveRDS(m_const_r_boys_w, "m_const_r_boys_w_fit.rds")

post_const_r_boys_w <- extract.samples( m_const_r_boys_w ) 

print(m_const_r_boys_w, pars=c("c","sigma","lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_const_r_boys_w

#look at all traces
pdf(file="./Plots/traces_m_const_r_boys_w.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

print(traceplot(mod, pars="c", inc_warmup=F))
print(traceplot(mod, pars="sigma", inc_warmup=F))
print(traceplot(mod, pars="lp__", inc_warmup=F))

graphics.off()


# fit to girls
m_const_r_girls_w <- stan( model_code=model_constant_r,
                         data=data_list_girls_w,
                         iter=samps,
                         chains=num_chains, 
                         init=rep(list(start_list_w), num_chains),
                         control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                        )

saveRDS(m_const_r_girls_w, "m_const_r_girls_w_fit.rds")

post_const_r_girls_w <- extract.samples( m_const_r_girls_w ) 

print(m_const_r_girls_w, pars=c("c","sigma","lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_const_r_girls_w

#look at all traces
pdf(file="./Plots/traces_m_const_r_girls_w.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

print(traceplot(mod, pars="c", inc_warmup=F))
print(traceplot(mod, pars="sigma", inc_warmup=F))
print(traceplot(mod, pars="lp__", inc_warmup=F))

graphics.off()






######## r=constant*h

model_prop_r <- '
data {
  int<lower=1> N;           // number of people=measurements
  vector<lower=0>[N] hh;    // height for each person n
  vector<lower=0>[N] y;     // observed waist circumference for person n
}

parameters {
  real<lower=0> p;        // constant factor of height (constrained positive)
  real<lower=0> sigma;    // scale of the likelihood
}

model {
  vector[N] r;
  vector[N] circum;
  
  p ~ normal(1,10);
  sigma ~ exponential(1);  // exponential(beta), where here beta = lambda = 1/mean

  for (n in 1:N) {

    r[n] = p*hh[n];
    circum[n] = 2*pi()*r[n];

  } // for

  y ~ normal(circum,sigma);
}

generated quantities {
  vector[N] r;
  vector[N] circum;

  for (n in 1:N) {

    r[n] = p*hh[n];
    circum[n] = 2*pi()*r[n];

  } // for
}
'

# fit to boys
m_prop_r_boys_w <- stan( model_code=model_prop_r,
                        data=data_list_boys_w,
                        iter=samps,
                        chains=num_chains, 
                        init=rep(list(start_list_w), num_chains),
                        control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                       )

saveRDS(m_prop_r_boys_w, "m_prop_r_boys_w_fit.rds")

post_prop_r_boys_w <- extract.samples( m_prop_r_boys_w ) 

print(m_prop_r_boys_w, pars=c("p","sigma","lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_prop_r_boys_w

#look at all traces
pdf(file="./Plots/traces_m_prop_r_boys_w.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

print(traceplot(mod, pars="p", inc_warmup=F))
print(traceplot(mod, pars="sigma", inc_warmup=F))
print(traceplot(mod, pars="lp__", inc_warmup=F))

graphics.off()




# fit to girls
m_prop_r_girls_w <- stan( model_code=model_prop_r,
                         data=data_list_girls_w,
                         iter=samps,
                         chains=num_chains, 
                         init=rep(list(start_list_w), num_chains),
                         control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                        )

saveRDS(m_prop_r_girls_w, "m_prop_r_girls_w_fit.rds")

post_prop_r_girls_w <- extract.samples( m_prop_r_girls_w ) 

print(m_prop_r_girls_w, pars=c("p","sigma","lp__"),   
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_prop_r_girls_w

#look at all traces
pdf(file="./Plots/traces_m_prop_r_girls_w.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

print(traceplot(mod, pars="p", inc_warmup=F))
print(traceplot(mod, pars="sigma", inc_warmup=F))
print(traceplot(mod, pars="lp__", inc_warmup=F))

graphics.off()






######## r=h^q

model_root_r <- '
data {
  int<lower=1> N;           // number of people=measurements
  vector<lower=0>[N] hh;    // height for each person n
  vector<lower=0>[N] y;     // observed waist circumference for person n
}

parameters {
  real<lower=0> q;        // constant root of height (constrained > 2)
  real<lower=0> sigma;    // scale of the likelihood
}

model {
  vector[N] r;
  vector[N] circum;
  
  q ~ normal(0.5,10);
  sigma ~ exponential(1);  // exponential(beta), where here beta = lambda = 1/mean

  for (n in 1:N) {

    r[n] = hh[n]^q;
    circum[n] = 2*pi()*r[n];

  } // for

  y ~ normal(circum,sigma);
}

generated quantities {
  vector[N] r;
  vector[N] circum;

  for (n in 1:N) {

    r[n] = hh[n]^q;
    circum[n] = 2*pi()*r[n];

  } // for
}
'

# fit to boys
m_root_r_boys_w <- stan( model_code=model_root_r,
                        data=data_list_boys_w,
                        iter=samps,
                        chains=num_chains, 
                        init=rep(list(start_list_w), num_chains),
                        control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                       )

saveRDS(m_root_r_boys_w, "m_root_r_boys_w_fit.rds")

post_root_r_boys_w <- extract.samples( m_root_r_boys_w ) 

print(m_root_r_boys_w, pars=c("q","sigma","lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_root_r_boys_w

#look at all traces
pdf(file="./Plots/traces_m_root_r_boys_w.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

print(traceplot(mod, pars="q", inc_warmup=F))
print(traceplot(mod, pars="sigma", inc_warmup=F))
print(traceplot(mod, pars="lp__", inc_warmup=F))

graphics.off()




# fit to girls
m_root_r_girls_w <- stan( model_code=model_root_r,
                         data=data_list_girls_w,
                         iter=samps,
                         chains=num_chains, 
                         init=rep(list(start_list_w), num_chains),
                         control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                        )

saveRDS(m_root_r_girls_w, "m_root_r_girls_w_fit.rds")

post_root_r_girls_w <- extract.samples( m_root_r_girls_w ) 

print(m_root_r_girls_w, pars=c("q","sigma","lp__"),   
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_root_r_girls_w

#look at all traces
pdf(file="./Plots/traces_m_root_r_girls_w.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

print(traceplot(mod, pars="q", inc_warmup=F))
print(traceplot(mod, pars="sigma", inc_warmup=F))
print(traceplot(mod, pars="lp__", inc_warmup=F))

graphics.off()






########## calculate predicted waist circumference given height, using different relationships between h and r

LMSboys.ht.wc <- LMS.ht.wc[which(LMS.ht.wc$sex==0),]
LMSgirls.ht.wc <- LMS.ht.wc[which(LMS.ht.wc$sex==1),]

# r=constant
c <- mean(post_const_r_boys_w$c)
radius <- c
wcircumference <- 2*pi*radius
LMSboys.ht.wc$wc.const.h <- wcircumference

c <- mean(post_const_r_girls_w$c)
radius <- c
wcircumference <- 2*pi*radius
LMSgirls.ht.wc$wc.const.h <- wcircumference


# r=constant*(h+f)
p <- mean(post_prop_r_boys_w$p) #1/14
radius <- p*LMSboys.ht.wc$height
wcircumference <- 2*pi*radius
LMSboys.ht.wc$wc.prop.h <- wcircumference

LMSboys.ht.wc[1:10,c("waistcircum","wc.prop.h")]

p <- mean(post_prop_r_girls_w$p) #1/14
radius <- p*LMSgirls.ht.wc$height
wcircumference <- 2*pi*radius
LMSgirls.ht.wc$wc.prop.h <- wcircumference

LMSgirls.ht.wc[1:10,c("waistcircum","wc.prop.h")]


# r=h^q
q <- mean(post_root_r_boys_w$q) #2.6
radius <- LMSboys.ht.wc$height^q
wcircumference <- 2*pi*radius
LMSboys.ht.wc$wc.root.h <- wcircumference

LMSboys.ht.wc[1:10,c("waistcircum","wc.root.h")]

q <- mean(post_root_r_girls_w$q) #2.6
radius <- LMSgirls.ht.wc$height^q
wcircumference <- 2*pi*radius
LMSgirls.ht.wc$wc.root.h <- wcircumference

LMSgirls.ht.wc[1:10,c("waistcircum","wc.root.h")]


LMS.ht.wc <- rbind(LMSboys.ht.wc, LMSgirls.ht.wc)


########## calculate predicted head circumference/height, using different relationships between h and r

LMS.ht.wc$wch.calc <- LMS.ht.wc$waistcircum/LMS.ht.wc$height
LMS.ht.wc$wch.const.h <- LMS.ht.wc$wc.const.h/LMS.ht.wc$height
LMS.ht.wc$wch.prop.h <- LMS.ht.wc$wc.prop.h/LMS.ht.wc$height
LMS.ht.wc$wch.root.h <- LMS.ht.wc$wc.root.h/LMS.ht.wc$height



# plot of head circumference and height
pdf(file="./Plots/waistcirc_height.pdf",
  height=10, width=8)
layout(matrix(c(1, 2,
                3, 4,
                5, 6), nrow=3, byrow=TRUE))
par(mar = c(2, 2, 0, 0), oma = c(6, 6, 6, 6)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

### boys height
plot( x=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"years"], y=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"M"],
      type="n",
      ylim=c( min( LMS.ht.raw[which(LMS.ht.raw$Sex==1),"M"] ), max( LMS.ht.raw[which(LMS.ht.raw$Sex==0),"M"] ) ), # min girls height, max boys height
      xlim=c( min( LMS.ht.raw$years ), max( LMS.ht.raw$years ) ), # min height years, max height years
      #xlab="Age (yrs)", ylab="Height (cm)",
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c("","","","",""), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(80,100,120,140,160,180), labels=c(80,100,120,140,160,180), cex.axis=1)
lines( x=rep(age_cut_boys,501), y=0:500, col=grey(0.5), lwd=2, lty=2 ) # age fit cutoff

text(x = -3,
     y = 130,
     labels = "Height (cm)",
     xpd = NA, # draw outside plotting region
     srt = 90, # rotate 90 degrees
     cex = 1.5)

lines( x=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"years"], y=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"M"], col="black", lwd=4 ) # boys

text(x=4,y=165,labels='\\MA',vfont=c("sans serif","bold"),cex = 8) #add Mars symbol

legend(8.5, 120,
        legend=c(as.expression( bquote(r==.(round(mean(post_const_r_boys_w$c),2))) ),
                 as.expression( bquote(r==.(round(mean(post_prop_r_boys_w$p),2))*h) ),
                 as.expression( bquote(r==h^.(round(mean(post_root_r_boys_w$q),2))) ), 
                 "LMS median", "Calculated median"), bg="white", #bty="n",
       col=c("blue", "blue", "blue", "black", grey(0.5)), lwd=c(3,3,3,3,3), lty=c(3,2,1,1,1), cex=1.2, seg.len=3)



### girls height
plot( x=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"years"], y=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"M"],
      type="n",
      ylim=c( min( LMS.ht.raw[which(LMS.ht.raw$Sex==1),"M"] ), max( LMS.ht.raw[which(LMS.ht.raw$Sex==0),"M"] ) ), # min girls height, max boys height
      xlim=c( min( LMS.ht.raw$years ), max( LMS.ht.raw$years ) ), # min headcirc years, max height years
      #xlab="Age (yrs)", ylab="Height (cm)",
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c("","","","",""), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(80,100,120,140,160,180), labels=c("","","","","",""), cex.axis=1)
lines( x=rep(age_cut_girls,501), y=0:500, col=grey(0.5), lwd=2, lty=2 ) # age fit cutoff

lines( x=LMS.ht.raw[which(LMS.ht.raw$Sex==1),"years"], y=LMS.ht.raw[which(LMS.ht.raw$Sex==1),"M"], col="black", lwd=4 ) # girls

text(x=4,y=165,labels='\\VE',vfont=c("sans serif","bold"),cex = 8) #add Venus symbol

legend(8.5, 120,
        legend=c(as.expression( bquote(r==.(round(mean(post_const_r_girls_w$c),2))) ),
                 as.expression( bquote(r==.(round(mean(post_prop_r_girls_w$p),2))*h) ),
                 as.expression( bquote(r==h^.(round(mean(post_root_r_girls_w$q),2))) ), 
                 "LMS median", "Calculated median"), bg="white", #bty="n",
       col=c("red", "red", "red", "black", grey(0.5)), lwd=c(3,3,3,3,3), lty=c(3,2,1,1,1), cex=1.2, seg.len=3)



### boys waist circumference
plot( x=LMSboys.wc.raw$month/12, y=LMSboys.wc.raw$M,
      type="n",
      ylim=c( min( LMSgirls.wc.raw$M ), max( LMSboys.wc.raw$M ) ), # min girls waist circ, max boys waist circ
      xlim=c( min( LMS.ht.raw$years ), max( LMS.ht.raw$years ) ), # min height years, max height years
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c("","","","",""), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(55,60,65,70,75), labels=c(55,60,65,70,75), cex.axis=1)
lines( x=rep(age_cut_boys,501), y=0:500, col=grey(0.5), lwd=2, lty=2 ) # age fit cutoff

text(x = -3,
     y = 68,
     labels = "Waist circumference (cm)",
     xpd = NA, # draw outside plotting region
     srt = 90, # rotate 90 degrees
     cex = 1.5)

# real waist circumferences
lines( x=LMSboys.wc.raw$month/12, y=LMSboys.wc.raw$M, col="black", lwd=4 )

# estimates
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==0),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==0),"wc.const.h"], col="blue", lwd=4, lty=3 )
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==0),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==0),"wc.prop.h"], col="blue", lwd=4, lty=2 )
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==0),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==0),"wc.root.h"], col="blue", lwd=4, lty=1 )


### girls waist circumference
plot( x=LMSgirls.wc.raw$month/12, y=LMSgirls.wc.raw$M,
      type="n",
      ylim=c( min( LMSgirls.wc.raw$M ), max( LMSboys.wc.raw$M ) ), # min girls waist circ, max boys waist circ
      xlim=c( min( LMS.ht.raw$years ), max( LMS.ht.raw$years ) ), # min height years, max height years
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c("","","","",""), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(55,60,65,70,75), labels=c("","","","",""), cex.axis=1)
lines( x=rep(age_cut_girls,501), y=0:500, col=grey(0.5), lwd=2, lty=2 ) # age fit cutoff

# real waist circumferences
lines( x=LMSgirls.wc.raw$month/12, y=LMSgirls.wc.raw$M, col="black", lwd=4 )

# estimates
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==1),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==1),"wc.const.h"], col="red", lwd=4, lty=3 )
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==1),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==1),"wc.prop.h"], col="red", lwd=4, lty=2 )
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==1),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==1),"wc.root.h"], col="red", lwd=4, lty=1 )



### boys waist circumference/height
plot( x=LMS.ht.wc[which(LMS.ht.wc$sex==0),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==0),"wch"],
      type="n",
      ylim=c( min( LMSboys.wh.raw$M ) - 0.05, max( LMSboys.wh.raw$M ) + 0.05 ),
      xlim=c( min( LMS.ht.raw$years ), max( LMS.ht.raw$years ) ), # min height years, max height years
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c(0,5,10,15,20), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(0.4,0.45,0.5), labels=c(0.4,0.45,0.5), cex.axis=1)
lines( x=rep(age_cut_boys,501), y=0:500, col=grey(0.5), lwd=2, lty=2 ) # age fit cutoff

text(x = -3,
     y = 0.47,
     labels = "Waist circumference/Height",
     xpd = NA, # draw outside plotting region
     srt = 90, # rotate 90 degrees
     cex = 1.5)

# LMS waist circumference/height
lines( x=LMSboys.wh.raw$month/12, y=LMSboys.wh.raw$M, col="black", lwd=4 )

# calculated waist circumference/height
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==0),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==0),"wch.calc"], col=grey(0.5), lwd=4, lty=1 )

# estimates
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==0),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==0),"wch.const.h"], col="blue", lwd=4, lty=3 )
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==0),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==0),"wch.prop.h"], col="blue", lwd=4, lty=2 )
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==0),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==0),"wch.root.h"], col="blue", lwd=4, lty=1 )



### girls waist circumference/height
plot( x=LMS.ht.wc[which(LMS.ht.wc$sex==1),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==1),"wch"],
      type="n",
      ylim=c( min( LMSboys.wh.raw$M ) - 0.05, max( LMSboys.wh.raw$M ) + 0.05 ),
      xlim=c( min( LMS.ht.raw$years ), max( LMS.ht.raw$years ) ), # min height years, max height years
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c(0,5,10,15,20), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(0.4,0.45,0.5), labels=c("","",""), cex.axis=1)
lines( x=rep(age_cut_girls,501), y=0:500, col=grey(0.5), lwd=2, lty=2 ) # age fit cutoff

# LMS waist circumference/height
lines( x=LMSgirls.wh.raw$month/12, y=LMSgirls.wh.raw$M, col="black", lwd=4 )

# calculated waist circumference/height
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==1),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==1),"wch.calc"], col=grey(0.5), lwd=4, lty=1 )

# estimates
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==1),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==1),"wch.const.h"], col="red", lwd=4, lty=3 )
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==1),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==1),"wch.prop.h"], col="red", lwd=4, lty=2 )
lines( x=LMS.ht.wc[which(LMS.ht.wc$sex==1),"years"], y=LMS.ht.wc[which(LMS.ht.wc$sex==1),"wch.root.h"], col="red", lwd=4, lty=1 )


mtext("Age (years)", side = 1, outer = TRUE, cex = 1.2, line = 2.2, adj=0.5)

graphics.off()


