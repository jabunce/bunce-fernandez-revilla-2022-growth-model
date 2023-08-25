
#Read the data from the csv data file into R:

# CDC height
LMS.ht.raw <- read.csv(file="./Data/statage.csv", header=TRUE)

# German head circumference
LMS.hc.raw <- read.csv(file= "./Data/Shienkiewitz2011_head_circum_German_data.csv", header = TRUE)


#Check the variable names and dimensions in the data frame
names(LMS.ht.raw)
dim(LMS.ht.raw)

names(LMS.hc.raw)
dim(LMS.hc.raw)



######### create dataset with median height and median head circumference per year

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


# head circumference: cut out time points for which height not measured
LMSboys.hc.raw <- LMS.hc.raw[which(LMS.hc.raw$sex==0),]
LMSboys.hc.cut <- LMSboys.hc.raw[which(LMSboys.hc.raw$years %in% LMSboys.ht.mod$years),c("sex","years","M")]
LMSgirls.hc.raw <- LMS.hc.raw[which(LMS.hc.raw$sex==1),]
LMSgirls.hc.cut <- LMSgirls.hc.raw[which(LMSgirls.hc.raw$years %in% LMSgirls.ht.mod$years),c("sex","years","M")]
LMS.hc.cut <- rbind(LMSboys.hc.cut, LMSgirls.hc.cut)

# combine: cut out height time points for which head circumference not measured
LMSboys.ht.cut <- LMSboys.ht.mod[which(LMSboys.ht.mod$years %in% LMSboys.hc.cut$years),c("sex","years","height")]
LMSgirls.ht.cut <- LMSgirls.ht.mod[which(LMSgirls.ht.mod$years %in% LMSgirls.hc.cut$years),c("sex","years","height")]
LMS.ht.cut <- rbind(LMSboys.ht.cut, LMSgirls.ht.cut)

LMS.ht.hc <- cbind(LMS.ht.cut,LMS.hc.cut[,"M"])
names(LMS.ht.hc)[ncol(LMS.ht.hc)] <- "headcirc"


###################### simulate data for head circumference from LMS

SampAge <- 10 # number of simulated people per age

# boys

#first age
simHeadcirc_boys <- cbind( rep(LMSboys.hc.cut[1, "years"], times=SampAge),
                           sample( size=SampAge,
                                   x=rnorm(SampAge, mean=LMSboys.hc.cut[1,"M"], sd=0.01),
                                   replace=TRUE
                                  ),
                           rep(LMSboys.ht.cut[1, "height"], times=SampAge)
                          )

# the rest of the ages
for ( t in 2:nrow(LMSboys.hc.cut) ) {
    simHeadcirc_boys <- rbind( simHeadcirc_boys, cbind( rep(LMSboys.hc.cut[t, "years"], times=SampAge),
                                                        sample( size=SampAge,
                                                                x=rnorm(SampAge, mean=LMSboys.hc.cut[t,"M"], sd=0.01),
                                                                replace=TRUE
                                                               ),
                                                        rep(LMSboys.ht.cut[t, "height"], times=SampAge)
                                                       )
                              )
} #for t

simHeadcirc_boys <- as.data.frame(simHeadcirc_boys)
names(simHeadcirc_boys) <- c("years","headcirc","height")

#plot(x=simHeadcirc_boys$years, y=simHeadcirc_boys$headcirc)


# girls

#first age
simHeadcirc_girls <- cbind( rep(LMSgirls.hc.cut[1,"years"], times=SampAge),
                           sample( size=SampAge,
                                   x=rnorm(SampAge, mean=LMSgirls.hc.cut[1,"M"], sd=0.01),
                                   replace=TRUE
                                  ),
                           rep(LMSgirls.ht.cut[1, "height"], times=SampAge)
                          )

# the rest of the ages
for ( t in 2:nrow(LMSgirls.hc.cut) ) {
    simHeadcirc_girls <- rbind( simHeadcirc_girls, cbind( rep(LMSgirls.hc.cut[t, "years"], times=SampAge),
                                                        sample( size=SampAge,
                                                                x=rnorm(SampAge, mean=LMSgirls.hc.cut[t,"M"], sd=0.01),
                                                                replace=TRUE
                                                               ),
                                                        rep(LMSgirls.ht.cut[t, "height"], times=SampAge)
                                                       )
                              )
} #for t

simHeadcirc_girls <- as.data.frame(simHeadcirc_girls)
names(simHeadcirc_girls) <- c("years","headcirc","height")

#plot(x=simHeadcirc_girls$years, y=simHeadcirc_girls$headcirc)



###################### Stan fitting
rstan_options(auto_write = TRUE) #to let stan make a copy of the model and use multiple cores
options(mc.cores = parallel::detectCores())

samps <- 2000     # number of mcmc samples
num_chains <- 4   # number of mcmc chains



######## r=constant

data_list_boys <- list(
  N = nrow(simHeadcirc_boys),                    # number of people (=number of head circumference measurements)
  hh = simHeadcirc_boys$height,                  # vector of height for each person
  y = simHeadcirc_boys$headcirc,                 # vector of head circumference for each person
  m = mean(simHeadcirc_boys$headcirc)/(2*pi)     # mean head radius across ages
)

data_list_girls <- list(
  N = nrow(simHeadcirc_girls),                    # number of people (=number of head circumference measurements)
  hh = simHeadcirc_girls$height,                  # vector of height for each person
  y = simHeadcirc_girls$headcirc,                 # vector of head circumference for each person
  m = mean(simHeadcirc_girls$headcirc)/(2*pi)     # mean head radius across ages
)

start_list <- list(
  c = mean(c(simHeadcirc_boys$headcirc,simHeadcirc_girls$headcirc))/(2*pi), # constant = r
  p = 1/14,                                                                 # factor of height
  q = 1/2,                                                                  # exponent of height
  sigma = 1
)

model_constant_r <- '
data {
  int<lower=1> N;         // number of people=measurements
  vector<lower=0>[N] y;   // observed head circumference for person n
  real<lower=0> m;        // mean observed head radius
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
m_const_r_boys <- stan( model_code=model_constant_r,
                        data=data_list_boys,
                        iter=samps,
                        chains=num_chains, 
                        init=rep(list(start_list), num_chains),
                        control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                       )

saveRDS(m_const_r_boys, "m_const_r_boys_fit.rds")

post_const_r_boys <- extract.samples( m_const_r_boys ) 

print(m_const_r_boys, pars=c("c","sigma","lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_const_r_boys

#look at all traces
pdf(file="./Plots/traces_m_const_r_boys.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

print(traceplot(mod, pars="c", inc_warmup=F))
print(traceplot(mod, pars="sigma", inc_warmup=F))
print(traceplot(mod, pars="lp__", inc_warmup=F))

graphics.off()


# fit to girls
m_const_r_girls <- stan( model_code=model_constant_r,
                         data=data_list_girls,
                         iter=samps,
                         chains=num_chains, 
                         init=rep(list(start_list), num_chains),
                         control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                        )

saveRDS(m_const_r_girls, "m_const_r_girls_fit.rds")

post_const_r_girls <- extract.samples( m_const_r_girls ) 

print(m_const_r_girls, pars=c("c","sigma","lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_const_r_girls

#look at all traces
pdf(file="./Plots/traces_m_const_r_girls.pdf",
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
  vector<lower=0>[N] y;     // observed head circumference for person n
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
m_prop_r_boys <- stan( model_code=model_prop_r,
                        data=data_list_boys,
                        iter=samps,
                        chains=num_chains, 
                        init=rep(list(start_list), num_chains),
                        control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                       )

saveRDS(m_prop_r_boys, "m_prop_r_boys_fit.rds")

post_prop_r_boys <- extract.samples( m_prop_r_boys ) 

print(m_prop_r_boys, pars=c("p","sigma","lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_prop_r_boys

#look at all traces
pdf(file="./Plots/traces_m_prop_r_boys.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

print(traceplot(mod, pars="p", inc_warmup=F))
print(traceplot(mod, pars="sigma", inc_warmup=F))
print(traceplot(mod, pars="lp__", inc_warmup=F))

graphics.off()




# fit to girls
m_prop_r_girls <- stan( model_code=model_prop_r,
                         data=data_list_girls,
                         iter=samps,
                         chains=num_chains, 
                         init=rep(list(start_list), num_chains),
                         control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                        )

saveRDS(m_prop_r_girls, "m_prop_r_girls_fit.rds")

post_prop_r_girls <- extract.samples( m_prop_r_girls ) 

print(m_prop_r_girls, pars=c("p","sigma","lp__"),   
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_prop_r_girls

#look at all traces
pdf(file="./Plots/traces_m_prop_r_girls.pdf",
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
  vector<lower=0>[N] y;     // observed head circumference for person n
}

parameters {
  real<lower=0> q;        // constant root of height (constrained positive)
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
m_root_r_boys <- stan( model_code=model_root_r,
                        data=data_list_boys,
                        iter=samps,
                        chains=num_chains, 
                        init=rep(list(start_list), num_chains),
                        control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                       )

saveRDS(m_root_r_boys, "m_root_r_boys_fit.rds")

post_root_r_boys <- extract.samples( m_root_r_boys ) 

print(m_root_r_boys, pars=c("q","sigma","lp__"),  
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_root_r_boys

#look at all traces
pdf(file="./Plots/traces_m_root_r_boys.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

print(traceplot(mod, pars="q", inc_warmup=F))
print(traceplot(mod, pars="sigma", inc_warmup=F))
print(traceplot(mod, pars="lp__", inc_warmup=F))

graphics.off()




# fit to girls
m_root_r_girls <- stan( model_code=model_root_r,
                         data=data_list_girls,
                         iter=samps,
                         chains=num_chains, 
                         init=rep(list(start_list), num_chains),
                         control=list(adapt_delta=0.99, max_treedepth=10) #default treedepth is 10
                        )

saveRDS(m_root_r_girls, "m_root_r_girls_fit.rds")

post_root_r_girls <- extract.samples( m_root_r_girls ) 

print(m_root_r_girls, pars=c("q","sigma","lp__"),   
      probs = c(0.025,0.975), digits_summary=2)

mod <- m_root_r_girls

#look at all traces
pdf(file="./Plots/traces_m_root_r_girls.pdf",
  height=3, width=8)
par(mfrow=c(2,1))

print(traceplot(mod, pars="q", inc_warmup=F))
print(traceplot(mod, pars="sigma", inc_warmup=F))
print(traceplot(mod, pars="lp__", inc_warmup=F))

graphics.off()






########## calculate predicted head circumference given height, using different relationships between h and r

LMSboys.ht.hc <- LMS.ht.hc[which(LMS.ht.hc$sex==0),]
LMSgirls.ht.hc <- LMS.ht.hc[which(LMS.ht.hc$sex==1),]

# r=constant
c <- mean(post_const_r_boys$c) #mean(LMSboys.ht.hc$headcirc)/(2*pi)
radius <- c
hcircumference <- 2*pi*radius
LMSboys.ht.hc$hc.const.h <- hcircumference

c <- mean(post_const_r_girls$c)  #mean(LMSgirls.ht.hc$headcirc)/(2*pi)
radius <- c
hcircumference <- 2*pi*radius
LMSgirls.ht.hc$hc.const.h <- hcircumference


# r=constant*h
p <- mean(post_prop_r_boys$p) #1/14
radius <- p*LMSboys.ht.hc$height
hcircumference <- 2*pi*radius
LMSboys.ht.hc$hc.prop.h <- hcircumference

LMSboys.ht.hc[1:10,c("headcirc","hc.prop.h")]

p <- mean(post_prop_r_girls$p) #1/14
radius <- p*LMSgirls.ht.hc$height
hcircumference <- 2*pi*radius
LMSgirls.ht.hc$hc.prop.h <- hcircumference

LMSgirls.ht.hc[1:10,c("headcirc","hc.prop.h")]


# r=h^q
q <- mean(post_root_r_boys$q) #1/2
radius <- LMSboys.ht.hc$height^q
hcircumference <- 2*pi*radius
LMSboys.ht.hc$hc.root.h <- hcircumference

LMSboys.ht.hc[1:10,c("headcirc","hc.root.h")]

q <- mean(post_root_r_girls$q) #1/2
radius <- LMSgirls.ht.hc$height^q
hcircumference <- 2*pi*radius
LMSgirls.ht.hc$hc.root.h <- hcircumference

LMSgirls.ht.hc[1:10,c("headcirc","hc.root.h")]


LMS.ht.hc <- rbind(LMSboys.ht.hc, LMSgirls.ht.hc)


########## calculate predicted head circumference/height, using different relationships between h and r

LMS.ht.hc$hch <- LMS.ht.hc$headcirc/LMS.ht.hc$height
LMS.ht.hc$hch.const.h <- LMS.ht.hc$hc.const.h/LMS.ht.hc$height
LMS.ht.hc$hch.prop.h <- LMS.ht.hc$hc.prop.h/LMS.ht.hc$height
LMS.ht.hc$hch.root.h <- LMS.ht.hc$hc.root.h/LMS.ht.hc$height



# plot of head circumference and height
pdf(file="./Plots/headcirc_height.pdf",
  height=10, width=8)
layout(matrix(c(1, 2,
                3, 4,
                5, 6), nrow=3, byrow=TRUE))
par(mar = c(2, 2, 0, 0), oma = c(6, 6, 6, 6)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

### boys height
plot( x=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"years"], y=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"M"],
      type="n",
      ylim=c( min( LMS.ht.raw[which(LMS.ht.raw$Sex==1),"M"] ), max( LMS.ht.raw[which(LMS.ht.raw$Sex==0),"M"] ) ), # min girls height, max boys height
      xlim=c( min( LMS.hc.raw$years ), max( LMS.ht.raw$years ) ), # min headcirc years, max height years
      #xlab="Age (yrs)", ylab="Height (cm)",
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c("","","","",""), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(80,100,120,140,160,180), labels=c(80,100,120,140,160,180), cex.axis=1)

text(x = -4,
     y = 130,
     labels = "Height (cm)",
     xpd = NA, # draw outside plotting region
     srt = 90, # rotate 90 degrees
     cex = 1.5)

lines( x=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"years"], y=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"M"], col="black", lwd=4 ) # boys

text(x=2,y=165,labels='\\MA',vfont=c("sans serif","bold"),cex = 8) #add Mars symbol


### girls height
plot( x=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"years"], y=LMS.ht.raw[which(LMS.ht.raw$Sex==0),"M"],
      type="n",
      ylim=c( min( LMS.ht.raw[which(LMS.ht.raw$Sex==1),"M"] ), max( LMS.ht.raw[which(LMS.ht.raw$Sex==0),"M"] ) ), # min girls height, max boys height
      xlim=c( min( LMS.hc.raw$years ), max( LMS.ht.raw$years ) ), # min headcirc years, max height years
      #xlab="Age (yrs)", ylab="Height (cm)",
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c("","","","",""), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(80,100,120,140,160,180), labels=c("","","","","",""), cex.axis=1)

lines( x=LMS.ht.raw[which(LMS.ht.raw$Sex==1),"years"], y=LMS.ht.raw[which(LMS.ht.raw$Sex==1),"M"], col="black", lwd=4 ) # girls

text(x=2,y=165,labels='\\VE',vfont=c("sans serif","bold"),cex = 8) #add Venus symbol


### boys head circumference
plot( x=LMS.hc.raw[which(LMS.hc.raw$sex==0),"years"], y=LMS.hc.raw[which(LMS.hc.raw$sex==0),"M"],
      type="n",
      ylim=c( min( LMS.hc.raw[which(LMS.hc.raw$sex==1),"M"] ), max( LMS.ht.hc[which(LMS.ht.hc$sex==0),"hc.prop.h"] ) ), # min girls headcirc, max boys predicted headcirc
      xlim=c( min( LMS.hc.raw$years ), max( LMS.ht.raw$years ) ), # min headcirc years, max height years
      #xlab="Age (yrs)", ylab="Head circumference (cm)",
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c("","","","",""), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(45,50,55,60,65), labels=c(45,50,55,60,65), cex.axis=1)

text(x = -4,
     y = 55,
     labels = "Head circumference (cm)",
     xpd = NA, # draw outside plotting region
     srt = 90, # rotate 90 degrees
     cex = 1.5)

# real head circumferences
lines( x=LMS.hc.raw[which(LMS.hc.raw$sex==0),"years"], y=LMS.hc.raw[which(LMS.hc.raw$sex==0),"M"], col="black", lwd=4 )

# estimates
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==0),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==0),"hc.const.h"], col="blue", lwd=4, lty=3 )
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==0),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==0),"hc.prop.h"], col="blue", lwd=4, lty=2 )
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==0),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==0),"hc.root.h"], col="blue", lwd=4, lty=1 )


### girls head circumference
plot( x=LMS.hc.raw[which(LMS.hc.raw$sex==0),"years"], y=LMS.hc.raw[which(LMS.hc.raw$sex==0),"M"],
      type="n",
      ylim=c( min( LMS.hc.raw[which(LMS.hc.raw$sex==1),"M"] ), max( LMS.ht.hc[which(LMS.ht.hc$sex==0),"hc.prop.h"] ) ), # min girls headcirc, max boys predicted headcirc
      xlim=c( min( LMS.hc.raw$years ), max( LMS.ht.raw$years ) ), # min headcirc years, max height years
      #xlab="Age (yrs)", ylab="Head circumference (cm)",
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c("","","","",""), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(45,50,55,60,65), labels=c("","","","",""), cex.axis=1)

# real head circumferences
lines( x=LMS.hc.raw[which(LMS.hc.raw$sex==1),"years"], y=LMS.hc.raw[which(LMS.hc.raw$sex==1),"M"], col="black", lwd=4 )

# estimates
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==1),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==1),"hc.const.h"], col="red", lwd=4, lty=3 )
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==1),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==1),"hc.prop.h"], col="red", lwd=4, lty=2 )
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==1),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==1),"hc.root.h"], col="red", lwd=4, lty=1 )



### boys head circumference/height
plot( x=LMS.hc.raw[which(LMS.hc.raw$sex==0),"years"], y=LMS.hc.raw[which(LMS.hc.raw$sex==0),"M"],
      type="n",
      ylim=c( min( LMS.ht.hc$hch ) - 0.05, max( LMS.ht.hc$hch ) + 0.05 ),
      xlim=c( min( LMS.hc.raw$years ), max( LMS.ht.raw$years ) ), # min headcirc years, max height years
      #xlab="Age (yrs)", ylab="Head circumference/Height",
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c(0,5,10,15,20), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(0.3,0.4,0.5,0.6), labels=c(0.3,0.4,0.5,0.6), cex.axis=1)

text(x = -4,
     y = 0.45,
     labels = "Head circumference/Height",
     xpd = NA, # draw outside plotting region
     srt = 90, # rotate 90 degrees
     cex = 1.5)

# real head circumference/height
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==0),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==0),"hch"], col="black", lwd=4 )

# estimates
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==0),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==0),"hch.const.h"], col="blue", lwd=4, lty=3 )
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==0),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==0),"hch.prop.h"], col="blue", lwd=4, lty=2 )
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==0),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==0),"hch.root.h"], col="blue", lwd=4, lty=1 )

legend(8, 0.6,
        legend=c(as.expression( bquote(r==.(round(mean(post_const_r_boys$c),2))) ),
                 as.expression( bquote(r==.(round(mean(post_prop_r_boys$p),2))*h) ),
                 as.expression( bquote(r==h^.(round(mean(post_root_r_boys$q),2))) ), 
                 "LMS median"), #bty="n",
       col=c("blue", "blue", "blue", "black"), lwd=c(3,3,3,3), lty=c(3,2,1,1), cex=1.2, seg.len=3)


### girls head circumference/height
plot( x=LMS.hc.raw[which(LMS.hc.raw$sex==0),"years"], y=LMS.hc.raw[which(LMS.hc.raw$sex==0),"M"],
      type="n",
      ylim=c( min( LMS.ht.hc$hch ) - 0.05, max( LMS.ht.hc$hch ) + 0.05 ),
      xlim=c( min( LMS.hc.raw$years ), max( LMS.ht.raw$years ) ), # min headcirc years, max height years
      #xlab="Age (yrs)", ylab="Head circumference/Height",
      xaxt="n", yaxt="n", cex=0.5
    )
axis(side=1, at=c(0,5,10,15,20), labels=c(0,5,10,15,20), cex.axis=1) #side: (1=bottom, 2=left, 3=top, 4=right)
axis(side=2, at=c(0.3,0.4,0.5,0.6), labels=c("","","",""), cex.axis=1)

# real head circumference/height
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==1),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==1),"hch"], col="black", lwd=4 )

# estimates
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==1),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==1),"hch.const.h"], col="red", lwd=4, lty=3 )
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==1),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==1),"hch.prop.h"], col="red", lwd=4, lty=2 )
lines( x=LMS.ht.hc[which(LMS.ht.hc$sex==1),"years"], y=LMS.ht.hc[which(LMS.ht.hc$sex==1),"hch.root.h"], col="red", lwd=4, lty=1 )

legend(8, 0.6,
        legend=c(as.expression( bquote(r==.(round(mean(post_const_r_girls$c),2))) ),
                 as.expression( bquote(r==.(round(mean(post_prop_r_girls$p),2))*h) ),
                 as.expression( bquote(r==h^.(round(mean(post_root_r_girls$q),2))) ), 
                 "LMS median"), #bty="n",
       col=c("red", "red", "red", "black"), lwd=c(3,3,3,3), lty=c(3,2,1,1), cex=1.2, seg.len=3)



mtext("Age (years)", side = 1, outer = TRUE, cex = 1.2, line = 2.2, adj=0.5)

graphics.off()

