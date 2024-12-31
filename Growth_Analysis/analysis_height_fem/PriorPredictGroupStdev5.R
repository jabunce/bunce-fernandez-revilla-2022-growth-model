

##### prior predictive simulations from composite model, multiple groups


Nsims <- 100 # number of trajectories to simulate 

muQ1 <- 0.08613371 #0.08619359 #0.0864 #0.088 #0.098 #0.094
muQ2 <- 0.1629413 #0.1587045 #0.163 #0.166 #0.18 #0.22
muQ3 <- 0.1770267 #0.1758627 #0.177 #0.179 #0.223 #0.22
muQ4 <- 0.05811899 #0.05711325 #0.0582 #0.0612 #0.09 #0.22 #0.165
muQ5 <- 0.1279527 #0.1313124 #0.128 #0.128 #0.12 #0.135 #0.05

muK1 <- 75.48489 #75.69238 #75.6 #70 #53.8 #49.8
muK2 <- 10.83212 #11.34571 #10.8 #9.87 #8.8 #6.49
muK3 <- 3.391654 #3.496804 #3.4 #3.19 #2.63 #2.41
muK4 <- 8.281045 #8.382135 #8.28 #8.04 #4.83 #2.14 #2.5
muK5 <- 6.752648 #6.763815 #6.77 #6.54 #7.35 #6.36 #10.25

muH1 <- 53.64355 #53.79338 #53.7 #50.4 #40.46 #37.03
muH2 <- 9.506736 #9.771146 #9.49 #8.76 #8 #6.75
muH3 <- 3.187564 #3.274489 #3.19 #3 #2.9 #2.61
muH4 <- 4.940765 #4.991676 #4.94 #4.81 #3.2 #2.27 #2.18
muH5 <- 4.972467 #4.995162 #4.98 #4.87 #5.3 #4.88 #6 

muI1 <- 0
muI2 <- 0.1252565 #0.1295001 #0.126 #0.126 #0.125
muI3 <- 0.7485214 #0.7489688 #0.769 #0.762 #0.75
muI4 <- 1.750699 #1.773671 #1.84 #1.79 #1.75 #3.75 #2.75
muI5 <- 8.823956 #8.894477 #8.83 #8.68 #8.75 #10.75 #3.75
muIM <- 20 # max age for start of last process


# transformed
muLQ1 <- log(muQ1)         # assumes muQ1 is the median on the original (un-logged) scale
muLQ2 <- log(muQ2)
muLQ3 <- log(muQ3)
muLQ4 <- log(muQ4)        
muLQ5 <- log(muQ5)

muLK1 <- log(muK1) 
muLK2 <- log(muK2)
muLK3 <- log(muK3)
muLK4 <- log(muK4)
muLK5 <- log(muK5)

muR1 <- muH1 - 0.5*muK1
muR2 <- muH2 - 0.5*muK2
muR3 <- muH3 - 0.5*muK3
muR4 <- muH4 - 0.5*muK4
muR5 <- muH5 - 0.5*muK5

muLI1 <- 0
muLI2 <- log(muI2)
muLI3 <- log(muI3)
muLI4 <- log(muI4)
muLI5 <- log(muI5)
muLIM <- log(muIM)



# stdev of the offset to the base trajectory for the mean (across individuals) trajectory of the ethnic group
# on transformed scale
gexpLQ1 <- 0.005 #0.01 #0.0001 #0.001  # mean(post5$"offsetIndivStdevs[1]")
gexpLQ2 <- 0.005 #0.01 #0.0001 #0.001        
gexpLQ3 <- 0.005 #0.01 #0.0001 #0.001
gexpLQ4 <- 0.001 #0.005 #0.001 #0.01 #0.001 #0.0001 #0.001 #0.005 ##0.001        
gexpLQ5 <- 0.0005 #0.005 #0.0005 #0.001 #0.01 #0.001 #0.0001 #0.001 ##0.005

gexpLK1 <- 0.005 #0.01 #0.001 #0.0001 #0.001 #0.005      
gexpLK2 <- 0.005 #0.01 #0.001 #0.0001 #0.001 #0.005 
gexpLK3 <- 0.005 #0.01 #0.001 #0.0001 #0.001 #0.005 
gexpLK4 <- 0.001 #0.005 #0.001 #0.01 #0.001 #0.0001 #0.001 #0.005 #0.01 ##0.005 
gexpLK5 <- 0.0005 #0.005 #0.0005 #0.001 #0.01 #0.001 #0.0001 #0.001 #0.005  #0.01 ##0.001 

gexpR1 <- 0.005 #0.01 #0.001 #0.0001 #0.001 #0.01       
gexpR2 <- 0.005 #0.01 #0.001 #0.0001 #0.001 #0.01
gexpR3 <- 0.005 #0.01 #0.001 #0.0001 #0.001 #0.01
gexpR4 <- 0.001 #0.005 #0.001 #0.01 #0.001 #0.0001 #0.001 #0.01 #0.05 ##0.01
gexpR5 <- 0.0005 #0.005 #0.0005 #0.001 #0.01 #0.001 #0.0001 #0.001 #0.01 ##0.05

gexpLI1 <- 0.0000001      
gexpLI2 <- 0.005 #0.01 #0.001 #0.0001 #0.001
gexpLI3 <- 0.005 #0.01 #0.001 #0.0001 #0.001 #0.01 ##0.001
gexpLI4 <- 0.005 #0.001 #0.01 #0.001 #0.0001 #0.001 #0.01 ##0.001
gexpLI5 <- 0.005 #0.0005 #0.001 #0.01 #0.001 #0.0001 #0.001 #0.01 ##0.001
gexpLIM <- 0.0000001

# on original scale
# stdev on original scale = [( e^(sdL^2) - 1 )*e^( 2*mu + sdL^2 )]^0.5
gexpQ1 <- (( exp(gexpLQ1^2) - 1 )*exp( 2*muLQ1 + gexpLQ1^2 ))^0.5    
gexpQ2 <- (( exp(gexpLQ2^2) - 1 )*exp( 2*muLQ2 + gexpLQ2^2 ))^0.5          
gexpQ3 <- (( exp(gexpLQ3^2) - 1 )*exp( 2*muLQ3 + gexpLQ3^2 ))^0.5     
gexpQ4 <- (( exp(gexpLQ4^2) - 1 )*exp( 2*muLQ4 + gexpLQ4^2 ))^0.5          
gexpQ5 <- (( exp(gexpLQ5^2) - 1 )*exp( 2*muLQ5 + gexpLQ5^2 ))^0.5   

gexpK1 <- (( exp(gexpLK1^2) - 1 )*exp( 2*muLK1 + gexpLK1^2 ))^0.5         
gexpK2 <- (( exp(gexpLK2^2) - 1 )*exp( 2*muLK2 + gexpLK2^2 ))^0.5   
gexpK3 <- (( exp(gexpLK3^2) - 1 )*exp( 2*muLK3 + gexpLK3^2 ))^0.5   
gexpK4 <- (( exp(gexpLK4^2) - 1 )*exp( 2*muLK4 + gexpLK4^2 ))^0.5   
gexpK5 <- (( exp(gexpLK5^2) - 1 )*exp( 2*muLK5 + gexpLK5^2 ))^0.5   

gexpH1 <- gexpR1        
gexpH2 <- gexpR2   
gexpH3 <- gexpR3   
gexpH4 <- gexpR4    
gexpH5 <- gexpR5    

gexpI1 <- 0.00001      
gexpI2 <- (( exp(gexpLI2^2) - 1 )*exp( 2*muLI2 + gexpLI2^2 ))^0.5    
gexpI3 <- (( exp(gexpLI3^2) - 1 )*exp( 2*muLI3 + gexpLI3^2 ))^0.5    
gexpI4 <- (( exp(gexpLI4^2) - 1 )*exp( 2*muLI4 + gexpLI4^2 ))^0.5      
gexpI5 <- (( exp(gexpLI5^2) - 1 )*exp( 2*muLI5 + gexpLI5^2 ))^0.5      
gexpIM <- (( exp(gexpLIM^2) - 1 )*exp( 2*muLIM + gexpLIM^2 ))^0.5 





# mean offsets to the mean group trajectory for each individual, if the individual trajectories fit the data at the expense of the mean group trajectory, then reduce these prior variances
iexpLQ1 <- 0.001 #0.005 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.0005 #0.001 ##0.0015     # mean(post5$"offsetIndivStdevs[1]")
iexpLQ2 <- 0.005 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.0005 #0.001 ##0.0015        
iexpLQ3 <- 0.05 #0.01 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.0005 #0.001 #0.01 ##0.005 
iexpLQ4 <- 0.05 #0.1 #0.05 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.00001 #0.00005 #0.0001 #0.0005 #0.001 ##0.0015          
iexpLQ5 <- 0.05 #0.1 #0.05 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.00001 #0.00005 #0.0001 #0.0005 #0.001 #0.01 ##0.005 

iexpLK1 <- 0.001 #0.005 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.001 #0.005 ##0.006      
iexpLK2 <- 0.005 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.001 #0.001 #0.005 ##0.006 
iexpLK3 <- 0.05 #0.01 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.001 #0.001 #0.005 #0.06 ##0.01  
iexpLK4 <- 0.05 #0.1 #0.05 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.00005 #0.0001 #0.0005 #0.001 #0.005 ##0.006  
iexpLK5 <- 0.05 #0.1 #0.05 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.00005 #0.0001 #0.0005 #0.001 #0.005 #0.06 ##0.01 

iexpR1 <- 0.005 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.005 #0.01 ##0.015      
iexpR2 <- 0.005 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.005 #0.01 ##0.015
iexpR3 <- 0.05 #0.01 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.001 #0.005 #0.01 #0.15 ##0.05 #0.015
iexpR4 <- 0.05 #0.1 #0.05 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.0001 #0.0005 #0.001 #0.005 #0.01 ##0.015 #0.015 
iexpR5 <- 0.05 #0.1 #0.05 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.0001 #0.0005 #0.001 #0.005 #0.01 #0.15 ##0.05 #0.005

iexpLI1 <- 0.0000001      
iexpLI2 <- 0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001
iexpLI3 <- 0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 ##0.01
iexpLI4 <- 0.05 #0.1 #0.05 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.01 ##0.001
iexpLI5 <- 0.05 #0.1 #0.05 #0.008 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 ##0.01
iexpLIM <- 0.0000001


# on original scale
# stdev on original scale = [( e^(sdL^2) - 1 )*e^( 2*mu + sdL^2 )]^0.5
iexpQ1 <- (( exp(iexpLQ1^2) - 1 )*exp( 2*muLQ1 + iexpLQ1^2 ))^0.5    
iexpQ2 <- (( exp(iexpLQ2^2) - 1 )*exp( 2*muLQ2 + iexpLQ2^2 ))^0.5          
iexpQ3 <- (( exp(iexpLQ3^2) - 1 )*exp( 2*muLQ3 + iexpLQ3^2 ))^0.5     
iexpQ4 <- (( exp(iexpLQ4^2) - 1 )*exp( 2*muLQ4 + iexpLQ4^2 ))^0.5          
iexpQ5 <- (( exp(iexpLQ5^2) - 1 )*exp( 2*muLQ5 + iexpLQ5^2 ))^0.5   

iexpK1 <- (( exp(iexpLK1^2) - 1 )*exp( 2*muLK1 + iexpLK1^2 ))^0.5         
iexpK2 <- (( exp(iexpLK2^2) - 1 )*exp( 2*muLK2 + iexpLK2^2 ))^0.5   
iexpK3 <- (( exp(iexpLK3^2) - 1 )*exp( 2*muLK3 + iexpLK3^2 ))^0.5   
iexpK4 <- (( exp(iexpLK4^2) - 1 )*exp( 2*muLK4 + iexpLK4^2 ))^0.5   
iexpK5 <- (( exp(iexpLK5^2) - 1 )*exp( 2*muLK5 + iexpLK5^2 ))^0.5   

iexpH1 <- gexpR1        
iexpH2 <- gexpR2   
iexpH3 <- gexpR3   
iexpH4 <- gexpR4    
iexpH5 <- gexpR5    

iexpI1 <- 0.00001      
iexpI2 <- (( exp(iexpLI2^2) - 1 )*exp( 2*muLI2 + iexpLI2^2 ))^0.5    
iexpI3 <- (( exp(iexpLI3^2) - 1 )*exp( 2*muLI3 + iexpLI3^2 ))^0.5    
iexpI4 <- (( exp(iexpLI4^2) - 1 )*exp( 2*muLI4 + iexpLI4^2 ))^0.5      
iexpI5 <- (( exp(iexpLI5^2) - 1 )*exp( 2*muLI5 + iexpLI5^2 ))^0.5      
iexpIM <- (( exp(iexpLIM^2) - 1 )*exp( 2*muLIM + iexpLIM^2 ))^0.5 


# stdevs for normal hyperpriors on individual-level stdevs
# on transformed scale 
ihsdLQ1 <- 0.001 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01
ihsdLQ2 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01        
ihsdLQ3 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01
ihsdLQ4 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01          
ihsdLQ5 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01 

ihsdLK1 <- 0.001 #0.005 #0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01     
ihsdLK2 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01 
ihsdLK3 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01  
ihsdLK4 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01  
ihsdLK5 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01 

ihsdR1 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01      
ihsdR2 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01
ihsdR3 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01
ihsdR4 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01
ihsdR5 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01

ihsdLI1 <- 0.0000001      
ihsdLI2 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01
ihsdLI3 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01
ihsdLI4 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01
ihsdLI5 <- 0.01 #0.005 #0.01 #0.005 #0.01 #0.001 #0.000001 #0.0001 #0.01
ihsdLIM <- 0.0000001



muLsigma <- 0.002  # mean(post4$"Lsigma")
musigma <- (( exp(muLsigma^2) - 1 )*exp( 2*log(100) + muLsigma^2 ))^0.5  # want 95% of observations within 2*stdev = 0.5cm of actual height at height=100cm




# holders for sims for individuals
Q1 <- rep(0,Nsims)
Q2 <- rep(0,Nsims)
Q3 <- rep(0,Nsims)
Q4 <- rep(0,Nsims)
Q5 <- rep(0,Nsims)

K1 <- rep(0,Nsims)
K2 <- rep(0,Nsims)
K3 <- rep(0,Nsims)
K4 <- rep(0,Nsims)
K5 <- rep(0,Nsims)

H1 <- rep(0,Nsims)
H2 <- rep(0,Nsims)
H3 <- rep(0,Nsims)
H4 <- rep(0,Nsims)
H5 <- rep(0,Nsims)

I1 <- rep(0,Nsims)
I2 <- rep(0,Nsims)
I3 <- rep(0,Nsims)
I4 <- rep(0,Nsims)
I5 <- rep(0,Nsims)


# holders for sims for group mean
gQ1 <- rep(0,Nsims)
gQ2 <- rep(0,Nsims)
gQ3 <- rep(0,Nsims)
gQ4 <- rep(0,Nsims)
gQ5 <- rep(0,Nsims)

gK1 <- rep(0,Nsims)
gK2 <- rep(0,Nsims)
gK3 <- rep(0,Nsims)
gK4 <- rep(0,Nsims)
gK5 <- rep(0,Nsims)

gH1 <- rep(0,Nsims)
gH2 <- rep(0,Nsims)
gH3 <- rep(0,Nsims)
gH4 <- rep(0,Nsims)
gH5 <- rep(0,Nsims)

gI1 <- rep(0,Nsims)
gI2 <- rep(0,Nsims)
gI3 <- rep(0,Nsims)
gI4 <- rep(0,Nsims)
gI5 <- rep(0,Nsims)




for (n in 1:Nsims) {

  gsdLQ1 <- rnorm( n=1, mean=gexpLQ1, sd=0.001 ) #0.005 0.001 0.005 #0.001 #0.005 0.01 0.001 #0.00001
  gsdLQ2 <- rnorm( n=1, mean=gexpLQ2, sd=0.001 )          
  gsdLQ3 <- rnorm( n=1, mean=gexpLQ3, sd=0.001 )
  gsdLQ4 <- rnorm( n=1, mean=gexpLQ4, sd=0.001 )        
  gsdLQ5 <- rnorm( n=1, mean=gexpLQ5, sd=0.001 )

  gsdLK1 <- rnorm( n=1, mean=gexpLK1, sd=0.001 )      
  gsdLK2 <- rnorm( n=1, mean=gexpLK2, sd=0.001 )
  gsdLK3 <- rnorm( n=1, mean=gexpLK3, sd=0.001 )
  gsdLK4 <- rnorm( n=1, mean=gexpLK4, sd=0.001 )
  gsdLK5 <- rnorm( n=1, mean=gexpLK5, sd=0.001 )

  gsdR1 <- rnorm( n=1, mean=gexpR1, sd=0.001 )      
  gsdR2 <- rnorm( n=1, mean=gexpR2, sd=0.001 )
  gsdR3 <- rnorm( n=1, mean=gexpR3, sd=0.001 )
  gsdR4 <- rnorm( n=1, mean=gexpR4, sd=0.001 )
  gsdR5 <- rnorm( n=1, mean=gexpR5, sd=0.001 )

  gsdLI1 <- 0.00000001      
  gsdLI2 <- rnorm( n=1, mean=gexpLI2, sd=0.001 )
  gsdLI3 <- rnorm( n=1, mean=gexpLI3, sd=0.001 )
  gsdLI4 <- rnorm( n=1, mean=gexpLI4, sd=0.001 )
  gsdLI5 <- rnorm( n=1, mean=gexpLI5, sd=0.001 )

  offsetGroupStdevs <- c( gsdLQ1, gsdLQ2, gsdLQ3, gsdLQ4, gsdLQ5,
                          gsdLK1, gsdLK2, gsdLK3, gsdLK4, gsdLK5,
                          gsdR1,  gsdR2,  gsdR3,  gsdR4,  gsdR5,
                          gsdLI1, gsdLI2, gsdLI3, gsdLI4, gsdLI5 )

  t <- diag(offsetGroupStdevs)
  CholEtaG <- 4 #40 #4
  U <- rlkjcorr( n=1, K=length(offsetGroupStdevs), eta=CholEtaG )  # eta=1, all coorelations equally likely; eta>1, extreme correlations less likely 
  T <- t %*% U %*% t # vcov matrix with variances (t^2) on the diagonal

  GrpOffset <- mvrnorm( n=1, mu=rep(0,length(offsetGroupStdevs)), Sigma=T )



  isdLQ1 <- rnorm( n=1, mean=iexpLQ1, sd=ihsdLQ1 )
  isdLQ2 <- rnorm( n=1, mean=iexpLQ2, sd=ihsdLQ2 )        
  isdLQ3 <- rnorm( n=1, mean=iexpLQ3, sd=ihsdLQ3 )
  isdLQ4 <- rnorm( n=1, mean=iexpLQ4, sd=ihsdLQ4 )        
  isdLQ5 <- rnorm( n=1, mean=iexpLQ5, sd=ihsdLQ5 )

  isdLK1 <- rnorm( n=1, mean=iexpLK1, sd=ihsdLK1 )      
  isdLK2 <- rnorm( n=1, mean=iexpLK2, sd=ihsdLK2 )
  isdLK3 <- rnorm( n=1, mean=iexpLK3, sd=ihsdLK3 )
  isdLK4 <- rnorm( n=1, mean=iexpLK4, sd=ihsdLK4 )
  isdLK5 <- rnorm( n=1, mean=iexpLK5, sd=ihsdLK5 )

  isdR1 <- rnorm( n=1, mean=iexpR1, sd=ihsdR1 )      
  isdR2 <- rnorm( n=1, mean=iexpR1, sd=ihsdR1 )
  isdR3 <- rnorm( n=1, mean=iexpR1, sd=ihsdR1 )
  isdR4 <- rnorm( n=1, mean=iexpR1, sd=ihsdR1 )
  isdR5 <- rnorm( n=1, mean=iexpR1, sd=ihsdR1 )

  isdLI1 <- rnorm( n=1, mean=iexpLI1, sd=ihsdLI1 )      
  isdLI2 <- rnorm( n=1, mean=iexpLI2, sd=ihsdLI2 )
  isdLI3 <- rnorm( n=1, mean=iexpLI3, sd=ihsdLI3 )
  isdLI4 <- rnorm( n=1, mean=iexpLI4, sd=ihsdLI4 )
  isdLI5 <- rnorm( n=1, mean=iexpLI5, sd=ihsdLI5 )


  offsetIndivStdevs <- c( isdLQ1, isdLQ2, isdLQ3, isdLQ4, isdLQ5,
                          isdLK1, isdLK2, isdLK3, isdLK4, isdLK5,
                          isdR1,  isdR2,  isdR3,  isdR4,  isdR5,
                          isdLI1, isdLI2, isdLI3, isdLI4, isdLI5 )

  s <- diag(offsetIndivStdevs)
  CholEtaI <- 4
  R <- rlkjcorr( n=1, K=length(offsetIndivStdevs), eta=CholEtaI )  # eta=1, all coorelations equally likely; eta>1, extreme correlations less likely 
  S <- s %*% R %*% s # vcov matrix with variances (s^2) on the diagonal

  IndOffset <- mvrnorm( n=1, mu=rep(0,length(offsetIndivStdevs)), Sigma=S )



  Q1[n] <- exp( muLQ1 + IndOffset[1] )
  Q2[n] <- exp( muLQ2 + IndOffset[2] )      
  Q3[n] <- exp( muLQ3 + IndOffset[3] )
  Q4[n] <- exp( muLQ4 + IndOffset[4] )      
  Q5[n] <- exp( muLQ5 + IndOffset[5] )

  K1[n] <- exp( muLK1 + IndOffset[6] )   
  K2[n] <- exp( muLK2 + IndOffset[7] )
  K3[n] <- exp( muLK3 + IndOffset[8] )
  K4[n] <- exp( muLK4 + IndOffset[9] )
  K5[n] <- exp( muLK5 + IndOffset[10] )

  H1[n] <- K1[n]/2 + muR1 + IndOffset[11]    
  H2[n] <- K2[n]/2 + muR2 + IndOffset[12]
  H3[n] <- K3[n]/2 + muR3 + IndOffset[13]
  H4[n] <- K4[n]/2 + muR4 + IndOffset[14]
  H5[n] <- K5[n]/2 + muR5 + IndOffset[15]

  I1[n] <- 0   
  I2[n] <- exp( muLI2 + IndOffset[17] )
  I3[n] <- exp( muLI3 + IndOffset[18] )
  I4[n] <- exp( muLI4 + IndOffset[19] )
  I5[n] <- exp( muLI5 + IndOffset[20] )

  # sims for group mean
  gQ1[n] <- exp( muLQ1 + GrpOffset[1] )
  gQ2[n] <- exp( muLQ2 + GrpOffset[2] )      
  gQ3[n] <- exp( muLQ3 + GrpOffset[3] )
  gQ4[n] <- exp( muLQ4 + GrpOffset[4] )      
  gQ5[n] <- exp( muLQ5 + GrpOffset[5] )

  gK1[n] <- exp( muLK1 + GrpOffset[6] )   
  gK2[n] <- exp( muLK2 + GrpOffset[7] )
  gK3[n] <- exp( muLK3 + GrpOffset[8] )
  gK4[n] <- exp( muLK4 + GrpOffset[9] )
  gK5[n] <- exp( muLK5 + GrpOffset[10] )

  gH1[n] <- gK1[n]/2 + muR1 + GrpOffset[11]    
  gH2[n] <- gK2[n]/2 + muR2 + GrpOffset[12]
  gH3[n] <- gK3[n]/2 + muR3 + GrpOffset[13]
  gH4[n] <- gK4[n]/2 + muR4 + GrpOffset[14]
  gH5[n] <- gK5[n]/2 + muR5 + GrpOffset[15]

  gI1[n] <- 0   
  gI2[n] <- exp( muLI2 + GrpOffset[17] )
  gI3[n] <- exp( muLI3 + GrpOffset[18] )
  gI4[n] <- exp( muLI4 + GrpOffset[19] )
  gI5[n] <- exp( muLI5 + GrpOffset[20] )

} # for n



colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.75)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)


colorlist2 <- hcl.colors(n=11, palette="Green-Brown",
                        alpha=0.75)
names(colorlist2) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist2)



pdf(file="./Plots/Prior_predict_m6_mh.pdf",
    height=10, width=10)
par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age
  

  ##### group-level variance

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels = FALSE )
  axis( side=2, at=seq( 0, 200, by=50 ) )
  #mtext("Age since conception (yrs)", side = 1, outer = T, cex = 1, line = 3, adj=2)
  mtext("Height (cm)", side = 2, outer = T, cex = 1.5, line = 1, adj=0.5)



  # plot possible group mean trajectories from this model given the priors
  for ( z in 1:Nsims ){

    # infant
    lines(x = x,
          y = ifelse( x > gI1[z], ( 2*gH1[z]/gK1[z] * ( 1 - exp(gK1[z]*gQ1[z]*( gI1[z] - x )/( 1 + 2*gQ1[z] )) ) )^(1/gQ1[z]), NA),
          col=colorlist["1.3"], lwd=0.25, lty=1)

    # child 1
    lines(x = x,
          y = ifelse( x > gI2[z], ( 2*gH2[z]/gK2[z] * ( 1 - exp(gK2[z]*gQ2[z]*( gI2[z] - x )/( 1 + 2*gQ2[z] )) ) )^(1/gQ2[z]), NA),
          col="plum", lwd=0.25, lty=1)

    # child 2
      lines(x = x,
          y = ifelse( x > gI3[z], ( 2*gH3[z]/gK3[z] * ( 1 - exp(gK3[z]*gQ3[z]*( gI3[z] - x )/( 1 + 2*gQ3[z] )) ) )^(1/gQ3[z]), NA),
          col=colorlist2["1.3"], lwd=0.25, lty=1)

    # child 3
    lines(x = x,
          y = ifelse( x > gI4[z], ( 2*gH4[z]/gK4[z] * ( 1 - exp(gK4[z]*gQ4[z]*( gI4[z] - x )/( 1 + 2*gQ4[z] )) ) )^(1/gQ4[z]), NA),
          col=colorlist2["2.3"], lwd=0.25, lty=1)

    # adolescent
    lines(x = x,
          y = ifelse( x > gI5[z], ( 2*gH5[z]/gK5[z] * ( 1 - exp(gK5[z]*gQ5[z]*( gI5[z] - x )/( 1 + 2*gQ5[z] )) ) )^(1/gQ5[z]), NA),
          col=colorlist["2.3"], lwd=0.25, lty=1)

  } #for z



  # baseline trajectory

  # infant
  lines(x = x,
        y = ifelse( x > muI1, ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1), NA),
          col=colorlist["1.1"], lwd=3, lty=1)

  # child 1
  lines(x = x,
        y = ifelse( x > muI2, ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2), NA),
        col="plum4", lwd=3, lty=1)

  # child 2
  lines(x = x,
        y = ifelse( x > muI3, ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3), NA),
        col=colorlist2["1.1"], lwd=3, lty=1)

  # child 3
  lines(x = x,
        y = ifelse( x > muI4, ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4), NA),
        col=colorlist2["2.1"], lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ifelse( x > muI5, ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5), NA),
        col=colorlist["2.1"], lwd=3, lty=1)

  

  text(1, 190, "Prior group-level variance", cex = 1.35, adj=0)



  #### cumulative trajectories

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels = FALSE )
  axis( side=2, at=seq( 0, 200, by=50 ), labels = FALSE )



  # plot possible group mean trajectories from this model given the priors
  for ( z in 1:Nsims ){


    lines(x = x,
          y = ifelse( x <= gI2[z], 0.012 + ( 2*gH1[z]/gK1[z] * ( 1 - exp(gK1[z]*gQ1[z]*( gI1[z] - x )/( 1 + 2*gQ1[z] )) ) )^(1/gQ1[z]),

              ifelse( x <= gI3[z], 0.012 + ( 2*gH1[z]/gK1[z] * ( 1 - exp(gK1[z]*gQ1[z]*( gI1[z] - x )/( 1 + 2*gQ1[z] )) ) )^(1/gQ1[z]) +
                                           ( 2*gH2[z]/gK2[z] * ( 1 - exp(gK2[z]*gQ2[z]*( gI2[z] - x )/( 1 + 2*gQ2[z] )) ) )^(1/gQ2[z]),

              ifelse( x <= gI4[z], 0.012 + ( 2*gH1[z]/gK1[z] * ( 1 - exp(gK1[z]*gQ1[z]*( gI1[z] - x )/( 1 + 2*gQ1[z] )) ) )^(1/gQ1[z]) +
                                           ( 2*gH2[z]/gK2[z] * ( 1 - exp(gK2[z]*gQ2[z]*( gI2[z] - x )/( 1 + 2*gQ2[z] )) ) )^(1/gQ2[z]) +
                                           ( 2*gH3[z]/gK3[z] * ( 1 - exp(gK3[z]*gQ3[z]*( gI3[z] - x )/( 1 + 2*gQ3[z] )) ) )^(1/gQ3[z]),

              ifelse( x <= gI5[z], 0.012 + ( 2*gH1[z]/gK1[z] * ( 1 - exp(gK1[z]*gQ1[z]*( gI1[z] - x )/( 1 + 2*gQ1[z] )) ) )^(1/gQ1[z]) +
                                           ( 2*gH2[z]/gK2[z] * ( 1 - exp(gK2[z]*gQ2[z]*( gI2[z] - x )/( 1 + 2*gQ2[z] )) ) )^(1/gQ2[z]) +
                                           ( 2*gH3[z]/gK3[z] * ( 1 - exp(gK3[z]*gQ3[z]*( gI3[z] - x )/( 1 + 2*gQ3[z] )) ) )^(1/gQ3[z]) +
                                           ( 2*gH4[z]/gK4[z] * ( 1 - exp(gK4[z]*gQ4[z]*( gI4[z] - x )/( 1 + 2*gQ4[z] )) ) )^(1/gQ4[z]),

                                   0.012 + ( 2*gH1[z]/gK1[z] * ( 1 - exp(gK1[z]*gQ1[z]*( gI1[z] - x )/( 1 + 2*gQ1[z] )) ) )^(1/gQ1[z]) +
                                           ( 2*gH2[z]/gK2[z] * ( 1 - exp(gK2[z]*gQ2[z]*( gI2[z] - x )/( 1 + 2*gQ2[z] )) ) )^(1/gQ2[z]) +
                                           ( 2*gH3[z]/gK3[z] * ( 1 - exp(gK3[z]*gQ3[z]*( gI3[z] - x )/( 1 + 2*gQ3[z] )) ) )^(1/gQ3[z]) +
                                           ( 2*gH4[z]/gK4[z] * ( 1 - exp(gK4[z]*gQ4[z]*( gI4[z] - x )/( 1 + 2*gQ4[z] )) ) )^(1/gQ4[z]) +
                                           ( 2*gH5[z]/gK5[z] * ( 1 - exp(gK5[z]*gQ5[z]*( gI5[z] - x )/( 1 + 2*gQ5[z] )) ) )^(1/gQ5[z])
              ) ) ) ),
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  # baseline trajectory
    lines(x = x,
          y = ifelse( x <= muI2, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1),

              ifelse( x <= muI3, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2),

              ifelse( x <= muI4, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                         ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3),

              ifelse( x <= muI5, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                         ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3) +
                                         ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4),

                                 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                         ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3) +
                                         ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4) +
                                         ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5)
              ) ) ) ),
          col="black", lwd=3, lty=1)





  # Individual-level variance


  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )
  axis( side=1, at=seq( 0, 25, by=5 ) )
  axis( side=2, at=seq( 0, 200, by=50 ) )
  mtext("Age since conception (yrs)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)
  #mtext("Height (cm)", side = 2, outer = T, cex = 1, line = 3, adj=0.5)


  # plot possible individual trajectories from this model given the priors
  for ( z in 1:Nsims ){

    # infant
    lines(x = x,
          y = ifelse( x > I1[z], ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]), NA),
          col=colorlist["1.3"], lwd=0.25, lty=1)

    # child 1
    lines(x = x,
          y = ifelse( x > I2[z], ( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z]), NA),
          col="plum", lwd=0.25, lty=1)

    # child 2
      lines(x = x,
          y = ifelse( x > I3[z], ( 2*H3[z]/K3[z] * ( 1 - exp(K3[z]*Q3[z]*( I3[z] - x )/( 1 + 2*Q3[z] )) ) )^(1/Q3[z]), NA),
          col=colorlist2["1.3"], lwd=0.25, lty=1)

    # child 3
    lines(x = x,
          y = ifelse( x > I4[z], ( 2*H4[z]/K4[z] * ( 1 - exp(K4[z]*Q4[z]*( I4[z] - x )/( 1 + 2*Q4[z] )) ) )^(1/Q4[z]), NA),
          col=colorlist2["2.3"], lwd=0.25, lty=1)

    # adolescent
    lines(x = x,
          y = ifelse( x > I5[z], ( 2*H5[z]/K5[z] * ( 1 - exp(K5[z]*Q5[z]*( I5[z] - x )/( 1 + 2*Q5[z] )) ) )^(1/Q5[z]), NA),
          col=colorlist["2.3"], lwd=0.25, lty=1)

  } #for z



  # baseline trajectory

  # infant
  lines(x = x,
        y = ifelse( x > muI1, ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1), NA),
          col=colorlist["1.1"], lwd=3, lty=1)

  # child 1
  lines(x = x,
        y = ifelse( x > muI2, ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2), NA),
        col="plum4", lwd=3, lty=1)

  # child 2
  lines(x = x,
        y = ifelse( x > muI3, ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3), NA),
        col=colorlist2["1.1"], lwd=3, lty=1)

  # child 3
  lines(x = x,
        y = ifelse( x > muI4, ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4), NA),
        col=colorlist2["2.1"], lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ifelse( x > muI5, ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5), NA),
        col=colorlist["2.1"], lwd=3, lty=1)


  text(1, 190, "Prior individual-level variance", cex = 1.35, adj=0)



  #### cumulative trajectories

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ) )
  axis( side=2, at=seq( 0, 200, by=50 ), labels = FALSE )


  # plot possible individual trajectories from this model given the priors
  for ( z in 1:Nsims ){

    lines(x = x,
          y = ifelse( x <= I2[z], 0.012 + ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]),

              ifelse( x <= I3[z], 0.012 + ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]) +
                                          ( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z]),

              ifelse( x <= I4[z], 0.012 + ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]) +
                                          ( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z]) +
                                          ( 2*H3[z]/K3[z] * ( 1 - exp(K3[z]*Q3[z]*( I3[z] - x )/( 1 + 2*Q3[z] )) ) )^(1/Q3[z]),

              ifelse( x <= I5[z], 0.012 + ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]) +
                                          ( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z]) +
                                          ( 2*H3[z]/K3[z] * ( 1 - exp(K3[z]*Q3[z]*( I3[z] - x )/( 1 + 2*Q3[z] )) ) )^(1/Q3[z]) +
                                          ( 2*H4[z]/K4[z] * ( 1 - exp(K4[z]*Q4[z]*( I4[z] - x )/( 1 + 2*Q4[z] )) ) )^(1/Q4[z]),

                                  0.012 + ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]) +
                                          ( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z]) +
                                          ( 2*H3[z]/K3[z] * ( 1 - exp(K3[z]*Q3[z]*( I3[z] - x )/( 1 + 2*Q3[z] )) ) )^(1/Q3[z]) +
                                          ( 2*H4[z]/K4[z] * ( 1 - exp(K4[z]*Q4[z]*( I4[z] - x )/( 1 + 2*Q4[z] )) ) )^(1/Q4[z]) +
                                          ( 2*H5[z]/K5[z] * ( 1 - exp(K5[z]*Q5[z]*( I5[z] - x )/( 1 + 2*Q5[z] )) ) )^(1/Q5[z])
              ) ) ) ),
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  # baseline trajectory
    lines(x = x,
          y = ifelse( x <= muI2, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1),

              ifelse( x <= muI3, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2),

              ifelse( x <= muI4, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                         ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3),

              ifelse( x <= muI5, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                         ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3) +
                                         ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4),

                                 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                         ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                         ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3) +
                                         ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4) +
                                         ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5)
              ) ) ) ),
          col="black", lwd=3, lty=1)




graphics.off()


