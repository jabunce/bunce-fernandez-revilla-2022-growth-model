 

##### prior predictive simulations for composite model, one group


Nsims <- 100 # number of trajectories to simulate 

#means defining the baseline trajectory, chosen to make trajectory look reasonable
muQ1 <- 0.08062444 #0.08637589 #0.09691564 #0.08838383 ##0.08631607  
muQ2 <- 0.1563793 #0.1649906 #0.1854612 #0.1701386 ##0.1658155   
muQ3 <- 0.1866936 #0.179 #0.303743 #0.1991625 ##0.2082999   
muQ4 <- 0.04793841 #0.0543 #0.04376139 #0.05003629 ##0.3358043  
muQ5 <- 0.1116389 #0.126 #0.1094167 #0.1158356 ##0.4211652 

muK1 <- 84.80028 #75.36287 #73.5461 #69.26481 ##74.50596  
muK2 <- 12.43408 #12.0736 #11.38923 #9.749453 ##12.2533  
muK3 <- 3.289623 #3.48 #1.830075 #2.802159 ##3.928223  
muK4 <- 9.161336 #7.9 #10.15139 #8.473641 ##1.517058    
muK5 <- 7.654668 #6.42 #7.927593 #6.974529 ##2.713713 

muH1 <- 59.00294 #53.58602 #54.4514 #50.00313 ##53.16193
muH2 <- 10.50578 #10.22789 #10.59861 #8.616612 ##10.30698
muH3 <- 3.269981 #3.3 #2.972541 #2.899098 ##4.048139
muH4 <- 5.386189 #4.75 #5.834366 #4.991565 ##2.655676
muH5 <- 5.441465 #4.81 #5.524491 #5.130509 ##4.973799  

muI1 <- 0
muI2 <- 0.1226996 #0.1241712 #0.1213675 #0.1261745 ##0.1248041    
muI3 <- 0.7190094 #0.75 #0.765 #0.8543406 #0.7586653 ##0.6130044 
muI4 <- 2.163776 #1.75 #5 #1.88 #2.212005 #1.81026 ##4.664357  
muI5 <- 10.80519 #10.6 #10.83705 #10.52904 ##12.00318 
muIM <- 20 # max age for start of last process


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

# offset to the base trajectory for the mean (across individuals) trajectory of the ethnic group. Make these priors as weak as possible.
sdLQ1 <- 0.0005 #0.001 #0.005 #0.01 #0.001 #0.002 #0.005 #0.001 #0.0001
sdLQ2 <- 0.0005 #0.001 #0.005 #0.01 #0.001 #0.002 #0.005 #0.001 #0.0001      
sdLQ3 <- 0.0005 #0.001 #0.005 #0.01 #0.001 #0.002 #0.005 #0.001 #0.0001  
sdLQ4 <- 0.0005 #0.001 #0.005 #0.01 #0.001 #0.002 #0.005 #0.001 #0.0001       
sdLQ5 <- 0.0005 #0.001 #0.005 #0.01 #0.001 #0.002 #0.005 #0.001 #0.0001 

sdLK1 <- 0.0005 #0.001 #0.005 #0.01 #0.001 #0.002 #0.005 #0.01 #0.001      
sdLK2 <- 0.0005 #0.001 #0.005 #0.01 #0.001 #0.002 #0.005 #0.01 #0.001  
sdLK3 <- 0.0005 #0.001 #0.005 #0.01 #0.001 #0.002 #0.005 #0.01 #0.001  
sdLK4 <- 0.0005 #0.001 #0.005 #0.01 #0.001 #0.002 #0.005 #0.01 #0.001
sdLK5 <- 0.0005 #0.001 #0.005 #0.01 #0.001 #0.002 #0.005 #0.01 #0.001    

sdR1 <- 0.0005 #0.01 #0.1 #1 #5 #0.001         
sdR2 <- 0.0005 #0.001 #0.01 #0.1 #1 #0.001 
sdR3 <- 0.0005 #0.001 #0.005 #0.05 #0.5 #0.001   
sdR4 <- 0.0005 #0.001 #0.005 #0.05 #0.5 #0.001  
sdR5 <- 0.0005 #0.001 #0.005 #0.05 #0.5 #0.001

sdLI1 <- 0.000000001     
sdLI2 <- 0.0005 #0.001 #0.005 #0.001 #0.01 #0.001 #0.002 #0.005 #0.01 #0.001 
sdLI3 <- 0.0005 #0.001 #0.005 #0.001 #0.01 #0.001 #0.002 #0.005 #0.01 #0.001    
sdLI4 <- 0.0005 #0.001 #0.005 #0.001 #0.01 #0.001 #0.002 #0.005 #0.01 #0.001  
sdLI5 <- 0.0005 #0.001 #0.005 #0.001 #0.01 #0.001 #0.002 #0.005 #0.01 #0.001    
sdLIM <- 0.000000001


# stdev on original scale = [( e^(sdL^2) - 1 )*e^( 2*mu + sdL^2 )]^0.5
sdQ1 <- (( exp(sdLQ1^2) - 1 )*exp( 2*muLQ1 + sdLQ1^2 ))^0.5    #0.094
sdQ2 <- (( exp(sdLQ2^2) - 1 )*exp( 2*muLQ2 + sdLQ2^2 ))^0.5    #0.22      
sdQ3 <- (( exp(sdLQ3^2) - 1 )*exp( 2*muLQ3 + sdLQ3^2 ))^0.5     
sdQ4 <- (( exp(sdLQ4^2) - 1 )*exp( 2*muLQ4 + sdLQ4^2 ))^0.5          
sdQ5 <- (( exp(sdLQ5^2) - 1 )*exp( 2*muLQ5 + sdLQ5^2 ))^0.5       

sdK1 <- (( exp(sdLK1^2) - 1 )*exp( 2*muLK1 + sdLK1^2 ))^0.5   #49.8      
sdK2 <- (( exp(sdLK2^2) - 1 )*exp( 2*muLK2 + sdLK2^2 ))^0.5   #6.49
sdK3 <- (( exp(sdLK3^2) - 1 )*exp( 2*muLK3 + sdLK3^2 ))^0.5   #2.41
sdK4 <- (( exp(sdLK4^2) - 1 )*exp( 2*muLK4 + sdLK4^2 ))^0.5   
sdK5 <- (( exp(sdLK5^2) - 1 )*exp( 2*muLK5 + sdLK5^2 ))^0.5    

sdH1 <- sdR1  #37.3      
sdH2 <- sdR2   #6.75
sdH3 <- sdR3   #2.61
sdH4 <- sdR4    
sdH5 <- sdR5     

sdI1 <- 0.00001      
sdI2 <- (( exp(sdLI2^2) - 1 )*exp( 2*muLI2 + sdLI2^2 ))^0.5    #0.125
sdI3 <- (( exp(sdLI3^2) - 1 )*exp( 2*muLI3 + sdLI3^2 ))^0.5    #0.75
sdI4 <- (( exp(sdLI4^2) - 1 )*exp( 2*muLI4 + sdLI4^2 ))^0.5      #3.75
sdI5 <- (( exp(sdLI5^2) - 1 )*exp( 2*muLI5 + sdLI5^2 ))^0.5      #10.75       
sdIM <- (( exp(sdLIM^2) - 1 )*exp( 2*muLIM + sdLIM^2 ))^0.5 #0.00001

muLsigma_h <- 0.002
musigma_h <- (( exp(muLsigma_h^2) - 1 )*exp( 2*log(100) + muLsigma_h^2 ))^0.5       # max of 0.005 :  95% of observations within 2*stdev = 1cm of actual height at 100cm
muLsigma_w <- 0.002
musigma_w <- (( exp(muLsigma_w^2) - 1 )*exp( 2*log(50000) + muLsigma_w^2 ))^0.5     # max of 0.005 : 95% observations within 0.5 kg of actual weight at 50kg


offsetGroupStdevs <- c( sdQ1, sdQ2, sdQ3, sdQ4, sdQ5,
                        sdK1, sdK2, sdK3, sdK4, sdK5,
                        sdH1, sdH2, sdH3, sdH4, sdH5,
                        sdI1, sdI2, sdI3, sdI4, sdI5) 



# holders for sims
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



for (n in 1:Nsims) {

  GrpOffset <- mvrnorm( n=1, mu=rep(0,length(offsetGroupStdevs)), Sigma=diag(offsetGroupStdevs) )  # assumes no covariance among parameters

  # sims for group mean
  Q1[n] <- muQ1 + GrpOffset[1]
  Q2[n] <- muQ2 + GrpOffset[2]       
  Q3[n] <- muQ3 + GrpOffset[3]
  Q4[n] <- muQ4 + GrpOffset[4]       
  Q5[n] <- muQ5 + GrpOffset[5]

  K1[n] <- muK1 + GrpOffset[6]     
  K2[n] <- muK2 + GrpOffset[7]
  K3[n] <- muK3 + GrpOffset[8]
  K4[n] <- muK4 + GrpOffset[9]
  K5[n] <- muK5 + GrpOffset[10]

  H1[n] <- muH1 + GrpOffset[11]    
  H2[n] <- muH2 + GrpOffset[12]
  H3[n] <- muH3 + GrpOffset[13]
  H4[n] <- muH4 + GrpOffset[14]
  H5[n] <- muH5 + GrpOffset[15]

  I1[n] <- muI1 + GrpOffset[16]    
  I2[n] <- muI2 + GrpOffset[17]
  I3[n] <- muI3 + GrpOffset[18]
  I4[n] <- muI4 + GrpOffset[19]
  I5[n] <- muI5 + GrpOffset[20]


  # # q, H, K, and I cannot be < 0

  if ( Q1[n] < 0 ) { Q1[n] <- -1*Q1[n] }
  if ( Q2[n] < 0 ) { Q2[n] <- -1*Q2[n] }
  if ( Q3[n] < 0 ) { Q3[n] <- -1*Q3[n] }
  if ( Q4[n] < 0 ) { Q4[n] <- -1*Q4[n] }
  if ( Q5[n] < 0 ) { Q5[n] <- -1*Q5[n] }

  if ( K1[n] < 0 ) { K1[n] <- -1*K1[n] }
  if ( K2[n] < 0 ) { K2[n] <- -1*K2[n] }
  if ( K3[n] < 0 ) { K3[n] <- -1*K3[n] }
  if ( K4[n] < 0 ) { K4[n] <- -1*K4[n] }
  if ( K5[n] < 0 ) { K5[n] <- -1*K5[n] }

  if ( H1[n] < 0 ) { H1[n] <- -1*H1[n] }
  if ( H2[n] < 0 ) { H2[n] <- -1*H2[n] }
  if ( H3[n] < 0 ) { H3[n] <- -1*H3[n] }
  if ( H4[n] < 0 ) { H4[n] <- -1*H4[n] }
  if ( H5[n] < 0 ) { H5[n] <- -1*H5[n] }

  if ( I1[n] < 0 ) { I1[n] <- -1*I1[n] }
  if ( I2[n] < 0 ) { I2[n] <- -1*I2[n] }
  if ( I3[n] < 0 ) { I3[n] <- -1*I3[n] }
  if ( I4[n] < 0 ) { I4[n] <- -1*I4[n] }
  if ( I5[n] < 0 ) { I5[n] <- -1*I5[n] }

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




pdf(file="./Plots/Prior_predict_5h.pdf",
    height=5, width=8)
par(mfrow = c(1, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  # domain sequence for plotting
  x <- seq(from=0, to=(25+0.75), by=0.1)


  #### component trajectories

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26) ) #, xlab="Age since conception (yrs)", ylab="Height (cm)" )
  axis( side=1, at=seq( 0, 25, by=5 ) )
  axis( side=2, at=seq( 0, 200, by=50 ) )
  mtext("Age since conception (yrs)", side = 1, outer = T, cex = 1, line = 1, adj=0.5)
  mtext("Height (cm)", side = 2, outer = T, cex = 1, line = 2, adj=0.5)


  # plot possible individual trajectories from this model given the priors
  for ( z in 1:Nsims ){

    # 1
    lines(x = x,
          y = ifelse( x >= I1[z], ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]), NA),
          col=colorlist["1.3"], lwd=0.25, lty=1)

    # 2
    lines(x = x,
          y = ifelse( x >= I2[z], ( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z]), NA),
          col="plum", lwd=0.25, lty=1)

    # 3
      lines(x = x,
          y = ifelse( x >= I3[z], ( 2*H3[z]/K3[z] * ( 1 - exp(K3[z]*Q3[z]*( I3[z] - x )/( 1 + 2*Q3[z] )) ) )^(1/Q3[z]), NA),
          col=colorlist2["1.3"], lwd=0.25, lty=1)

    # 4
    lines(x = x,
          y = ifelse( x >= I4[z], ( 2*H4[z]/K4[z] * ( 1 - exp(K4[z]*Q4[z]*( I4[z] - x )/( 1 + 2*Q4[z] )) ) )^(1/Q4[z]), NA),
          col=colorlist2["2.3"], lwd=0.25, lty=1)

    # 5
    lines(x = x,
          y = ifelse( x >= I5[z], ( 2*H5[z]/K5[z] * ( 1 - exp(K5[z]*Q5[z]*( I5[z] - x )/( 1 + 2*Q5[z] )) ) )^(1/Q5[z]), NA),
          col=colorlist["2.3"], lwd=0.25, lty=1)

  } #for z


  # mean component trajectories

  # 1
  lines(x = x,
        y = ifelse( x >= muI1, ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1), NA),
          col=colorlist["1.1"], lwd=3, lty=1)

  # 2
  lines(x = x,
        y = ifelse( x >= muI2, ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2), NA),
        col="plum4", lwd=3, lty=1)

  # 3
  lines(x = x,
        y = ifelse( x >= muI3, ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3), NA),
        col=colorlist2["1.1"], lwd=3, lty=1)

  #4
  lines(x = x,
        y = ifelse( x >= muI4, ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4), NA),
        col=colorlist2["2.1"], lwd=3, lty=1)

  # 5
  lines(x = x,
        y = ifelse( x >= muI5, ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5), NA),
        col=colorlist["2.1"], lwd=3, lty=1)




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
          y = ifelse( x < I2[z], 0.012 + ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]),

              ifelse( x < I3[z], 0.012 + ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]) +
                                         ( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z]),

              ifelse( x < I4[z], 0.012 + ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]) +
                                         ( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z]) +
                                         ( 2*H3[z]/K3[z] * ( 1 - exp(K3[z]*Q3[z]*( I3[z] - x )/( 1 + 2*Q3[z] )) ) )^(1/Q3[z]),

              ifelse( x < I5[z], 0.012 + ( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z]) +
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


  # mean trajectory

    lines(x = x,
          y = ifelse( x < muI2, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1),

              ifelse( x < muI3, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                        ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2),

              ifelse( x < muI4, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                        ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                        ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3),

              ifelse( x < muI5, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
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







pdf(file="./Plots/Prior_predict_5w.pdf",
    height=5, width=8)
par(mfrow = c(1, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #### component trajectories

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,4000), xlim=c(0,26), axes=F ) #, xlab="Age since conception (yrs)", ylab="Height (cm)" )
  axis( side=1, at=seq( 0, 25, by=5 ) )
  axis( side=2, at=seq( 0, 4000, by=500 ) )
  mtext("Age since conception (yrs)", side = 1, outer = T, cex = 1, line = 1, adj=0.5)
  mtext("Weight (g)", side = 2, outer = T, cex = 1, line = 2, adj=0.5)


  # plot possible individual trajectories from this model given the priors
  for ( z in 1:Nsims ){

    # 1
    lines(x = x,
          y = ifelse( x > I1[z], pi*( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z] + 2), NA),
          col=colorlist["1.3"], lwd=0.25, lty=1)

    # 2
    lines(x = x,
          y = ifelse( x > I2[z], pi*( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z] + 2), NA),
          col="plum", lwd=0.25, lty=1)

    # 3
    lines(x = x,
          y = ifelse( x > I3[z], pi*( 2*H3[z]/K3[z] * ( 1 - exp(K3[z]*Q3[z]*( I3[z] - x )/( 1 + 2*Q3[z] )) ) )^(1/Q3[z] + 2), NA),
          col=colorlist2["1.3"], lwd=0.25, lty=1)

    # 4
    lines(x = x,
          y = ifelse( x > I4[z], pi*( 2*H4[z]/K4[z] * ( 1 - exp(K4[z]*Q4[z]*( I4[z] - x )/( 1 + 2*Q4[z] )) ) )^(1/Q4[z] + 2), NA),
          col=colorlist2["2.3"], lwd=0.25, lty=1)

    # 5
    lines(x = x,
          y = ifelse( x > I5[z], pi*( 2*H5[z]/K5[z] * ( 1 - exp(K5[z]*Q5[z]*( I5[z] - x )/( 1 + 2*Q5[z] )) ) )^(1/Q5[z] + 2), NA),
          col=colorlist["2.3"], lwd=0.25, lty=1)

  } #for z


  # mean component trajectories

  # 1
  lines(x = x,
        y = ifelse( x > muI1, pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2), NA),
        col=colorlist["1.1"], lwd=3, lty=1)

  # 2
  lines(x = x,
        y = ifelse( x > muI2, pi*( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2 + 2), NA),
        col="plum4", lwd=3, lty=1)

  # 3
  lines(x = x,
        y = ifelse( x > muI3, pi*( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3 + 2), NA),
        col=colorlist2["1.1"], lwd=3, lty=1)

  # 4
  lines(x = x,
        y = ifelse( x > muI4, pi*( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4 + 2), NA),
        col=colorlist2["2.1"], lwd=3, lty=1)

  # 5
  lines(x = x,
        y = ifelse( x > muI5, pi*( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5 + 2), NA),
        col=colorlist["2.1"], lwd=3, lty=1)



  #### cumulative trajectories

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,4000), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ) )
  axis( side=2, at=seq( 0, 4000, by=500 ), labels = FALSE )

  # plot possible individual trajectories from this model given the priors
  for ( z in 1:Nsims ){


    lines(x = x,
          y = ifelse( x < I2[z], 1.02e-6 + pi*( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z] + 2),

              ifelse( x < I3[z], 1.02e-6 + pi*( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z] + 2) +
                                          pi*( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z] + 2),

              ifelse( x < I4[z], 1.02e-6 + pi*( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z] + 2) +
                                          pi*( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z] + 2) +
                                          pi*( 2*H3[z]/K3[z] * ( 1 - exp(K3[z]*Q3[z]*( I3[z] - x )/( 1 + 2*Q3[z] )) ) )^(1/Q3[z] + 2),

              ifelse( x < I5[z], 1.02e-6 + pi*( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z] + 2) +
                                          pi*( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z] + 2) +
                                          pi*( 2*H3[z]/K3[z] * ( 1 - exp(K3[z]*Q3[z]*( I3[z] - x )/( 1 + 2*Q3[z] )) ) )^(1/Q3[z] + 2) +
                                          pi*( 2*H4[z]/K4[z] * ( 1 - exp(K4[z]*Q4[z]*( I4[z] - x )/( 1 + 2*Q4[z] )) ) )^(1/Q4[z] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*H1[z]/K1[z] * ( 1 - exp(K1[z]*Q1[z]*( I1[z] - x )/( 1 + 2*Q1[z] )) ) )^(1/Q1[z] + 2) +
                                          pi*( 2*H2[z]/K2[z] * ( 1 - exp(K2[z]*Q2[z]*( I2[z] - x )/( 1 + 2*Q2[z] )) ) )^(1/Q2[z] + 2) +
                                          pi*( 2*H3[z]/K3[z] * ( 1 - exp(K3[z]*Q3[z]*( I3[z] - x )/( 1 + 2*Q3[z] )) ) )^(1/Q3[z] + 2) +
                                          pi*( 2*H4[z]/K4[z] * ( 1 - exp(K4[z]*Q4[z]*( I4[z] - x )/( 1 + 2*Q4[z] )) ) )^(1/Q4[z] + 2) +
                                          pi*( 2*H5[z]/K5[z] * ( 1 - exp(K5[z]*Q5[z]*( I5[z] - x )/( 1 + 2*Q5[z] )) ) )^(1/Q5[z] + 2) 

              ) ) ) ), 
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  # mean trajectory

    lines(x = x,
          y = ifelse( x < muI2, 1.02e-6 + pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2),

              ifelse( x < muI3, 1.02e-6 + pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2) +
                                         pi*( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2 + 2),

              ifelse( x < muI4, 1.02e-6 + pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2) +
                                         pi*( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2 + 2) +
                                         pi*( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3 + 2),

              ifelse( x < muI5, 1.02e-6 + pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2) +
                                         pi*( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2 + 2) +
                                         pi*( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3 + 2) +
                                         pi*( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4 + 2),                                                                                                                                                            

                                1.02e-6 + pi*( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( muI1 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1 + 2) +
                                         pi*( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI2 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2 + 2) +
                                         pi*( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI3 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3 + 2) +
                                         pi*( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI4 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4 + 2) +
                                         pi*( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI5 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5 + 2) 

              ) ) ) ), 
          col="black", lwd=3, lty=1)

graphics.off()



