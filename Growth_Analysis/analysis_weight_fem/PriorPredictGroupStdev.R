

##### prior predictive simulations from composite model, multiple groups


Nsims <- 100 # number of trajectories to simulate 

#means of posterior parameter estimates from m2, defining the baseline trajectory
LQ1hat <- mean(post2$mLQ1)
LQ2hat <- mean(post2$mLQ2)        
LQ3hat <- mean(post2$mLQ3) 
LK1hat <- mean(post2$mLK1)       
LK2hat <- mean(post2$mLK2) 
LK3hat <- mean(post2$mLK3) 
R1hat <- mean(post2$mR1) 
R2hat <- mean(post2$mR2) 
R3hat <- mean(post2$mR3)    


# holders for sims for individuals
LQ1 <- rep(0,Nsims)
LQ2 <- rep(0,Nsims)
LQ3 <- rep(0,Nsims)
LK1 <- rep(0,Nsims)
LK2 <- rep(0,Nsims)
LK3 <- rep(0,Nsims)
R1 <- rep(0,Nsims)
R2 <- rep(0,Nsims)
R3 <- rep(0,Nsims)


# holders for sims for group mean
gLQ1 <- rep(0,Nsims)
gLQ2 <- rep(0,Nsims)
gLQ3 <- rep(0,Nsims)
gLK1 <- rep(0,Nsims)
gLK2 <- rep(0,Nsims)
gLK3 <- rep(0,Nsims)
gR1 <- rep(0,Nsims)
gR2 <- rep(0,Nsims)
gR3 <- rep(0,Nsims)



for (n in 1:Nsims) {

  # offsets to the mean group trajectory for each individual, stdev's taken from posterior stdevs from m2
  # imLQ1 <- mean(post2$"sigmaInd[1]")       # means for normal distribution for stdevs
  # imLQ2 <- mean(post2$"sigmaInd[2]")          
  # imLQ3 <- mean(post2$"sigmaInd[3]") 
  # imLK1 <- mean(post2$"sigmaInd[4]")       
  # imLK2 <- mean(post2$"sigmaInd[5]")  
  # imLK3 <- mean(post2$"sigmaInd[6]")  
  # imR1 <- mean(post2$"sigmaInd[7]")       
  # imR2 <- mean(post2$"sigmaInd[8]") 
  # imR3 <- mean(post2$"sigmaInd[9]") 

  imLQ1 <- 0.0005 
  imLQ2 <- 0.001        
  imLQ3 <- 0.001
  imLK1 <- 0.0005     
  imLK2 <- 0.001
  imLK3 <- 0.001
  imR1 <-  0.001      
  imR2 <-  0.001
  imR3 <-  0.001

  isLQ1 <- 0.0005 #0.005     # stdevs for normal distribution
  isLQ2 <- 0.005          
  isLQ3 <- 0.005  
  isLK1 <- 0.0005 #0.005        
  isLK2 <- 0.005  
  isLK3 <- 0.005  
  isR1 <-  0.005       
  isR2 <-  0.005 
  isR3 <-  0.005 

  # sample (a stdev) from a distribution with the above means and stdevs
  isdLQ1 <- rnorm( n=1, mean=imLQ1, sd=isLQ1 )
  isdLQ2 <- rnorm( n=1, mean=imLQ2, sd=isLQ2 )        
  isdLQ3 <- rnorm( n=1, mean=imLQ3, sd=isLQ3 )
  isdLK1 <- rnorm( n=1, mean=imLK1, sd=isLK1 )      
  isdLK2 <- rnorm( n=1, mean=imLK2, sd=isLK2 )
  isdLK3 <- rnorm( n=1, mean=imLK3, sd=isLK3 )
  isdR1 <- rnorm( n=1, mean=imR1, sd=isR1 )      
  isdR2 <- rnorm( n=1, mean=imR2, sd=isR2 )
  isdR3 <- rnorm( n=1, mean=imR3, sd=isR3 ) 

  offsetIndivStdevs <- c( isdLQ1, isdLQ2, isdLQ3,
                          isdLK1, isdLK2, isdLK3,
                          isdR1, isdR2, isdR3 )

  s <- diag(offsetIndivStdevs)
  CholEtaR <- 25 #8
  R <- rlkjcorr( n=1, K=length(offsetIndivStdevs), eta=CholEtaR )  # eta=1, all coorelations equally likely; eta>1, extreme correlations less likely 
  S <- s %*% R %*% s # vcov matrix with variances (s^2) on the diagonal

  IndOffset <- mvrnorm( n=1, mu=rep(0,length(offsetIndivStdevs)), Sigma=S )



  # mean (across indivs) offsets to the baseline group mean trajectory
  gexpLQ1 <- 1/imLQ1
  gexpLQ2 <- 1/imLQ2         
  gexpLQ3 <- 1/imLQ3 
  gexpLK1 <- 1/imLK1       
  gexpLK2 <- 1/imLK2 
  gexpLK3 <- 1/imLK3 
  gexpR1 <- 1/imR1        
  gexpR2 <- 1/imR2
  gexpR3 <- 1/imR3 

  gsdLQ1 <- rexp( n=1, rate=gexpLQ1 )      # mean of exponential distribution = 1/rate
  gsdLQ2 <- rexp( n=1, rate=gexpLQ2 )        
  gsdLQ3 <- rexp( n=1, rate=gexpLQ3 )
  gsdLK1 <- rexp( n=1, rate=gexpLK1 )      
  gsdLK2 <- rexp( n=1, rate=gexpLK2 )
  gsdLK3 <- rexp( n=1, rate=gexpLK3 )
  gsdR1 <- rexp( n=1, rate=gexpR1 )      
  gsdR2 <- rexp( n=1, rate=gexpR2 )
  gsdR3 <- rexp( n=1, rate=gexpR3 )  

  offsetGroupStdevs <- c( gsdLQ1, gsdLQ2, gsdLQ3,
                          gsdLK1, gsdLK2, gsdLK3,
                          gsdR1, gsdR2, gsdR3 )

  t <- diag(offsetGroupStdevs)
  CholEtaU <- 4 #2
  U <- rlkjcorr( n=1, K=length(offsetGroupStdevs), eta=CholEtaU )  # eta=1, all coorelations equally likely; eta>1, extreme correlations less likely 
  T <- t %*% U %*% t # vcov matrix with variances (t^2) on the diagonal

  GrpOffset <- mvrnorm( n=1, mu=rep(0,length(offsetGroupStdevs)), Sigma=T )


  # sims for individuals
  LQ1[n] <- LQ1hat + IndOffset[1]
  LQ2[n] <- LQ2hat + IndOffset[2]       
  LQ3[n] <- LQ3hat + IndOffset[3]
  LK1[n] <- LK1hat + IndOffset[4]     
  LK2[n] <- LK2hat + IndOffset[5]
  LK3[n] <- LK3hat + IndOffset[6]
  R1[n] <- R1hat + IndOffset[7]    
  R2[n] <- R2hat + IndOffset[8]
  R3[n] <- R3hat + IndOffset[9]

  # LQ1[n] <- LQ1hat + IndOffset[1] + GrpOffset[1]
  # LQ2[n] <- LQ2hat + IndOffset[2] + GrpOffset[2]       
  # LQ3[n] <- LQ3hat + IndOffset[3] + GrpOffset[3]
  # LK1[n] <- LK1hat + IndOffset[4] + GrpOffset[4]    
  # LK2[n] <- LK2hat + IndOffset[5] + GrpOffset[5]
  # LK3[n] <- LK3hat + IndOffset[6] + GrpOffset[6]
  # R1[n] <- R1hat + IndOffset[7] + GrpOffset[7]    
  # R2[n] <- R2hat + IndOffset[8] + GrpOffset[8] 
  # R3[n] <- R3hat + IndOffset[9] + GrpOffset[9]

  # sims for group means
  gLQ1[n] <- LQ1hat + GrpOffset[1]
  gLQ2[n] <- LQ2hat + GrpOffset[2]       
  gLQ3[n] <- LQ3hat + GrpOffset[3]
  gLK1[n] <- LK1hat + GrpOffset[4]     
  gLK2[n] <- LK2hat + GrpOffset[5]
  gLK3[n] <- LK3hat + GrpOffset[6]
  gR1[n] <- R1hat + GrpOffset[7]    
  gR2[n] <- R2hat + GrpOffset[8]
  gR3[n] <- R3hat + GrpOffset[9]

} # for n



pdf(file="./Plots/Prior_predict_m3_fw.pdf",
    height=10, width=10)
par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age: from 0 = starting at conception, from 0.75 = starting at birth
  x2 <- seq(from=(0), to=(25+0.75), by=0.1)       # look at negative exponential all the way back to conception
  

  ##### group-level variance

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,80), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels = FALSE )
  axis( side=2, at=seq( 0, 80, by=20 ) )
  #mtext("Age since conception (yrs)", side = 1, outer = T, cex = 1, line = 3, adj=2)
  mtext("Weight (kg)", side = 2, outer = T, cex = 1.5, line = 3, adj=-0.28)


  # plot possible group mean trajectories from this model given the priors
  for ( z in 1:Nsims ){

    gQ1 <- exp(gLQ1[z])
    gQ2 <- exp(gLQ2[z])
    gQ3 <- exp(gLQ3[z])
    gK1 <- exp(gLK1[z])
    gK2 <- exp(gLK2[z])
    gK3 <- exp(gLK3[z])
    gH1 <- gK1/2 + gR1[z]
    gH2 <- gK2/2 + gR2[z]
    gH3 <- gK3/2 + gR3[z]

    # # composite trajectory
    # lines(x = x,
    #       y = ( -1*exp(-1*gK1*gQ1*x/( 1 + 2*gQ1 )) + 2*gH1/gK1 )^(1/gQ1) +
    #           ( -1*exp(-1*gK2*gQ2*x/( 1 + 2*gQ2 )) + 2*gH2/gK2 )^(1/gQ2) +
    #           ( -1*exp(-1*gK3*gQ3*x/( 1 + 2*gQ3 )) + 2*gH3/gK3 )^(1/gQ3),
    #           col=grey(0.5), lwd=0.25, lty=1)

    # infant
    lines(x = x2,
          y = ( -1*exp(-1*gK1*gQ1*x2/( 1 + 2*gQ1 )) + 2*gH1/gK1*pi^(gQ1/( 1 + 2*gQ1 )) )^(( 1 + 2*gQ1 )/gQ1),
              col="red", lwd=0.25, lty=1)

    # child
    lines(x = x,
          y = ( -1*exp(-1*gK2*gQ2*x/( 1 + 2*gQ2 )) + 2*gH2/gK2*pi^(gQ2/( 1 + 2*gQ2 )) )^(( 1 + 2*gQ2 )/gQ2),
          col="blue", lwd=0.25, lty=1)

    # adolescent
    lines(x = x,
          y = ( -1*exp(-1*gK3*gQ3*x/( 1 + 2*gQ3 )) + 2*gH3/gK3*pi^(gQ3/( 1 + 2*gQ3 )) )^(( 1 + 2*gQ3 )/gQ3),
              col="green", lwd=0.25, lty=1)

  } #for z



  # baseline trajectory
  Q1hat <- exp(LQ1hat)
  Q2hat <- exp(LQ2hat)
  Q3hat <- exp(LQ3hat)
  K1hat <- exp(LK1hat)
  K2hat <- exp(LK2hat)
  K3hat <- exp(LK3hat)
  H1hat <- K1hat/2 + R1hat
  H2hat <- K2hat/2 + R2hat
  H3hat <- K3hat/2 + R3hat

  # # composite trajectory
  # lines(x = x,
  #       y=( -1*exp(-1*K1hat*Q1hat*x/( 1 + 2*Q1hat )) + 2*H1hat/K1hat*pi^(Q1hat/( 1 + 2*Q1hat )) )^(( 1 + 2*Q1hat )/Q1hat) +
  #         ( -1*exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat )) + 2*H2hat/K2hat*pi^(Q2hat/( 1 + 2*Q2hat )) )^(( 1 + 2*Q2hat )/Q2hat) +
  #         ( -1*exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat )) + 2*H3hat/K3hat*pi^(Q3hat/( 1 + 2*Q3hat )) )^(( 1 + 2*Q3hat )/Q3hat),
  #       col="black", lwd=2, lty=1)

  # infant
  lines(x = x2,
        y=( -1*exp(-1*K1hat*Q1hat*x2/( 1 + 2*Q1hat )) + 2*H1hat/K1hat*pi^(Q1hat/( 1 + 2*Q1hat )) )^(( 1 + 2*Q1hat )/Q1hat),
          col="red4", lwd=3, lty=1)

  # child
  lines(x = x,
        y=( -1*exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat )) + 2*H2hat/K2hat*pi^(Q2hat/( 1 + 2*Q2hat )) )^(( 1 + 2*Q2hat )/Q2hat),
          col="darkblue", lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y=( -1*exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat )) + 2*H3hat/K3hat*pi^(Q3hat/( 1 + 2*Q3hat )) )^(( 1 + 2*Q3hat )/Q3hat),
          col="darkgreen", lwd=3, lty=1)


  text(1, 70, "Prior group-level variance", cex = 1.35, adj=0)
  symbol.Female(centerx = 18, centery = 71, rayonx=0.5, lwd=2, col="black") # Female symbol from HelpersMG package



  #### cumulative trajectories

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,80), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels = FALSE )
  axis( side=2, at=seq( 0, 80, by=20 ), labels = FALSE )

  # plot possible group mean trajectories from this model given the priors
  for ( z in 1:Nsims ){

    gQ1 <- exp(gLQ1[z])
    gQ2 <- exp(gLQ2[z])
    gQ3 <- exp(gLQ3[z])
    gK1 <- exp(gLK1[z])
    gK2 <- exp(gLK2[z])
    gK3 <- exp(gLK3[z])
    gH1 <- gK1/2 + gR1[z]
    gH2 <- gK2/2 + gR2[z]
    gH3 <- gK3/2 + gR3[z]

    # composite trajectory
    lines(x = x,
          y = ( -1*exp(-1*gK1*gQ1*x/( 1 + 2*gQ1 )) + 2*gH1/gK1*pi^(gQ1/( 1 + 2*gQ1 )) )^(( 1 + 2*gQ1 )/gQ1) +
              ( -1*exp(-1*gK2*gQ2*x/( 1 + 2*gQ2 )) + 2*gH2/gK2*pi^(gQ2/( 1 + 2*gQ2 )) )^(( 1 + 2*gQ2 )/gQ2) +
              ( -1*exp(-1*gK3*gQ3*x/( 1 + 2*gQ3 )) + 2*gH3/gK3*pi^(gQ3/( 1 + 2*gQ3 )) )^(( 1 + 2*gQ3 )/gQ3),
              col=grey(0.5), lwd=0.25, lty=1)

  } #for z

  # baseline trajectory
  lines(x = x,
        y = ( -1*exp(-1*K1hat*Q1hat*x/( 1 + 2*Q1hat )) + 2*H1hat/K1hat*pi^(Q1hat/( 1 + 2*Q1hat )) )^(( 1 + 2*Q1hat )/Q1hat) +
            ( -1*exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat )) + 2*H2hat/K2hat*pi^(Q2hat/( 1 + 2*Q2hat )) )^(( 1 + 2*Q2hat )/Q2hat) +
            ( -1*exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat )) + 2*H3hat/K3hat*pi^(Q3hat/( 1 + 2*Q3hat )) )^(( 1 + 2*Q3hat )/Q3hat),
        col="black", lwd=3, lty=1)





  ############### individual variance

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,80), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ) )
  axis( side=2, at=seq( 0, 80, by=20 ) )
  mtext("Age since conception (yrs)", side = 1, outer = T, cex = 1.5, line = 3, adj=2.6)
  #mtext("Weight (kg)", side = 2, outer = T, cex = 1, line = 3, adj=0.5)


  # plot possible individual trajectories from this model given the priors
  for ( z in 1:Nsims ){

    Q1 <- exp(LQ1[z])
    Q2 <- exp(LQ2[z])
    Q3 <- exp(LQ3[z])
    K1 <- exp(LK1[z])
    K2 <- exp(LK2[z])
    K3 <- exp(LK3[z])
    H1 <- K1/2 + R1[z]
    H2 <- K2/2 + R2[z]
    H3 <- K3/2 + R3[z]

    # # composite trajectory
    # lines(x = x,
    #       y = ( -1*exp(-1*K1*Q1*x/( 1 + 2*Q1 )) + 2*H1/K1*pi^(Q1/( 1 + 2*Q1 )) )^(( 1 + 2*Q1 )/Q1) +
    #           ( -1*exp(-1*K2*Q2*x/( 1 + 2*Q2 )) + 2*H2/K2*pi^(Q2/( 1 + 2*Q2 )) )^(( 1 + 2*Q2 )/Q2) +
    #           ( -1*exp(-1*K3*Q3*x/( 1 + 2*Q3 )) + 2*H3/K3*pi^(Q3/( 1 + 2*Q3 )) )^(( 1 + 2*Q3 )/Q3),
    #           col=grey(0.5), lwd=0.25, lty=1)

    # infant
    lines(x = x2,
          y = ( -1*exp(-1*K1*Q1*x/( 1 + 2*Q1 )) + 2*H1/K1*pi^(Q1/( 1 + 2*Q1 )) )^(( 1 + 2*Q1 )/Q1),
              col="red", lwd=0.25, lty=1)

    # child
    lines(x = x,
          y = ( -1*exp(-1*K2*Q2*x/( 1 + 2*Q2 )) + 2*H2/K2*pi^(Q2/( 1 + 2*Q2 )) )^(( 1 + 2*Q2 )/Q2),
          col="blue", lwd=0.25, lty=1)

    # adolescent
    lines(x = x,
          y = ( -1*exp(-1*K3*Q3*x/( 1 + 2*Q3 )) + 2*H3/K3*pi^(Q3/( 1 + 2*Q3 )) )^(( 1 + 2*Q3 )/Q3),
              col="green", lwd=0.25, lty=1)

  } #for z



  # baseline trajectory
  Q1hat <- exp(LQ1hat)
  Q2hat <- exp(LQ2hat)
  Q3hat <- exp(LQ3hat)
  K1hat <- exp(LK1hat)
  K2hat <- exp(LK2hat)
  K3hat <- exp(LK3hat)
  H1hat <- K1hat/2 + R1hat
  H2hat <- K2hat/2 + R2hat
  H3hat <- K3hat/2 + R3hat

  # # composite trajectory
  # lines(x = x,
  #       y=( -1*exp(-1*K1hat*Q1hat*x/( 1 + 2*Q1hat )) + 2*H1hat/K1hat*pi^(Q1hat/( 1 + 2*Q1hat )) )^(( 1 + 2*Q1hat )/Q1hat) +
  #         ( -1*exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat )) + 2*H2hat/K2hat*pi^(Q2hat/( 1 + 2*Q2hat )) )^(( 1 + 2*Q2hat )/Q2hat) +
  #         ( -1*exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat )) + 2*H3hat/K3hat*pi^(Q3hat/( 1 + 2*Q3hat )) )^(( 1 + 2*Q3hat )/Q3hat),
  #       col="black", lwd=2, lty=1)

  # infant
  lines(x = x2,
        y = ( -1*exp(-1*K1hat*Q1hat*x2/( 1 + 2*Q1hat )) + 2*H1hat/K1hat*pi^(Q1hat/( 1 + 2*Q1hat )) )^(( 1 + 2*Q1hat )/Q1hat),
          col="red4", lwd=3, lty=1)

  # child
  lines(x = x,
        y = ( -1*exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat )) + 2*H2hat/K2hat*pi^(Q2hat/( 1 + 2*Q2hat )) )^(( 1 + 2*Q2hat )/Q2hat),
          col="darkblue", lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ( -1*exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat )) + 2*H3hat/K3hat*pi^(Q3hat/( 1 + 2*Q3hat )) )^(( 1 + 2*Q3hat )/Q3hat),
          col="darkgreen", lwd=3, lty=1)

  text(1, 70, "Prior individual-level variance", cex = 1.35, adj=0)
  symbol.Female(centerx = 20, centery = 71, rayonx=0.5, lwd=2, col="black") # Female symbol from HelpersMG package




  #### cumulative trajectories

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,80), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ) )
  axis( side=2, at=seq( 0, 80, by=20 ), labels = FALSE )

  # plot possible individual trajectories from this model given the priors
  for ( z in 1:Nsims ){

    Q1 <- exp(LQ1[z])
    Q2 <- exp(LQ2[z])
    Q3 <- exp(LQ3[z])
    K1 <- exp(LK1[z])
    K2 <- exp(LK2[z])
    K3 <- exp(LK3[z])
    H1 <- K1/2 + R1[z]
    H2 <- K2/2 + R2[z]
    H3 <- K3/2 + R3[z]

    # composite trajectory
    lines(x = x,
          y = ( -1*exp(-1*K1*Q1*x/( 1 + 2*Q1 )) + 2*H1/K1*pi^(Q1/( 1 + 2*Q1 )) )^(( 1 + 2*Q1 )/Q1) +
              ( -1*exp(-1*K2*Q2*x/( 1 + 2*Q2 )) + 2*H2/K2*pi^(Q2/( 1 + 2*Q2 )) )^(( 1 + 2*Q2 )/Q2) +
              ( -1*exp(-1*K3*Q3*x/( 1 + 2*Q3 )) + 2*H3/K3*pi^(Q3/( 1 + 2*Q3 )) )^(( 1 + 2*Q3 )/Q3),
              col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  # baseline trajectory
  lines(x = x,
        y=( -1*exp(-1*K1hat*Q1hat*x/( 1 + 2*Q1hat )) + 2*H1hat/K1hat*pi^(Q1hat/( 1 + 2*Q1hat )) )^(( 1 + 2*Q1hat )/Q1hat) +
          ( -1*exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat )) + 2*H2hat/K2hat*pi^(Q2hat/( 1 + 2*Q2hat )) )^(( 1 + 2*Q2hat )/Q2hat) +
          ( -1*exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat )) + 2*H3hat/K3hat*pi^(Q3hat/( 1 + 2*Q3hat )) )^(( 1 + 2*Q3hat )/Q3hat),
        col="black", lwd=3, lty=1)



graphics.off()


