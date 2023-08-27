

##### prior predictive simulations from composite model, multiple groups


Nsims <- 100 # number of trajectories to simulate 

# from Jolicoeur et al. 1992 Table 2
Ahat <- mean(post5$muA) # 181.0095 #175.3        
D1hat <- mean(post5$muD1) # 0.0008682864  #0.155        
D2hat <- mean(post5$muD2) # 5.020883 #8.358          
D3hat <- mean(post5$muD3) # 11.41781 #13.55      
C1hat <- mean(post5$muC1) # 0.1076357 #0.458
C2hat <- mean(post5$muC2) # 1.070365 #2.62 
C3hat <- mean(post5$muC3) # 7.848545 #19.55

# holders for sims for indivs
A <- rep(0,Nsims)
D1 <- rep(0,Nsims)
D2 <- rep(0,Nsims)
D3 <- rep(0,Nsims)
C1 <- rep(0,Nsims)
C2 <- rep(0,Nsims)
C3 <- rep(0,Nsims)


# holders for sims for group mean
gA <- rep(0,Nsims)
gD1 <- rep(0,Nsims)
gD2 <- rep(0,Nsims)
gD3 <- rep(0,Nsims)
gC1 <- rep(0,Nsims)
gC2 <- rep(0,Nsims)
gC3 <- rep(0,Nsims)



for (n in 1:Nsims) {

  # offsets to the mean group trajectory for each individual, stdev's taken from posterior stdevs from m2
  imA <- 0.01 #1 #10       # means for normal distribution for stdevs
  imD1 <- 0.0000001 #0.00001 #0.0001        
  imD2 <- 0.0005 #0.05 #0.5
  imD3 <- 0.0005 #0.05 #0.5      
  imC1 <- 0.000001 #0.0001 #0.001
  imC2 <- 0.0001 #0.01 #0.1
  imC3 <- 0.001 #0.1 #1     

  isA <- 1       # stdevs for normal distribution
  isD1 <- 0.00001         
  isD2 <- 0.01 
  isD3 <- 0.01       
  isC1 <- 0.0001 
  isC2 <- 0.01 
  isC3 <- 0.1      

  # sample (a stdev) from a distribution with the above means and stdevs
  isdA <- rnorm( n=1, mean=imA, sd=isA )
  isdD1 <- rnorm( n=1, mean=imD1, sd=isD1 )        
  isdD2 <- rnorm( n=1, mean=imD2, sd=isD2 )
  isdD3 <- rnorm( n=1, mean=imD3, sd=isD3 )      
  isdC1 <- rnorm( n=1, mean=imC1, sd=isC1 )
  isdC2 <- rnorm( n=1, mean=imC2, sd=isC2 )
  isdC3 <- rnorm( n=1, mean=imC3, sd=isC3 )      


  offsetIndivStdevs <- c( isdA,
                          isdD1, isdD2, isdD3,
                          isdC1, isdC2, isdC3 )

  s <- diag(offsetIndivStdevs)
  CholEtaR <- 4
  R <- rlkjcorr( n=1, K=length(offsetIndivStdevs), eta=CholEtaR )  # eta=1, all coorelations equally likely; eta>1, extreme correlations less likely 
  S <- s %*% R %*% s # vcov matrix with variances (s^2) on the diagonal

  IndOffset <- mvrnorm( n=1, mu=rep(0,length(offsetIndivStdevs)), Sigma=S )



  # mean (across indivs) offsets to the baseline group mean trajectory
  gexpA <- 1/imA
  gexpD1 <- 1/imD1         
  gexpD2 <- 1/imD2 
  gexpD3 <- 1/imD3       
  gexpC1 <- 1/imC1 
  gexpC2 <- 1/imC2 
  gexpC3 <- 1/imC3       

  gsdA <- rexp( n=1, rate=gexpA )      # mean of exponential distribution = 1/rate
  gsdD1 <- rexp( n=1, rate=gexpD1 )        
  gsdD2 <- rexp( n=1, rate=gexpD2 )
  gsdD3 <- rexp( n=1, rate=gexpD3 )      
  gsdC1 <- rexp( n=1, rate=gexpC1 )
  gsdC2 <- rexp( n=1, rate=gexpC2 )
  gsdC3 <- rexp( n=1, rate=gexpC3 )      
  

  offsetGroupStdevs <- c( gsdA,
                          gsdD1, gsdD2, gsdD3,
                          gsdC1, gsdC2, gsdC3 )

  t <- diag(offsetGroupStdevs)
  CholEtaU <- 4
  U <- rlkjcorr( n=1, K=length(offsetGroupStdevs), eta=CholEtaU )  # eta=1, all coorelations equally likely; eta>1, extreme correlations less likely 
  T <- t %*% U %*% t # vcov matrix with variances (t^2) on the diagonal

  GrpOffset <- mvrnorm( n=1, mu=rep(0,length(offsetGroupStdevs)), Sigma=T )


  # sims for individuals
  A[n] <- Ahat + IndOffset[1]
  D1[n] <- D1hat + IndOffset[2]       
  D2[n] <- D2hat + IndOffset[3]
  D3[n] <- D3hat + IndOffset[4]     
  C1[n] <- C1hat + IndOffset[5]
  C2[n] <- C2hat + IndOffset[6]
  C3[n] <- C3hat + IndOffset[7]    

  # A[n] <- Ahat + IndOffset[1] + GrpOffset[1]
  # D1[n] <- D1hat + IndOffset[2] + GrpOffset[2]       
  # D2[n] <- D2hat + IndOffset[3] + GrpOffset[3]
  # D3[n] <- D3hat + IndOffset[4] + GrpOffset[4]     
  # C1[n] <- C1hat + IndOffset[5] + GrpOffset[5]
  # C2[n] <- C2hat + IndOffset[6] + GrpOffset[6]
  # C3[n] <- C3hat + IndOffset[7] + GrpOffset[7] 

  # sims for group means
  gA[n] <- Ahat + GrpOffset[1]
  gD1[n] <- D1hat + GrpOffset[2]       
  gD2[n] <- D2hat + GrpOffset[3]
  gD3[n] <- D3hat + GrpOffset[4]     
  gC1[n] <- C1hat + GrpOffset[5]
  gC2[n] <- C2hat + GrpOffset[6]
  gC3[n] <- C3hat + GrpOffset[7]    


} # for n



pdf(file="./Plots/Prior_predict_JPAm7.pdf",
    height=5, width=10)
par(mfrow = c(1, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age: from 0 = starting at conception, from 0.75 = starting at birth
  x2 <- seq(from=(0), to=(25+0.75), by=0.1)       # look at negative exponential all the way back to conception
  

  ##### group-level variance

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ) )
  axis( side=2, at=seq( 0, 200, by=50 ) )
  mtext("Age since conception (yrs)", side = 1, outer = T, cex = 1, line = 2, adj=0.5)
  mtext("Height (cm)", side = 2, outer = T, cex = 1, line = 2, adj=0.5)



  # plot possible group trajectories from this model given the priors
  for ( z in 1:Nsims ){

    # sims
    lines(x = x,
          y=gA[z]*exp( -1/( gC1[z]*log( x/gD1[z] + 1 ) + (x/gD2[z])^gC2[z] + (x/gD3[z])^gC3[z] ) ),
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  # mean trajectory
  lines(x = x,
        y=Ahat*exp( -1/( C1hat*log( x/D1hat + 1 ) + (x/D2hat)^C2hat + (x/D3hat)^C3hat ) ),
        col="black", lwd=3, lty=1)


  text(1, 190, "Prior group-level variance", cex = 1.35, adj=0)





  ############### individual variance

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ) )
  axis( side=2, at=seq( 0, 200, by=50 ), labels = FALSE )

  # plot possible individual trajectories from this model given the priors
  for ( z in 1:Nsims ){

    # sims
    lines(x = x,
          y=A[z]*exp( -1/( C1[z]*log( x/D1[z] + 1 ) + (x/D2[z])^C2[z] + (x/D3[z])^C3[z] ) ),
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  # mean trajectory
  lines(x = x,
        y=Ahat*exp( -1/( C1hat*log( x/D1hat + 1 ) + (x/D2hat)^C2hat + (x/D3hat)^C3hat ) ),
        col="black", lwd=3, lty=1)


  text(1, 190, "Prior individual-level variance", cex = 1.35, adj=0)


graphics.off()


