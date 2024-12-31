

##### prior predictive simulations from composite model, multiple groups


Nsims <- 100 # number of trajectories to simulate 

# from Jolicoeur et al. 1992 Table 2
Ahat <- 179.7224 #mean(post7$muA) # 181.0095 #175.3 
C1hat <- 0.471358 #mean(post7$muC1) # 0.1076357 #0.458
C2hat <- 2.760237 #mean(post7$muC2) # 1.070365 #2.62 
C3hat <- 19.54227 #mean(post7$muC3) # 7.848545 #19.55       
D1hat <- 0.1618025 #mean(post7$muD1) # 0.0008682864  #0.155        
D2hat <- 8.229972 #mean(post7$muD2) # 5.020883 #8.358          
D3hat <- 13.55839 #mean(post7$muD3) # 11.41781 #13.55      


# holders for sims for indivs
A <- rep(0,Nsims)
C1 <- rep(0,Nsims)
C2 <- rep(0,Nsims)
C3 <- rep(0,Nsims)
D1 <- rep(0,Nsims)
D2 <- rep(0,Nsims)
D3 <- rep(0,Nsims)



# holders for sims for group mean
gA <- rep(0,Nsims)
gC1 <- rep(0,Nsims)
gC2 <- rep(0,Nsims)
gC3 <- rep(0,Nsims)
gD1 <- rep(0,Nsims)
gD2 <- rep(0,Nsims)
gD3 <- rep(0,Nsims)


# mean stdev of offsets to the baseline trajectory for each group
gsA <- 1 #2 #4 #8 #10 #0.1 #1 #10       # means for normal distribution for stdevs
gsC1 <- 0.0001 #0.0005 #0.001 #0.0001 #0.0005 ##0.001 #0.01 #0.0001 #0.001 #0.01 #0.000001 #0.005 #0.000001 #0.0001 #0.001
gsC2 <- 0.001 #0.005 #0.01 #0.001 #0.005 ##0.01 #0.1 #0.001 #0.01 #0.1 #0.000001 #0.005 ##0.0001 #0.01 #0.1
gsC3 <- 0.0005 #0.001 #0.05 #0.005 #0.01 ##0.05 #0.5 #0.005 #0.05 #0.5 #0.000001 #0.005 ##0.001 #0.1 #1 
gsD1 <- 0.00005 #0.0001 #0.0005 #0.001 #0.0001 #0.0005 ##0.001 #0.01 #0.0001 #0.001 #0.01 #0.000001 #0.005 ##0.0000001 #0.00001 #0.0001        
gsD2 <- 0.001 #0.005 #0.01 #0.05 #0.005 #0.01 ##0.05 #0.5 #0.005 #0.05 #0.5 #0.000001 #0.005 ##0.0005 #0.05 #0.5
gsD3 <- 0.001 #0.005 #0.01 #0.05 #0.005 #0.01 ##0.05 #0.5 #0.005 #0.05 #0.5 #0.000001 #0.005 ##0.0005 #0.05 #0.5  


# mean stdev of offsets to the group mean trajectory for each individual
imA <- 2 #5 #8 #10 #1 #5 #0.01 #1 #10       # means for normal distribution for stdevs
imC1 <- 0.0005 #0.00005 #0.0001 ##0.0005 #0.001 #0.00001 #0.0001 #0.005 #0.01 #0.000001 #0.005 #0.000001 #0.0001 #0.001
imC2 <- 0.005 #0.01 #0.001 #0.005 ##0.01 #0.0001 #0.001 #0.05 #0.1 #0.000001 #0.005 ##0.0001 #0.01 #0.1
imC3 <- 0.05 #0.005 #0.01 ##0.05 #0.0005 #0.005 #0.1 #0.5 #0.000001 #0.005 ##0.001 #0.1 #1 
imD1 <- 0.0001 #0.0005 #0.00005 #0.0001 ##0.0005 #0.001 #0.00001 #0.0001 #0.005 #0.01 #0.000001 #0.005 ##0.0000001 #0.00001 #0.0001        
imD2 <- 0.01 #0.001 #0.005 ##0.01 #0.05 #0.0005 #0.005 #0.1 #0.5 #0.000001 #0.005 ##0.0005 #0.05 #0.5
imD3 <- 0.005 #0.01 #0.001 #0.005 ##0.01 #0.05 #0.0005 #0.005 #0.1 #0.5 #0.000001 #0.005 ##0.0005 #0.05 #0.5      
    
# # stdev of stdev of offsets to the group mean trajectory for each individual
isA <- 2 #4 #4 
isC1 <- 0.000005 #0.00001 #0.0001 #0.0005 #0.0000001 
isC2 <- 0.00001 #0.0001 #0.005 #0.0000001
isC3 <- 0.00001 #0.0001 #0.05 #0.0000001 
isD1 <- 0.000005 #0.00001 #0.0001 #0.0001 #0.0000001         
isD2 <- 0.00001 #0.0001 #0.01 #0.0000001 
isD3 <- 0.00001 #0.0001 #0.005 #0.0000001 


muLsigma <- 0.002  # mean(post4$"Lsigma")
musigma <- (( exp(muLsigma^2) - 1 )*exp( 2*log(100) + muLsigma^2 ))^0.5  # want 95% of observations within 2*stdev = 0.5cm of actual height at height=100cm




for (n in 1:Nsims) {

  # stdev of indiv offsets to the group mean trajectory
  # sample (a stdev) from a distribution with the above means and stdevs
  isdA <-  rnorm( n=1, mean=imA, sd=isA ) #5 sd=10    rexp( n=1, rate=1/imA ) # # #
  isdC1 <- rnorm( n=1, mean=imC1, sd=isC1 ) #rexp( n=1, rate=1/imC1 ) #rnorm( n=1, mean=imC1, sd=isC1 )  # #
  isdC2 <- rnorm( n=1, mean=imC2, sd=isC2 ) #rexp( n=1, rate=1/imC2 ) #rnorm( n=1, mean=imC2, sd=isC2 )  # #
  isdC3 <- rnorm( n=1, mean=imC3, sd=isC3 ) #rexp( n=1, rate=1/imC3 ) #rnorm( n=1, mean=imC3, sd=isC3 )  # # 
  isdD1 <- rnorm( n=1, mean=imD1, sd=isD1 ) #rexp( n=1, rate=1/imD1 ) #rnorm( n=1, mean=imD1, sd=isD1 )  # #        
  isdD2 <- rnorm( n=1, mean=imD2, sd=isD2 ) #rexp( n=1, rate=1/imD2 ) #rnorm( n=1, mean=imD2, sd=isD2 )  # #
  isdD3 <- rnorm( n=1, mean=imD3, sd=isD3 ) #rexp( n=1, rate=1/imD3 ) #rnorm( n=1, mean=imD3, sd=isD3 )  # #      
     

  offsetIndivStdevs <- c( isdA,
                          isdC1, isdC2, isdC3,
                          isdD1, isdD2, isdD3
                        )

  s <- diag(offsetIndivStdevs)
  CholEtaI <- 4
  R <- rlkjcorr( n=1, K=length(offsetIndivStdevs), eta=CholEtaI )  # eta=1, all coorelations equally likely; eta>1, extreme correlations less likely 
  S <- s %*% R %*% s # vcov matrix with variances (s^2) on the diagonal

  IndOffset <- mvrnorm( n=1, mu=rep(0,length(offsetIndivStdevs)), Sigma=S )



  # stdev of group offsets to the baseline trajectory
  gsdA <- rnorm( n=1, mean=gsA, sd=4 ) #5 sd=10     rexp( n=1, rate=1/gsA )      # mean of exponential distribution = 1/rate
  gsdC1 <- rexp( n=1, rate=1/gsC1 )
  gsdC2 <- rexp( n=1, rate=1/gsC2 )
  gsdC3 <- rexp( n=1, rate=1/gsC3 ) 
  gsdD1 <- rexp( n=1, rate=1/gsD1 )        
  gsdD2 <- rexp( n=1, rate=1/gsD2 )
  gsdD3 <- rexp( n=1, rate=1/gsD3 )      
     
  

  offsetGroupStdevs <- c( gsdA,
                          gsdC1, gsdC2, gsdC3,
                          gsdD1, gsdD2, gsdD3
                        )

  t <- diag(offsetGroupStdevs)
  CholEtaG <- 4
  U <- rlkjcorr( n=1, K=length(offsetGroupStdevs), eta=CholEtaG )  # eta=1, all coorelations equally likely; eta>1, extreme correlations less likely 
  T <- t %*% U %*% t # vcov matrix with variances (t^2) on the diagonal

  GrpOffset <- mvrnorm( n=1, mu=rep(0,length(offsetGroupStdevs)), Sigma=T )


  # sims for individuals
  A[n] <- Ahat + IndOffset[1]
  C1[n] <- C1hat + IndOffset[2]
  C2[n] <- C2hat + IndOffset[3]
  C3[n] <- C3hat + IndOffset[4]
  D1[n] <- D1hat + IndOffset[5]       
  D2[n] <- D2hat + IndOffset[6]
  D3[n] <- D3hat + IndOffset[7]     
    

  # A[n] <- Ahat + IndOffset[1] + GrpOffset[1]
  # C1[n] <- C1hat + IndOffset[2] + GrpOffset[2]
  # C2[n] <- C2hat + IndOffset[3] + GrpOffset[3]
  # C3[n] <- C3hat + IndOffset[4] + GrpOffset[4] 
  # D1[n] <- D1hat + IndOffset[5] + GrpOffset[5]       
  # D2[n] <- D2hat + IndOffset[6] + GrpOffset[6]
  # D3[n] <- D3hat + IndOffset[7] + GrpOffset[7]     


  # sims for group means
  gA[n] <- Ahat + GrpOffset[1]
  gC1[n] <- C1hat + GrpOffset[2]
  gC2[n] <- C2hat + GrpOffset[3]
  gC3[n] <- C3hat + GrpOffset[4] 
  gD1[n] <- D1hat + GrpOffset[5]       
  gD2[n] <- D2hat + GrpOffset[6]
  gD3[n] <- D3hat + GrpOffset[7]     
   


} # for n



pdf(file="./Plots/Prior_predict_JPAm8.pdf",
    height=5, width=10)
par(mfrow = c(1, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age: from 0 = starting at conception, from 0.75 = starting at birth
  

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


