

##### prior predictive simulations for composite model, one group


Nsims <- 100 # number of trajectories to simulate 

# from Jolicoeur et al. 1992 Table 2
muA <- 175.3        
muD1 <- 0.155        
muD2 <- 8.358          
muD3 <- 13.55      
muC1 <- 0.458
muC2 <- 2.62 
muC3 <- 19.55

# holders for sims
A <- rep(0,Nsims)
D1 <- rep(0,Nsims)
D2 <- rep(0,Nsims)
D3 <- rep(0,Nsims)
C1 <- rep(0,Nsims)
C2 <- rep(0,Nsims)
C3 <- rep(0,Nsims)


for (n in 1:Nsims) {

  # offset to the base trajectory for the mean (across individuals) trajectory of the ethnic group. Make these priors as weak as possible.
  sdA <- 100
  sdD1 <- 0.05#0.1 #0.05 #0.25 #0.05 #0.0001        
  sdD2 <- 5#5 #5 #0.01 #0.05 #0.5
  sdD3 <- 10#10 #10 #0.25 #0.05 #0.5      
  sdC1 <- 0.05#0.1 #0.05 #0.25 #0.05 #0.001
  sdC2 <- 2#2 #2 #0.01 #0.05 #0.2
  sdC3 <- 10#10 #10 #0.25 #0.05 #0.1      

  offsetGroupStdevs <- c( sdA,
                          sdD1, sdD2, sdD3,
                          sdC1, sdC2, sdC3)
  GrpOffset <- mvrnorm( n=1, mu=rep(0,length(offsetGroupStdevs)), Sigma=diag(offsetGroupStdevs) )  # assumes no covariance among parameters

  # sims for group mean
  A[n] <- muA + GrpOffset[1]
  D1[n] <- muD1 + GrpOffset[2]       
  D2[n] <- muD2 + GrpOffset[3]
  D3[n] <- muD3 + GrpOffset[4]     
  C1[n] <- muC1 + GrpOffset[5]
  C2[n] <- muC2 + GrpOffset[6]
  C3[n] <- muC3 + GrpOffset[7]    

} # for n




pdf(file="./Plots/Prior_predict_JPA.pdf",
    height=5, width=5)
par(mfrow = c(1, 1))
par(mar = c(0, 0, 0, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age


  #### cumulative trajectories

  #set up plot
  par(mar=c(2, 2, 0, 0)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ) )
  axis( side=2, at=seq( 0, 200, by=50 ), labels = TRUE )
  mtext("Age since conception (yrs)", side = 1, outer = T, cex = 1, line = 1, adj=0.6)
  mtext("Height (cm)", side = 2, outer = T, cex = 1, line = 2, adj=0.6)

  # plot possible individual trajectories from this model given the priors
  for ( z in 1:Nsims ){

    #composite trajectory
    lines(x = x,
          y=A[z]*exp( -1/( C1[z]*log( x/D1[z] + 1 ) + (x/D2[z])^C2[z] + (x/D3[z])^C3[z] ) ),
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  # mean trajectory
  lines(x = x,
        y=muA*exp( -1/( muC1*log( x/muD1 + 1 ) + (x/muD2)^muC2 + (x/muD3)^muC3 ) ),
        col="black", lwd=3, lty=1)



graphics.off()



