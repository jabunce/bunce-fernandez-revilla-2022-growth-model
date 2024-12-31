

##### prior predictive simulations for composite model, one group


Nsims <- 100 # number of trajectories to simulate 

# JPA-1 model from Jolicoeur et al. 1992 table 2, parameter values from table 4
muA <- 175.3 #175.2 # 
muC1 <- 0.458 #0.426 #
muC2 <- 2.62 #2.842 # 
muC3 <- 19.55 #19.66 #       
muD1 <- 0.155 #2.866 #        
muD2 <- 8.358 #8.61 #          
muD3 <- 13.55 #13.2 #      
#muE <- 0.339

# offset to the base trajectory for the mean (across individuals) trajectory of the ethnic group. Make these priors as weak as possible.
sdA <- 20
sdC1 <- 0.005 #0.001 #0.05
sdC2 <- 0.005 #0.001 #0.05 #0.5 
sdC3 <- 0.005 #0.001 #0.05 #1
sdD1 <- 0.005 #0.001 #0.05       
sdD2 <- 0.005 #0.001 #0.05 #1
sdD3 <- 0.005 #0.001 #0.05 #1      
#sdE <- 0.1 

offsetGroupStdevs <- c( sdA,
                        sdC1, sdC2, sdC3,
                        sdD1, sdD2, sdD3)     #, sdE)


muLsigma <- 0.002
musigma <- (( exp(muLsigma^2) - 1 )*exp( 2*log(100) + muLsigma^2 ))^0.5  # want 95% of observations within 2*stdev = 0.5cm of actual height


# holders for sims
A <- rep(0,Nsims)
D1 <- rep(0,Nsims)
D2 <- rep(0,Nsims)
D3 <- rep(0,Nsims)
C1 <- rep(0,Nsims)
C2 <- rep(0,Nsims)
C3 <- rep(0,Nsims)
#E <- rep(0,Nsims)


for (n in 1:Nsims) {

  GrpOffset <- mvrnorm( n=1, mu=rep(0,length(offsetGroupStdevs)), Sigma=diag(offsetGroupStdevs) )  # assumes no covariance among parameters
 
  # sims for group mean
  A[n] <- muA + GrpOffset[1]
  C1[n] <- muC1 + GrpOffset[2]       
  C2[n] <- muC2 + GrpOffset[3]
  C3[n] <- muC3 + GrpOffset[4]     
  D1[n] <- muD1 + GrpOffset[5]
  D2[n] <- muD2 + GrpOffset[6]
  D3[n] <- muD3 + GrpOffset[7]
  #E[n] <- muE + GrpOffset[8]   

} # for n




pdf(file="./Plots/Prior_predict_JPA1.pdf",
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

    # JPA-1 trajectory, undefined at x=0 even though R takes the limit of exp(-1/0)
    lines(x = x,
          y = A[z]*exp( -1/( C1[z]*log( x/D1[z] + 1 ) + (x/D2[z])^C2[z] + (x/D3[z])^C3[z] ) ), # JPA-1 model from Jolicoeur et al. 1992 table 2         #A[z]*( 1 - 1/( 1 + ((x + E[z])/D1[z])^C1[z] + ((x + E[z])/D2[z])^C2[z] + ((x + E[z])/D3[z])^C3[z] ) ),   # JPA-2 model from Jolicoeur et al. 1992 table 2
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  # mean trajectory
  lines(x = x,
        y = muA*exp( -1/( muC1*log( x/muD1 + 1 ) + (x/muD2)^muC2 + (x/muD3)^muC3 ) ),       #muA*( 1 - 1/( 1 + ((x + muE)/muD1)^muC1 + ((x + muE)/muD2)^muC2 + ((x + muE)/muD3)^muC3 ) ),
        col="black", lwd=3, lty=1)


graphics.off()



