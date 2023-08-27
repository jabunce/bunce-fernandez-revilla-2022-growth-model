

##### prior predictive simulations for composite model, one group


Nsims <- 100 # number of trajectories to simulate 

#means defining the baseline trajectory, chosen to make trajectory look reasonable
muLQ1 <- -1.6
muLQ2 <- -1.75        
muLQ3 <- -2
muLQ4 <- -3.5        
muLQ5 <- -10.2
muLK1 <- 3.08      
muLK2 <- 1.8
muLK3 <- 1.4
muLK4 <- 2.6
muLK5 <- 9.95
muR1 <- 12
muR2 <- 3
muR3 <- 1.1
muR4 <- 0.8
muR5 <- 1.2

muQ1 <- exp(muLQ1)      # transformation of parameters
muQ2 <- exp(muLQ2)
muQ3 <- exp(muLQ3)
muQ4 <- exp(muLQ4)
muQ5 <- exp(muLQ5)
muK1 <- exp(muLK1)
muK2 <- exp(muLK2)
muK3 <- exp(muLK3)
muK4 <- exp(muLK4)
muK5 <- exp(muLK5)
muH1 <- muK1/2 + muR1
muH2 <- muK2/2 + muR2
muH3 <- muK3/2 + muR3
muH4 <- muK4/2 + muR4
muH5 <- muK5/2 + muR5   

# holders for sims
LQ1 <- rep(0,Nsims)
LQ2 <- rep(0,Nsims)
LQ3 <- rep(0,Nsims)
LQ4 <- rep(0,Nsims)
LQ5 <- rep(0,Nsims)
LK1 <- rep(0,Nsims)
LK2 <- rep(0,Nsims)
LK3 <- rep(0,Nsims)
LK4 <- rep(0,Nsims)
LK5 <- rep(0,Nsims)
R1 <- rep(0,Nsims)
R2 <- rep(0,Nsims)
R3 <- rep(0,Nsims)
R4 <- rep(0,Nsims)
R5 <- rep(0,Nsims)

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


for (n in 1:Nsims) {

  # # offset to the base trajectory for the mean (across individuals) trajectory of the ethnic group. Make these priors as weak as possible.
  sdLQ1 <- 0.05 #0.05
  sdLQ2 <- 0.05 #0.01      
  sdLQ3 <- 0.05
  sdLQ4 <- 0.05        
  sdLQ5 <- 0.05
  sdLK1 <- 0.05      
  sdLK2 <- 0.05
  sdLK3 <- 0.05
  sdLK4 <- 0.05
  sdLK5 <- 0.05
  sdR1 <- 0.05      
  sdR2 <- 0.05
  sdR3 <- 0.05
  sdR4 <- 0.05
  sdR5 <- 0.05

  offsetGroupStdevs <- c( sdLQ1, sdLQ2, sdLQ3, sdLQ4, sdLQ5,
                          sdLK1, sdLK2, sdLK3, sdLK4, sdLK5,
                          sdR1, sdR2, sdR3, sdR4, sdR5)
  GrpOffset <- mvrnorm( n=1, mu=rep(0,length(offsetGroupStdevs)), Sigma=diag(offsetGroupStdevs) )  # assumes no covariance among parameters

  # sims for group mean
  LQ1[n] <- muLQ1 + GrpOffset[1]
  LQ2[n] <- muLQ2 + GrpOffset[2]       
  LQ3[n] <- muLQ3 + GrpOffset[3]
  LQ4[n] <- muLQ4 + GrpOffset[4]       
  LQ5[n] <- muLQ5 + GrpOffset[5]
  LK1[n] <- muLK1 + GrpOffset[6]     
  LK2[n] <- muLK2 + GrpOffset[7]
  LK3[n] <- muLK3 + GrpOffset[8]
  LK4[n] <- muLK4 + GrpOffset[9]
  LK5[n] <- muLK5 + GrpOffset[10]
  R1[n] <- muR1 + GrpOffset[11]    
  R2[n] <- muR2 + GrpOffset[12]
  R3[n] <- muR3 + GrpOffset[13]
  R4[n] <- muR4 + GrpOffset[14]
  R5[n] <- muR5 + GrpOffset[15]


  Q1[n] <- exp(LQ1[n])
  Q2[n] <- exp(LQ2[n])
  Q3[n] <- exp(LQ3[n])
  Q4[n] <- exp(LQ4[n])
  Q5[n] <- exp(LQ5[n])
  K1[n] <- exp(LK1[n])
  K2[n] <- exp(LK2[n])
  K3[n] <- exp(LK3[n])
  K4[n] <- exp(LK4[n])
  K5[n] <- exp(LK5[n])
  H1[n] <- K1[n]/2 + R1[n]
  H2[n] <- K2[n]/2 + R2[n]
  H3[n] <- K3[n]/2 + R3[n]
  H4[n] <- K4[n]/2 + R4[n]
  H5[n] <- K5[n]/2 + R5[n]

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




pdf(file="./Plots/Prior_predict_5.pdf",
    height=5, width=8)
par(mfrow = c(1, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age
  x2 <- seq(from=(0), to=(25+0.75), by=0.1)            # look at first function all the way back to conception
  

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


    # infant
    lines(x = x2,
          y=( -1*exp(-1*K1[z]*Q1[z]*x2/( 1 + 2*Q1[z] )) + 2*H1[z]/K1[z] )^(1/Q1[z]),
          col=colorlist["1.3"], lwd=0.25, lty=1)

    # child
    lines(x = x,
          y=( -1*exp(-1*K2[z]*Q2[z]*x/( 1 + 2*Q2[z] )) + 2*H2[z]/K2[z] )^(1/Q2[z]),
          col="plum", lwd=0.25, lty=1)

    # adolescent
    lines(x = x,
          y=( -1*exp(-1*K3[z]*Q3[z]*x/( 1 + 2*Q3[z] )) + 2*H3[z]/K3[z] )^(1/Q3[z]),
          col=colorlist2["1.3"], lwd=0.25, lty=1)

    # child
    lines(x = x,
          y=( -1*exp(-1*K4[z]*Q4[z]*x/( 1 + 2*Q4[z] )) + 2*H4[z]/K4[z] )^(1/Q4[z]),
          col=colorlist2["2.3"], lwd=0.25, lty=1)

    # adolescent
    lines(x = x,
          y=( -1*exp(-1*K5[z]*Q5[z]*x/( 1 + 2*Q5[z] )) + 2*H5[z]/K5[z] )^(1/Q5[z]),
          col=colorlist["2.3"], lwd=0.25, lty=1)

  } #for z


  # mean component trajectories

  # infant
  lines(x = x2,
        y = ( -1*exp(-1*muK1*muQ1*x2/( 1 + 2*muQ1 )) + 2*muH1/muK1 )^(1/muQ1),
          col=colorlist["1.1"], lwd=3, lty=1)

  # child
  lines(x = x,
        y = ( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2 )^(1/muQ2),
        col="plum4", lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3 )^(1/muQ3),
        col=colorlist2["1.1"], lwd=3, lty=1)

  # child
  lines(x = x,
        y = ( -1*exp(-1*muK4*muQ4*x/( 1 + 2*muQ4 )) + 2*muH4/muK4 )^(1/muQ4),
        col=colorlist2["2.1"], lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ( -1*exp(-1*muK5*muQ5*x/( 1 + 2*muQ5 )) + 2*muH5/muK5 )^(1/muQ5),
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

    #composite trajectory
    lines(x = x,
          y=( -1*exp(-1*K1[z]*Q1[z]*x/( 1 + 2*Q1[z] )) + 2*H1[z]/K1[z] )^(1/Q1[z]) +
            ( -1*exp(-1*K2[z]*Q2[z]*x/( 1 + 2*Q2[z] )) + 2*H2[z]/K2[z] )^(1/Q2[z]) +
            ( -1*exp(-1*K3[z]*Q3[z]*x/( 1 + 2*Q3[z] )) + 2*H3[z]/K3[z] )^(1/Q3[z]) +
            ( -1*exp(-1*K4[z]*Q4[z]*x/( 1 + 2*Q4[z] )) + 2*H4[z]/K4[z] )^(1/Q4[z]) +
            ( -1*exp(-1*K5[z]*Q5[z]*x/( 1 + 2*Q5[z] )) + 2*H5[z]/K5[z] )^(1/Q5[z]),
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  # mean trajectory
  lines(x = x,
        y = ( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1 )^(1/muQ1) +
            ( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2 )^(1/muQ2) +
            ( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3 )^(1/muQ3) +
            ( -1*exp(-1*muK4*muQ4*x/( 1 + 2*muQ4 )) + 2*muH4/muK4 )^(1/muQ4) +
            ( -1*exp(-1*muK5*muQ5*x/( 1 + 2*muQ5 )) + 2*muH5/muK5 )^(1/muQ5),
          col="black", lwd=3, lty=1)



graphics.off()



