

###############################################################################


######## load posteriors from complete models, need to run PrepareData to plot raw data points

# male
post3_m <- readRDS("./analysis_hw_mal/post6.RDS")


# female
post3_f <- readRDS("./analysis_hw_fem/post6.RDS")






######## combine trajectory plots

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


# point and line colors and sizes
BerPoint_lwd <- 0.5
BerPoint_col <- colorlist["2.3"]
BerPoint_cex <- 0.5
BerPoint_pch <- 1

MatPoint_lwd <- 0.5
MatPoint_col <- colorlist["1.3"]
MatPoint_cex <- 0.5
MatPoint_pch <- 1


BerIndivTraj_lwd <- 0.25
BerIndivTraj_col <- colorlist["2.4"]
BerIndivTraj_lty <- 1

MatIndivTraj_lwd <- 0.25
MatIndivTraj_col <- colorlist["1.4"]
MatIndivTraj_lty <- 1


BerMeanTraj_lwd <- 3
BerMeanTraj_col <- colorlist["2.1"]
BerMeanTraj_lty <- 1

MatMeanTraj_lwd <- 3
MatMeanTraj_col <- colorlist["1.1"]
MatMeanTraj_lty <- 1


BerVelTraj_lwd <- 2
BerVelTraj_col <- colorlist2["2.2"]
BerVelTrajArea_col <- colorlist2["2.3"]
BerVelTraj_lty <- 1

MatVelTraj_lwd <- 2
MatVelTraj_col <- colorlist2["1.2"]
MatVelTrajArea_col <- colorlist2["1.4"]
MatVelTraj_lty <- 1



pdf(file="./combined_plots/Plots/Composite_model_combined_h.pdf",
    height=10, width=10)
layout( mat=matrix(c(1,2,3,4),2,2,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
#layout.show(4)
#par(xpd=TRUE) # clip at figure region, not plot region
#par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 5, 5)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age


  ################ female height
  post <- post3_f

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE )
  axis( side=2, at=seq( 0, 200, by=50), labels=TRUE )


  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 

  N <- nrow(Com.fem)                      # number of observations
  J <- length(unique(Com.fem$ID))         # number of people measured
  E <- length(unique(Com.fem$Ethnicity))  # number of ethnic groups
  EthID <- Com.fem[match(unique(Com.fem$ID), Com.fem$ID),c("ID","Ethnicity")] # ethnicity for each ID

  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$Height, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.fem$TotAge, y=Mat.fem$Height, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)




  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z



  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=4 ), labels=FALSE, las=2 )
  par(xpd=FALSE) # clip to plot region



  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)




  par(xpd=NA) # clip plotting to device region


  # set horizontal spacing for legend text
  legtext <- c("Data",
               "Estimated individual trajectories", 
               "Estimated mean trajectory",
               "Estimated mean velocity"
               )
  xcoords <- c(0,
               4,
               19.5,
               32
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-13.6, y=21,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c("white",
               "white",
               "white", 
               "white"
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd,
               BerMeanTraj_lwd,
               BerVelTraj_lwd
               ),
         seg.len=4 )



  # set horizontal spacing for legend text
  # legtext <- c("U.S. data",
  #              "Estimated U.S. individual trajectories", 
  #              "Estimated U.S. mean trajectory",
  #              "Estimated U.S. mean velocity"
  #              )
  legtext <- c("U.S.",
               "U.S.", 
               "U.S.",
               "U.S."
               )
  xcoords <- c(0,
               8.5,  #5.5
               24.2, #23
               36.8  #38
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-12, y=20,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(BerPoint_col,
               BerIndivTraj_col,
               BerMeanTraj_col, 
               BerVelTraj_col
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd,
               BerMeanTraj_lwd,
               BerVelTraj_lwd
               ),
         seg.len=4 )


  # set horizontal spacing for legend text
  # legtext <- c("Matsigenka",
  #              "Est. Matsigenka indiv. traj.",
  #              "Est. Matsigenka mean traj.",
  #              "Est. Matsigenka mean vel."
  #              )
  legtext <-c("Matsigenka",
               "Matsigenka",
               "Matsigenka",
               "Matsigenka"
               )
  xcoords <- c(0,
               8.5,
               24.2,
               36.8
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)


  legend(x=-12, y=19,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(MatPoint_col,
               MatIndivTraj_col,
               MatMeanTraj_col,
               MatVelTraj_col
              ),
         merge=FALSE,
         pch=c(MatPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               MatIndivTraj_lty,
               MatMeanTraj_lty,
               MatVelTraj_lty
               ),
         lwd=c(MatPoint_lwd,
               MatIndivTraj_lwd,
               MatMeanTraj_lwd,
               MatVelTraj_lwd
               ),
         seg.len=4 )

  rect(xleft = -7.5,
       ybottom = 17.3,
       xright = 62,
       ytop = 20.9,
       lwd=1)

  symbol.Female(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package



#graphics.off()




  ################ male height
  post <- post3_m

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 200, by=50), labels=FALSE )
  par(xpd=FALSE) # clip to plot region


  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 


  N <- nrow(Com.mal)                      # number of observations
  J <- length(unique(Com.mal$ID))         # number of people measured
  E <- length(unique(Com.mal$Ethnicity))  # number of ethnic groups
  EthID <- Com.mal[match(unique(Com.mal$ID), Com.mal$ID),c("ID","Ethnicity")] # ethnicity for each ID


  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.mal$TotAge, y=Mat.mal$Height, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)


 
  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)





  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=4 ), las=2, labels=FALSE )
  text( x=28.75, y=seq( 0, 16, by=4 ), labels=seq( 0, 16, by=4 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE)


  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)


  par(xpd=NA) # clip plotting to device region
  text("Growth velocity (cm/year)", x=31.5, y=8, srt=270, las=3, cex=1.8)

  symbol.Male(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package



  mtext("Age since conception (years)", side = 1, outer = T, cex = 1.5, line = -23, adj=0.5)
  mtext("Height (cm)", side = 2, outer = T, cex = 1.5, line = 2, adj=0.8) #


graphics.off()






#######################################################################################################################################
#######################################################################################################################################

######## weight








pdf(file="./combined_plots/Plots/Composite_model_combined_w.pdf",
    height=10, width=10)
layout( mat=matrix(c(1,2,3,4),2,2,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
#layout.show(4)
#par(xpd=TRUE) # clip at figure region, not plot region
#par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 5, 5)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age


  ################ female weight
  post <- post3_f

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,6000), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE )
  axis( side=2, at=seq( 0, 6000, by=1000), labels=TRUE )


  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 

  N <- nrow(Com.fem)                      # number of observations
  J <- length(unique(Com.fem$ID))         # number of people measured
  E <- length(unique(Com.fem$Ethnicity))  # number of ethnic groups
  EthID <- Com.fem[match(unique(Com.fem$ID), Com.fem$ID),c("ID","Ethnicity")] # ethnicity for each ID

  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$CellWeightg, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.fem$TotAge, y=Mat.fem$CellWeightg, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)




  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,400), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 400, by=50 ), labels=FALSE, las=2 )
  par(xpd=FALSE) # clip to plot region



  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] ) 
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)





  # new plot for figure legend 
  par(new=TRUE) #add to existing plot
  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  par(xpd=NA) # clip plotting to device region


  # set horizontal spacing for legend text
  legtext <- c("Data",
               "Estimated individual trajectories", 
               "Estimated mean trajectory",
               "Estimated mean velocity"
               )
  xcoords <- c(0,
               4,
               19.5,
               32
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-13.6, y=21,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c("white",
               "white",
               "white", 
               "white"
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd,
               BerMeanTraj_lwd,
               BerVelTraj_lwd
               ),
         seg.len=4 )



  # set horizontal spacing for legend text
  # legtext <- c("U.S. data",
  #              "Estimated U.S. individual trajectories", 
  #              "Estimated U.S. mean trajectory",
  #              "Estimated U.S. mean velocity"
  #              )
  legtext <- c("U.S.",
               "U.S.", 
               "U.S.",
               "U.S."
               )
  xcoords <- c(0,
               8.5,  #5.5
               24.2, #23
               36.8  #38
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-12, y=20,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(BerPoint_col,
               BerIndivTraj_col,
               BerMeanTraj_col, 
               BerVelTraj_col
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd,
               BerMeanTraj_lwd,
               BerVelTraj_lwd
               ),
         seg.len=4 )


  # set horizontal spacing for legend text
  # legtext <- c("Matsigenka",
  #              "Est. Matsigenka indiv. traj.",
  #              "Est. Matsigenka mean traj.",
  #              "Est. Matsigenka mean vel."
  #              )
  legtext <-c("Matsigenka",
               "Matsigenka",
               "Matsigenka",
               "Matsigenka"
               )
  xcoords <- c(0,
               8.5,
               24.2,
               36.8
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)


  legend(x=-12, y=19,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(MatPoint_col,
               MatIndivTraj_col,
               MatMeanTraj_col,
               MatVelTraj_col
              ),
         merge=FALSE,
         pch=c(MatPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               MatIndivTraj_lty,
               MatMeanTraj_lty,
               MatVelTraj_lty
               ),
         lwd=c(MatPoint_lwd,
               MatIndivTraj_lwd,
               MatMeanTraj_lwd,
               MatVelTraj_lwd
               ),
         seg.len=4 )

  rect(xleft = -7.5,
       ybottom = 17.3,
       xright = 62,
       ytop = 20.9,
       lwd=1)

  symbol.Female(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package



#graphics.off()




  ################ male weight
  post <- post3_m

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,6000), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 6000, by=1000), labels=FALSE )
  par(xpd=FALSE) # clip to plot region


  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 


  N <- nrow(Com.mal)                      # number of observations
  J <- length(unique(Com.mal$ID))         # number of people measured
  E <- length(unique(Com.mal$Ethnicity))  # number of ethnic groups
  EthID <- Com.mal[match(unique(Com.mal$ID), Com.mal$ID),c("ID","Ethnicity")] # ethnicity for each ID


  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$CellWeightg, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.mal$TotAge, y=Mat.mal$CellWeightg, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)


 
  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)





  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,400), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 400, by=50 ), las=2, labels=FALSE )
  text( x=28.75, y=seq( 0, 400, by=50 ), labels=seq( 0, 400, by=50 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE)


  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] ) 
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] ) 
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)




  # new plot for figure labels 
  par(new=TRUE) #add to existing plot
  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  par(xpd=NA) # clip plotting to device region

  text("Growth velocity (g/year)", x=31.5, y=8, srt=270, las=3, cex=1.8)

  symbol.Male(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package


  mtext("Age since conception (years)", side = 1, outer = T, cex = 1.5, line = -23, adj=0.5)
  mtext("Skeletal cell weight (g)", side = 2, outer = T, cex = 1.5, line = 2, adj=0.85) #0.83)


graphics.off()







####################################################################################################
####################################################################################################
##### height and unbiased weight


# male
post3_m <- readRDS("./analysis_hwfull_mal/post6.RDS")
#str(post3_m)

# female
post3_f <- readRDS("./analysis_hwfull_fem/post6.RDS")
#str(post3_f)






pdf(file="./combined_plots/Plots/Composite_model_combined_unbiased.pdf",
    height=10, width=10)
layout( mat=matrix(c(1,2,3,4),2,2,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
#layout.show(4)
#par(xpd=TRUE) # clip at figure region, not plot region
#par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 5, 5)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age


  ################ female height
  post <- post3_f

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=seq( 0, 200, by=50), labels=TRUE )


  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 

  N <- nrow(Com.fem)                      # number of observations
  J <- length(unique(Com.fem$ID))         # number of people measured
  E <- length(unique(Com.fem$Ethnicity))  # number of ethnic groups
  EthID <- Com.fem[match(unique(Com.fem$ID), Com.fem$ID),c("ID","Ethnicity")] # ethnicity for each ID

  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$Height, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.fem$TotAge, y=Mat.fem$Height, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)




  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  #axis( side=4, at=seq( 0, 16, by=4 ), labels=FALSE, las=2 )
  par(xpd=FALSE) # clip to plot region



  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)




  par(xpd=NA) # clip plotting to device region


  # set horizontal spacing for legend text
  legtext <- c("Data",
               "Estimated individual trajectories", 
               "Estimated mean trajectory",
               "Estimated mean velocity"
               )
  xcoords <- c(0,
               4,
               19.5,
               32
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-13.6, y=21,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c("white",
               "white",
               "white", 
               "white"
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd,
               BerMeanTraj_lwd,
               BerVelTraj_lwd
               ),
         seg.len=4 )



  # set horizontal spacing for legend text
  # legtext <- c("U.S. data",
  #              "Estimated U.S. individual trajectories", 
  #              "Estimated U.S. mean trajectory",
  #              "Estimated U.S. mean velocity"
  #              )
  legtext <- c("U.S.",
               "U.S.", 
               "U.S.",
               "U.S."
               )
  xcoords <- c(0,
               8.5,  #5.5
               24.2, #23
               36.8  #38
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-12, y=20,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(BerPoint_col,
               BerIndivTraj_col,
               BerMeanTraj_col, 
               BerVelTraj_col
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd,
               BerMeanTraj_lwd,
               BerVelTraj_lwd
               ),
         seg.len=4 )


  # set horizontal spacing for legend text
  # legtext <- c("Matsigenka",
  #              "Est. Matsigenka indiv. traj.",
  #              "Est. Matsigenka mean traj.",
  #              "Est. Matsigenka mean vel."
  #              )
  legtext <-c("Matsigenka",
               "Matsigenka",
               "Matsigenka",
               "Matsigenka"
               )
  xcoords <- c(0,
               8.5,
               24.2,
               36.8
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)


  legend(x=-12, y=19,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(MatPoint_col,
               MatIndivTraj_col,
               MatMeanTraj_col,
               MatVelTraj_col
              ),
         merge=FALSE,
         pch=c(MatPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               MatIndivTraj_lty,
               MatMeanTraj_lty,
               MatVelTraj_lty
               ),
         lwd=c(MatPoint_lwd,
               MatIndivTraj_lwd,
               MatMeanTraj_lwd,
               MatVelTraj_lwd
               ),
         seg.len=4 )

  rect(xleft = -7.5,
       ybottom = 17.3,
       xright = 62,
       ytop = 20.9,
       lwd=1)

  symbol.Female(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package




  ################ male height
  post <- post3_m

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=FALSE )
  axis( side=2, at=seq( 0, 200, by=50), labels=FALSE )
  par(xpd=FALSE) # clip to plot region


 Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 


  N <- nrow(Com.mal)                      # number of observations
  J <- length(unique(Com.mal$ID))         # number of people measured
  E <- length(unique(Com.mal$Ethnicity))  # number of ethnic groups
  EthID <- Com.mal[match(unique(Com.mal$ID), Com.mal$ID),c("ID","Ethnicity")] # ethnicity for each ID


  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.mal$TotAge, y=Mat.mal$Height, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)


 
  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z



  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)





  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=4 ), las=2, labels=FALSE )
  text( x=28.75, y=seq( 0, 16, by=4 ), labels=seq( 0, 16, by=4 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE)


  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)


  par(xpd=NA) # clip plotting to device region
  text("Growth velocity (cm/year)", x=31.5, y=8, srt=270, las=3, cex=1.8)

  symbol.Male(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package




  ################ female weight
  post <- post3_f

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,6000), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 6000, by=1000), labels=TRUE )
  par(xpd=FALSE) # clip to plot region


  
  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 

  N <- nrow(Com.fem)                      # number of observations
  J <- length(unique(Com.fem$ID))         # number of people measured
  E <- length(unique(Com.fem$Ethnicity))  # number of ethnic groups
  EthID <- Com.fem[match(unique(Com.fem$ID), Com.fem$ID),c("ID","Ethnicity")] # ethnicity for each ID

  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$CellWeightg, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.fem$TotAge, y=Mat.fem$CellWeightg, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)




  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,400), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 400, by=50 ), labels=FALSE, las=2 )
  par(xpd=FALSE) # clip to plot region



  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] ) 
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)



  par(xpd=TRUE)
  #text("Growth velocity (kg/year)", x=34, y=8, srt=270, las=3)


  par(new=TRUE) #add to existing plot
  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  par(xpd=NA) # clip plotting to device region

  symbol.Female(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package



  ################ male weight
  post <- post3_m

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,6000), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 6000, by=1000), labels=FALSE )
  par(xpd=FALSE) # clip to plot region


  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 


  N <- nrow(Com.mal)                      # number of observations
  J <- length(unique(Com.mal$ID))         # number of people measured
  E <- length(unique(Com.mal$Ethnicity))  # number of ethnic groups
  EthID <- Com.mal[match(unique(Com.mal$ID), Com.mal$ID),c("ID","Ethnicity")] # ethnicity for each ID


  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$CellWeightg, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.mal$TotAge, y=Mat.mal$CellWeightg, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)


 
  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)





  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,400), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 400, by=50 ), las=2, labels=FALSE )
  text( x=28.75, y=seq( 0, 400, by=50 ), labels=seq( 0, 400, by=50 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE)


  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] ) 
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] ) 
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)



  par(new=TRUE) #add to existing plot
  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  par(xpd=NA) # clip plotting to device region

  text("Growth velocity (g/year)", x=31.5, y=8, srt=270, las=3, cex=1.8)

  symbol.Male(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package


  mtext("Age since conception (years)", side = 1, outer = T, cex = 1.5, line = 2, adj=0.5)
  mtext("Height (cm)", side = 2, outer = T, cex = 1.5, line = 2, adj=0.8)
  mtext("Skeletal cell weight (g)", side = 2, outer = T, cex = 1.5, line = 2, adj=0.15) #0.2



graphics.off()








####################################################################################################
####################################################################################################
##### just fit to height


# male
post3_m <- readRDS("./analysis_height_mal/post6.RDS")
#str(post3_m)

# female
post3_f <- readRDS("./analysis_height_fem/post6.RDS")
#str(post3_f)






pdf(file="./combined_plots/Plots/Composite_model_combined_justHeight.pdf",
    height=10, width=10)
layout( mat=matrix(c(1,2,3,4),2,2,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
#layout.show(4)
#par(xpd=TRUE) # clip at figure region, not plot region
#par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 5, 5)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age


  ################ female height
  post <- post3_f

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=seq( 0, 200, by=50), labels=TRUE )


  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 

  N <- nrow(Com.fem)                      # number of observations
  J <- length(unique(Com.fem$ID))         # number of people measured
  E <- length(unique(Com.fem$Ethnicity))  # number of ethnic groups
  EthID <- Com.fem[match(unique(Com.fem$ID), Com.fem$ID),c("ID","Ethnicity")] # ethnicity for each ID

  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$Height, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.fem$TotAge, y=Mat.fem$Height, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)




  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  #axis( side=4, at=seq( 0, 16, by=4 ), labels=FALSE, las=2 )
  par(xpd=FALSE) # clip to plot region



  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)




  par(xpd=NA) # clip plotting to device region


  # set horizontal spacing for legend text
  legtext <- c("Data",
               "Estimated individual trajectories", 
               "Estimated mean trajectory",
               "Estimated mean velocity"
               )
  xcoords <- c(0,
               4,
               19.5,
               32
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-13.6, y=21,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c("white",
               "white",
               "white", 
               "white"
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd,
               BerMeanTraj_lwd,
               BerVelTraj_lwd
               ),
         seg.len=4 )



  # set horizontal spacing for legend text
  # legtext <- c("U.S. data",
  #              "Estimated U.S. individual trajectories", 
  #              "Estimated U.S. mean trajectory",
  #              "Estimated U.S. mean velocity"
  #              )
  legtext <- c("U.S.",
               "U.S.", 
               "U.S.",
               "U.S."
               )
  xcoords <- c(0,
               8.5,  #5.5
               24.2, #23
               36.8  #38
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-12, y=20,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(BerPoint_col,
               BerIndivTraj_col,
               BerMeanTraj_col, 
               BerVelTraj_col
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd,
               BerMeanTraj_lwd,
               BerVelTraj_lwd
               ),
         seg.len=4 )


  # set horizontal spacing for legend text
  # legtext <- c("Matsigenka",
  #              "Est. Matsigenka indiv. traj.",
  #              "Est. Matsigenka mean traj.",
  #              "Est. Matsigenka mean vel."
  #              )
  legtext <-c("Matsigenka",
               "Matsigenka",
               "Matsigenka",
               "Matsigenka"
               )
  xcoords <- c(0,
               8.5,
               24.2,
               36.8
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)


  legend(x=-12, y=19,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(MatPoint_col,
               MatIndivTraj_col,
               MatMeanTraj_col,
               MatVelTraj_col
              ),
         merge=FALSE,
         pch=c(MatPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               MatIndivTraj_lty,
               MatMeanTraj_lty,
               MatVelTraj_lty
               ),
         lwd=c(MatPoint_lwd,
               MatIndivTraj_lwd,
               MatMeanTraj_lwd,
               MatVelTraj_lwd
               ),
         seg.len=4 )

  rect(xleft = -7.5,
       ybottom = 17.3,
       xright = 62,
       ytop = 20.9,
       lwd=1)

  symbol.Female(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package




  ################ male height
  post <- post3_m

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=FALSE )
  axis( side=2, at=seq( 0, 200, by=50), labels=FALSE )
  par(xpd=FALSE) # clip to plot region


 Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 


  N <- nrow(Com.mal)                      # number of observations
  J <- length(unique(Com.mal$ID))         # number of people measured
  E <- length(unique(Com.mal$Ethnicity))  # number of ethnic groups
  EthID <- Com.mal[match(unique(Com.mal$ID), Com.mal$ID),c("ID","Ethnicity")] # ethnicity for each ID


  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]),

                ifelse( x <= I[2], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]),

                ifelse( x <= I[3], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]),

                ifelse( x <= I[4], 0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]),

                                   0.012 + ( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1]) +
                                           ( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2]) +
                                           ( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3]) +
                                           ( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4]) +
                                           ( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5])

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.mal$TotAge, y=Mat.mal$Height, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)


 
  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z



  lines(x = x,
        y = ifelse( x < mI[1], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]),

            ifelse( x < mI[2], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]),

            ifelse( x < mI[3], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]),

            ifelse( x < mI[4], 0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]),

                               0.012 + ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]) +
                                       ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]) +
                                       ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]) +
                                       ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]) +
                                       ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5])
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)





  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=4 ), las=2, labels=FALSE )
  text( x=28.75, y=seq( 0, 16, by=4 ), labels=seq( 0, 16, by=4 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE)


  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0    - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ),

             ifelse( x < mI[2], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ),

             ifelse( x < mI[3], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ),

             ifelse( x < mI[4], 1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ),

                                1/( 1 + 2*mQ[1] )*2^(1/mQ[1])*mH[1]*exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] ))*(mH[1]/mK[1]*( 1 - exp((mK[1]*mQ[1]*( 0     - x ))/( 1 + 2*mQ[1] )) ))^( 1/mQ[1] - 1 ) +
                                1/( 1 + 2*mQ[2] )*2^(1/mQ[2])*mH[2]*exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] ))*(mH[2]/mK[2]*( 1 - exp((mK[2]*mQ[2]*( mI[1] - x ))/( 1 + 2*mQ[2] )) ))^( 1/mQ[2] - 1 ) +
                                1/( 1 + 2*mQ[3] )*2^(1/mQ[3])*mH[3]*exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] ))*(mH[3]/mK[3]*( 1 - exp((mK[3]*mQ[3]*( mI[2] - x ))/( 1 + 2*mQ[3] )) ))^( 1/mQ[3] - 1 ) +
                                1/( 1 + 2*mQ[4] )*2^(1/mQ[4])*mH[4]*exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] ))*(mH[4]/mK[4]*( 1 - exp((mK[4]*mQ[4]*( mI[3] - x ))/( 1 + 2*mQ[4] )) ))^( 1/mQ[4] - 1 ) +
                                1/( 1 + 2*mQ[5] )*2^(1/mQ[5])*mH[5]*exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] ))*(mH[5]/mK[5]*( 1 - exp((mK[5]*mQ[5]*( mI[4] - x ))/( 1 + 2*mQ[5] )) ))^( 1/mQ[5] - 1 )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)


  par(xpd=NA) # clip plotting to device region
  text("Growth velocity (cm/year)", x=31.5, y=8, srt=270, las=3, cex=1.8)

  symbol.Male(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package




  ################ female weight
  post <- post3_f

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,6000), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 6000, by=1000), labels=TRUE )
  par(xpd=FALSE) # clip to plot region


  
  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 

  N <- nrow(Com.fem)                      # number of observations
  J <- length(unique(Com.fem$ID))         # number of people measured
  E <- length(unique(Com.fem$Ethnicity))  # number of ethnic groups
  EthID <- Com.fem[match(unique(Com.fem$ID), Com.fem$ID),c("ID","Ethnicity")] # ethnicity for each ID

  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$CellWeightg, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.fem$TotAge, y=Mat.fem$CellWeightg, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)




  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,400), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 400, by=50 ), labels=FALSE, las=2 )
  par(xpd=FALSE) # clip to plot region



  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] ) 
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] )
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)



  par(xpd=TRUE)
  #text("Growth velocity (kg/year)", x=34, y=8, srt=270, las=3)


  par(new=TRUE) #add to existing plot
  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  par(xpd=NA) # clip plotting to device region

  symbol.Female(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package



  ################ male weight
  post <- post3_m

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,6000), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 6000, by=1000), labels=FALSE )
  par(xpd=FALSE) # clip to plot region


  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 


  N <- nrow(Com.mal)                      # number of observations
  J <- length(unique(Com.mal$ID))         # number of people measured
  E <- length(unique(Com.mal$Ethnicity))  # number of ethnic groups
  EthID <- Com.mal[match(unique(Com.mal$ID), Com.mal$ID),c("ID","Ethnicity")] # ethnicity for each ID


  # plot individual posterior trajectories
  for ( j in 1:J ){ 

    for ( z in 1:5 ){
      Q[z] <- mean( pull(post, paste("Q[", j, ",", z, "]", sep="")) ) #pull is a command from package dplyr to extract a vector from a tibble
      K[z] <- mean( pull(post, paste("K[", j, ",", z, "]", sep="")) )
      H[z] <- mean( pull(post, paste("H[", j, ",", z, "]", sep="")) )
      I[z] <- mean( pull(post, paste("I[", j, ",", z, "]", sep="")) )
    } # for z

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

    } else {            # if Matsigenka

      lines(x = x,
            y = ifelse( x <= I[1], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2),

                ifelse( x <= I[2], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2),

                ifelse( x <= I[3], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2),

                ifelse( x <= I[4], 1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2),                                                                                                                                                                  

                                   1.02e-6 + pi*( 2*H[1]/K[1] * ( 1 - exp(K[1]*Q[1]*( 0 -    x )/( 1 + 2*Q[1] )) ) )^(1/Q[1] + 2) +
                                            pi*( 2*H[2]/K[2] * ( 1 - exp(K[2]*Q[2]*( I[1] - x )/( 1 + 2*Q[2] )) ) )^(1/Q[2] + 2) +
                                            pi*( 2*H[3]/K[3] * ( 1 - exp(K[3]*Q[3]*( I[2] - x )/( 1 + 2*Q[3] )) ) )^(1/Q[3] + 2) +
                                            pi*( 2*H[4]/K[4] * ( 1 - exp(K[4]*Q[4]*( I[3] - x )/( 1 + 2*Q[4] )) ) )^(1/Q[4] + 2) +
                                            pi*( 2*H[5]/K[5] * ( 1 - exp(K[5]*Q[5]*( I[4] - x )/( 1 + 2*Q[5] )) ) )^(1/Q[5] + 2)

              ) ) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$CellWeightg, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.mal$TotAge, y=Mat.mal$CellWeightg, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)


 
  # Berkeley

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)




  # Matsigenka

  # plot posterior mean trajectory
  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z


  lines(x = x,
          y = ifelse( x <= mI[1], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2),

              ifelse( x <= mI[2], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2),

              ifelse( x <= mI[3], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2),

              ifelse( x <= mI[4], 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                           pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                           pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                           pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2),                                                                                                                                                                  

                                 1.02e-6 + pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2) +
                                          pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2) +
                                          pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2) +
                                          pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2) +
                                          pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2)
          ) ) ) ),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)





  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,400), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 400, by=50 ), las=2, labels=FALSE )
  text( x=28.75, y=seq( 0, 400, by=50 ), labels=seq( 0, 400, by=50 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE)


  # Berkeley

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 1, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 1, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 1, ",", z, "]", sep="")) )
  } # for z

  y_primeBer = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] ) 
             ) ) ) )                
  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # Matsigenka
  
  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 2, ",", z, "]", sep="")) ) 
    mK[z] <- mean( pull(post, paste("mK[", 2, ",", z, "]", sep="")) )
    mH[z] <- mean( pull(post, paste("mH[", 2, ",", z, "]", sep="")) )
    mI[z] <- mean( pull(post, paste("mI[", 2, ",", z, "]", sep="")) )
  } # for z

  y_primeMat = ifelse( x < mI[1], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ),

            ifelse( x < mI[2], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ),

            ifelse( x < mI[3], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ),

            ifelse( x < mI[4], (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                               (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                               (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                               (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ),

                              (pi*mH[1]*( 2*mQ[1] + 1 )*2^( 2 + 1/mQ[1] ))/( 1 + 2*mQ[1] )*exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] ))*( mH[1]/mK[1]*( 1 - exp(mK[1]*mQ[1]*( 0     - x )/( 1 + 2*mQ[1] )) ) )^( 1 + 1/mQ[1] ) + 
                              (pi*mH[2]*( 2*mQ[2] + 1 )*2^( 2 + 1/mQ[2] ))/( 1 + 2*mQ[2] )*exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] ))*( mH[2]/mK[2]*( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^( 1 + 1/mQ[2] ) +
                              (pi*mH[3]*( 2*mQ[3] + 1 )*2^( 2 + 1/mQ[3] ))/( 1 + 2*mQ[3] )*exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] ))*( mH[3]/mK[3]*( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^( 1 + 1/mQ[3] ) +
                              (pi*mH[4]*( 2*mQ[4] + 1 )*2^( 2 + 1/mQ[4] ))/( 1 + 2*mQ[4] )*exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] ))*( mH[4]/mK[4]*( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^( 1 + 1/mQ[4] ) +
                              (pi*mH[5]*( 2*mQ[5] + 1 )*2^( 2 + 1/mQ[5] ))/( 1 + 2*mQ[5] )*exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] ))*( mH[5]/mK[5]*( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^( 1 + 1/mQ[5] ) 
             ) ) ) )                
  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)



  par(new=TRUE) #add to existing plot
  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  par(xpd=NA) # clip plotting to device region

  text("Growth velocity (g/year)", x=31.5, y=8, srt=270, las=3, cex=1.8)

  symbol.Male(centerx = 5, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package


  mtext("Age since conception (years)", side = 1, outer = T, cex = 1.5, line = 2, adj=0.5)
  mtext("Height (cm)", side = 2, outer = T, cex = 1.5, line = 2, adj=0.8)
  mtext("Skeletal cell weight (g)", side = 2, outer = T, cex = 1.5, line = 2, adj=0.15) #0.2



graphics.off()







