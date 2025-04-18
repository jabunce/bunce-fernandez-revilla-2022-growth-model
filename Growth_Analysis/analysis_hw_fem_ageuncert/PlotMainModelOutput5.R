

################## m6 composite model for Berkeley and Matsigenka, covarying random effects for individual

post <- post6



######## plotting colors

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





pdf(file="./Plots/Composite_model_sample_mat_ber_randeff_cov5h.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )


  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 

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
  points(x=Com.fem[which(Com.fem$Ethnicity==1),]$RandAge, y=Ber.fem$Height, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex)
  points(x=Com.fem[which(Com.fem$Ethnicity==2),]$RandAge, y=Mat.fem$Height, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex)


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

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=2 ), las=2 )
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



  par(xpd=TRUE)
  text("Growth velocity (cm/year)", x=34, y=8, srt=270, las=3)

  legend(-1, 16.65,
         legend=c("U.S. data","Matsigenka",
                  "Estimated U.S. individual trajectories", "Matsigenka",
                 "Estimated U.S. mean trajectory","Matsigenka",
                 "Estimated U.S. velocity","Matsigenka"
                 ), bty="o", bg="white",
         col=c(BerPoint_col, MatPoint_col,
                BerIndivTraj_col,MatIndivTraj_col,
                BerMeanTraj_col, MatMeanTraj_col,
                BerVelTraj_col, MatVelTraj_col),
         merge=FALSE,
         pch=c(1,1, NA,NA, NA,NA, NA,NA),
         lty=c(0,0, BerIndivTraj_lty,MatIndivTraj_lty, BerMeanTraj_lty,MatMeanTraj_lty, BerVelTraj_lty,MatVelTraj_lty),
         lwd=c(BerPoint_lwd,MatPoint_lwd, BerIndivTraj_lwd,MatIndivTraj_lwd, BerMeanTraj_lwd,MatMeanTraj_lwd, BerVelTraj_lwd,MatVelTraj_lwd), cex=0.45, seg.len=4)

symbol.Female(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()



###########################################################################################


post <- post6

pdf(file="./Plots/Composite_model_sample_mat_ber_randeff_cov5w.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,6000), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Weight (g)" )



  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 

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
  points(x=Com.fem[which(Com.fem$Ethnicity==1),]$RandAge, y=Ber.fem$CellWeightg, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex)
  points(x=Com.fem[which(Com.fem$Ethnicity==2),]$RandAge, y=Mat.fem$CellWeightg, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex)


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

  plot( x=0, y=16, type="n", ylim=c(0,400), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 400, by=50 ), las=2 )
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



  par(xpd=TRUE)
  text("Growth velocity (g/year)", x=34, y=200, srt=270, las=3)

  legend(-1, 400.65,
         legend=c("U.S. data","Matsigenka",
                  "Estimated U.S. individual trajectories", "Matsigenka",
                 "Estimated U.S. mean trajectory","Matsigenka",
                 "Estimated U.S. velocity","Matsigenka"
                 ), bty="o", bg="white",
         col=c(BerPoint_col, MatPoint_col,
                BerIndivTraj_col,MatIndivTraj_col,
                BerMeanTraj_col, MatMeanTraj_col,
                BerVelTraj_col, MatVelTraj_col),
         merge=FALSE,
         pch=c(1,1, NA,NA, NA,NA, NA,NA),
         lty=c(0,0, BerIndivTraj_lty,MatIndivTraj_lty, BerMeanTraj_lty,MatMeanTraj_lty, BerVelTraj_lty,MatVelTraj_lty),
         lwd=c(BerPoint_lwd,MatPoint_lwd, BerIndivTraj_lwd,MatIndivTraj_lwd, BerMeanTraj_lwd,MatMeanTraj_lwd, BerVelTraj_lwd,MatVelTraj_lwd), cex=0.45, seg.len=4)

symbol.Female(centerx = 23, centery = 350, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()




################################################################################################################



# ######################## Plot parameter posteriors for composite model


post <- post6


# function to calculate proportions of phases for weighting by age
propparam <- function(x=1, Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                           K1=1, K2=1, K3=1, K4=1, K5=1,
                           H1=1, H2=1, H3=1, H4=1, H5=1,
                           I1=1, I2=1, I3=1, I4=1){
      
      I0 <- rep(0, times=length(I1))

      # total height grown since egg size at age x
      totHeight <- ifelse( x <= I1, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1),

                   ifelse( x <= I2, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2),

                   ifelse( x <= I3, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2) +
                                            ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3),

                   ifelse( x <= I4, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2) +
                                            ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3) +
                                            ( 2*H4/K4 * ( 1 - exp(K4*Q4*( I3 - x )/( 1 + 2*Q4 )) ) )^(1/Q4),

                                    0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2) +
                                            ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3) +
                                            ( 2*H4/K4 * ( 1 - exp(K4*Q4*( I3 - x )/( 1 + 2*Q4 )) ) )^(1/Q4) +
                                            ( 2*H5/K5 * ( 1 - exp(K5*Q5*( I4 - x )/( 1 + 2*Q5 )) ) )^(1/Q5)

                   ) ) ) ) - 0.012

      # heights of each process at time x
      Height1 <- ifelse( x >= I0 , ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1), 0) 

      Height2 <- ifelse( x >= I1 , ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2), 0) 

      Height3 <- ifelse( x >= I2 , ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3), 0) 

      Height4 <- ifelse( x >= I3 , ( 2*H4/K4 * ( 1 - exp(K4*Q4*( I3 - x )/( 1 + 2*Q4 )) ) )^(1/Q4), 0) 

      Height5 <- ifelse( x >= I4 , ( 2*H5/K5 * ( 1 - exp(K5*Q5*( I4 - x )/( 1 + 2*Q5 )) ) )^(1/Q5), 0) 

      # vector of proportions of total height contributed by each process at time x
      p <- rbind(Height1/totHeight,
                 Height2/totHeight,
                 Height3/totHeight,
                 Height4/totHeight,
                 Height5/totHeight)


      # max asymptotic height of each process
      maxHeight1 <- (2*H1/K1)^(1/Q1)
      maxHeight2 <- (2*H2/K2)^(1/Q2)
      maxHeight3 <- (2*H3/K3)^(1/Q3)
      maxHeight4 <- (2*H4/K4)^(1/Q4)
      maxHeight5 <- (2*H5/K5)^(1/Q5)

      # vector of proportions of original metabolic activity for each process at time x.
      # On average, cells lose 75% of their metabolic activity by the time an individual growth process finishes at asymptote, or 75% of cells convert to things that don't divide.
      # Mostly because active bone marrow converted to marrow fat, and bone cells converted to bone matrix.
      m <- rbind( 1 - 0.75*(Height1/maxHeight1),
                  1 - 0.75*(Height2/maxHeight2),
                  1 - 0.75*(Height3/maxHeight3),
                  1 - 0.75*(Height4/maxHeight4),
                  1 - 0.75*(Height5/maxHeight5) )


      return( list(p=p,m=m) )

} # propparam


vpropparam <- Vectorize(propparam) # vectorize the function so that it can take vectors as arguments


a <- seq(from=(0.1), to=(26.75), by=0.1)  #c(0.1,1,2)




##### female

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

Q1ber <- post$"mQ[1,1]"
Q2ber <- post$"mQ[1,2]" 
Q3ber <- post$"mQ[1,3]" 
Q4ber <- post$"mQ[1,4]" 
Q5ber <- post$"mQ[1,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

K1ber <- post$"mK[1,1]"
K2ber <- post$"mK[1,2]" 
K3ber <- post$"mK[1,3]" 
K4ber <- post$"mK[1,4]" 
K5ber <- post$"mK[1,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]"
H3mat <- post$"mH[2,3]"
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

H1ber <- post$"mH[1,1]"
H2ber <- post$"mH[1,2]"
H3ber <- post$"mH[1,3]"
H4ber <- post$"mH[1,4]" 
H5ber <- post$"mH[1,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

I1ber <- post$"mI[1,1]"
I2ber <- post$"mI[1,2]" 
I3ber <- post$"mI[1,3]" 
I4ber <- post$"mI[1,4]" 


tQmat_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKmat_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHmat_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

tQber_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKber_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHber_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

conQ_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conK_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conH_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))


# don't multiply Q's by m. Metabolism doesn't affect allometry. H and K remain fairly constant after adolescence and adulthood, as reporterd for human metabolism

for ( t in 1:length(a) ){
      Pmat <- matrix(unlist(vpropparam(x=a[t],Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                              K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                              H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                              I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["p",]), nrow=5, byrow=FALSE)
      Pber <- matrix(unlist(vpropparam(x=a[t],Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                              K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                              H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                              I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["p",]), nrow=5, byrow=FALSE)

      Mmat <- matrix(unlist(vpropparam(x=a[t],Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                              K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                              H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                              I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["m",]), nrow=5, byrow=FALSE)
      Mber <- matrix(unlist(vpropparam(x=a[t],Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                              K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                              H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                              I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["m",]), nrow=5, byrow=FALSE)

      tQmat_f[t,] <- Pmat[1,]*Q1mat +
                     Pmat[2,]*Q2mat +
                     Pmat[3,]*Q3mat +
                     Pmat[4,]*Q4mat +
                     Pmat[5,]*Q5mat

      tKmat_f[t,] <- Pmat[1,]*Mmat[1,]*K1mat +
                     Pmat[2,]*Mmat[2,]*K2mat +
                     Pmat[3,]*Mmat[3,]*K3mat +
                     Pmat[4,]*Mmat[4,]*K4mat +
                     Pmat[5,]*Mmat[5,]*K5mat

      tHmat_f[t,] <- (Pmat[1,]*Mmat[1,]*H1mat*(2/32) + # original H in units of g/cm^2 of 5% of total body skin surface. Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
                      Pmat[2,]*Mmat[2,]*H2mat*(2/32) +
                      Pmat[3,]*Mmat[3,]*H3mat*(2/32) +
                      Pmat[4,]*Mmat[4,]*H4mat*(2/32) +
                      Pmat[5,]*Mmat[5,]*H5mat*(2/32) )



      tQber_f[t,] <- Pber[1,]*Q1ber +
                     Pber[2,]*Q2ber +
                     Pber[3,]*Q3ber +
                     Pber[4,]*Q4ber +
                     Pber[5,]*Q5ber

      tKber_f[t,] <- Pber[1,]*Mber[1,]*K1ber +
                     Pber[2,]*Mber[2,]*K2ber +
                     Pber[3,]*Mber[3,]*K3ber +
                     Pber[4,]*Mber[4,]*K4ber +
                     Pber[5,]*Mber[5,]*K5ber
                   
      tHber_f[t,] <- (Pber[1,]*Mber[1,]*H1ber*(2/32) +
                      Pber[2,]*Mber[2,]*H2ber*(2/32) +
                      Pber[3,]*Mber[3,]*H3ber*(2/32) +
                      Pber[4,]*Mber[4,]*H4ber*(2/32) +
                      Pber[5,]*Mber[5,]*H5ber*(2/32) )

      #contrasts
      conQ_f[t,] <- tQber_f[t,] - tQmat_f[t,]
      conK_f[t,] <- tKber_f[t,] - tKmat_f[t,] 
      conH_f[t,] <- tHber_f[t,] - tHmat_f[t,]  

} # for t




################################################


# line colors and sizes
colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.5)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)

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


BerArea_col <- colorlist["2.3"]

MatArea_col <- colorlist["1.3"]

ConLine_lwd <- 3
ConLine_col <- "black"
ConArea_col <- grey(0.5,alpha=0.5)

ZerLine_lwd <- 1
ZerLine_lty <- "33"       #lty: first number in string is dash length, second is white space length
ZerLine_col <- "black"


# values for plotting
Nsims <- length(post$"mQ[2,1]")
n_samps <- 100 # how many samples to draw from posterior for plotting
samps <- sample.int(Nsims, n_samps, replace=FALSE) #random draws from the posterior





pdf(file="./Plots/params_combined_f_ageuncert.pdf",
    height=10, width=10)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6
                      ),
        nrow=3, ncol=2, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1,1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 8)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels


# female  ###########################################################################################################################################################################


tQmat <- tQmat_f
tKmat <- tKmat_f
tHmat <- tHmat_f

tQber <- tQber_f
tKber <- tKber_f
tHber <- tHber_f

conQ <- conQ_f
conK <- conK_f
conH <- conH_f



################ Q

 #set up plot
  #par(mar=c(5, 6, 2, 6), oma = c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0.07, 0.18), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.10,0.18,0.02), las=1 )
  axis( side=1, at=seq(0,25,5), labels=NA, las=1 )
  box(col = "black") 
  par(xpd=FALSE)

  # for ( z in 1:n_samps ){

  #   # US
  #   lines(x = a,
  #         y = tQber[,samps[z]],
  #         col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

  #   # Matsigenka
  #   lines(x = a,
  #         y = tQmat[,samps[z]],
  #         col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

  # } #for z


  ints <-  t(apply(tQber, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = BerArea_col, border=NA)

  ints <-  t(apply(tQmat, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)


  lines(x = a,
        y = rowMeans(tQber),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = a,
        y = rowMeans(tQmat),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  #par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(0,0.2),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region


  #par(xpd=TRUE)
  #text(expression(paste(bolditalic("q"))), x=-7, y=0.13, srt=0, las=3, cex=1.5)
  #text("Age since conception (years)", x=13, y=0.055, srt=0, las=3)



  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.005,0.005+0.05), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.005, 0, 0.005), labels=c(-0.005, 0, 0.005), las=2 )
  par(xpd=FALSE)


  # for ( z in 1:n_samps ){

  #   lines(x = a,
  #         y = conQ[,samps[z]],
  #         col=grey(0.5), lwd=0.25, lty=1)

  # } #for z


  ints <-  t(apply(conQ, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = a,
        y = rowMeans(conQ),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=FALSE) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region

  # lines(x = c(min(a),max(a)),
  #       y = c(0,0),
  #       col="black", lwd=1, lty="33") #lty: first number in string is dash length, second is white space length


  par(xpd=TRUE)
  #text("Contrast", x=33, y=0, srt=270, las=3)



#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast")
xcoords <- c(0, 5, 12) # 0, 7, 23
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x=-9, y=15.2,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerMeanTraj_col, MatMeanTraj_col, ConLine_col),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=1.8,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = -9,
       ybottom = 13.3,
       xright = 17.91,
       ytop = 14.9,
       lwd=1)


# row labels

mtext(expression(paste(bolditalic("q"))),   side = 1, outer = T, cex = 1.5, adj=-0.42, line = -11 ) #side = c(bottom, left, top, right)






# column labels

symbol.Female(  centerx = 5, centery = 12, rayonx=0.4, lwd=1.5, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.385, line = -110.5 )


par(xpd=FALSE)


################### K

plot( x=0, y=60, type="n", ylim=c(4,24), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(8,24,4), labels=seq(8,24,4), las=1 )
  axis( side=1, at=seq(0,25,5), labels = NA, las=1 )
  box(col = "black") 
  par(xpd=FALSE)


  # for ( z in 1:n_samps ){

  #   # US
  #   lines(x = a,
  #         y = tKber[,samps[z]],
  #         col=colorlist["2.3"], lwd=0.25, lty=1)

  #   # Matsigenka
  #   lines(x = a,
  #         y = tKmat[,samps[z]],
  #         col=colorlist["1.3"], lwd=0.25, lty=1)

  # } #for z

  # all values of vertices passed to polygon() must be within the plotting range

  mina <- 0.75
  indexmina <- min(which(a > mina))

  ints <-  t(apply(tKber, 1, HPDI, prob=0.9))[,]  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = BerArea_col, border=NA)

  ints <-  t(apply(tKmat, 1, HPDI, prob=0.9))
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = MatArea_col, border=NA)


  lines(x = a,
        y = rowMeans(tKber),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = a,
        y = rowMeans(tKmat),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(4,50),
        col="black", lwd=1, lty=1)


  mtext(expression(paste(bolditalic("K"))),   side = 1, outer = T, cex = 1.5, adj=-0.42, line = -11 )
  mtext(expression(paste("(g/g)")),           side = 1, outer = T, cex = 1.5, adj=-0.55, line = -8 )

  par(xpd=FALSE) # plotting clipped to plot region


  par(xpd=TRUE)
  #text(expression(paste(bolditalic("K"))), x=-7, y=16, srt=0, las=3, cex=1.5)
  #text(expression(paste("(g/g)")), x=-7, y=15, srt=0, las=3, cex=1)
  #text("Age since conception (years)", x=13, y=2, srt=0, las=3)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.7-0.2,0.2+5), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.4, 0, 0.4), labels=c(-0.4, 0, 0.4), las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conK, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)

  lines(x = a,
        y = rowMeans(conK),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=FALSE) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region

  par(xpd=NA)
  text("Contrast", x=34, y=2, srt=270, las=3, cex=2.75)
  par(xpd=FALSE)


################ H


plot( x=0, y=60, type="n", ylim=c(0.33-0.15,1), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.3,1,0.1), labels=seq(0.3,1,0.1), las=1 )
  axis( side=1, at=seq(0,25,5), labels=seq(0,25,5), las=1 )
  box(col = "black") 
  par(xpd=FALSE)


  # for ( z in 1:n_samps ){

  #   # US
  #   lines(x = a,
  #         y = tHber[,samps[z]],
  #         col=colorlist["2.3"], lwd=0.25, lty=1)

  #   # Matsigenka
  #   lines(x = a,
  #         y = tHmat[,samps[z]],
  #         col=colorlist["1.3"], lwd=0.25, lty=1)

  # } #for z

  # all values of vertices passed to polygon() must be within the plotting range

  mina <- 0.75
  indexmina <- min(which(a > mina))

  ints <-  t(apply(tHber, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = BerArea_col, border=NA)

  ints <-  t(apply(tHmat, 1, HPDI, prob=0.9))
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = MatArea_col, border=NA)


  lines(x = a,
        y = rowMeans(tHber),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = a,
        y = rowMeans(tHmat),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(0,2),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region


  par(xpd=NA)
  #text(expression(paste(bolditalic("H"))), x=-7, y=12, srt=0, las=3, cex=1.5)
  #text(expression(paste("(g/cm"^2,")")), x=-7, y=11, srt=0, las=3, cex=1)
  #text("Age since conception (years)", x=13, y=1.5, srt=0, las=3)
  mtext(expression(paste(bolditalic("H"))),   side = 1, outer = T, cex = 1.5, adj=-0.42, line = -11 )
  mtext(expression(paste("(g/cm"^2,")")),     side = 1, outer = T, cex = 1.5, adj=-0.70, line = -8 )


  text("birth", x=0.75, y=-0.05, srt=0, las=3, cex=1.5)
  par(xpd=FALSE)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.03-0.005,0.005+0.15), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.02, 0, 0.02), labels=c(-0.02, 0, 0.02), las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conH, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = a,
        y = rowMeans(conH),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=FALSE) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region

  mtext("Age since conception (years)",       side = 1, outer = T, cex = 1.5, adj=0.6, line = 6.5 )




################ Q

 #set up plot
  #par(mar=c(5, 6, 2, 6), oma = c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0.07, 0.18), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  
par(xpd=FALSE)



################### K

plot( x=0, y=60, type="n", ylim=c(4,24), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )


  par(xpd=FALSE) # plotting clipped to plot region



################ H


plot( x=0, y=60, type="n", ylim=c(0.33-0.15,1), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  
  par(xpd=FALSE) # plotting clipped to plot region


graphics.off()



