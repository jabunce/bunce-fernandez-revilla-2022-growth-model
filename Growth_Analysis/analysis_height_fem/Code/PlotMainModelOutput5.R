

################## m6 composite model for Berkeley and Matsigenka, covarying random effects for individual

post <- post6

pdf(file="./Plots/Composite_model_sample_mat_ber_randeff_cov5.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )

  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$Height, lwd=0.5, col="red", cex=0.5)
  points(x=Mat.fem$TotAge, y=Mat.fem$Height, lwd=0.5, col="blue", cex=0.5)

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
            col=rgb(1,0,0,max=1,alpha=0.5), lwd=0.25, lty=1)

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
            col=rgb(0,0,1,max=1,alpha=0.5), lwd=0.25, lty=1)

    } # else

  } #for j


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
        col="dark red", lwd=3, lty=1)



  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col="red", lwd=3, lty=1)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col="red", lwd=3, lty=1)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col="red", lwd=3, lty=1)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col="red", lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col="red", lwd=3, lty=1)



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
        col="dark blue", lwd=3, lty=1)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col="blue", lwd=3, lty=1)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col="blue", lwd=3, lty=1)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col="blue", lwd=3, lty=1)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col="blue", lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col="blue", lwd=3, lty=1)





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
  lines(x = x, y=y_primeBer, col="orange", lwd=2, lty=1)


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
  lines(x = x, y=y_primeMat, col="cyan", lwd=2, lty=1)



  par(xpd=TRUE)
  text("Growth velocity (cm/year)", x=34, y=8, srt=270, las=3)

  legend(-1, 16.65,
         legend=c("U.S. data","Matsigenka",
                  "Estimated U.S. individual trajectories", "Matsigenka",
                 "Estimated U.S. mean trajectory","Matsigenka",
                 "Estimated U.S. velocity","Matsigenka"
                 ), bty="o", bg="white",
         col=c("red", "blue",
                rgb(1,0,0,max=1,alpha=0.5),rgb(0,0,1,max=1,alpha=0.5),
                "dark red", "dark blue",
                "orange", "cyan"),
         merge=FALSE,
         pch=c(1,1, NA,NA, NA,NA, NA,NA),
         lty=c(0,0, 1,1, 1,1, 1,1),
         lwd=c(0.5,0.5, 0.25,0.25, 3,3, 2,2), cex=0.45, seg.len=4)

symbol.Female(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()







# ######################## Plot parameter posteriors for composite model


post <- post6


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
      # On average, cells lose 75% of their metabolic activity by the time an individual growth process finishes at asymptote.
      # Mostly because active bone marrow converted to marrow fat, and bone cells converted to bone matrix.
      m <- rbind( 1 - 0.75*(Height1/maxHeight1),
                  1 - 0.75*(Height2/maxHeight2),
                  1 - 0.75*(Height3/maxHeight3),
                  1 - 0.75*(Height4/maxHeight4),
                  1 - 0.75*(Height5/maxHeight5) )


      return( list(p=p,m=m) )

} # propparam


vpropparam <- Vectorize(propparam) # vectorize the function so that it can take vectors as arguments


# e <- 5

# Q1 <- Q1ber[1:e]
# Q2 <- Q2ber[1:e]
# Q3 <- Q3ber[1:e]
# Q4 <- Q4ber[1:e]
# Q5 <- Q5ber[1:e]

# K1 <- K1ber[1:e]
# K2 <- K2ber[1:e]
# K3 <- K3ber[1:e]
# K4 <- K4ber[1:e]
# K5 <- K5ber[1:e]

# H1 <- H1ber[1:e]
# H2 <- H2ber[1:e]
# H3 <- H3ber[1:e]
# H4 <- H4ber[1:e]
# H5 <- H5ber[1:e]

# I1 <- I1ber[1:e]
# I2 <- I2ber[1:e]
# I3 <- I3ber[1:e]
# I4 <- I4ber[1:e]

# d <- 5

# p <- matrix(unlist(vpropparam(x=d, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                                    K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                                    H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                                    I1=I1, I2=I2, I3=I3, I4=I4)["p",]), nrow=5, byrow=FALSE)
# p
# colSums(p)


# m <- matrix(unlist(vpropparam(x=d, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                                    K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                                    H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                                    I1=I1, I2=I2, I3=I3, I4=I4)["m",]), nrow=5, byrow=FALSE)
# m
# colSums(m)





a <- seq(from=(0.1), to=(26.75), by=0.1)  #c(0.1,1,2)

tQmat <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKmat <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHmat <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

tQber <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKber <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHber <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

conQ <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conK <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conH <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))


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

      tQmat[t,] <- Pmat[1,]*Q1mat +
                   Pmat[2,]*Q2mat +
                   Pmat[3,]*Q3mat +
                   Pmat[4,]*Q4mat +
                   Pmat[5,]*Q5mat

      tKmat[t,] <- Pmat[1,]*Mmat[1,]*K1mat +
                   Pmat[2,]*Mmat[2,]*K2mat +
                   Pmat[3,]*Mmat[3,]*K3mat +
                   Pmat[4,]*Mmat[4,]*K4mat +
                   Pmat[5,]*Mmat[5,]*K5mat

      tHmat[t,] <- Pmat[1,]*Mmat[1,]*H1mat +
                   Pmat[2,]*Mmat[2,]*H2mat +
                   Pmat[3,]*Mmat[3,]*H3mat +
                   Pmat[4,]*Mmat[4,]*H4mat +
                   Pmat[5,]*Mmat[5,]*H5mat


      tQber[t,] <- Pber[1,]*Q1ber +
                   Pber[2,]*Q2ber +
                   Pber[3,]*Q3ber +
                   Pber[4,]*Q4ber +
                   Pber[5,]*Q5ber

      tKber[t,] <- Pber[1,]*Mber[1,]*K1ber +
                   Pber[2,]*Mber[2,]*K2ber +
                   Pber[3,]*Mber[3,]*K3ber +
                   Pber[4,]*Mber[4,]*K4ber +
                   Pber[5,]*Mber[5,]*K5ber
                   
      tHber[t,] <- Pber[1,]*Mber[1,]*H1ber +
                   Pber[2,]*Mber[2,]*H2ber +
                   Pber[3,]*Mber[3,]*H3ber +
                   Pber[4,]*Mber[4,]*H4ber +
                   Pber[5,]*Mber[5,]*H5ber

      #contrasts
      conQ[t,] <- tQber[t,] - tQmat[t,]
      conK[t,] <- tKber[t,] - tKmat[t,] 
      conH[t,] <- tHber[t,] - tHmat[t,]  

} # for t

#tQmat[,1]
#rowMeans(tQber)
#rowMeans(tKmat)
#rowMeans(tHmat)

#rowMeans(conQ)
#rowMeans(conK)
#rowMeans(conH)



colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.75)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)

Nsims <- length(post$"mQ[2,1]")
n_samps <- 100 # how many samples to draw from posterior for plotting
samps <- sample.int(Nsims, n_samps, replace=FALSE)








####### Q


pdf(file="./Plots/Composite_model_Q.pdf",
    height=7, width=7)

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,max(c(rowMeans(tQmat),rowMeans(tQber)))), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="q" )


  for ( z in 1:n_samps ){

    # US
    lines(x = a,
          y = tQber[,samps[z]],
          col=colorlist["2.3"], lwd=0.25, lty=1)

    # Matsigenka
    lines(x = a,
          y = tQmat[,samps[z]],
          col=colorlist["1.3"], lwd=0.25, lty=1)

  } #for z


  lines(x = a,
        y = rowMeans(tQber),
        col=colorlist["2.1"], lwd=3, lty=1)
  lines(x = a,
        y = rowMeans(tQmat),
        col=colorlist["1.1"], lwd=3, lty=1)




  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-1*max(abs(rowMeans(conQ))),max(abs(rowMeans(conQ)))), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( -0.1, 0.1, by=0.05 ), las=2 )
  par(xpd=FALSE)


  for ( z in 1:n_samps ){

    lines(x = a,
          y = conQ[,samps[z]],
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  lines(x = a,
        y = rowMeans(conQ),
        col="black", lwd=3, lty=1)


  lines(x = c(min(a),max(a)),
        y = c(0,0),
        col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length



  par(xpd=TRUE)
  text("Contrast", x=30, y=0, srt=270, las=3)


  # legend
  legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast", "")
  xcoords <- c(0, 0.002, 0.0068, 0.011)
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(-1, max(abs(rowMeans(conQ))),       #x="topleft", inset=c(0,-0.5), 
         text.width=textwidths, #x=c(0.0085,0.01), y=c(1,0.5), #mean(exp(post$berLQ1)) - 0.0005, 1.4,
         legend=legtext, bty="o", bg="white",
         col=c(colorlist["2.3"], colorlist["1.3"], "black","white"),
         lty=c(1,1,1,1),
         lwd=c(3,3,3,3),
         cex=1.4,#1.4,
         x.intersp=0.5,
         seg.len=1,
         horiz=TRUE)

symbol.Female(centerx = 23, centery = max(abs(rowMeans(conQ))), rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()








###### K

pdf(file="./Plots/Composite_model_K.pdf",
    height=7, width=7)

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,max(c(rowMeans(tKmat),rowMeans(tKber)))), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="K" )
  #axis( side=4, at=seq( 0, 70, by=10 ), las=2 )


  for ( z in 1:n_samps ){

    # US
    lines(x = a,
          y = tKber[,samps[z]],
          col=colorlist["2.3"], lwd=0.25, lty=1)

    # Matsigenka
    lines(x = a,
          y = tKmat[,samps[z]],
          col=colorlist["1.3"], lwd=0.25, lty=1)

  } #for z


  lines(x = a,
        y = rowMeans(tKber),
        col=colorlist["2.1"], lwd=3, lty=1)
  lines(x = a,
        y = rowMeans(tKmat),
        col=colorlist["1.1"], lwd=3, lty=1)

#   lines(x = a,
#         y = rep(5, times=length(a)))

#   lines(x = a,
#         y = rep(10, times=length(a)))




  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-1*max(abs(rowMeans(conK))),max(abs(rowMeans(conK)))), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( -10, 10, by=5), las=2 )
  par(xpd=FALSE)


  for ( z in 1:n_samps ){

    lines(x = a,
          y = conK[,samps[z]],
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  lines(x = a,
        y = rowMeans(conK),
        col="black", lwd=3, lty=1)


  lines(x = c(min(a),max(a)),
        y = c(0,0),
        col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length



  par(xpd=TRUE)
  text("Contrast", x=30, y=0, srt=270, las=3)


  # legend
  legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast", "")
  xcoords <- c(0, 0.002, 0.0068, 0.011)
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(-1, max(abs(rowMeans(conK))),       #x="topleft", inset=c(0,-0.5), 
         text.width=textwidths, #x=c(0.0085,0.01), y=c(1,0.5), #mean(exp(post$berLQ1)) - 0.0005, 1.4,
         legend=legtext, bty="o", bg="white",
         col=c(colorlist["2.3"], colorlist["1.3"], "black","white"),
         lty=c(1,1,1,1),
         lwd=c(3,3,3,3),
         cex=1.4,#1.4,
         x.intersp=0.5,
         seg.len=1,
         horiz=TRUE)

symbol.Female(centerx = 23, centery = max(abs(rowMeans(conK))), rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()







###### H

pdf(file="./Plots/Composite_model_H.pdf",
    height=7, width=7)

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,max(c(rowMeans(tHmat),rowMeans(tHber)))), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="H" )
  #axis( side=4, at=seq( 0, 70, by=10 ), las=2 )


  for ( z in 1:n_samps ){

    # US
    lines(x = a,
          y = tHber[,samps[z]],
          col=colorlist["2.3"], lwd=0.25, lty=1)

    # Matsigenka
    lines(x = a,
          y = tHmat[,samps[z]],
          col=colorlist["1.3"], lwd=0.25, lty=1)

  } #for z


  lines(x = a,
        y = rowMeans(tHber),
        col=colorlist["2.1"], lwd=3, lty=1)
  lines(x = a,
        y = rowMeans(tHmat),
        col=colorlist["1.1"], lwd=3, lty=1)

#   lines(x = a,
#         y = rep(5, times=length(a)))

#   lines(x = a,
#         y = rep(10, times=length(a)))




  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-1*max(abs(rowMeans(conH))),max(abs(rowMeans(conH)))), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( -10, 10, by=5), las=2 )
  par(xpd=FALSE)


  for ( z in 1:n_samps ){

    lines(x = a,
          y = conH[,samps[z]],
          col=grey(0.5), lwd=0.25, lty=1)

  } #for z


  lines(x = a,
        y = rowMeans(conH),
        col="black", lwd=3, lty=1)


  lines(x = c(min(a),max(a)),
        y = c(0,0),
        col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length



  par(xpd=TRUE)
  text("Contrast", x=30, y=0, srt=270, las=3)


  # legend
  legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast", "")
  xcoords <- c(0, 0.002, 0.0068, 0.011)
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(-1, max(abs(rowMeans(conH))),       #x="topleft", inset=c(0,-0.5), 
         text.width=textwidths, #x=c(0.0085,0.01), y=c(1,0.5), #mean(exp(post$berLQ1)) - 0.0005, 1.4,
         legend=legtext, bty="o", bg="white",
         col=c(colorlist["2.3"], colorlist["1.3"], "black","white"),
         lty=c(1,1,1,1),
         lwd=c(3,3,3,3),
         cex=1.4,#1.4,
         x.intersp=0.5,
         seg.len=1,
         horiz=TRUE)

symbol.Female(centerx = 23, centery = max(abs(rowMeans(conH))), rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()








########### I


mean_I_list <- list( I1mat, I1ber,
                     I2mat, I2ber,
                     I3mat, I3ber,
                     I4mat, I4ber )


names(mean_I_list) <- c( "I1mat", "I1ber",
                         "I2mat", "I2ber",
                         "I3mat", "I3ber",
                         "I4mat", "I4ber" )

#str(mean_I_list)



contr_I_list <- list( I1ber - I1mat,
                      I2ber - I2mat,
                      I3ber - I3mat,
                      I4ber - I4mat )


names(contr_I_list) <- c( "ber.mat1",
                          "ber.mat2",
                          "ber.mat3",
                          "ber.mat4" )

#str(contr_I_list)



pdf(file="./Plots/Composite_model_I.pdf", 
height=7, width=7)
layout( matrix(c(1,2,3,4,5,6,7,8), 4, 2, byrow = FALSE),
        heights=c(1,1,1,1),
        widths=c(1,1) )
par(mar = c(2, 1, 2, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


I1_plot <- denschart3( mean_I_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(0.1,0.2),        #( mean( mean_I_list$I1mat ) - 1, mean( mean_I_list$I1ber ) + 1 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I1mat ), mean( mean_I_list$I1mat ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( mean_I_list$I1ber ), mean( mean_I_list$I1ber ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(0.1,0.2), labels=c(0.1,0.2), cex.axis=1.5)
#mtext("Age (years since conception)", side = 1, outer = T, cex = 1.5, line = 3, adj=1)

#mtext("Composite model characteristic posteriors", side = 3, outer = F, cex = 2, line = 1)
par(xpd=NA) # plotting clipped to device region

# legend
legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast", "")
xcoords <- c(0, 2, 7, 11)
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x="topleft", inset=c(0,0), text.width=textwidths, #x=c(0.0085,0.01), y=c(1,0.5), #mean(exp(post$berLQ1)) - 0.0005, 1.4,
         legend=legtext, bty="o", bg="white",
         col=c("red", "blue", "black","white"),
         lty=c(1,1,1,1),
         lwd=c(3,3,3,3),
         cex=1.4,
         x.intersp=0.5,
         seg.len=3,
         horiz=TRUE)


# text(x=mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5 + 14,
#      y=1.2,labels='\\MA',
#      vfont=c("sans serif","bold"),
#      cex = 6) #add Mars symbol

symbol.Female(centerx = 0.15, #mean( mean_I_list$I1mat ) - 5 + 14,
            centery = 0, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package




#graphics.off()


par(xpd=FALSE)


I2_plot <- denschart3( mean_I_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(0.5,1),        #( mean( mean_I_list$I2mat ) - 0.2, mean( mean_I_list$I2ber ) + 0.2),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I2mat ), mean( mean_I_list$I2mat ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( mean_I_list$I2ber ), mean( mean_I_list$I2ber ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(0.5,1), labels=c(0.5,1), cex.axis=1.5)
#mtext("Age (years since conception)", side = 1, outer = T, cex = 1.5, line = 3, adj=0.5)


I3_plot <- denschart3( mean_I_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(1,5),        #( mean( mean_I_list$I3mat ) - 1, mean( mean_I_list$I3ber ) + 1),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I3mat ), mean( mean_I_list$I3mat ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( mean_I_list$I3ber ), mean( mean_I_list$I3ber ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(1,5), labels=c(1,5), cex.axis=1.5)
#mtext("Age (years since conception)", side = 1, outer = T, cex = 1.5, line = 3, adj=0.5)


I4_plot <- denschart3( mean_I_list[c(7,8)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(8,11),        #( mean( mean_I_list$I4mat ) - 1, mean( mean_I_list$I4ber ) + 1),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I4mat ), mean( mean_I_list$I4mat ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( mean_I_list$I4ber ), mean( mean_I_list$I4ber ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(8,11), labels=c(8,11), cex.axis=1.5)
mtext("Age (years since conception)", side = 1, outer = T, cex = 1.5, line = 3, adj=0.5)



### parameter contrasts


I1_cont_plot <- denschart3( contr_I_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.05,0.05),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat1), mean(contr_I_list$ber.mat1) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.05,0.05), labels=c(-0.05,0.05), cex.axis=1.5)




I2_cont_plot <- denschart3( contr_I_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.1,0.1),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat2), mean(contr_I_list$ber.mat2) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.1,0.1), labels=c(-0.1,0.1), cex.axis=1.5)


I3_cont_plot <- denschart3( contr_I_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-3,3),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat3), mean(contr_I_list$ber.mat3) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-3,3), labels=c(-3,3), cex.axis=1.5)



I4_cont_plot <- denschart3( contr_I_list[4],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-2,2),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat4), mean(contr_I_list$ber.mat4) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-2,2), labels=c(-2,2), cex.axis=1.5)


graphics.off()










# ######################## Plot characteristic posteriors for composite model

post <- post6
#str(post6, list.len=100000)


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



maxagevel <- function(Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                      K1=1, K2=1, K3=1, K4=1, K5=1,
                      H1=1, H2=1, H3=1, H4=1, H5=1,
                      I1=1, I2=1, I3=1, I4=1){


    secderiv <- function(x, Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                            K1=1, K2=1, K3=1, K4=1, K5=1,
                            H1=1, H2=1, H3=1, H4=1, H5=1,
                            I1=1, I2=1, I3=1, I4=1){

      I0 <- rep(0, times=length(I1))

      ifelse( x < I1, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ),

      ifelse( x < I2, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ),

      ifelse( x < I3, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ) +
                      2^(1/Q3)*( 1/( 1 + 2*Q3 )^2*H3^2*Q3*( 1/Q3 - 1 )*exp((2*K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 2 ) - 1/( 1 + 2*Q3 )^2*H3*K3*Q3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))))^( 1/Q3 - 1 ) ),

      ifelse( x < I4, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ) +
                      2^(1/Q3)*( 1/( 1 + 2*Q3 )^2*H3^2*Q3*( 1/Q3 - 1 )*exp((2*K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 2 ) - 1/( 1 + 2*Q3 )^2*H3*K3*Q3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))))^( 1/Q3 - 1 ) ) +
                      2^(1/Q4)*( 1/( 1 + 2*Q4 )^2*H4^2*Q4*( 1/Q4 - 1 )*exp((2*K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 2 ) - 1/( 1 + 2*Q4 )^2*H4*K4*Q4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))))^( 1/Q4 - 1 ) ),

                      2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ) +
                      2^(1/Q3)*( 1/( 1 + 2*Q3 )^2*H3^2*Q3*( 1/Q3 - 1 )*exp((2*K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 2 ) - 1/( 1 + 2*Q3 )^2*H3*K3*Q3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))))^( 1/Q3 - 1 ) ) +
                      2^(1/Q4)*( 1/( 1 + 2*Q4 )^2*H4^2*Q4*( 1/Q4 - 1 )*exp((2*K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 2 ) - 1/( 1 + 2*Q4 )^2*H4*K4*Q4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))))^( 1/Q4 - 1 ) ) +
                      2^(1/Q5)*( 1/( 1 + 2*Q5 )^2*H5^2*Q5*( 1/Q5 - 1 )*exp((2*K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))*(H5/K5*( 1 - exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 )) ))^( 1/Q5 - 2 ) - 1/( 1 + 2*Q5 )^2*H5*K5*Q5*exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))*(H5/K5*( 1 - exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))))^( 1/Q5 - 1 ) ) 
      ) ) ) ) 
    } # secderiv


    # find childhood age at which height velocity starts to increase for puberty
    a <- seq(from=(8), to=(24), by=0.1)
    lowerlim <- 10.5
    found <- FALSE
    for ( t in 1:length(a) ){
        if ( found == FALSE ) {
            if ( secderiv(x=a[t], Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
                                  K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
                                  H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
                                  I1=I1, I2=I2, I3=I3, I4=I4) > 0 ) {
                found <- TRUE
                lowerlim <- a[t]
            } # if
        } # if
    } # for t 

    tmax_sol <- uniroot( secderiv, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
                                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
                                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
                                   I1=I1, I2=I2, I3=I3, I4=I4, lower=lowerlim, upper=25 )   # root between t=10.5 and t=21, to ignore high fetal/infant growth rate. Choose lower bound where slope of velocity is positive, because upper bound has negative slope.


    firderiv <- function(x=1, Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                              K1=1, K2=1, K3=1, K4=1, K5=1,
                              H1=1, H2=1, H3=1, H4=1, H5=1,
                              I1=1, I2=1, I3=1, I4=1){

      I0 <- rep(0, times=length(I1))

      ifelse( x < I1, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ),

      ifelse( x < I2, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ),

      ifelse( x < I3, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ) +
                      1/( 1 + 2*Q3 )*2^(1/Q3)*H3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 1 ),

      ifelse( x < I4, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ) +
                      1/( 1 + 2*Q3 )*2^(1/Q3)*H3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 1 ) +
                      1/( 1 + 2*Q4 )*2^(1/Q4)*H4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 1 ),

                      1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ) +
                      1/( 1 + 2*Q3 )*2^(1/Q3)*H3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 1 ) +
                      1/( 1 + 2*Q4 )*2^(1/Q4)*H4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 1 ) +
                      1/( 1 + 2*Q5 )*2^(1/Q5)*H5*exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))*(H5/K5*( 1 - exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 )) ))^( 1/Q5 - 1 )
      ) ) ) )  
    } # firderiv

  # curve(firderiv(x, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
  #                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
  #                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
  #                   I1=I1, I2=I2, I3=I3, I4=I4), from=5, to=21)
  # curve(secderiv(x, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
  #                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
  #                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
  #                   I1=I1, I2=I2, I3=I3, I4=I4), from=5, to=21, add = TRUE, col = "red")

    return( list(agemax=tmax_sol$root, maxvel=firderiv(x=tmax_sol$root,Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
                                                                       K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
                                                                       H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
                                                                       I1=I1, I2=I2, I3=I3, I4=I4)) )
} # maxagevel


vmaxagevel <- Vectorize(maxagevel) # vectorize the function so that it can take vectors as arguments



# Q1 <- Q1ber[1:20]
# Q2 <- Q2ber[1:20]
# Q3 <- Q3ber[1:20]
# Q4 <- Q4ber[1:20]
# Q5 <- Q5ber[1:20]

# K1 <- K1ber[1:20]
# K2 <- K2ber[1:20]
# K3 <- K3ber[1:20]
# K4 <- K4ber[1:20]
# K5 <- K5ber[1:20]

# H1 <- H1ber[1:20]
# H2 <- H2ber[1:20]
# H3 <- H3ber[1:20]
# H4 <- H4ber[1:20]
# H5 <- H5ber[1:20]

# I1 <- I1ber[1:20]
# I2 <- I2ber[1:20]
# I3 <- I3ber[1:20]
# I4 <- I4ber[1:20]

# unlist(vmaxagevel(Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                   I1=I1, I2=I2, I3=I3, I4=I4)["agemax",])
# unlist(vmaxagevel(Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                   I1=I1, I2=I2, I3=I3, I4=I4)["maxvel",])



mean_char_list <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       (2*H1ber/K1ber)^(1/Q1ber) +
                       (2*H2ber/K2ber)^(1/Q2ber) +
                       (2*H3ber/K3ber)^(1/Q3ber) +
                       (2*H4ber/K4ber)^(1/Q4ber) +
                       (2*H5ber/K5ber)^(1/Q5ber),


                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                         K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                         H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                         I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["agemax",]),


                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",]),

                       unlist(vmaxagevel(Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                         K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                         H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                         I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["maxvel",])
                      )


names(mean_char_list) <- c(
                          "matMaxHeight",
                          "berMaxHeight",

                          "matMaxVelAge",
                          "berMaxVelAge",

                          "matMaxVel",
                          "berMaxVel"
                        )

#str(mean_char_list)



contr_char_list <- list(
                       (2*H1ber/K1ber)^(1/Q1ber) +
                       (2*H2ber/K2ber)^(1/Q2ber) +
                       (2*H3ber/K3ber)^(1/Q3ber) +
                       (2*H4ber/K4ber)^(1/Q4ber) +
                       (2*H5ber/K5ber)^(1/Q5ber) - 
                       (
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat)
                       ),

                       unlist(vmaxagevel(Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                         K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                         H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                         I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["agemax",]) - 
                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                         K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                         H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                         I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["maxvel",]) -
                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",])
                      )


names(contr_char_list) <- c(
                          "ber.matMaxHeight",
                          "ber.matMaxVelAge",
                          "ber.matMaxVel"
                        )

#str(contr_char_list)



pdf(file="./Plots/char_post_plot.pdf", 
height=7, width=10)
layout( matrix(c(1,2,3,4,5,6), 3, 2, byrow = FALSE),
        heights=c(1,1,1),
        widths=c(1,1) )
par(mar = c(2, 1, 2, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


MaxHeight_post_plot <- denschart3( mean_char_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_char_list$matMaxHeight ), mean( mean_char_list$matMaxHeight ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( mean_char_list$berMaxHeight ), mean( mean_char_list$berMaxHeight ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(145,160), labels=c(145,160), cex.axis=1.5)
mtext("Max height (cm)", side = 1, outer = T, cex = 1.5, line = 3, adj=1)

#mtext("Composite model characteristic posteriors", side = 3, outer = F, cex = 2, line = 1)
par(xpd=NA) # plotting clipped to device region

# legend
legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast", "")
xcoords <- c(0, 2, 7, 11)
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x="topleft", inset=c(0,0), text.width=textwidths, #x=c(0.0085,0.01), y=c(1,0.5), #mean(exp(post$berLQ1)) - 0.0005, 1.4,
         legend=legtext, bty="o", bg="white",
         col=c("red", "blue", "black","white"),
         lty=c(1,1,1,1),
         lwd=c(3,3,3,3),
         cex=1.4,
         x.intersp=0.5,
         seg.len=3,
         horiz=TRUE)


# text(x=mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5 + 14,
#      y=1.2,labels='\\MA',
#      vfont=c("sans serif","bold"),
#      cex = 6) #add Mars symbol

symbol.Female(centerx = mean( mean_char_list$matMaxHeight ) - 5 + 14,
            centery = 0.7, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package




#graphics.off()


par(xpd=FALSE)


MaxVelAge_post_plot <- denschart3( mean_char_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( mean_char_list$matMaxVelAge ) - 0.2, mean( mean_char_list$berMaxVelAge ) + 0.2),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_char_list$matMaxVelAge ), mean( mean_char_list$matMaxVelAge ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( mean_char_list$berMaxVelAge ), mean( mean_char_list$berMaxVelAge ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(11.4,11.7), labels=c(11.4,11.7), cex.axis=1.5)
mtext("Age at maximum childhood velocity (years)", side = 1, outer = T, cex = 1.5, line = 3, adj=0.5)


MaxVel_post_plot <- denschart3( mean_char_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( mean_char_list$berMaxVel ) - 1, mean( mean_char_list$matMaxVel ) + 1),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_char_list$matMaxVel ), mean( mean_char_list$matMaxVel ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( mean_char_list$berMaxVel ), mean( mean_char_list$berMaxVel ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(6.5,7), labels=c(6.5,7), cex.axis=1.5)
mtext("Maximum childhood velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = 3, adj=0.5)



### parameter contrasts


MaxWeight_cont_plot <- denschart3( contr_char_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-20,20),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxHeight), mean(contr_char_list$ber.matMaxHeight) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-15,15), labels=c(-15,15), cex.axis=1.5)




MaxVelAge_cont_plot <- denschart3( contr_char_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge), mean(contr_char_list$ber.matMaxVelAge) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.3,0.3), labels=c(-0.3,0.3), cex.axis=1.5)


MaxVel_cont_plot <- denschart3( contr_char_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-1.5,1.5),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel), mean(contr_char_list$ber.matMaxVel) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-1,1), labels=c(-1,1), cex.axis=1.5)


graphics.off()

