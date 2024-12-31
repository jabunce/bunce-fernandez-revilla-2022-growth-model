

################## m6 composite model for Berkeley and Matsigenka, covarying random effects for individual

post <- post6

pdf(file="./Plots/Composite_model_sample_mat_ber_randeff_cov5h.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )

  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=0.5, col="red", cex=0.5)
  points(x=Mat.mal$TotAge, y=Mat.mal$Height, lwd=0.5, col="blue", cex=0.5)

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

symbol.Male(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()



###########################################################################################


post <- post6

pdf(file="./Plots/Composite_model_sample_mat_ber_randeff_cov5w.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,5000), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Weight (g)" )


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$CellWeightg, lwd=0.5, col="red", cex=0.5)
  points(x=Mat.mal$TotAge, y=Mat.mal$CellWeightg, lwd=0.5, col="blue", cex=0.5)


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
            col=rgb(1,0,0,max=1,alpha=0.5), lwd=0.25, lty=1)

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
        col="dark red", lwd=3, lty=1)



  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col="red", lwd=3, lty=1)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col="red", lwd=3, lty=1)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col="red", lwd=3, lty=1)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col="red", lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
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
        col="dark blue", lwd=3, lty=1)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col="blue", lwd=3, lty=1)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col="blue", lwd=3, lty=1)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col="blue", lwd=3, lty=1)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col="blue", lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col="blue", lwd=3, lty=1)





  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,400), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 400, by=50 ), las=2 )
  par(xpd=TRUE)


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
  lines(x = x, y=y_primeBer, col="orange", lwd=2, lty=1)


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
  lines(x = x, y=y_primeMat, col="cyan", lwd=2, lty=1)



  par(xpd=TRUE)
  text("Growth velocity (g/year)", x=32, y=200, srt=270, las=3)

  legend(-1, 400.65,
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

symbol.Male(centerx = 23, centery = 400, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()


