

###############################################################################


######## load posteriors from complete models, need to run PrepareData to plot raw data points

# male
post3_m <- readRDS("./analysis_hw_mal/post6.RDS")
#str(post3_m)

# female
post3_f <- readRDS("./analysis_hw_fem/post6.RDS")
#str(post3_f)



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
BerVelTraj_lty <- 1

MatVelTraj_lwd <- 2
MatVelTraj_col <- colorlist2["1.2"]
MatVelTraj_lty <- 1






pdf(file="./combined_plots/Plots/intervention_height_trajectories.pdf",
    height=10, width=10)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6
                      ),
        nrow=3, ncol=2, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1,1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 9, 8)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels


# female  ###########################################################################################################################################################################


  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age


  ################ female height un-modified
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





#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


  # set horizontal spacing for legend text
  legtext <- c(" ",
               " ", 
               "Estimated mean trajectory",
               "Estimated mean velocity"
               )
  xcoords <- c(0,
               4,
               19.5,
               27.5
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-29.2, #-13.6,
         y=16.5, #21,
         ncol=4,
         cex=1.6,
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
         pch=c(NA,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               0,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(0,
               0,
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
  legtext <- c(" ",
               " ", 
               "U.S.",
               "U.S."
               )
  xcoords <- c(0,
               8.5,  #5.5
               24.2, #23
               32.2  #38
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-30.5,#-12,
         y=15.5, #20,
         ncol=4,
         cex=1.6,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(NA,
               NA,
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
               0,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(0,
               0,
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
  legtext <-c(" ",
               " ",
               "Matsigenka",
               "Matsigenka"
               )
  xcoords <- c(0,
               8.5,
               24.2,
               32.2
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)


  legend(x=-30.5, #-12,
         y=14.5, #19,
         ncol=4,
         cex=1.6,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(NA,
               NA,
               MatMeanTraj_col,
               MatVelTraj_col
              ),
         merge=FALSE,
         pch=c(NA,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               0,
               MatMeanTraj_lty,
               MatVelTraj_lty
               ),
         lwd=c(0,
               0,
               MatMeanTraj_lwd,
               MatVelTraj_lwd
               ),
         seg.len=4 )

  rect(xleft = 0, #-5,
       ybottom = 12.8,
       xright = 21,
       ytop = 16.2,
       lwd=1)






# row labels


par(xpd=NA) # plotting clipped to device region

mtext("No",                         side = 1, outer = T, cex = 1.5, adj=-0.315, line = -52 ) #side = c(bottom, left, top, right)
mtext("intervention",               side = 1, outer = T, cex = 1.5, adj=-0.475, line = -49 )

mtext("Equal",                      side = 1, outer = T, cex = 1.5, adj=-0.365, line = -32 )
mtext("metabolism",                 side = 1, outer = T, cex = 1.5, adj=-0.475, line = -29 )

mtext("Metabolic",                  side = 1, outer = T, cex = 1.5, adj=-0.425, line = -12 )
mtext("intervention",               side = 1, outer = T, cex = 1.5, adj=-0.465, line = -9 )


mtext("Age since conception (years)",       side = 1, outer = T, cex = 1.5, adj=0.5, line = 3.8 )
mtext("Height (cm)",                        side = 2, outer = T, cex = 1.5, adj=0.5, line = 2.8)







# column labels
symbol.Female(centerx = 5, centery = 12, rayonx=0.4, lwd=1.5, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.107, line = -110.5 )

symbol.Male(  centerx = 16.5, centery = 11.4, rayonx=0.4, lwd=1.5, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.385, line = -110.5 )


par(xpd=FALSE)







################ female height: K's and H's equal between US and Matsigenka 
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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )



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










################ female height: K2, H3 equal between US and Matsigenka 
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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  #mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  #mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  #mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  #mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  #mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  #mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  #mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  #mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  #mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  #mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  #mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  #mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  #mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  #mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  #mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  #mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )



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






# male  ###########################################################################################################################################################################


  ###### no intervention

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







  ###### all K's and H's equal

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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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
  text("Growth velocity (cm/year)", x=32, y=8, srt=270, las=3, cex=2.2)




  ###### all K2, K3 equal

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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  #mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  #mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  #mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  #mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  #mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  #mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  #mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  #mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  #mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  #mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  #mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  #mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  #mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  #mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  #mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  #mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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




graphics.off()




#######################################################################
#######################################################################















pdf(file="./combined_plots/Plots/intervention_weight_trajectories.pdf",
    height=10, width=10)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6
                      ),
        nrow=3, ncol=2, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1,1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 9, 8)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels


# female  ###########################################################################################################################################################################


  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age


  ################ female weight un-modified
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





#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


  # set horizontal spacing for legend text
  legtext <- c(" ",
               " ", 
               "Estimated mean trajectory",
               "Estimated mean velocity"
               )
  xcoords <- c(0,
               4,
               19.5,
               27.5
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-29.2, #-13.6,
         y=16.5, #21,
         ncol=4,
         cex=1.6,
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
         pch=c(NA,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               0,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(0,
               0,
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
  legtext <- c(" ",
               " ", 
               "U.S.",
               "U.S."
               )
  xcoords <- c(0,
               8.5,  #5.5
               24.2, #23
               32.2  #38
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-30.5,#-12,
         y=15.5, #20,
         ncol=4,
         cex=1.6,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(NA,
               NA,
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
               0,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(0,
               0,
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
  legtext <-c(" ",
               " ",
               "Matsigenka",
               "Matsigenka"
               )
  xcoords <- c(0,
               8.5,
               24.2,
               32.2
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)


  legend(x=-30.5, #-12,
         y=14.5, #19,
         ncol=4,
         cex=1.6,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(NA,
               NA,
               MatMeanTraj_col,
               MatVelTraj_col
              ),
         merge=FALSE,
         pch=c(NA,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               0,
               MatMeanTraj_lty,
               MatVelTraj_lty
               ),
         lwd=c(0,
               0,
               MatMeanTraj_lwd,
               MatVelTraj_lwd
               ),
         seg.len=4 )

  rect(xleft = 0, #-5,
       ybottom = 12.8,
       xright = 21,
       ytop = 16.2,
       lwd=1)






# row labels


par(xpd=NA) # plotting clipped to device region

mtext("No",                         side = 1, outer = T, cex = 1.5, adj=-0.315, line = -52 ) #side = c(bottom, left, top, right)
mtext("intervention",               side = 1, outer = T, cex = 1.5, adj=-0.475, line = -49 )

mtext("Equal",                      side = 1, outer = T, cex = 1.5, adj=-0.365, line = -32 )
mtext("metabolism",                 side = 1, outer = T, cex = 1.5, adj=-0.475, line = -29 )

mtext("Metabolic",                  side = 1, outer = T, cex = 1.5, adj=-0.425, line = -12 )
mtext("intervention",               side = 1, outer = T, cex = 1.5, adj=-0.465, line = -9 )


mtext("Age since conception (years)",       side = 1, outer = T, cex = 1.5, adj=0.5, line = 3.8 )
mtext("Weight (g)",                        side = 2, outer = T, cex = 1.5, adj=0.5, line = 2.8)







# column labels
symbol.Female(centerx = 5, centery = 12, rayonx=0.4, lwd=1.5, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.107, line = -110.5 )

symbol.Male(  centerx = 16.5, centery = 11.4, rayonx=0.4, lwd=1.5, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.385, line = -110.5 )


par(xpd=FALSE)







################ female height: K's and H's equal between US and Matsigenka 
  post <- post3_f

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,6000), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=seq( 0, 6000, by=1000), labels=TRUE )


  Q <- rep(0, times=5) 
  K <- rep(0, times=5) 
  H <- rep(0, times=5) 
  I <- rep(0, times=5) 

  N <- nrow(Com.fem)                      # number of observations
  J <- length(unique(Com.fem$ID))         # number of people measured
  E <- length(unique(Com.fem$Ethnicity))  # number of ethnic groups
  EthID <- Com.fem[match(unique(Com.fem$ID), Com.fem$ID),c("ID","Ethnicity")] # ethnicity for each ID

  

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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )



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










################ female height: K2, H3 equal between US and Matsigenka 
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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  #mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  #mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  #mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  #mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  #mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  #mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  #mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  #mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  #mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  #mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  #mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  #mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  #mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  #mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  #mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  #mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )



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






# male  ###########################################################################################################################################################################


  ###### no intervention

  post <- post3_m

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,6000), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=FALSE )
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







  ###### all K's and H's equal

  post <- post3_m

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,6000), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=FALSE )
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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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


  par(xpd=NA) # clip plotting to device region
  text("Growth velocity (g/year)", x=32, y=200, srt=270, las=3, cex=2.2)




  ###### all K2, K3 equal

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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  #mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  #mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  #mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  #mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  #mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  #mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  #mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  #mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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


  ## test what would happen to Matsigenka with US-like H and K
  #mQ[1] <- mean( pull(post, paste("mQ[", 1, ",", 1, "]", sep="")) )
  #mQ[2] <- mean( pull(post, paste("mQ[", 1, ",", 2, "]", sep="")) )
  #mQ[3] <- mean( pull(post, paste("mQ[", 1, ",", 3, "]", sep="")) )
  # mQ[4] <- mean( pull(post, paste("mQ[", 1, ",", 4, "]", sep="")) )
  # mQ[5] <- mean( pull(post, paste("mQ[", 1, ",", 5, "]", sep="")) ) 

  #mK[1] <- mean( pull(post, paste("mK[", 1, ",", 1, "]", sep="")) )
  mK[2] <- mean( pull(post, paste("mK[", 1, ",", 2, "]", sep="")) )
  mK[3] <- mean( pull(post, paste("mK[", 1, ",", 3, "]", sep="")) )
  #mK[4] <- mean( pull(post, paste("mK[", 1, ",", 4, "]", sep="")) )
  #mK[5] <- mean( pull(post, paste("mK[", 1, ",", 5, "]", sep="")) )
  
  #mH[1] <- mean( pull(post, paste("mH[", 1, ",", 1, "]", sep="")) )
  #mH[2] <- mean( pull(post, paste("mH[", 1, ",", 2, "]", sep="")) )
  #mH[3] <- mean( pull(post, paste("mH[", 1, ",", 3, "]", sep="")) )
  #mH[4] <- mean( pull(post, paste("mH[", 1, ",", 4, "]", sep="")) )
  #mH[5] <- mean( pull(post, paste("mH[", 1, ",", 5, "]", sep="")) )


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




graphics.off()







#################################################################################################################
#################################################################################################################
####### Intervention height trajectory characteristics



######## function to determine max velocity and age at max velocity during puberty
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
    a <- seq(from=10, to=24, by=0.1) # choose "from" such that velocity is increasing at puberty
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

# x <- 1

# Q1 <- Q1ber[1:x]
# Q2 <- Q2ber[1:x]
# Q3 <- Q3ber[1:x]
# Q4 <- Q4ber[1:x]
# Q5 <- Q5ber[1:x]

# K1 <- K1ber[1:x]
# K2 <- K2ber[1:x]
# K3 <- K3ber[1:x]
# K4 <- K4ber[1:x]
# K5 <- K5ber[1:x]

# H1 <- H1ber[1:x]
# H2 <- H2ber[1:x]
# H3 <- H3ber[1:x]
# H4 <- H4ber[1:x]
# H5 <- H5ber[1:x]

# I1 <- I1ber[1:x]
# I2 <- I2ber[1:x]
# I3 <- I3ber[1:x]
# I4 <- I4ber[1:x]

# unlist(vmaxagevel(Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                   I1=I1, I2=I2, I3=I3, I4=I4)["agemax",])
# unlist(vmaxagevel(Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                   I1=I1, I2=I2, I3=I3, I4=I4)["maxvel",])






######## function to determine x-axis range for plotting
xrange <- function( D1=c(1,2,3), D2=c(2,3,4) ){

    # determine which distribution is to the right of the other
    if (mean(D1) > mean(D2)) {DR <- D1} else {DR <- D2} 
    if (mean(D1) > mean(D2)) {DL <- D2} else {DL <- D1}
    
    # boundaries for plotting
    Lbound <- HPDI(DL,0.9)[1] - 0.5*( mean(DL) - HPDI(DL,0.9)[1] )
    Rbound <- HPDI(DR,0.9)[2] + 0.5*( HPDI(DL,0.9)[2] - mean(DL) )

    # axis labels
    Llab <- HPDI(DL,0.9)[1]
    Rlab <- HPDI(DR,0.9)[2]

    x <- formatC(x=(Rlab - Llab), format = "f", digits = 10)
    decimals <- attr(regexpr("(?<=\\.)0+|$", x, perl = TRUE), "match.length") #number of zeros after decimal point before first integer

    Llabr <- round(Llab, digits=decimals+1)
    Rlabr <- round(Rlab, digits=decimals+1)

    if ( Llabr > Lbound && Rlabr < Rbound ) {
        Llabo <- Llabr
        Rlabo <- Rlabr
    } else {
        Llabo <- formatC(x=Llab, format = "f", digits = 3)
        Rlabo <- formatC(x=Rlab, format = "f", digits = 3)
    } #else

    return( list(Lbound=Lbound, Rbound=Rbound, Llab=Llabo, Rlab=Rlabo) )
} # xrange


#D1 <- (2*H1mat/K1mat)^(1/Q1mat)
#D2 <- (2*H1ber/K1ber)^(1/Q1ber)
#unlist(xrange(D1=D1, D2=D2)$Lbound)



######## function to determine x-axis range for plotting contrasts
xrangecon <- function( D1=c(1,2,3) ){
    
    # boundaries for plotting
    if ( abs(HPDI(D1,0.9)[1]) > abs(HPDI(D1,0.9)[2]) ) {
        bound <- abs(HPDI(D1,0.9)[1]) + 0.2*abs(HPDI(D1,0.9)[1])
        lab <- abs(HPDI(D1,0.9)[1])
    } else { 
        bound <- abs(HPDI(D1,0.9)[2]) + 0.2*abs(HPDI(D1,0.9)[2])
        lab <- abs(HPDI(D1,0.9)[2])
    } #else

    x <- formatC(x=lab, format = "f", digits = 10)
    decimals <- attr(regexpr("(?<=\\.)0+|$", x, perl = TRUE), "match.length") #number of zeros after decimal point before first integer

    labr <- round(lab, digits=decimals+1)

    if ( labr < bound ) {
        Llab <- -1*labr
        Rlab <- labr
    } else {
        labo <- 
        Llab <- formatC(x=-1*lab, format = "f", digits = 3)
        Rlab <- formatC(x=lab, format = "f", digits = 3)
    } #else

    Lbound <- -1*bound
    Rbound <- bound

    return( list(Lbound=Lbound, Rbound=Rbound, Llab=Llab, Rlab=Rlab) )
} # xrangecon


#D1 <- contr_char_list$ber.matMaxHeight1
#xrangecon(D1=D1)




######## plotting colors

colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.5)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)


# area and line colors
BerLine_lwd <- 4.5
BerLine_col <- colorlist["2.1"]

MatLine_lwd <- 4.5
MatLine_col <- colorlist["1.1"]

BerArea_col <- colorlist["2.3"]

MatArea_col <- colorlist["1.3"]

ConLine_lwd <- 4.5
ConLine_col <- "black"

ConArea_col <- grey(0.5)

ZerLine_lwd <- 4
ZerLine_lty <- "11"       #lty: first number in string is dash length, second is white space length
ZerLine_col <- "black"






################################################## intervention 1


######## female characteristics

post <- post3_f

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

K1mat <- post$"mK[1,1]" ### Matsigenka K's match Berkeley K's
K2mat <- post$"mK[1,2]" 
K3mat <- post$"mK[1,3]" 
K4mat <- post$"mK[1,4]" 
K5mat <- post$"mK[1,5]"

K1ber <- post$"mK[1,1]"
K2ber <- post$"mK[1,2]" 
K3ber <- post$"mK[1,3]" 
K4ber <- post$"mK[1,4]" 
K5ber <- post$"mK[1,5]"

H1mat <- post$"mH[1,1]" ### Matsigenka H's match Berkeley H's
H2mat <- post$"mH[1,2]" 
H3mat <- post$"mH[1,3]" 
H4mat <- post$"mH[1,4]" 
H5mat <- post$"mH[1,5]"

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



mean_char_list_f <- list(
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


names(mean_char_list_f) <- c(
                          "matMaxHeight",
                          "berMaxHeight",

                          "matMaxVelAge",
                          "berMaxVelAge",

                          "matMaxVel",
                          "berMaxVel"
                        )

#str(mean_char_list_f)



contr_char_list_f <- list(mean_char_list_f$berMaxHeight - mean_char_list_f$matMaxHeight,
                          mean_char_list_f$berMaxVelAge - mean_char_list_f$matMaxVelAge,
                          mean_char_list_f$berMaxVel - mean_char_list_f$matMaxVel)


names(contr_char_list_f) <- c(
                          "ber.matMaxHeight",
                          "ber.matMaxVelAge",
                          "ber.matMaxVel"
                        )

#str(contr_char_list_f)




######## male characteristics

post <- post3_m

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

K1mat <- post$"mK[1,1]" ### Matsigenka K's match Berkeley K's
K2mat <- post$"mK[1,2]" 
K3mat <- post$"mK[1,3]" 
K4mat <- post$"mK[1,4]" 
K5mat <- post$"mK[1,5]"

K1ber <- post$"mK[1,1]"
K2ber <- post$"mK[1,2]" 
K3ber <- post$"mK[1,3]" 
K4ber <- post$"mK[1,4]" 
K5ber <- post$"mK[1,5]"

H1mat <- post$"mH[1,1]" ### Matsigenka H's match Berkeley H's
H2mat <- post$"mH[1,2]" 
H3mat <- post$"mH[1,3]" 
H4mat <- post$"mH[1,4]" 
H5mat <- post$"mH[1,5]"

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



mean_char_list_m <- list(
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


names(mean_char_list_m) <- c(
                          "matMaxHeight",
                          "berMaxHeight",

                          "matMaxVelAge",
                          "berMaxVelAge",

                          "matMaxVel",
                          "berMaxVel"
                        )

#str(mean_char_list_m)



contr_char_list_m <- list(mean_char_list_m$berMaxHeight - mean_char_list_m$matMaxHeight,
                          mean_char_list_m$berMaxVelAge - mean_char_list_m$matMaxVelAge,
                          mean_char_list_m$berMaxVel - mean_char_list_m$matMaxVel)


names(contr_char_list_m) <- c(
                          "ber.matMaxHeight",
                          "ber.matMaxVelAge",
                          "ber.matMaxVel"
                        )

#str(contr_char_list_m)





pdf(file="./combined_plots/Plots/Chars_combined_inter1.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 2, 3, 0, 0, 0, 0, 0, 0, 4, 5, 6, 0,0,0,0,0,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        7, 8, 9, 0, 0, 0, 0, 0, 0, 10,11,12,0,0,0,0,0,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1, 0.2, 1,1, 0.2, 1,1, 0.2, 1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels


# female  ###########################################################################################################################################################################

post <- post3_f
mean_char_list <- mean_char_list_f
contr_char_list <- contr_char_list_f


MaxHeight_post_plot <- denschart3( mean_char_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxHeight ), mean( mean_char_list$matMaxHeight ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxHeight ), mean( mean_char_list$berMaxHeight ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast")
xcoords <- c(0, 7, 23)
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x=-17, y=20.5,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerArea_col, MatArea_col, ConArea_col),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=3,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = -16.5,
       ybottom = 14.5,
       xright = 51.1,
       ytop = 20,
       lwd=1)


# row labels

mtext("Max height",           side = 1, outer = T, cex = 2, adj=-0.17, line = -103 )
mtext("(cm)",                 side = 1, outer = T, cex = 2, adj=-0.13, line = -101 )

mtext("Age at max velocity",  side = 1, outer = T, cex = 2, adj=-0.23, line = -90.5 )
mtext("(years)",              side = 1, outer = T, cex = 2, adj=-0.14, line = -88.5 )

mtext("Max velocity",         side = 1, outer = T, cex = 2, adj=-0.17, line = -78 )
mtext("(cm/year)",            side = 1, outer = T, cex = 2, adj=-0.15, line = -76 )



# column labels
symbol.Female(centerx = 11, centery = 10.8, rayonx=0.7, lwd=3, col="black")

symbol.Male(  centerx = 38.5, centery = 10.4, rayonx=0.7, lwd=3, col="black")




# column brackets
lines(x=c(-2, 24),
      y=c(7.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(-2, -2),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(24, 24),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)


lines(x=c(26, 51),
      y=c(7.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(26, 26),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(51, 51),
     y=c(6.5,7.5),
      col="black", lwd=4, lty=1)


par(xpd=FALSE)




MaxVelAge_post_plot <- denschart3( mean_char_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxVelAge ) - 0.5, mean( mean_char_list$berMaxVelAge ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVelAge ), mean( mean_char_list$matMaxVelAge ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVelAge ), mean( mean_char_list$berMaxVelAge ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
           xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),
     labels=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
               xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),     
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)




MaxVel_post_plot <- denschart3( mean_char_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxVel ) - 0.5, mean( mean_char_list$berMaxVel ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVel ), mean( mean_char_list$matMaxVel ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVel ), mean( mean_char_list$berMaxVel ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
           xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
                xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)



### parameter contrasts


MaxWeight_cont_plot <- denschart3( contr_char_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-20,20),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[1]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[1]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxHeight), mean(contr_char_list$ber.matMaxHeight) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    cex.axis=cex_axis)



MaxVelAge_cont_plot <- denschart3( contr_char_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[2]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[2]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVelAge), mean(contr_char_list$ber.matMaxVelAge) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    cex.axis=cex_axis)



MaxVel_cont_plot <- denschart3( contr_char_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-1.5,1.5),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[3]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[3]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVel), mean(contr_char_list$ber.matMaxVel) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    cex.axis=cex_axis)





# male ###########################################################################################################################################################################

post <- post3_m
mean_char_list <- mean_char_list_m
contr_char_list <- contr_char_list_m


MaxHeight_post_plot <- denschart3( mean_char_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxHeight ), mean( mean_char_list$matMaxHeight ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxHeight ), mean( mean_char_list$berMaxHeight ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



par(xpd=FALSE)


MaxVelAge_post_plot <- denschart3( mean_char_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxVelAge ) - 0.5, mean( mean_char_list$berMaxVelAge ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVelAge ), mean( mean_char_list$matMaxVelAge ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVelAge ), mean( mean_char_list$berMaxVelAge ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
           xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),
     labels=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
               xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),     
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)




MaxVel_post_plot <- denschart3( mean_char_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxVel ) - 0.5, mean( mean_char_list$berMaxVel ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVel ), mean( mean_char_list$matMaxVel ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVel ), mean( mean_char_list$berMaxVel ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
           xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
                xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)



### parameter contrasts


MaxWeight_cont_plot <- denschart3( contr_char_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-20,20),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[1]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[1]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxHeight), mean(contr_char_list$ber.matMaxHeight) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    cex.axis=cex_axis)



MaxVelAge_cont_plot <- denschart3( contr_char_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[2]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[2]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVelAge), mean(contr_char_list$ber.matMaxVelAge) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    cex.axis=cex_axis)



MaxVel_cont_plot <- denschart3( contr_char_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-1.5,1.5),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[3]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[3]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVel), mean(contr_char_list$ber.matMaxVel) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    cex.axis=cex_axis)



graphics.off()














################################################## intervention 2


######## female characteristics

post <- post3_f

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
K2mat <- post$"mK[1,2]" ### female Matsigenka K2 and H3 match Berkeley's
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
H3mat <- post$"mH[1,3]" ### female Matsigenka K2 and H3 match Berkeley's 
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



mean_char_list_f <- list(
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


names(mean_char_list_f) <- c(
                          "matMaxHeight",
                          "berMaxHeight",

                          "matMaxVelAge",
                          "berMaxVelAge",

                          "matMaxVel",
                          "berMaxVel"
                        )

#str(mean_char_list_f)



contr_char_list_f <- list(mean_char_list_f$berMaxHeight - mean_char_list_f$matMaxHeight,
                          mean_char_list_f$berMaxVelAge - mean_char_list_f$matMaxVelAge,
                          mean_char_list_f$berMaxVel - mean_char_list_f$matMaxVel)


names(contr_char_list_f) <- c(
                          "ber.matMaxHeight",
                          "ber.matMaxVelAge",
                          "ber.matMaxVel"
                        )

#str(contr_char_list_f)




######## male characteristics

post <- post3_m

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
K2mat <- post$"mK[1,2]" ### male Matsigenka K2 and K3 match Berkeley's
K3mat <- post$"mK[1,3]" 
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



mean_char_list_m <- list(
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


names(mean_char_list_m) <- c(
                          "matMaxHeight",
                          "berMaxHeight",

                          "matMaxVelAge",
                          "berMaxVelAge",

                          "matMaxVel",
                          "berMaxVel"
                        )

#str(mean_char_list_m)



contr_char_list_m <- list(mean_char_list_m$berMaxHeight - mean_char_list_m$matMaxHeight,
                          mean_char_list_m$berMaxVelAge - mean_char_list_m$matMaxVelAge,
                          mean_char_list_m$berMaxVel - mean_char_list_m$matMaxVel)


names(contr_char_list_m) <- c(
                          "ber.matMaxHeight",
                          "ber.matMaxVelAge",
                          "ber.matMaxVel"
                        )

#str(contr_char_list_m)





pdf(file="./combined_plots/Plots/Chars_combined_inter2.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 2, 3, 0, 0, 0, 0, 0, 0, 4, 5, 6, 0,0,0,0,0,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        7, 8, 9, 0, 0, 0, 0, 0, 0, 10,11,12,0,0,0,0,0,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1, 0.2, 1,1, 0.2, 1,1, 0.2, 1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels


# female  ###########################################################################################################################################################################

post <- post3_f
mean_char_list <- mean_char_list_f
contr_char_list <- contr_char_list_f


MaxHeight_post_plot <- denschart3( mean_char_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxHeight ), mean( mean_char_list$matMaxHeight ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxHeight ), mean( mean_char_list$berMaxHeight ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast")
xcoords <- c(0, 7, 23)
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x=-17, y=20.5,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerArea_col, MatArea_col, ConArea_col),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=3,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = -16.5,
       ybottom = 14.5,
       xright = 51.1,
       ytop = 20,
       lwd=1)


# row labels

mtext("Max height",           side = 1, outer = T, cex = 2, adj=-0.17, line = -103 )
mtext("(cm)",                 side = 1, outer = T, cex = 2, adj=-0.13, line = -101 )

mtext("Age at max velocity",  side = 1, outer = T, cex = 2, adj=-0.23, line = -90.5 )
mtext("(years)",              side = 1, outer = T, cex = 2, adj=-0.14, line = -88.5 )

mtext("Max velocity",         side = 1, outer = T, cex = 2, adj=-0.17, line = -78 )
mtext("(cm/year)",            side = 1, outer = T, cex = 2, adj=-0.15, line = -76 )



# column labels
symbol.Female(centerx = 11, centery = 10.8, rayonx=0.7, lwd=3, col="black")

symbol.Male(  centerx = 38.5, centery = 10.4, rayonx=0.7, lwd=3, col="black")




# column brackets
lines(x=c(-2, 24),
      y=c(7.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(-2, -2),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(24, 24),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)


lines(x=c(26, 51),
      y=c(7.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(26, 26),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(51, 51),
     y=c(6.5,7.5),
      col="black", lwd=4, lty=1)


par(xpd=FALSE)




MaxVelAge_post_plot <- denschart3( mean_char_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxVelAge ) - 0.5, mean( mean_char_list$berMaxVelAge ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVelAge ), mean( mean_char_list$matMaxVelAge ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVelAge ), mean( mean_char_list$berMaxVelAge ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
           xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),
     labels=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
               xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),     
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)




MaxVel_post_plot <- denschart3( mean_char_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxVel ) - 0.5, mean( mean_char_list$berMaxVel ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVel ), mean( mean_char_list$matMaxVel ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVel ), mean( mean_char_list$berMaxVel ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
           xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
                xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)



### parameter contrasts


MaxWeight_cont_plot <- denschart3( contr_char_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-20,20),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[1]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[1]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxHeight), mean(contr_char_list$ber.matMaxHeight) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    cex.axis=cex_axis)



MaxVelAge_cont_plot <- denschart3( contr_char_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[2]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[2]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVelAge), mean(contr_char_list$ber.matMaxVelAge) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    cex.axis=cex_axis)



MaxVel_cont_plot <- denschart3( contr_char_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-1.5,1.5),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[3]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[3]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVel), mean(contr_char_list$ber.matMaxVel) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    cex.axis=cex_axis)





# male ###########################################################################################################################################################################

post <- post3_m
mean_char_list <- mean_char_list_m
contr_char_list <- contr_char_list_m


MaxHeight_post_plot <- denschart3( mean_char_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxHeight ), mean( mean_char_list$matMaxHeight ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxHeight ), mean( mean_char_list$berMaxHeight ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



par(xpd=FALSE)


MaxVelAge_post_plot <- denschart3( mean_char_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxVelAge ) - 0.5, mean( mean_char_list$berMaxVelAge ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVelAge ), mean( mean_char_list$matMaxVelAge ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVelAge ), mean( mean_char_list$berMaxVelAge ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
           xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),
     labels=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
               xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),     
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)




MaxVel_post_plot <- denschart3( mean_char_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range( mean( mean_char_list$matMaxVel ) - 0.5, mean( mean_char_list$berMaxVel ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVel ), mean( mean_char_list$matMaxVel ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVel ), mean( mean_char_list$berMaxVel ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
           xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
                xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)



### parameter contrasts


MaxWeight_cont_plot <- denschart3( contr_char_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-20,20),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[1]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[1]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxHeight), mean(contr_char_list$ber.matMaxHeight) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    cex.axis=cex_axis)



MaxVelAge_cont_plot <- denschart3( contr_char_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[2]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[2]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVelAge), mean(contr_char_list$ber.matMaxVelAge) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    cex.axis=cex_axis)



MaxVel_cont_plot <- denschart3( contr_char_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            #xlim=range(-1.5,1.5),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[3]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[3]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVel), mean(contr_char_list$ber.matMaxVel) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    cex.axis=cex_axis)



graphics.off()







###################################################################################################################################
###################################################################################################################################
#### overall parameter estimates

###### Intervention 1: equal q, K, And H



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

post <- post3_f

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

K1mat <- post$"mK[1,1]" # use K's for Berkeley
K2mat <- post$"mK[1,2]" 
K3mat <- post$"mK[1,3]" 
K4mat <- post$"mK[1,4]" 
K5mat <- post$"mK[1,5]"

K1ber <- post$"mK[1,1]"
K2ber <- post$"mK[1,2]" 
K3ber <- post$"mK[1,3]" 
K4ber <- post$"mK[1,4]" 
K5ber <- post$"mK[1,5]"

H1mat <- post$"mH[1,1]" # use H's for Berkeley
H2mat <- post$"mH[1,2]"
H3mat <- post$"mH[1,3]"
H4mat <- post$"mH[1,4]" 
H5mat <- post$"mH[1,5]"

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




##### male

post <- post3_m

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

K1mat <- post$"mK[1,1]" # use K's for Berkeley
K2mat <- post$"mK[1,2]" 
K3mat <- post$"mK[1,3]" 
K4mat <- post$"mK[1,4]" 
K5mat <- post$"mK[1,5]"

K1ber <- post$"mK[1,1]"
K2ber <- post$"mK[1,2]" 
K3ber <- post$"mK[1,3]" 
K4ber <- post$"mK[1,4]" 
K5ber <- post$"mK[1,5]"

H1mat <- post$"mH[1,1]" #use H's for Berkeley
H2mat <- post$"mH[1,2]" 
H3mat <- post$"mH[1,3]"
H4mat <- post$"mH[1,4]" 
H5mat <- post$"mH[1,5]"

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


tQmat_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKmat_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHmat_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

tQber_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKber_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHber_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

conQ_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conK_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conH_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))


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

      tQmat_m[t,] <- Pmat[1,]*Q1mat +
                     Pmat[2,]*Q2mat +
                     Pmat[3,]*Q3mat +
                     Pmat[4,]*Q4mat +
                     Pmat[5,]*Q5mat

      tKmat_m[t,] <- Pmat[1,]*Mmat[1,]*K1mat +
                     Pmat[2,]*Mmat[2,]*K2mat +
                     Pmat[3,]*Mmat[3,]*K3mat +
                     Pmat[4,]*Mmat[4,]*K4mat +
                     Pmat[5,]*Mmat[5,]*K5mat

      tHmat_m[t,] <- (Pmat[1,]*Mmat[1,]*H1mat*(2/32) + # original H in units of g/cm^2 of 5% of total body skin surface. Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
                      Pmat[2,]*Mmat[2,]*H2mat*(2/32) +
                      Pmat[3,]*Mmat[3,]*H3mat*(2/32) +
                      Pmat[4,]*Mmat[4,]*H4mat*(2/32) +
                      Pmat[5,]*Mmat[5,]*H5mat*(2/32) )



      tQber_m[t,] <- Pber[1,]*Q1ber +
                     Pber[2,]*Q2ber +
                     Pber[3,]*Q3ber +
                     Pber[4,]*Q4ber +
                     Pber[5,]*Q5ber

      tKber_m[t,] <- Pber[1,]*Mber[1,]*K1ber +
                     Pber[2,]*Mber[2,]*K2ber +
                     Pber[3,]*Mber[3,]*K3ber +
                     Pber[4,]*Mber[4,]*K4ber +
                     Pber[5,]*Mber[5,]*K5ber
                   
      tHber_m[t,] <- (Pber[1,]*Mber[1,]*H1ber*(2/32) +
                      Pber[2,]*Mber[2,]*H2ber*(2/32) +
                      Pber[3,]*Mber[3,]*H3ber*(2/32) +
                      Pber[4,]*Mber[4,]*H4ber*(2/32) +
                      Pber[5,]*Mber[5,]*H5ber*(2/32) )

      #contrasts
      conQ_m[t,] <- tQber_m[t,] - tQmat_m[t,]
      conK_m[t,] <- tKber_m[t,] - tKmat_m[t,] 
      conH_m[t,] <- tHber_m[t,] - tHmat_m[t,]  

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







pdf(file="./combined_plots/Plots/params_combined_intervention1.pdf",
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

post <- post3_f

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
  axis( side=4, at=c(-0.005, 0.005), labels=NA, las=2 )
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


  par(xpd=NA) # plotting clipped to device region
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

  legend(x=-5, y=15.2,
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

  rect(xleft = -5,
       ybottom = 13.3,
       xright = 21.91,
       ytop = 14.9,
       lwd=1)


# row labels

mtext(expression(paste(bolditalic("q"))),   side = 1, outer = T, cex = 1.5, adj=-0.12, line = -55 ) #side = c(bottom, left, top, right)

mtext(expression(paste(bolditalic("K"))),   side = 1, outer = T, cex = 1.5, adj=-0.12, line = -32 )
mtext(expression(paste("(g/g)")),           side = 1, outer = T, cex = 1.5, adj=-0.16, line = -29 )

mtext(expression(paste(bolditalic("H"))),   side = 1, outer = T, cex = 1.5, adj=-0.12, line = -12 )
mtext(expression(paste("(g/cm"^2,")")),     side = 1, outer = T, cex = 1.5, adj=-0.20, line = -9 )


mtext("Age since conception (years)",       side = 1, outer = T, cex = 1.5, adj=0.6, line = 3.8 )




# column labels
symbol.Female(centerx = 5, centery = 12, rayonx=0.4, lwd=1.5, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.107, line = -110.5 )

symbol.Male(  centerx = 16.5, centery = 11.6, rayonx=0.4, lwd=1.5, col="black")
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
  par(xpd=FALSE) # plotting clipped to plot region


  par(xpd=TRUE)
  #text(expression(paste(bolditalic("K"))), x=-7, y=16, srt=0, las=3, cex=1.5)
  #text(expression(paste("(g/g)")), x=-7, y=15, srt=0, las=3, cex=1)
  #text("Age since conception (years)", x=13, y=2, srt=0, las=3)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.7-0.2,0.2+5), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.4, 0.4), labels=NA, las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conK, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)

  lines(x = a,
        y = rowMeans(conK),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region



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

  text("birth", x=0.75, y=-0.05, srt=0, las=3, cex=1.5)
  par(xpd=FALSE)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.03-0.005,0.005+0.15), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.02, 0.02), labels=NA, las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conH, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = a,
        y = rowMeans(conH),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region



# male  ###########################################################################################################################################################################

post <- post3_m

tQmat <- tQmat_m
tKmat <- tKmat_m
tHmat <- tHmat_m

tQber <- tQber_m
tKber <- tKber_m
tHber <- tHber_m

conQ <- conQ_m
conK <- conK_m
conH <- conH_m



################ Q

 #set up plot
  #par(mar=c(5, 6, 2, 6), oma = c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0.07, 0.18), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.10,0.18,0.02), labels=NA, las=1 )
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


  par(xpd=TRUE)
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


  #par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region

  # lines(x = c(min(a),max(a)),
  #       y = c(0,0),
  #       col="black", lwd=1, lty="33") #lty: first number in string is dash length, second is white space length




par(xpd=FALSE)



################### K

plot( x=0, y=60, type="n", ylim=c(4,24), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(8,24,4), labels=NA, las=1 )
  axis( side=1, at=seq(0,25,5), labels=NA, las=1 )
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
  par(xpd=FALSE) # plotting clipped to plot region


  #par(xpd=TRUE)
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

  #par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region


  par(xpd=NA)  # plotting clipped to devise region
  text("Contrast", x=33, y=2.5, srt=270, las=2, cex=2.3)
  par(xpd=FALSE) # plotting clipped to plot region



################ H


plot( x=0, y=60, type="n", ylim=c(0.33-0.15,1), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.3,1,0.1), labels=NA, las=1 )
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

  #par(xpd=TRUE)
  #text(expression(paste(bolditalic("H"))), x=-7, y=12, srt=0, las=3, cex=1.5)
  #text(expression(paste("(g/cm"^2,")")), x=-7, y=11, srt=0, las=3, cex=1)
  #text("Age since conception (years)", x=13, y=1.5, srt=0, las=3)

  par(xpd=NA)
  text("birth", x=0.75, y=-0.05, srt=0, las=3, cex=1.5)
  par(xpd=FALSE)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.03-0.005,0.005+0.15), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.02, 0, 0.02), labels=c(-0.01, 0, 0.01), las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conH, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = a,
        y = rowMeans(conH),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  #par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region


graphics.off()






##############################################################################
#### Intervention 2


##### female

post <- post3_f

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
K2mat <- post$"mK[1,2]" # use K2 for Berkeley
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
H3mat <- post$"mH[1,3]" # use H3 for Berkeley
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




##### male

post <- post3_m

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
K2mat <- post$"mK[1,2]" # use K2 and K3 for Berkeley
K3mat <- post$"mK[1,3]" 
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


tQmat_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKmat_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHmat_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

tQber_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKber_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHber_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

conQ_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conK_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conH_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))


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

      tQmat_m[t,] <- Pmat[1,]*Q1mat +
                     Pmat[2,]*Q2mat +
                     Pmat[3,]*Q3mat +
                     Pmat[4,]*Q4mat +
                     Pmat[5,]*Q5mat

      tKmat_m[t,] <- Pmat[1,]*Mmat[1,]*K1mat +
                     Pmat[2,]*Mmat[2,]*K2mat +
                     Pmat[3,]*Mmat[3,]*K3mat +
                     Pmat[4,]*Mmat[4,]*K4mat +
                     Pmat[5,]*Mmat[5,]*K5mat

      tHmat_m[t,] <- (Pmat[1,]*Mmat[1,]*H1mat*(2/32) + # original H in units of g/cm^2 of 5% of total body skin surface. Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
                      Pmat[2,]*Mmat[2,]*H2mat*(2/32) +
                      Pmat[3,]*Mmat[3,]*H3mat*(2/32) +
                      Pmat[4,]*Mmat[4,]*H4mat*(2/32) +
                      Pmat[5,]*Mmat[5,]*H5mat*(2/32) )



      tQber_m[t,] <- Pber[1,]*Q1ber +
                     Pber[2,]*Q2ber +
                     Pber[3,]*Q3ber +
                     Pber[4,]*Q4ber +
                     Pber[5,]*Q5ber

      tKber_m[t,] <- Pber[1,]*Mber[1,]*K1ber +
                     Pber[2,]*Mber[2,]*K2ber +
                     Pber[3,]*Mber[3,]*K3ber +
                     Pber[4,]*Mber[4,]*K4ber +
                     Pber[5,]*Mber[5,]*K5ber
                   
      tHber_m[t,] <- (Pber[1,]*Mber[1,]*H1ber*(2/32) +
                      Pber[2,]*Mber[2,]*H2ber*(2/32) +
                      Pber[3,]*Mber[3,]*H3ber*(2/32) +
                      Pber[4,]*Mber[4,]*H4ber*(2/32) +
                      Pber[5,]*Mber[5,]*H5ber*(2/32) )

      #contrasts
      conQ_m[t,] <- tQber_m[t,] - tQmat_m[t,]
      conK_m[t,] <- tKber_m[t,] - tKmat_m[t,] 
      conH_m[t,] <- tHber_m[t,] - tHmat_m[t,]  

} # for t




###############################################




pdf(file="./combined_plots/Plots/params_combined_intervention2.pdf",
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

post <- post3_f

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
  axis( side=4, at=c(-0.005, 0.005), labels=NA, las=2 )
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


  par(xpd=NA) # plotting clipped to device region
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

  legend(x=-5, y=15.2,
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

  rect(xleft = -5,
       ybottom = 13.3,
       xright = 21.91,
       ytop = 14.9,
       lwd=1)


# row labels

mtext(expression(paste(bolditalic("q"))),   side = 1, outer = T, cex = 1.5, adj=-0.12, line = -55 ) #side = c(bottom, left, top, right)

mtext(expression(paste(bolditalic("K"))),   side = 1, outer = T, cex = 1.5, adj=-0.12, line = -32 )
mtext(expression(paste("(g/g)")),           side = 1, outer = T, cex = 1.5, adj=-0.16, line = -29 )

mtext(expression(paste(bolditalic("H"))),   side = 1, outer = T, cex = 1.5, adj=-0.12, line = -12 )
mtext(expression(paste("(g/cm"^2,")")),     side = 1, outer = T, cex = 1.5, adj=-0.20, line = -9 )


mtext("Age since conception (years)",       side = 1, outer = T, cex = 1.5, adj=0.6, line = 3.8 )




# column labels
symbol.Female(centerx = 5, centery = 12, rayonx=0.4, lwd=1.5, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.107, line = -110.5 )

symbol.Male(  centerx = 16.5, centery = 11.6, rayonx=0.4, lwd=1.5, col="black")
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
  par(xpd=FALSE) # plotting clipped to plot region


  par(xpd=TRUE)
  #text(expression(paste(bolditalic("K"))), x=-7, y=16, srt=0, las=3, cex=1.5)
  #text(expression(paste("(g/g)")), x=-7, y=15, srt=0, las=3, cex=1)
  #text("Age since conception (years)", x=13, y=2, srt=0, las=3)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.7-0.2,0.2+5), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.4, 0.4), labels=NA, las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conK, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)

  lines(x = a,
        y = rowMeans(conK),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region



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

  text("birth", x=0.75, y=-0.05, srt=0, las=3, cex=1.5)
  par(xpd=FALSE)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.03-0.005,0.005+0.15), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.02, 0.02), labels=NA, las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conH, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = a,
        y = rowMeans(conH),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region



# male  ###########################################################################################################################################################################

post <- post3_m

tQmat <- tQmat_m
tKmat <- tKmat_m
tHmat <- tHmat_m

tQber <- tQber_m
tKber <- tKber_m
tHber <- tHber_m

conQ <- conQ_m
conK <- conK_m
conH <- conH_m



################ Q

 #set up plot
  #par(mar=c(5, 6, 2, 6), oma = c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0.07, 0.18), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.10,0.18,0.02), labels=NA, las=1 )
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


  par(xpd=TRUE)
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


  #par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region

  # lines(x = c(min(a),max(a)),
  #       y = c(0,0),
  #       col="black", lwd=1, lty="33") #lty: first number in string is dash length, second is white space length




par(xpd=FALSE)



################### K

plot( x=0, y=60, type="n", ylim=c(4,24), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(8,24,4), labels=NA, las=1 )
  axis( side=1, at=seq(0,25,5), labels=NA, las=1 )
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
  par(xpd=FALSE) # plotting clipped to plot region


  #par(xpd=TRUE)
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

  #par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region


  par(xpd=NA)  # plotting clipped to devise region
  text("Contrast", x=33, y=2.5, srt=270, las=2, cex=2.3)
  par(xpd=FALSE) # plotting clipped to plot region



################ H


plot( x=0, y=60, type="n", ylim=c(0.33-0.15,1), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.3,1,0.1), labels=NA, las=1 )
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

  #par(xpd=TRUE)
  #text(expression(paste(bolditalic("H"))), x=-7, y=12, srt=0, las=3, cex=1.5)
  #text(expression(paste("(g/cm"^2,")")), x=-7, y=11, srt=0, las=3, cex=1)
  #text("Age since conception (years)", x=13, y=1.5, srt=0, las=3)

  par(xpd=NA)
  text("birth", x=0.75, y=-0.05, srt=0, las=3, cex=1.5)
  par(xpd=FALSE)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.03-0.005,0.005+0.15), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.02, 0, 0.02), labels=c(-0.01, 0, 0.01), las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conH, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = a,
        y = rowMeans(conH),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  #par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region


graphics.off()



