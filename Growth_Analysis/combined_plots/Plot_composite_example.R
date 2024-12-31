

######################################### Fig 1 #########################################


######## load posteriors from model

# Ber female height
post3_h <- readRDS("./analysis_hw_fem/post6.RDS")
#str(post3_h)

# Ber female weight
post3_w <- readRDS("./analysis_hwfull_fem/post6.RDS")
#str(post3_w)




######## plotting colors

colorlist <- palette.colors(palette = "Okabe-Ito", alpha=0.75) 
names(colorlist) <- c("black","orange","teal","green","yellow","blue","red","pink","grey")
colorlist
#pie(rep(1, 9), col = colorlist)


# line formats
cumu_lwd <- 4
cumu_lty <- "solid" #1
cumu_col <- "black"

inf_lwd <- 4
inf_lty <- "solid" #1
inf_col <- colorlist["blue"]

chil1_lwd <- 4
chil1_lty <- "solid" #1
chil1_col <- colorlist["red"]

chil2_lwd <- 4
chil2_lty <- "21" #2 #"33"       #lty: first number in string is dash length, second is white space length
chil2_col <- grey(0.5) #"black" #colorlist["yellow"]

chil3_lwd <- 4
chil3_lty <- "solid" #1
chil3_col <- colorlist["green"]

adol_lwd <- 4
adol_lty <- "solid" #1
adol_col <- colorlist["pink"]






pdf(file="./combined_plots/Plots/Composite_model_illustration.pdf",
    height=9, width=5)
layout( mat=matrix(c(1,2),2,1,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 4, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)



  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age



  ###### height

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,170), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=seq( 0, 170, by=40), labels=TRUE )


  post <- post3_h


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
        col=cumu_col, lwd=cumu_lwd, lty=cumu_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 - x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1]), NA),
        col=inf_col, lwd=inf_lwd, lty=inf_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], ( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2]), NA),
        col=chil1_col, lwd=chil1_lwd, lty=chil1_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], ( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3]), NA),
        col=chil2_col, lwd=chil2_lwd, lty=chil2_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], ( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4]), NA),
        col=chil3_col, lwd=chil3_lwd, lty=chil3_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], ( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5]), NA),
        col=adol_col, lwd=adol_lwd, lty=adol_lty)



  par(xpd=NA) # clip plotting to device region

  legend(-2.29, #-9.22,
          220, #200,
         legend=c("1. In utero",
                 "2. Infancy",
                 "3. Early childhood",
                 "4. Late childhood",
                 "5. Adolescence",
                 "Cumulative growth"), bty="o", bg="white",
         col=c(inf_col, chil1_col, chil2_col, chil3_col, adol_col, cumu_col),
         lty=c(inf_lty, chil1_lty, chil2_lty, chil3_lty, adol_lty, cumu_lty),
         lwd=c(inf_lwd, chil1_lwd, chil2_lwd, chil3_lwd, adol_lwd, cumu_lwd),
         cex=0.8, seg.len=3, ncol=2) # ncol=number of columns of legend items



  ###### weight

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,3500), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 3500, by=500), labels=TRUE )
  par(xpd=FALSE) # clip to plot region
  
  post <- post3_w


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
        col=cumu_col, lwd=cumu_lwd, lty=cumu_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, pi*( 2*mH[1]/mK[1] * ( 1 - exp(mK[1]*mQ[1]*( 0 -     x )/( 1 + 2*mQ[1] )) ) )^(1/mQ[1] + 2), NA),
        col=inf_col, lwd=inf_lwd, lty=inf_lty)

  # child 1
  lines(x = x,
        y = ifelse( x > mI[1], pi*( 2*mH[2]/mK[2] * ( 1 - exp(mK[2]*mQ[2]*( mI[1] - x )/( 1 + 2*mQ[2] )) ) )^(1/mQ[2] + 2), NA),
        col=chil1_col, lwd=chil1_lwd, lty=chil1_lty)

  # child 2
  lines(x = x,
        y = ifelse( x > mI[2], pi*( 2*mH[3]/mK[3] * ( 1 - exp(mK[3]*mQ[3]*( mI[2] - x )/( 1 + 2*mQ[3] )) ) )^(1/mQ[3] + 2), NA),
        col=chil2_col, lwd=chil2_lwd, lty=chil2_lty)

  # child 3
  lines(x = x,
        y = ifelse( x > mI[3], pi*( 2*mH[4]/mK[4] * ( 1 - exp(mK[4]*mQ[4]*( mI[3] - x )/( 1 + 2*mQ[4] )) ) )^(1/mQ[4] + 2), NA),
        col=chil3_col, lwd=chil3_lwd, lty=chil3_lty)

  # adolescent
  lines(x = x,
        y = ifelse( x > mI[4], pi*( 2*mH[5]/mK[5] * ( 1 - exp(mK[5]*mQ[5]*( mI[4] - x )/( 1 + 2*mQ[5] )) ) )^(1/mQ[5] + 2), NA),
        col=adol_col, lwd=adol_lwd, lty=adol_lty)


  par(xpd=NA) # clip plotting to device region
  mtext("Age since conception (years)", side = 1, outer = T, cex = 1.2, line = 2, adj=0.5)
  mtext("Height (cm)", side = 2, outer = T, cex = 1.2, line = 2, adj=0.8)
  mtext("Skeletal cell weight (g)", side = 2, outer = T, cex = 1.2, line = 2, adj=0.17)


graphics.off()



















################################################################################

################## Illustration of power relationship between r and h

pdf(file="./combined_plots/Plots/Power_illustration.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(10), by=0.01)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(0,10), xlim=c(0,10), cex=1.5,
        xlab=expression(paste("Time ", italic("t"))),
        ylab=expression(paste("Height ", italic("h"), " or Radius ", italic("r"))) )

  # height
  lines(x = x,
        y = x,
        col="black", lty=2, lwd=2)

  # r = 5
  lines(x = x,
        y = rep(5, times=length(x)),
        col="red", lty=1, lwd=3)

  # r = 0.5*h
  lines(x = x,
        y = 0.5*x,
        col="blue", lty=1, lwd=3)

  # r = h^0.5
  lines(x = x,
        y = x^0.5,
        col="black", lty=1, lwd=3)


  legend(0, 10,
         legend=c("Height",
                  expression(paste("Radius when ", italic("r"), " = 5")),
                  expression(paste("Radius when ", italic("r"), " = 1/2*", italic("h"))),
                  expression(paste("Radius when ", italic("r"), " = ", italic("h"), "^(1/2)"))),
         bty="o", bg="white",
         col=c("black", "red", "blue", "black"),
         lty=c(2,1,1,1),
         lwd=c(2,3,3,3), cex=0.8, seg.len=3)

graphics.off()






#####################################################################################

################## Proportion of body weight comprising living cells of the skeleton


pdf(file="./combined_plots/Plots/Weight_prop.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(25), by=0.01)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 5, 2, 2)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(0,1), xlim=c(0,22), axes=FALSE, ylab=NA, xlab=NA, xaxs = "i", yaxs = "i") #"i" makes axes start at 0 corner
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 20, by=5), labels=TRUE, cex.axis=0.75)
  axis( side=2, at=c(0, 0.05, 0.1, 0.5, 1), labels=c(0,0.05,0.1,0.5,1), cex.axis=0.75)


  lines(x = x,
        y = 0.05+0.95*exp(-4*x),
        col="black", lty=1, lwd=2)

  points(x=c(0.75,1.75,5.75,10.75,15.75,20.75),
         y=c(0.026,0.021,0.028,0.041,0.045,0.045), lwd=1, col=colorlist["blue"], cex=1.5, pch=16)


  par(xpd=NA) # clip plotting to device region
  lines(x = c(0.75,0.75),
        y = c(-0.13,1),
        col="black", lwd=1, lty=1)

  text("birth", x=0.75, y=-0.15, srt=0, las=3, cex=0.75)


  mtext("Age since conception (years)", side = 1, outer = T, cex = 0.75, line = -2, adj=0.6)
  mtext("Proportion of body weight comprising structural (skeletal) cells",
         side = 2, outer = T, cex = 0.75, line = -2, adj=0.7)


graphics.off()




