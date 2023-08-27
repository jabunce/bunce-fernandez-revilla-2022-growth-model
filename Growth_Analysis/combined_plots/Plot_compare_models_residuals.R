

######################################### Fig 2 #########################################


######## load posteriors from models, need to run PrepareData to plot raw data points

# Ber male height

# Composite model
post1_comp <- readRDS("./analysis_height_mal/post1.RDS")
#str(post1_comp)

# Composite-5 model
post4_comp5 <- readRDS("./analysis_height_mal/post4.RDS")
#str(post4_comp5)

# JPA model
post5_JPA <- readRDS("./analysis_height_mal/post5.RDS")
#str(post5_JPA)

# SITAR model
sitar_mat_mean <- readRDS("./analysis_height_mal/sitar_mat_mean.RDS")
sitar_ber_mean <- readRDS("./analysis_height_mal/sitar_ber_mean.RDS")
sitar_mat_vmean <- readRDS("./analysis_height_mal/sitar_mat_vmean.RDS")
sitar_ber_vmean <- readRDS("./analysis_height_mal/sitar_ber_vmean.RDS")


######## plotting colors

colorlist <- palette.colors(palette = "Okabe-Ito", alpha=0.75) 
names(colorlist) <- c("black","orange","teal","green","yellow","blue","red","pink","grey")
colorlist
#pie(rep(1, 9), col = colorlist)


# point and line colors
Point_pch <- 1 # 1=open circle, 19=solid circle
Point_lwd <- 0.5 # lines weight for open cirlce points
Point_col <- colorlist["grey"]
Point_cex <- 0.3

Comp_lwd <- 2
Comp_lty <- 1
Comp_col <- "black"

Comp5_lwd <- 2
Comp5_lty <- 1
Comp5_col <- colorlist["blue"]

JPA_lwd <- 2
JPA_lty <- 1
JPA_col <- colorlist["red"]

SITAR_lwd <- 2
SITAR_lty <- 1
SITAR_col <- colorlist["pink"]




pdf(file="./combined_plots/Plots/Model_compare.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,25), xlab="Age since conception (yrs)", ylab="Height (cm)" )


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=Point_lwd, col=Point_col, pch=Point_pch, cex=Point_cex)



  # plot composite model
  post <- post1_comp

  mQ1 = exp(mean(post$muLQ1))
  mQ2 = exp(mean(post$muLQ2))
  mQ3 = exp(mean(post$muLQ3))
  mK1 = exp(mean(post$muLK1))
  mK2 = exp(mean(post$muLK2))
  mK3 = exp(mean(post$muLK3))
  mH1 = mK1/2 + mean(post$muR1)
  mH2 = mK2/2 + mean(post$muR2)
  mH3 = mK3/2 + mean(post$muR3)

  lines(x = x,
        y = ( -1*exp(-1*mK1*mQ1*x/( 1 + 2*mQ1 )) + 2*mH1/mK1 )^(1/mQ1) +
            ( -1*exp(-1*mK2*mQ2*x/( 1 + 2*mQ2 )) + 2*mH2/mK2 )^(1/mQ2) +
            ( -1*exp(-1*mK3*mQ3*x/( 1 + 2*mQ3 )) + 2*mH3/mK3 )^(1/mQ3),
        col=Comp_col, lwd=Comp_lwd, lty=Comp_lty)


  # Plot composite-5 model
  post <- post4_comp5

  muQ1 = exp(mean(post$muLQ1))
  muQ2 = exp(mean(post$muLQ2))
  muQ3 = exp(mean(post$muLQ3))
  muQ4 = exp(mean(post$muLQ4))
  muQ5 = exp(mean(post$muLQ5))
  muK1 = exp(mean(post$muLK1))
  muK2 = exp(mean(post$muLK2))
  muK3 = exp(mean(post$muLK3))
  muK4 = exp(mean(post$muLK4))
  muK5 = exp(mean(post$muLK5))
  muH1 = muK1/2 + mean(post$muR1)
  muH2 = muK2/2 + mean(post$muR2)
  muH3 = muK3/2 + mean(post$muR3)
  muH4 = muK4/2 + mean(post$muR4)
  muH5 = muK5/2 + mean(post$muR5)

  lines(x = x,
        y = ( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1 )^(1/muQ1) +
            ( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2 )^(1/muQ2) +
            ( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3 )^(1/muQ3) +
            ( -1*exp(-1*muK4*muQ4*x/( 1 + 2*muQ4 )) + 2*muH4/muK4 )^(1/muQ4) +
            ( -1*exp(-1*muK5*muQ5*x/( 1 + 2*muQ5 )) + 2*muH5/muK5 )^(1/muQ5),
        col=Comp5_col, lwd=Comp5_lwd, lty=Comp5_lty)


  # plot JPA model 
  post <- post5_JPA
  lines(x = x,
        y=mean(post$muA)*exp( -1/( mean(post$muC1)*log( x/mean(post$muD1) + 1 ) + (x/mean(post$muD2))^mean(post$muC2) + (x/mean(post$muD3))^mean(post$muC3) ) ),
        col=JPA_col, lwd=JPA_lwd, lty=JPA_lty)

  # plot SITAR model
  points( x = sitar_ber_mean[,"TotAge"],
          y = sitar_ber_mean[,"Height"],
          type = "l",
          col=SITAR_col, lwd=SITAR_lwd, lty=SITAR_lty)





  #plot mean velocity trajectories

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=25, type="n", ylim=c(0,25), xlim=c(0,25), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 25, by=5 ), las=2 )
  par(xpd=FALSE) # don't plot beyond the axes
  #text("Growth velocity (cm/year)", x=33, y=13, srt=270, las=3)

  # plot composite velocity  
  post <- post1_comp

  mQ1 = exp(mean(post$muLQ1))
  mQ2 = exp(mean(post$muLQ2))
  mQ3 = exp(mean(post$muLQ3))
  mK1 = exp(mean(post$muLK1))
  mK2 = exp(mean(post$muLK2))
  mK3 = exp(mean(post$muLK3))
  mH1 = mK1/2 + mean(post$muR1)
  mH2 = mK2/2 + mean(post$muR2)
  mH3 = mK3/2 + mean(post$muR3)

  y_primem <- 1/(1+2*mQ1)*mK1*exp((-1*mK1*mQ1*x)/(1+2*mQ1))*( -1*exp((-1*mK1*mQ1*x)/(1+2*mQ1)) + 2*mH1/mK1 )^(1/mQ1-1) + 
                1/(1+2*mQ2)*mK2*exp((-1*mK2*mQ2*x)/(1+2*mQ2))*( -1*exp((-1*mK2*mQ2*x)/(1+2*mQ2)) + 2*mH2/mK2 )^(1/mQ2-1) +
                1/(1+2*mQ3)*mK3*exp((-1*mK3*mQ3*x)/(1+2*mQ3))*( -1*exp((-1*mK3*mQ3*x)/(1+2*mQ3)) + 2*mH3/mK3 )^(1/mQ3-1)
  lines(x = x, y=y_primem, col=Comp_col, lwd=Comp_lwd, lty=Comp_lty)


  # Plot composite-5 velocity
  post <- post4_comp5

  muQ1 = exp(mean(post$muLQ1))
  muQ2 = exp(mean(post$muLQ2))
  muQ3 = exp(mean(post$muLQ3))
  muQ4 = exp(mean(post$muLQ4))
  muQ5 = exp(mean(post$muLQ5))
  muK1 = exp(mean(post$muLK1))
  muK2 = exp(mean(post$muLK2))
  muK3 = exp(mean(post$muLK3))
  muK4 = exp(mean(post$muLK4))
  muK5 = exp(mean(post$muLK5))
  muH1 = muK1/2 + mean(post$muR1)
  muH2 = muK2/2 + mean(post$muR2)
  muH3 = muK3/2 + mean(post$muR3)
  muH4 = muK4/2 + mean(post$muR4)
  muH5 = muK5/2 + mean(post$muR5)

  y_prime <- 1/(1+2*muQ1)*muK1*exp((-1*muK1*muQ1*x)/(1+2*muQ1))*( -1*exp((-1*muK1*muQ1*x)/(1+2*muQ1)) + 2*muH1/muK1 )^(1/muQ1-1) + 
             1/(1+2*muQ2)*muK2*exp((-1*muK2*muQ2*x)/(1+2*muQ2))*( -1*exp((-1*muK2*muQ2*x)/(1+2*muQ2)) + 2*muH2/muK2 )^(1/muQ2-1) +
             1/(1+2*muQ3)*muK3*exp((-1*muK3*muQ3*x)/(1+2*muQ3))*( -1*exp((-1*muK3*muQ3*x)/(1+2*muQ3)) + 2*muH3/muK3 )^(1/muQ3-1) +
             1/(1+2*muQ4)*muK4*exp((-1*muK4*muQ4*x)/(1+2*muQ4))*( -1*exp((-1*muK4*muQ4*x)/(1+2*muQ4)) + 2*muH4/muK4 )^(1/muQ4-1) +
             1/(1+2*muQ5)*muK5*exp((-1*muK5*muQ5*x)/(1+2*muQ5))*( -1*exp((-1*muK5*muQ5*x)/(1+2*muQ5)) + 2*muH5/muK5 )^(1/muQ5-1)
  lines(x = x, y=y_prime, col=Comp5_col, lwd=Comp5_lwd, lty=Comp5_lty)


  # plot JPA model 
  post <- post5_JPA
  y_prime <- mean(post$muA)*exp( -1/( mean(post$muC1)*log( x/mean(post$muD1) + 1 ) + (x/mean(post$muD2))^mean(post$muC2) + (x/mean(post$muD3))^mean(post$muC3) ) )*
             ( mean(post$muC1)*x + ( mean(post$muD1) + x )*( mean(post$muC2)*(x/mean(post$muD2))^mean(post$muC2) + mean(post$muC3)*(x/mean(post$muD3))^mean(post$muC3) ) )*
              1/( x*( mean(post$muD1) + x )*( (x/mean(post$muD2))^mean(post$muC2) + (x/mean(post$muD3))^mean(post$muC3) + mean(post$muC1)*log( 1 + x/mean(post$muD1) ) )^2 )

  lines(x = x,
        y = y_prime,
        col=JPA_col, lwd=JPA_lwd, lty=JPA_lty)


  # plot SITAR model
  points( x = sitar_ber_vmean[,"TotAge"],
          y = sitar_ber_vmean[,"HeightVelocity"],
          type = "l",
          col=SITAR_col, lwd=SITAR_lwd, lty=SITAR_lty)


  par(xpd=TRUE)
  text("Growth velocity (cm/year)", x=33, y=12, srt=270, las=3)

  legend(-0.7, 25.5,
         legend=c("U.S. data",
                 "Composite-3 model",
                 "Composite-5 model",
                 "JPA-1 model",
                 "SITAR model"
                 ), bty="o", bg="white",
         col=c(Point_col, Comp_col, Comp5_col, JPA_col, SITAR_col),
         merge=FALSE,
         pch=c(Point_pch, NA, NA, NA, NA),
         lty=c(0, Comp_lty, Comp5_lty, JPA_lty, SITAR_lty),
         lwd=c(Point_lwd, Comp_lwd, Comp5_lwd, JPA_lwd, SITAR_lwd),
         cex=0.6, seg.len=3)


graphics.off()




#################################################################





######################################### Fig A.22 #########################################



####### Compare residuals


colorlist <- palette.colors(palette = "Okabe-Ito", alpha=0.75) 
names(colorlist) <- c("black","orange","teal","green","yellow","blue","red","pink","grey")
colorlist
#pie(rep(1, 9), col = colorlist)

Point_pch <- 1 # 1=open circle, 19=solid circle
Point_lwd <- 0.5 # lines weight for open circle points
Point_cex <- 0.5

Comp_lwd <- 0.75
Comp_lty <- 1
Comp_col <- "black"

Comp5_lwd <- 0.75
Comp5_lty <- 1
Comp5_col <- colorlist["blue"]

JPA_lwd <- 0.75
JPA_lty <- 1
JPA_col <- colorlist["red"]





pdf(file="./combined_plots/Plots/Residual_compare.pdf",
    height=5, width=5)
layout( mat=matrix(c(1,2,3),3,1,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
par(mar = c(1, 1, 1, 1), oma = c(5, 5, 1, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  # plot composite model

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-40,40), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-30,0,30), labels=c(-30,0,30) )


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")

  post <- post1_comp

  mQ1m = exp(mean(post$muLQ1))
  mQ1l = exp(HPDI(post$muLQ1, prob=0.9)[1])
  mQ1h = exp(HPDI(post$muLQ1, prob=0.9)[2])

  mQ2m = exp(mean(post$muLQ2))
  mQ2l = exp(HPDI(post$muLQ2, prob=0.9)[1])
  mQ2h = exp(HPDI(post$muLQ2, prob=0.9)[2])

  mQ3m = exp(mean(post$muLQ3))
  mQ3l = exp(HPDI(post$muLQ3, prob=0.9)[1])
  mQ3h = exp(HPDI(post$muLQ3, prob=0.9)[2])

  mK1m = exp(mean(post$muLK1))
  mK1l = exp(HPDI(post$muLK1, prob=0.9)[1])
  mK1h = exp(HPDI(post$muLK1, prob=0.9)[2])

  mK2m = exp(mean(post$muLK2))
  mK2l = exp(HPDI(post$muLK2, prob=0.9)[1])
  mK2h = exp(HPDI(post$muLK2, prob=0.9)[2])

  mK3m = exp(mean(post$muLK3))
  mK3l = exp(HPDI(post$muLK3, prob=0.9)[1])
  mK3h = exp(HPDI(post$muLK3, prob=0.9)[2])

  mH1m = mK1m/2 + mean(post$muR1)
  mH1l = mK1l/2 + HPDI(post$muR1, prob=0.9)[1]
  mH1h = mK1h/2 + HPDI(post$muR1, prob=0.9)[2]

  mH2m = mK2m/2 + mean(post$muR2)
  mH2l = mK2l/2 + HPDI(post$muR2, prob=0.9)[1]
  mH2h = mK2h/2 + HPDI(post$muR2, prob=0.9)[2]

  mH3m = mK3m/2 + mean(post$muR3)
  mH3l = mK3l/2 + HPDI(post$muR3, prob=0.9)[1]
  mH3h = mK3h/2 + HPDI(post$muR3, prob=0.9)[2]


  for ( i in 1:nrow(Ber.mal) ) {

    x <- Ber.mal$TotAge[i]

    lines(x = c( x, x ),
          y = c( Ber.mal$Height[i] - ( ( -1*exp(-1*mK1l*mQ1l*x/( 1 + 2*mQ1l )) + 2*mH1l/mK1l )^(1/mQ1l) +
                                       ( -1*exp(-1*mK2l*mQ2l*x/( 1 + 2*mQ2l )) + 2*mH2l/mK2l )^(1/mQ2l) +
                                       ( -1*exp(-1*mK3l*mQ3l*x/( 1 + 2*mQ3l )) + 2*mH3l/mK3l )^(1/mQ3l) ),

                 Ber.mal$Height[i] - ( ( -1*exp(-1*mK1h*mQ1h*x/( 1 + 2*mQ1h )) + 2*mH1h/mK1h )^(1/mQ1h) +
                                       ( -1*exp(-1*mK2h*mQ2h*x/( 1 + 2*mQ2h )) + 2*mH2h/mK2h )^(1/mQ2h) +
                                       ( -1*exp(-1*mK3h*mQ3h*x/( 1 + 2*mQ3h )) + 2*mH3h/mK3h )^(1/mQ3h) )
                 ),
          col=Comp_col, lwd=Comp_lwd, lty=Comp_lty)

    points( x = x,
            y = Ber.mal$Height[i] - ( ( -1*exp(-1*mK1m*mQ1m*x/( 1 + 2*mQ1m )) + 2*mH1m/mK1m )^(1/mQ1m) +
                                      ( -1*exp(-1*mK2m*mQ2m*x/( 1 + 2*mQ2m )) + 2*mH2m/mK2m )^(1/mQ2m) +
                                      ( -1*exp(-1*mK3m*mQ3m*x/( 1 + 2*mQ3m )) + 2*mH3m/mK3m )^(1/mQ3m) ),
            col=Comp_col, lwd=Point_lwd, pch=Point_pch, cex=Point_cex)

  } # for i

  text("Composite-3 model", x=0, y=35, cex=1, adj=0)



  # Plot composite-5 model

  #set up plot
  plot( x=0, y=10, type="n", ylim=c(-40,40), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE ) #(1=bottom, 2=left, 3=top, 4=right)
  axis( side=2, at=c(-30,0,30), labels=c(-30,0,30) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE )

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")

  post <- post4_comp5

  mQ1m = exp(mean(post$muLQ1))
  mQ1l = exp(HPDI(post$muLQ1, prob=0.9)[1])
  mQ1h = exp(HPDI(post$muLQ1, prob=0.9)[2])

  mQ2m = exp(mean(post$muLQ2))
  mQ2l = exp(HPDI(post$muLQ2, prob=0.9)[1])
  mQ2h = exp(HPDI(post$muLQ2, prob=0.9)[2])

  mQ3m = exp(mean(post$muLQ3))
  mQ3l = exp(HPDI(post$muLQ3, prob=0.9)[1])
  mQ3h = exp(HPDI(post$muLQ3, prob=0.9)[2])

  mQ4m = exp(mean(post$muLQ4))
  mQ4l = exp(HPDI(post$muLQ4, prob=0.9)[1])
  mQ4h = exp(HPDI(post$muLQ4, prob=0.9)[2])

  mQ5m = exp(mean(post$muLQ5))
  mQ5l = exp(HPDI(post$muLQ5, prob=0.9)[1])
  mQ5h = exp(HPDI(post$muLQ5, prob=0.9)[2])


  mK1m = exp(mean(post$muLK1))
  mK1l = exp(HPDI(post$muLK1, prob=0.9)[1])
  mK1h = exp(HPDI(post$muLK1, prob=0.9)[2])

  mK2m = exp(mean(post$muLK2))
  mK2l = exp(HPDI(post$muLK2, prob=0.9)[1])
  mK2h = exp(HPDI(post$muLK2, prob=0.9)[2])

  mK3m = exp(mean(post$muLK3))
  mK3l = exp(HPDI(post$muLK3, prob=0.9)[1])
  mK3h = exp(HPDI(post$muLK3, prob=0.9)[2])

  mK4m = exp(mean(post$muLK4))
  mK4l = exp(HPDI(post$muLK4, prob=0.9)[1])
  mK4h = exp(HPDI(post$muLK4, prob=0.9)[2])

  mK5m = exp(mean(post$muLK5))
  mK5l = exp(HPDI(post$muLK5, prob=0.9)[1])
  mK5h = exp(HPDI(post$muLK5, prob=0.9)[2])


  mH1m = mK1m/2 + mean(post$muR1)
  mH1l = mK1l/2 + HPDI(post$muR1, prob=0.9)[1]
  mH1h = mK1h/2 + HPDI(post$muR1, prob=0.9)[2]

  mH2m = mK2m/2 + mean(post$muR2)
  mH2l = mK2l/2 + HPDI(post$muR2, prob=0.9)[1]
  mH2h = mK2h/2 + HPDI(post$muR2, prob=0.9)[2]

  mH3m = mK3m/2 + mean(post$muR3)
  mH3l = mK3l/2 + HPDI(post$muR3, prob=0.9)[1]
  mH3h = mK3h/2 + HPDI(post$muR3, prob=0.9)[2]

  mH4m = mK4m/2 + mean(post$muR4)
  mH4l = mK4l/2 + HPDI(post$muR4, prob=0.9)[1]
  mH4h = mK4h/2 + HPDI(post$muR4, prob=0.9)[2]

  mH5m = mK5m/2 + mean(post$muR5)
  mH5l = mK5l/2 + HPDI(post$muR5, prob=0.9)[1]
  mH5h = mK5h/2 + HPDI(post$muR5, prob=0.9)[2]


  for ( i in 1:nrow(Ber.mal) ) {

    x <- Ber.mal$TotAge[i]

    lines(x = c( x, x ),
          y = c( Ber.mal$Height[i] - ( ( -1*exp(-1*mK1l*mQ1l*x/( 1 + 2*mQ1l )) + 2*mH1l/mK1l )^(1/mQ1l) +
                                       ( -1*exp(-1*mK2l*mQ2l*x/( 1 + 2*mQ2l )) + 2*mH2l/mK2l )^(1/mQ2l) +
                                       ( -1*exp(-1*mK3l*mQ3l*x/( 1 + 2*mQ3l )) + 2*mH3l/mK3l )^(1/mQ3l) +
                                       ( -1*exp(-1*mK4l*mQ4l*x/( 1 + 2*mQ4l )) + 2*mH4l/mK4l )^(1/mQ4l) +
                                       ( -1*exp(-1*mK5l*mQ5l*x/( 1 + 2*mQ5l )) + 2*mH5l/mK5l )^(1/mQ5l) ),

                 Ber.mal$Height[i] - ( ( -1*exp(-1*mK1h*mQ1h*x/( 1 + 2*mQ1h )) + 2*mH1h/mK1h )^(1/mQ1h) +
                                       ( -1*exp(-1*mK2h*mQ2h*x/( 1 + 2*mQ2h )) + 2*mH2h/mK2h )^(1/mQ2h) +
                                       ( -1*exp(-1*mK3h*mQ3h*x/( 1 + 2*mQ3h )) + 2*mH3h/mK3h )^(1/mQ3h) +
                                       ( -1*exp(-1*mK4h*mQ4h*x/( 1 + 2*mQ4h )) + 2*mH4h/mK4h )^(1/mQ4h) +
                                       ( -1*exp(-1*mK5h*mQ5h*x/( 1 + 2*mQ5h )) + 2*mH5h/mK5h )^(1/mQ5h) )
                 ),
          col=Comp5_col, lwd=Comp5_lwd, lty=Comp5_lty)

    points( x = x,
            y = Ber.mal$Height[i] - ( ( -1*exp(-1*mK1m*mQ1m*x/( 1 + 2*mQ1m )) + 2*mH1m/mK1m )^(1/mQ1m) +
                                      ( -1*exp(-1*mK2m*mQ2m*x/( 1 + 2*mQ2m )) + 2*mH2m/mK2m )^(1/mQ2m) +
                                      ( -1*exp(-1*mK3m*mQ3m*x/( 1 + 2*mQ3m )) + 2*mH3m/mK3m )^(1/mQ3m) +
                                      ( -1*exp(-1*mK4m*mQ4m*x/( 1 + 2*mQ4m )) + 2*mH4m/mK4m )^(1/mQ4m) +
                                      ( -1*exp(-1*mK5m*mQ5m*x/( 1 + 2*mQ5m )) + 2*mH5m/mK5m )^(1/mQ5m) ),
            col=Comp5_col, lwd=Point_lwd, pch=Point_pch, cex=Point_cex)

  } # for i

  text("Composite-5 Model", x=0, y=35, cex=1, adj=0)





  # plot JPA model

  #set up plot
  plot( x=0, y=10, type="n", ylim=c(-40,40), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE ) #(1=bottom, 2=left, 3=top, 4=right)
  axis( side=2, at=c(-30,0,30), labels=c(-30,0,30) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE )

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post5_JPA

  muAm = mean(post$muA)
  muAl = HPDI(post$muA, prob=0.9)[1]
  muAh = HPDI(post$muA, prob=0.9)[2]

  muD1m = mean(post$muD1)
  muD1l = HPDI(post$muD1, prob=0.9)[1]
  muD1h = HPDI(post$muD1, prob=0.9)[2]

  muD2m = mean(post$muD2)
  muD2l = HPDI(post$muD2, prob=0.9)[1]
  muD2h = HPDI(post$muD2, prob=0.9)[2]

  muD3m = mean(post$muD3)
  muD3l = HPDI(post$muD3, prob=0.9)[1]
  muD3h = HPDI(post$muD3, prob=0.9)[2]

  muC1m = mean(post$muC1)
  muC1l = HPDI(post$muC1, prob=0.9)[1]
  muC1h = HPDI(post$muC1, prob=0.9)[2]

  muC2m = mean(post$muC2)
  muC2l = HPDI(post$muC2, prob=0.9)[1]
  muC2h = HPDI(post$muC2, prob=0.9)[2]

  muC3m = mean(post$muC3)
  muC3l = HPDI(post$muC3, prob=0.9)[1]
  muC3h = HPDI(post$muC3, prob=0.9)[2]


  for ( i in 1:nrow(Ber.mal) ) {

    x <- Ber.mal$TotAge[i]

    lines(x = c( x, x ),
          y = c( Ber.mal$Height[i] - ( muAl*exp( -1/( muC1l*log( x/muD1l + 1 ) + (x/muD2l)^muC2l + (x/muD3l)^muC3l ) ) ),

                 Ber.mal$Height[i] - ( muAh*exp( -1/( muC1h*log( x/muD1h + 1 ) + (x/muD2h)^muC2h + (x/muD3h)^muC3h ) ) )
                ),
          col=JPA_col, lwd=JPA_lwd, lty=JPA_lty)

    points( x = x,
            y = Ber.mal$Height[i] - ( muAm*exp( -1/( muC1m*log( x/muD1m + 1 ) + (x/muD2m)^muC2m + (x/muD3m)^muC3m ) ) ),
            col=JPA_col, lwd=Point_lwd, pch=Point_pch, cex=Point_cex)

  } # for i

  text("JPA-1 Model", x=0, y=35, cex=1, adj=0)


  mtext("Age since conception (yrs)", side = 1, outer = T, cex = 0.8, line = 2, adj=0.5)
  mtext("Residual height (observed - predicted) (cm)", side = 2, outer = T, cex = 0.8, line = 2, adj=0.5)


graphics.off()







##################################################################



######################################### Fig A.24 #########################################



####### Residuals multilevel models


post3_mh <- readRDS("./analysis_height_mal/post3.RDS")
post3_fh <- readRDS("./analysis_height_fem/post3.RDS")
post3_mw <- readRDS("./analysis_weight_mal/post3.RDS")
post3_fw <- readRDS("./analysis_weight_fem/post3.RDS")


colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.5)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)

BerLine_lwd <- 0.5
BerLine_col <- colorlist["2.3"]

MatLine_lwd <- 0.5
MatLine_col <- colorlist["1.3"]

BerPoint_col <- colorlist["2.3"]
BerPoint_pch <- 1
BerPoint_lwd <- 0.5
BerPoint_cex <- 1

MatPoint_col <- colorlist["1.3"]
MatPoint_pch <- 1
MatPoint_lwd <- 0.5
MatPoint_cex <- 1



# residuals from estimated individual trajectories

pdf(file="./combined_plots/Plots/Residual_mult_indiv.pdf",
    height=7, width=5)
layout( mat=matrix(c(1,2,3,4,5,6,7,8),4,2,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
par(mar = c(1, 1, 1, 1), oma = c(5, 5, 1, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)



  # U.S. female height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-15,15), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-10,0,10), labels=c(-10,0,10) )
  axis( side=4, at=c(-10,0,10), labels=FALSE ) #right

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_fh


  for ( n in 1:nrow(Com.fem) ) {

    x <- Com.fem$TotAge[n]

    if (Com.fem$Ethnicity[n] == 1) {        #if U.S.

      Q1m = mean( post[[paste("Q1[",n,"]", sep="")]] )
      Q1l = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[1]
      Q1h = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[2]

      Q2m = mean( post[[paste("Q2[",n,"]", sep="")]] )
      Q2l = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[1]
      Q2h = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[2]

      Q3m = mean( post[[paste("Q3[",n,"]", sep="")]] )
      Q3l = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[1]
      Q3h = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[2]

      K1m = mean( post[[paste("K1[",n,"]", sep="")]] )
      K1l = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[1]
      K1h = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[2]

      K2m = mean( post[[paste("K2[",n,"]", sep="")]] )
      K2l = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[1]
      K2h = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[2]

      K3m = mean( post[[paste("K3[",n,"]", sep="")]] )
      K3l = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[1]
      K3h = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[2]

      H1m = mean( post[[paste("H1[",n,"]", sep="")]] )
      H1l = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[1]
      H1h = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[2]

      H2m = mean( post[[paste("H2[",n,"]", sep="")]] )
      H2l = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[1]
      H2h = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[2]

      H3m = mean( post[[paste("H3[",n,"]", sep="")]] )
      H3l = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[1]
      H3h = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[2]


      # lines(x = c( x, x ),
      #       y = c( Com.fem$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
      #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
      #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

      #              Com.fem$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
      #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
      #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
      #             ),
      #       col=BerLine_col, lwd=BerLine_lwd, lty=1)

      points( x = x,
              y = Com.fem$Height[n] - ( ( -1*exp(-1*K1m*Q1m*x/( 1 + 2*Q1m )) + 2*H1m/K1m )^(1/Q1m) +
                                        ( -1*exp(-1*K2m*Q2m*x/( 1 + 2*Q2m )) + 2*H2m/K2m )^(1/Q2m) +
                                        ( -1*exp(-1*K3m*Q3m*x/( 1 + 2*Q3m )) + 2*H3m/K3m )^(1/Q3m) ),
              col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

     } #if 


  } # for n

  text("U.S. height", x=0, y=13, cex=1, adj=0)
  symbol.Female(centerx = 10, centery = 14, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # U.S. male height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-15,15), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-10,0,10), labels=FALSE )


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_mh


  for ( n in 1:nrow(Com.mal) ) {

    x <- Com.mal$TotAge[n]

    if (Com.mal$Ethnicity[n] == 1) {        #if U.S.

      Q1m = mean( post[[paste("Q1[",n,"]", sep="")]] )
      Q1l = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[1]
      Q1h = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[2]

      Q2m = mean( post[[paste("Q2[",n,"]", sep="")]] )
      Q2l = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[1]
      Q2h = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[2]

      Q3m = mean( post[[paste("Q3[",n,"]", sep="")]] )
      Q3l = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[1]
      Q3h = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[2]

      K1m = mean( post[[paste("K1[",n,"]", sep="")]] )
      K1l = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[1]
      K1h = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[2]

      K2m = mean( post[[paste("K2[",n,"]", sep="")]] )
      K2l = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[1]
      K2h = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[2]

      K3m = mean( post[[paste("K3[",n,"]", sep="")]] )
      K3l = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[1]
      K3h = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[2]

      H1m = mean( post[[paste("H1[",n,"]", sep="")]] )
      H1l = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[1]
      H1h = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[2]

      H2m = mean( post[[paste("H2[",n,"]", sep="")]] )
      H2l = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[1]
      H2h = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[2]

      H3m = mean( post[[paste("H3[",n,"]", sep="")]] )
      H3l = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[1]
      H3h = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[2]


      # lines(x = c( x, x ),
      #       y = c( Com.mal$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
      #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
      #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

      #              Com.mal$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
      #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
      #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
      #             ),
      #       col=BerLine_col, lwd=BerLine_lwd, lty=1)

      points( x = x,
              y = Com.mal$Height[n] - ( ( -1*exp(-1*K1m*Q1m*x/( 1 + 2*Q1m )) + 2*H1m/K1m )^(1/Q1m) +
                                        ( -1*exp(-1*K2m*Q2m*x/( 1 + 2*Q2m )) + 2*H2m/K2m )^(1/Q2m) +
                                        ( -1*exp(-1*K3m*Q3m*x/( 1 + 2*Q3m )) + 2*H3m/K3m )^(1/Q3m) ),
              col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

     } # if


  } # for n

  text("U.S. height", x=0, y=13, cex=1, adj=0)
  symbol.Male(centerx = 10, centery = 13, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package



  # Matsigenka female height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-15,15), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-10,0,10), labels=c(-10,0,10) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-10,0,10), labels=FALSE ) #right


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_fh


  for ( n in 1:nrow(Com.fem) ) {

    x <- Com.fem$TotAge[n]

    if (Com.fem$Ethnicity[n] == 2) {        #if Machi

      Q1m = mean( post[[paste("Q1[",n,"]", sep="")]] )
      Q1l = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[1]
      Q1h = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[2]

      Q2m = mean( post[[paste("Q2[",n,"]", sep="")]] )
      Q2l = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[1]
      Q2h = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[2]

      Q3m = mean( post[[paste("Q3[",n,"]", sep="")]] )
      Q3l = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[1]
      Q3h = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[2]

      K1m = mean( post[[paste("K1[",n,"]", sep="")]] )
      K1l = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[1]
      K1h = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[2]

      K2m = mean( post[[paste("K2[",n,"]", sep="")]] )
      K2l = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[1]
      K2h = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[2]

      K3m = mean( post[[paste("K3[",n,"]", sep="")]] )
      K3l = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[1]
      K3h = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[2]

      H1m = mean( post[[paste("H1[",n,"]", sep="")]] )
      H1l = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[1]
      H1h = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[2]

      H2m = mean( post[[paste("H2[",n,"]", sep="")]] )
      H2l = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[1]
      H2h = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[2]

      H3m = mean( post[[paste("H3[",n,"]", sep="")]] )
      H3l = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[1]
      H3h = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[2]


    #   # lines(x = c( x, x ),
    #   #       y = c( Com.fem$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
    #   #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
    #   #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

    #   #              Com.fem$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
    #   #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
    #   #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
    #   #             ),
    #   #       col=MatLine_col, lwd=MatLine_lwd, lty=1)

      points( x = x,
              y = Com.fem$Height[n] - ( ( -1*exp(-1*K1m*Q1m*x/( 1 + 2*Q1m )) + 2*H1m/K1m )^(1/Q1m) +
                                        ( -1*exp(-1*K2m*Q2m*x/( 1 + 2*Q2m )) + 2*H2m/K2m )^(1/Q2m) +
                                        ( -1*exp(-1*K3m*Q3m*x/( 1 + 2*Q3m )) + 2*H3m/K3m )^(1/Q3m) ),
              col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

     } # if

  } # for n

  text("Matsigenka height", x=0, y=13, cex=1, adj=0)
  symbol.Female(centerx = 15, centery = 14, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # Matsigenka male height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-15,15), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-10,0,10), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_mh


  for ( n in 1:nrow(Com.mal) ) {

    x <- Com.mal$TotAge[n]

    if (Com.mal$Ethnicity[n] == 2) {        #if Machi

      Q1m = mean( post[[paste("Q1[",n,"]", sep="")]] )
      Q1l = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[1]
      Q1h = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[2]

      Q2m = mean( post[[paste("Q2[",n,"]", sep="")]] )
      Q2l = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[1]
      Q2h = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[2]

      Q3m = mean( post[[paste("Q3[",n,"]", sep="")]] )
      Q3l = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[1]
      Q3h = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[2]

      K1m = mean( post[[paste("K1[",n,"]", sep="")]] )
      K1l = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[1]
      K1h = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[2]

      K2m = mean( post[[paste("K2[",n,"]", sep="")]] )
      K2l = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[1]
      K2h = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[2]

      K3m = mean( post[[paste("K3[",n,"]", sep="")]] )
      K3l = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[1]
      K3h = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[2]

      H1m = mean( post[[paste("H1[",n,"]", sep="")]] )
      H1l = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[1]
      H1h = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[2]

      H2m = mean( post[[paste("H2[",n,"]", sep="")]] )
      H2l = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[1]
      H2h = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[2]

      H3m = mean( post[[paste("H3[",n,"]", sep="")]] )
      H3l = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[1]
      H3h = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[2]



    #   # lines(x = c( x, x ),
    #   #       y = c( Com.mal$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
    #   #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
    #   #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

    #   #              Com.mal$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
    #   #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
    #   #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
    #   #             ),
    #   #       col=MatLine_col, lwd=MatLine_lwd, lty=1)

      points( x = x,
              y = Com.mal$Height[n] - ( ( -1*exp(-1*K1m*Q1m*x/( 1 + 2*Q1m )) + 2*H1m/K1m )^(1/Q1m) +
                                        ( -1*exp(-1*K2m*Q2m*x/( 1 + 2*Q2m )) + 2*H2m/K2m )^(1/Q2m) +
                                        ( -1*exp(-1*K3m*Q3m*x/( 1 + 2*Q3m )) + 2*H3m/K3m )^(1/Q3m) ),
              col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

     } # if


  } # for n

  text("Matsigenka height", x=0, y=13, cex=1, adj=0)
  symbol.Male(centerx = 15, centery = 13, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package




  # U.S. female weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-15,15), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-10,0,10), labels=c(-10,0,10) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-10,0,10), labels=FALSE )

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_fw


  for ( n in 1:nrow(Com.fem) ) {

    x <- Com.fem$TotAge[n]

    if (Com.fem$Ethnicity[n] == 1) {        #if U.S.

      Q1m = mean( post[[paste("Q1[",n,"]", sep="")]] )
      Q1l = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[1]
      Q1h = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[2]

      Q2m = mean( post[[paste("Q2[",n,"]", sep="")]] )
      Q2l = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[1]
      Q2h = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[2]

      Q3m = mean( post[[paste("Q3[",n,"]", sep="")]] )
      Q3l = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[1]
      Q3h = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[2]

      K1m = mean( post[[paste("K1[",n,"]", sep="")]] )
      K1l = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[1]
      K1h = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[2]

      K2m = mean( post[[paste("K2[",n,"]", sep="")]] )
      K2l = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[1]
      K2h = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[2]

      K3m = mean( post[[paste("K3[",n,"]", sep="")]] )
      K3l = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[1]
      K3h = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[2]

      H1m = mean( post[[paste("H1[",n,"]", sep="")]] )
      H1l = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[1]
      H1h = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[2]

      H2m = mean( post[[paste("H2[",n,"]", sep="")]] )
      H2l = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[1]
      H2h = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[2]

      H3m = mean( post[[paste("H3[",n,"]", sep="")]] )
      H3l = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[1]
      H3h = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[2]



      # lines(x = c( x, x ),
      #       y = c( Com.fem$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
      #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
      #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

      #              Com.fem$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
      #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
      #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
      #             ),
      #       col=BerLine_col, lwd=BerLine_lwd, lty=1)

      points( x = x,
              y = Com.fem$Weight[n] - ( ( -1*exp(-1*mean(K1m)*mean(Q1m)*x/( 1 + 2*mean(Q1m) )) + 2*mean(H1m)/mean(K1m)*pi^(mean(Q1m)/( 1 + 2*mean(Q1m) )) )^(( 1 + 2*mean(Q1m) )/mean(Q1m)) +
                                        ( -1*exp(-1*mean(K2m)*mean(Q2m)*x/( 1 + 2*mean(Q2m) )) + 2*mean(H2m)/mean(K2m)*pi^(mean(Q2m)/( 1 + 2*mean(Q2m) )) )^(( 1 + 2*mean(Q2m) )/mean(Q2m)) +
                                        ( -1*exp(-1*mean(K3m)*mean(Q3m)*x/( 1 + 2*mean(Q3m) )) + 2*mean(H3m)/mean(K3m)*pi^(mean(Q3m)/( 1 + 2*mean(Q3m) )) )^(( 1 + 2*mean(Q3m) )/mean(Q3m)) ),
              col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

     } #if 


  } # for n

  text("U.S. weight", x=0, y=13, cex=1, adj=0)
  symbol.Female(centerx = 10, centery = 14, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # U.S. male weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-15,15), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-10,0,10), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_mw


  for ( n in 1:nrow(Com.mal) ) {

    x <- Com.mal$TotAge[n]

    if (Com.mal$Ethnicity[n] == 1) {        #if U.S.

      Q1m = mean( post[[paste("Q1[",n,"]", sep="")]] )
      Q1l = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[1]
      Q1h = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[2]

      Q2m = mean( post[[paste("Q2[",n,"]", sep="")]] )
      Q2l = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[1]
      Q2h = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[2]

      Q3m = mean( post[[paste("Q3[",n,"]", sep="")]] )
      Q3l = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[1]
      Q3h = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[2]

      K1m = mean( post[[paste("K1[",n,"]", sep="")]] )
      K1l = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[1]
      K1h = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[2]

      K2m = mean( post[[paste("K2[",n,"]", sep="")]] )
      K2l = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[1]
      K2h = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[2]

      K3m = mean( post[[paste("K3[",n,"]", sep="")]] )
      K3l = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[1]
      K3h = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[2]

      H1m = mean( post[[paste("H1[",n,"]", sep="")]] )
      H1l = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[1]
      H1h = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[2]

      H2m = mean( post[[paste("H2[",n,"]", sep="")]] )
      H2l = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[1]
      H2h = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[2]

      H3m = mean( post[[paste("H3[",n,"]", sep="")]] )
      H3l = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[1]
      H3h = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[2]



      # lines(x = c( x, x ),
      #       y = c( Com.mal$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
      #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
      #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

      #              Com.mal$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
      #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
      #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
      #             ),
      #       col=BerLine_col, lwd=BerLine_lwd, lty=1)

      points( x = x,
              y = Com.mal$Weight[n] - ( ( -1*exp(-1*mean(K1m)*mean(Q1m)*x/( 1 + 2*mean(Q1m) )) + 2*mean(H1m)/mean(K1m)*pi^(mean(Q1m)/( 1 + 2*mean(Q1m) )) )^(( 1 + 2*mean(Q1m) )/mean(Q1m)) +
                                        ( -1*exp(-1*mean(K2m)*mean(Q2m)*x/( 1 + 2*mean(Q2m) )) + 2*mean(H2m)/mean(K2m)*pi^(mean(Q2m)/( 1 + 2*mean(Q2m) )) )^(( 1 + 2*mean(Q2m) )/mean(Q2m)) +
                                        ( -1*exp(-1*mean(K3m)*mean(Q3m)*x/( 1 + 2*mean(Q3m) )) + 2*mean(H3m)/mean(K3m)*pi^(mean(Q3m)/( 1 + 2*mean(Q3m) )) )^(( 1 + 2*mean(Q3m) )/mean(Q3m)) ),
              col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

     } # if


  } # for n

  text("U.S. weight", x=0, y=13, cex=1, adj=0)
  symbol.Male(centerx = 10, centery = 13, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package



  # Matsigenka female weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-15,15), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE )
  axis( side=2, at=c(-10,0,10), labels=c(-10,0,10) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-10,0,10), labels=FALSE ) #right

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_fw


  for ( n in 1:nrow(Com.fem) ) {

    x <- Com.fem$TotAge[n]

    if (Com.fem$Ethnicity[n] == 2) {        #if Machi


      Q1m = mean( post[[paste("Q1[",n,"]", sep="")]] )
      Q1l = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[1]
      Q1h = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[2]

      Q2m = mean( post[[paste("Q2[",n,"]", sep="")]] )
      Q2l = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[1]
      Q2h = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[2]

      Q3m = mean( post[[paste("Q3[",n,"]", sep="")]] )
      Q3l = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[1]
      Q3h = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[2]

      K1m = mean( post[[paste("K1[",n,"]", sep="")]] )
      K1l = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[1]
      K1h = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[2]

      K2m = mean( post[[paste("K2[",n,"]", sep="")]] )
      K2l = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[1]
      K2h = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[2]

      K3m = mean( post[[paste("K3[",n,"]", sep="")]] )
      K3l = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[1]
      K3h = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[2]

      H1m = mean( post[[paste("H1[",n,"]", sep="")]] )
      H1l = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[1]
      H1h = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[2]

      H2m = mean( post[[paste("H2[",n,"]", sep="")]] )
      H2l = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[1]
      H2h = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[2]

      H3m = mean( post[[paste("H3[",n,"]", sep="")]] )
      H3l = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[1]
      H3h = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[2]



    #   # lines(x = c( x, x ),
    #   #       y = c( Com.fem$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
    #   #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
    #   #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

    #   #              Com.fem$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
    #   #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
    #   #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
    #   #             ),
    #   #       col=MatLine_col, lwd=MatLine_lwd, lty=1)

      points( x = x,
              y = Com.fem$Weight[n] - ( ( -1*exp(-1*mean(K1m)*mean(Q1m)*x/( 1 + 2*mean(Q1m) )) + 2*mean(H1m)/mean(K1m)*pi^(mean(Q1m)/( 1 + 2*mean(Q1m) )) )^(( 1 + 2*mean(Q1m) )/mean(Q1m)) +
                                        ( -1*exp(-1*mean(K2m)*mean(Q2m)*x/( 1 + 2*mean(Q2m) )) + 2*mean(H2m)/mean(K2m)*pi^(mean(Q2m)/( 1 + 2*mean(Q2m) )) )^(( 1 + 2*mean(Q2m) )/mean(Q2m)) +
                                        ( -1*exp(-1*mean(K3m)*mean(Q3m)*x/( 1 + 2*mean(Q3m) )) + 2*mean(H3m)/mean(K3m)*pi^(mean(Q3m)/( 1 + 2*mean(Q3m) )) )^(( 1 + 2*mean(Q3m) )/mean(Q3m)) ),
              col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

     } # if

  } # for n

  text("Matsigenka weight", x=0, y=13, cex=1, adj=0)
  symbol.Female(centerx = 15, centery = 14, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # Matsigenka male weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-15,15), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE )
  axis( side=2, at=c(-10,0,10), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_mw


  for ( n in 1:nrow(Com.mal) ) {

    x <- Com.mal$TotAge[n]

    if (Com.mal$Ethnicity[n] == 2) {        #if Machi

      Q1m = mean( post[[paste("Q1[",n,"]", sep="")]] )
      Q1l = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[1]
      Q1h = HPDI( post[[paste("Q1[",n,"]", sep="")]], prob=0.9)[2]

      Q2m = mean( post[[paste("Q2[",n,"]", sep="")]] )
      Q2l = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[1]
      Q2h = HPDI( post[[paste("Q2[",n,"]", sep="")]], prob=0.9)[2]

      Q3m = mean( post[[paste("Q3[",n,"]", sep="")]] )
      Q3l = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[1]
      Q3h = HPDI( post[[paste("Q3[",n,"]", sep="")]], prob=0.9)[2]

      K1m = mean( post[[paste("K1[",n,"]", sep="")]] )
      K1l = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[1]
      K1h = HPDI( post[[paste("K1[",n,"]", sep="")]], prob=0.9)[2]

      K2m = mean( post[[paste("K2[",n,"]", sep="")]] )
      K2l = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[1]
      K2h = HPDI( post[[paste("K2[",n,"]", sep="")]], prob=0.9)[2]

      K3m = mean( post[[paste("K3[",n,"]", sep="")]] )
      K3l = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[1]
      K3h = HPDI( post[[paste("K3[",n,"]", sep="")]], prob=0.9)[2]

      H1m = mean( post[[paste("H1[",n,"]", sep="")]] )
      H1l = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[1]
      H1h = HPDI( post[[paste("H1[",n,"]", sep="")]], prob=0.9)[2]

      H2m = mean( post[[paste("H2[",n,"]", sep="")]] )
      H2l = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[1]
      H2h = HPDI( post[[paste("H2[",n,"]", sep="")]], prob=0.9)[2]

      H3m = mean( post[[paste("H3[",n,"]", sep="")]] )
      H3l = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[1]
      H3h = HPDI( post[[paste("H3[",n,"]", sep="")]], prob=0.9)[2]


    #   # lines(x = c( x, x ),
    #   #       y = c( Com.mal$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
    #   #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
    #   #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

    #   #              Com.mal$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
    #   #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
    #   #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
    #   #             ),
    #   #       col=MatLine_col, lwd=MatLine_lwd, lty=1)

      points( x = x,
              y = Com.mal$Weight[n] - ( ( -1*exp(-1*mean(K1m)*mean(Q1m)*x/( 1 + 2*mean(Q1m) )) + 2*mean(H1m)/mean(K1m)*pi^(mean(Q1m)/( 1 + 2*mean(Q1m) )) )^(( 1 + 2*mean(Q1m) )/mean(Q1m)) +
                                        ( -1*exp(-1*mean(K2m)*mean(Q2m)*x/( 1 + 2*mean(Q2m) )) + 2*mean(H2m)/mean(K2m)*pi^(mean(Q2m)/( 1 + 2*mean(Q2m) )) )^(( 1 + 2*mean(Q2m) )/mean(Q2m)) +
                                        ( -1*exp(-1*mean(K3m)*mean(Q3m)*x/( 1 + 2*mean(Q3m) )) + 2*mean(H3m)/mean(K3m)*pi^(mean(Q3m)/( 1 + 2*mean(Q3m) )) )^(( 1 + 2*mean(Q3m) )/mean(Q3m)) ),
              col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

     } # else

  } # for n

  text("Matsigenka weight", x=0, y=13, cex=1, adj=0)
  symbol.Male(centerx = 15, centery = 13, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package


  mtext("Age since conception (yrs)", side = 1, outer = T, cex = 0.8, line = 2, adj=0.5)
  mtext("Residual height or weight (observed - predicted) (cm or kg)", side = 2, outer = T, cex = 0.8, line = 2, adj=0.5)


graphics.off()







##########################################################


######################################### Fig A.23 #########################################



# residuals from estimated mean trajectories

pdf(file="./combined_plots/Plots/Residual_mult_mean.pdf",
    height=7, width=5)
layout( mat=matrix(c(1,2,3,4,5,6,7,8),4,2,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
par(mar = c(1, 1, 1, 1), oma = c(5, 5, 1, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)



  # U.S. female height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-25,25), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-15,0,15), labels=c(-15,0,15) )
  axis( side=4, at=c(-15,0,15), labels=FALSE ) #right

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_fh


  for ( n in 1:nrow(Com.fem) ) {

    x <- Com.fem$TotAge[n]

    if (Com.fem$Ethnicity[n] == 1) {        #if U.S.

      Q1m = exp(mean(post$berLQ1))
      Q1l = exp(HPDI(post$berLQ1, prob=0.9)[1])
      Q1h = exp(HPDI(post$berLQ1, prob=0.9)[2])

      Q2m = exp(mean(post$berLQ2))
      Q2l = exp(HPDI(post$berLQ2, prob=0.9)[1])
      Q2h = exp(HPDI(post$berLQ2, prob=0.9)[2])

      Q3m = exp(mean(post$berLQ3))
      Q3l = exp(HPDI(post$berLQ3, prob=0.9)[1])
      Q3h = exp(HPDI(post$berLQ3, prob=0.9)[2])

      K1m = exp(mean(post$berLK1))
      K1l = exp(HPDI(post$berLK1, prob=0.9)[1])
      K1h = exp(HPDI(post$berLK1, prob=0.9)[2])

      K2m = exp(mean(post$berLK2))
      K2l = exp(HPDI(post$berLK2, prob=0.9)[1])
      K2h = exp(HPDI(post$berLK2, prob=0.9)[2])

      K3m = exp(mean(post$berLK3))
      K3l = exp(HPDI(post$berLK3, prob=0.9)[1])
      K3h = exp(HPDI(post$berLK3, prob=0.9)[2])

      H1m = K1m/2 + mean(post$berR1)
      H1l = K1l/2 + HPDI(post$berR1, prob=0.9)[1]
      H1h = K1h/2 + HPDI(post$berR1, prob=0.9)[2]

      H2m = K2m/2 + mean(post$berR2)
      H2l = K2l/2 + HPDI(post$berR2, prob=0.9)[1]
      H2h = K2h/2 + HPDI(post$berR2, prob=0.9)[2]

      H3m = K3m/2 + mean(post$berR3)
      H3l = K3l/2 + HPDI(post$berR3, prob=0.9)[1]
      H3h = K3h/2 + HPDI(post$berR3, prob=0.9)[2]


      # lines(x = c( x, x ),
      #       y = c( Com.fem$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
      #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
      #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

      #              Com.fem$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
      #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
      #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
      #             ),
      #       col=BerLine_col, lwd=BerLine_lwd, lty=1)

      points( x = x,
              y = Com.fem$Height[n] - ( ( -1*exp(-1*K1m*Q1m*x/( 1 + 2*Q1m )) + 2*H1m/K1m )^(1/Q1m) +
                                        ( -1*exp(-1*K2m*Q2m*x/( 1 + 2*Q2m )) + 2*H2m/K2m )^(1/Q2m) +
                                        ( -1*exp(-1*K3m*Q3m*x/( 1 + 2*Q3m )) + 2*H3m/K3m )^(1/Q3m) ),
              col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

     } #if 


  } # for n

  text("U.S. height", x=0, y=22, cex=1, adj=0)
  symbol.Female(centerx = 10, centery = 23, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # U.S. male height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-25,25), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-15,0,15), labels=FALSE )


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_mh


  for ( n in 1:nrow(Com.mal) ) {

    x <- Com.mal$TotAge[n]

    if (Com.mal$Ethnicity[n] == 1) {        #if U.S.


      Q1m = exp(mean(post$berLQ1))
      Q1l = exp(HPDI(post$berLQ1, prob=0.9)[1])
      Q1h = exp(HPDI(post$berLQ1, prob=0.9)[2])

      Q2m = exp(mean(post$berLQ2))
      Q2l = exp(HPDI(post$berLQ2, prob=0.9)[1])
      Q2h = exp(HPDI(post$berLQ2, prob=0.9)[2])

      Q3m = exp(mean(post$berLQ3))
      Q3l = exp(HPDI(post$berLQ3, prob=0.9)[1])
      Q3h = exp(HPDI(post$berLQ3, prob=0.9)[2])

      K1m = exp(mean(post$berLK1))
      K1l = exp(HPDI(post$berLK1, prob=0.9)[1])
      K1h = exp(HPDI(post$berLK1, prob=0.9)[2])

      K2m = exp(mean(post$berLK2))
      K2l = exp(HPDI(post$berLK2, prob=0.9)[1])
      K2h = exp(HPDI(post$berLK2, prob=0.9)[2])

      K3m = exp(mean(post$berLK3))
      K3l = exp(HPDI(post$berLK3, prob=0.9)[1])
      K3h = exp(HPDI(post$berLK3, prob=0.9)[2])

      H1m = K1m/2 + mean(post$berR1)
      H1l = K1l/2 + HPDI(post$berR1, prob=0.9)[1]
      H1h = K1h/2 + HPDI(post$berR1, prob=0.9)[2]

      H2m = K2m/2 + mean(post$berR2)
      H2l = K2l/2 + HPDI(post$berR2, prob=0.9)[1]
      H2h = K2h/2 + HPDI(post$berR2, prob=0.9)[2]

      H3m = K3m/2 + mean(post$berR3)
      H3l = K3l/2 + HPDI(post$berR3, prob=0.9)[1]
      H3h = K3h/2 + HPDI(post$berR3, prob=0.9)[2]


      # lines(x = c( x, x ),
      #       y = c( Com.mal$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
      #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
      #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

      #              Com.mal$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
      #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
      #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
      #             ),
      #       col=BerLine_col, lwd=BerLine_lwd, lty=1)

      points( x = x,
              y = Com.mal$Height[n] - ( ( -1*exp(-1*K1m*Q1m*x/( 1 + 2*Q1m )) + 2*H1m/K1m )^(1/Q1m) +
                                        ( -1*exp(-1*K2m*Q2m*x/( 1 + 2*Q2m )) + 2*H2m/K2m )^(1/Q2m) +
                                        ( -1*exp(-1*K3m*Q3m*x/( 1 + 2*Q3m )) + 2*H3m/K3m )^(1/Q3m) ),
              col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

     } # if


  } # for n

  text("U.S. height", x=0, y=22, cex=1, adj=0)
  symbol.Male(centerx = 10, centery = 22, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package



  # Matsigenka female height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-25,25), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-15,0,15), labels=c(-15,0,15) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-15,0,15), labels=FALSE ) #right


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_fh


  for ( n in 1:nrow(Com.fem) ) {

    x <- Com.fem$TotAge[n]

    if (Com.fem$Ethnicity[n] == 2) {        #if Machi

      Q1m = exp(mean(post$matLQ1))
      Q1l = exp(HPDI(post$matLQ1, prob=0.9)[1])
      Q1h = exp(HPDI(post$matLQ1, prob=0.9)[2])

      Q2m = exp(mean(post$matLQ2))
      Q2l = exp(HPDI(post$matLQ2, prob=0.9)[1])
      Q2h = exp(HPDI(post$matLQ2, prob=0.9)[2])

      Q3m = exp(mean(post$matLQ3))
      Q3l = exp(HPDI(post$matLQ3, prob=0.9)[1])
      Q3h = exp(HPDI(post$matLQ3, prob=0.9)[2])

      K1m = exp(mean(post$matLK1))
      K1l = exp(HPDI(post$matLK1, prob=0.9)[1])
      K1h = exp(HPDI(post$matLK1, prob=0.9)[2])

      K2m = exp(mean(post$matLK2))
      K2l = exp(HPDI(post$matLK2, prob=0.9)[1])
      K2h = exp(HPDI(post$matLK2, prob=0.9)[2])

      K3m = exp(mean(post$matLK3))
      K3l = exp(HPDI(post$matLK3, prob=0.9)[1])
      K3h = exp(HPDI(post$matLK3, prob=0.9)[2])

      H1m = K1m/2 + mean(post$matR1)
      H1l = K1l/2 + HPDI(post$matR1, prob=0.9)[1]
      H1h = K1h/2 + HPDI(post$matR1, prob=0.9)[2]

      H2m = K2m/2 + mean(post$matR2)
      H2l = K2l/2 + HPDI(post$matR2, prob=0.9)[1]
      H2h = K2h/2 + HPDI(post$matR2, prob=0.9)[2]

      H3m = K3m/2 + mean(post$matR3)
      H3l = K3l/2 + HPDI(post$matR3, prob=0.9)[1]
      H3h = K3h/2 + HPDI(post$matR3, prob=0.9)[2]


    #   # lines(x = c( x, x ),
    #   #       y = c( Com.fem$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
    #   #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
    #   #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

    #   #              Com.fem$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
    #   #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
    #   #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
    #   #             ),
    #   #       col=MatLine_col, lwd=MatLine_lwd, lty=1)

      points( x = x,
              y = Com.fem$Height[n] - ( ( -1*exp(-1*K1m*Q1m*x/( 1 + 2*Q1m )) + 2*H1m/K1m )^(1/Q1m) +
                                        ( -1*exp(-1*K2m*Q2m*x/( 1 + 2*Q2m )) + 2*H2m/K2m )^(1/Q2m) +
                                        ( -1*exp(-1*K3m*Q3m*x/( 1 + 2*Q3m )) + 2*H3m/K3m )^(1/Q3m) ),
              col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

     } # if

  } # for n

  text("Matsigenka height", x=0, y=22, cex=1, adj=0)
  symbol.Female(centerx = 15, centery = 23, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package




  # Matsigenka male height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-25,25), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-15,0,15), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_mh


  for ( n in 1:nrow(Com.mal) ) {

    x <- Com.mal$TotAge[n]

    if (Com.mal$Ethnicity[n] == 2) {        #if Machi

      Q1m = exp(mean(post$matLQ1))
      Q1l = exp(HPDI(post$matLQ1, prob=0.9)[1])
      Q1h = exp(HPDI(post$matLQ1, prob=0.9)[2])

      Q2m = exp(mean(post$matLQ2))
      Q2l = exp(HPDI(post$matLQ2, prob=0.9)[1])
      Q2h = exp(HPDI(post$matLQ2, prob=0.9)[2])

      Q3m = exp(mean(post$matLQ3))
      Q3l = exp(HPDI(post$matLQ3, prob=0.9)[1])
      Q3h = exp(HPDI(post$matLQ3, prob=0.9)[2])

      K1m = exp(mean(post$matLK1))
      K1l = exp(HPDI(post$matLK1, prob=0.9)[1])
      K1h = exp(HPDI(post$matLK1, prob=0.9)[2])

      K2m = exp(mean(post$matLK2))
      K2l = exp(HPDI(post$matLK2, prob=0.9)[1])
      K2h = exp(HPDI(post$matLK2, prob=0.9)[2])

      K3m = exp(mean(post$matLK3))
      K3l = exp(HPDI(post$matLK3, prob=0.9)[1])
      K3h = exp(HPDI(post$matLK3, prob=0.9)[2])

      H1m = K1m/2 + mean(post$matR1)
      H1l = K1l/2 + HPDI(post$matR1, prob=0.9)[1]
      H1h = K1h/2 + HPDI(post$matR1, prob=0.9)[2]

      H2m = K2m/2 + mean(post$matR2)
      H2l = K2l/2 + HPDI(post$matR2, prob=0.9)[1]
      H2h = K2h/2 + HPDI(post$matR2, prob=0.9)[2]

      H3m = K3m/2 + mean(post$matR3)
      H3l = K3l/2 + HPDI(post$matR3, prob=0.9)[1]
      H3h = K3h/2 + HPDI(post$matR3, prob=0.9)[2]



    #   # lines(x = c( x, x ),
    #   #       y = c( Com.mal$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
    #   #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
    #   #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

    #   #              Com.mal$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
    #   #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
    #   #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
    #   #             ),
    #   #       col=MatLine_col, lwd=MatLine_lwd, lty=1)

      points( x = x,
              y = Com.mal$Height[n] - ( ( -1*exp(-1*K1m*Q1m*x/( 1 + 2*Q1m )) + 2*H1m/K1m )^(1/Q1m) +
                                        ( -1*exp(-1*K2m*Q2m*x/( 1 + 2*Q2m )) + 2*H2m/K2m )^(1/Q2m) +
                                        ( -1*exp(-1*K3m*Q3m*x/( 1 + 2*Q3m )) + 2*H3m/K3m )^(1/Q3m) ),
              col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

     } # if


  } # for n

  text("Matsigenka height", x=0, y=22, cex=1, adj=0)
  symbol.Male(centerx = 15, centery = 22, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package




  # U.S. female weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-50,50), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-25,0,25), labels=c(-25,0,25) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-25,0,25), labels=FALSE )

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_fw


  for ( n in 1:nrow(Com.fem) ) {

    x <- Com.fem$TotAge[n]

    if (Com.fem$Ethnicity[n] == 1) {        #if U.S.

      Q1m = exp(mean(post$berLQ1))
      Q1l = exp(HPDI(post$berLQ1, prob=0.9)[1])
      Q1h = exp(HPDI(post$berLQ1, prob=0.9)[2])

      Q2m = exp(mean(post$berLQ2))
      Q2l = exp(HPDI(post$berLQ2, prob=0.9)[1])
      Q2h = exp(HPDI(post$berLQ2, prob=0.9)[2])

      Q3m = exp(mean(post$berLQ3))
      Q3l = exp(HPDI(post$berLQ3, prob=0.9)[1])
      Q3h = exp(HPDI(post$berLQ3, prob=0.9)[2])

      K1m = exp(mean(post$berLK1))
      K1l = exp(HPDI(post$berLK1, prob=0.9)[1])
      K1h = exp(HPDI(post$berLK1, prob=0.9)[2])

      K2m = exp(mean(post$berLK2))
      K2l = exp(HPDI(post$berLK2, prob=0.9)[1])
      K2h = exp(HPDI(post$berLK2, prob=0.9)[2])

      K3m = exp(mean(post$berLK3))
      K3l = exp(HPDI(post$berLK3, prob=0.9)[1])
      K3h = exp(HPDI(post$berLK3, prob=0.9)[2])

      H1m = K1m/2 + mean(post$berR1)
      H1l = K1l/2 + HPDI(post$berR1, prob=0.9)[1]
      H1h = K1h/2 + HPDI(post$berR1, prob=0.9)[2]

      H2m = K2m/2 + mean(post$berR2)
      H2l = K2l/2 + HPDI(post$berR2, prob=0.9)[1]
      H2h = K2h/2 + HPDI(post$berR2, prob=0.9)[2]

      H3m = K3m/2 + mean(post$berR3)
      H3l = K3l/2 + HPDI(post$berR3, prob=0.9)[1]
      H3h = K3h/2 + HPDI(post$berR3, prob=0.9)[2]



      # lines(x = c( x, x ),
      #       y = c( Com.fem$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
      #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
      #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

      #              Com.fem$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
      #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
      #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
      #             ),
      #       col=BerLine_col, lwd=BerLine_lwd, lty=1)

      points( x = x,
              y = Com.fem$Weight[n] - ( ( -1*exp(-1*mean(K1m)*mean(Q1m)*x/( 1 + 2*mean(Q1m) )) + 2*mean(H1m)/mean(K1m)*pi^(mean(Q1m)/( 1 + 2*mean(Q1m) )) )^(( 1 + 2*mean(Q1m) )/mean(Q1m)) +
                                        ( -1*exp(-1*mean(K2m)*mean(Q2m)*x/( 1 + 2*mean(Q2m) )) + 2*mean(H2m)/mean(K2m)*pi^(mean(Q2m)/( 1 + 2*mean(Q2m) )) )^(( 1 + 2*mean(Q2m) )/mean(Q2m)) +
                                        ( -1*exp(-1*mean(K3m)*mean(Q3m)*x/( 1 + 2*mean(Q3m) )) + 2*mean(H3m)/mean(K3m)*pi^(mean(Q3m)/( 1 + 2*mean(Q3m) )) )^(( 1 + 2*mean(Q3m) )/mean(Q3m)) ),
              col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

     } #if 


  } # for n

  text("U.S. weight", x=0, y=45, cex=1, adj=0)
  symbol.Female(centerx = 10, centery = 47, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # U.S. male weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-50,50), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-25,0,25), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_mw


  for ( n in 1:nrow(Com.mal) ) {

    x <- Com.mal$TotAge[n]

    if (Com.mal$Ethnicity[n] == 1) {        #if U.S.

      Q1m = exp(mean(post$berLQ1))
      Q1l = exp(HPDI(post$berLQ1, prob=0.9)[1])
      Q1h = exp(HPDI(post$berLQ1, prob=0.9)[2])

      Q2m = exp(mean(post$berLQ2))
      Q2l = exp(HPDI(post$berLQ2, prob=0.9)[1])
      Q2h = exp(HPDI(post$berLQ2, prob=0.9)[2])

      Q3m = exp(mean(post$berLQ3))
      Q3l = exp(HPDI(post$berLQ3, prob=0.9)[1])
      Q3h = exp(HPDI(post$berLQ3, prob=0.9)[2])

      K1m = exp(mean(post$berLK1))
      K1l = exp(HPDI(post$berLK1, prob=0.9)[1])
      K1h = exp(HPDI(post$berLK1, prob=0.9)[2])

      K2m = exp(mean(post$berLK2))
      K2l = exp(HPDI(post$berLK2, prob=0.9)[1])
      K2h = exp(HPDI(post$berLK2, prob=0.9)[2])

      K3m = exp(mean(post$berLK3))
      K3l = exp(HPDI(post$berLK3, prob=0.9)[1])
      K3h = exp(HPDI(post$berLK3, prob=0.9)[2])

      H1m = K1m/2 + mean(post$berR1)
      H1l = K1l/2 + HPDI(post$berR1, prob=0.9)[1]
      H1h = K1h/2 + HPDI(post$berR1, prob=0.9)[2]

      H2m = K2m/2 + mean(post$berR2)
      H2l = K2l/2 + HPDI(post$berR2, prob=0.9)[1]
      H2h = K2h/2 + HPDI(post$berR2, prob=0.9)[2]

      H3m = K3m/2 + mean(post$berR3)
      H3l = K3l/2 + HPDI(post$berR3, prob=0.9)[1]
      H3h = K3h/2 + HPDI(post$berR3, prob=0.9)[2]



      # lines(x = c( x, x ),
      #       y = c( Com.mal$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
      #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
      #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

      #              Com.mal$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
      #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
      #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
      #             ),
      #       col=BerLine_col, lwd=BerLine_lwd, lty=1)

      points( x = x,
              y = Com.mal$Weight[n] - ( ( -1*exp(-1*mean(K1m)*mean(Q1m)*x/( 1 + 2*mean(Q1m) )) + 2*mean(H1m)/mean(K1m)*pi^(mean(Q1m)/( 1 + 2*mean(Q1m) )) )^(( 1 + 2*mean(Q1m) )/mean(Q1m)) +
                                        ( -1*exp(-1*mean(K2m)*mean(Q2m)*x/( 1 + 2*mean(Q2m) )) + 2*mean(H2m)/mean(K2m)*pi^(mean(Q2m)/( 1 + 2*mean(Q2m) )) )^(( 1 + 2*mean(Q2m) )/mean(Q2m)) +
                                        ( -1*exp(-1*mean(K3m)*mean(Q3m)*x/( 1 + 2*mean(Q3m) )) + 2*mean(H3m)/mean(K3m)*pi^(mean(Q3m)/( 1 + 2*mean(Q3m) )) )^(( 1 + 2*mean(Q3m) )/mean(Q3m)) ),
              col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

     } # if


  } # for n

  text("U.S. weight", x=0, y=45, cex=1, adj=0)
  symbol.Male(centerx = 10, centery = 44, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package



  # Matsigenka female weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-50,50), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE )
  axis( side=2, at=c(-25,0,25), labels=c(-25,0,25) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-25,0,25), labels=FALSE ) #right

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_fw


  for ( n in 1:nrow(Com.fem) ) {

    x <- Com.fem$TotAge[n]

    if (Com.fem$Ethnicity[n] == 2) {        #if Machi


      Q1m = exp(mean(post$matLQ1))
      Q1l = exp(HPDI(post$matLQ1, prob=0.9)[1])
      Q1h = exp(HPDI(post$matLQ1, prob=0.9)[2])

      Q2m = exp(mean(post$matLQ2))
      Q2l = exp(HPDI(post$matLQ2, prob=0.9)[1])
      Q2h = exp(HPDI(post$matLQ2, prob=0.9)[2])

      Q3m = exp(mean(post$matLQ3))
      Q3l = exp(HPDI(post$matLQ3, prob=0.9)[1])
      Q3h = exp(HPDI(post$matLQ3, prob=0.9)[2])

      K1m = exp(mean(post$matLK1))
      K1l = exp(HPDI(post$matLK1, prob=0.9)[1])
      K1h = exp(HPDI(post$matLK1, prob=0.9)[2])

      K2m = exp(mean(post$matLK2))
      K2l = exp(HPDI(post$matLK2, prob=0.9)[1])
      K2h = exp(HPDI(post$matLK2, prob=0.9)[2])

      K3m = exp(mean(post$matLK3))
      K3l = exp(HPDI(post$matLK3, prob=0.9)[1])
      K3h = exp(HPDI(post$matLK3, prob=0.9)[2])

      H1m = K1m/2 + mean(post$matR1)
      H1l = K1l/2 + HPDI(post$matR1, prob=0.9)[1]
      H1h = K1h/2 + HPDI(post$matR1, prob=0.9)[2]

      H2m = K2m/2 + mean(post$matR2)
      H2l = K2l/2 + HPDI(post$matR2, prob=0.9)[1]
      H2h = K2h/2 + HPDI(post$matR2, prob=0.9)[2]

      H3m = K3m/2 + mean(post$matR3)
      H3l = K3l/2 + HPDI(post$matR3, prob=0.9)[1]
      H3h = K3h/2 + HPDI(post$matR3, prob=0.9)[2]



    #   # lines(x = c( x, x ),
    #   #       y = c( Com.fem$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
    #   #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
    #   #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

    #   #              Com.fem$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
    #   #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
    #   #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
    #   #             ),
    #   #       col=MatLine_col, lwd=MatLine_lwd, lty=1)

      points( x = x,
              y = Com.fem$Weight[n] - ( ( -1*exp(-1*mean(K1m)*mean(Q1m)*x/( 1 + 2*mean(Q1m) )) + 2*mean(H1m)/mean(K1m)*pi^(mean(Q1m)/( 1 + 2*mean(Q1m) )) )^(( 1 + 2*mean(Q1m) )/mean(Q1m)) +
                                        ( -1*exp(-1*mean(K2m)*mean(Q2m)*x/( 1 + 2*mean(Q2m) )) + 2*mean(H2m)/mean(K2m)*pi^(mean(Q2m)/( 1 + 2*mean(Q2m) )) )^(( 1 + 2*mean(Q2m) )/mean(Q2m)) +
                                        ( -1*exp(-1*mean(K3m)*mean(Q3m)*x/( 1 + 2*mean(Q3m) )) + 2*mean(H3m)/mean(K3m)*pi^(mean(Q3m)/( 1 + 2*mean(Q3m) )) )^(( 1 + 2*mean(Q3m) )/mean(Q3m)) ),
              col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

     } # if

  } # for n

  text("Matsigenka weight", x=0, y=45, cex=1, adj=0)
  symbol.Female(centerx = 15, centery = 47, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # Matsigenka male weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-50,50), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE )
  axis( side=2, at=c(-25,0,25), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post3_mw


  for ( n in 1:nrow(Com.mal) ) {

    x <- Com.mal$TotAge[n]

    if (Com.mal$Ethnicity[n] == 2) {        #if Machi

      Q1m = exp(mean(post$matLQ1))
      Q1l = exp(HPDI(post$matLQ1, prob=0.9)[1])
      Q1h = exp(HPDI(post$matLQ1, prob=0.9)[2])

      Q2m = exp(mean(post$matLQ2))
      Q2l = exp(HPDI(post$matLQ2, prob=0.9)[1])
      Q2h = exp(HPDI(post$matLQ2, prob=0.9)[2])

      Q3m = exp(mean(post$matLQ3))
      Q3l = exp(HPDI(post$matLQ3, prob=0.9)[1])
      Q3h = exp(HPDI(post$matLQ3, prob=0.9)[2])

      K1m = exp(mean(post$matLK1))
      K1l = exp(HPDI(post$matLK1, prob=0.9)[1])
      K1h = exp(HPDI(post$matLK1, prob=0.9)[2])

      K2m = exp(mean(post$matLK2))
      K2l = exp(HPDI(post$matLK2, prob=0.9)[1])
      K2h = exp(HPDI(post$matLK2, prob=0.9)[2])

      K3m = exp(mean(post$matLK3))
      K3l = exp(HPDI(post$matLK3, prob=0.9)[1])
      K3h = exp(HPDI(post$matLK3, prob=0.9)[2])

      H1m = K1m/2 + mean(post$matR1)
      H1l = K1l/2 + HPDI(post$matR1, prob=0.9)[1]
      H1h = K1h/2 + HPDI(post$matR1, prob=0.9)[2]

      H2m = K2m/2 + mean(post$matR2)
      H2l = K2l/2 + HPDI(post$matR2, prob=0.9)[1]
      H2h = K2h/2 + HPDI(post$matR2, prob=0.9)[2]

      H3m = K3m/2 + mean(post$matR3)
      H3l = K3l/2 + HPDI(post$matR3, prob=0.9)[1]
      H3h = K3h/2 + HPDI(post$matR3, prob=0.9)[2]

    #   # lines(x = c( x, x ),
    #   #       y = c( Com.mal$Height[n] - ( ( -1*exp(-1*K1l*Q1l*x/( 1 + 2*Q1l )) + 2*H1l/K1l )^(1/Q1l) +
    #   #                                    ( -1*exp(-1*K2l*Q2l*x/( 1 + 2*Q2l )) + 2*H2l/K2l )^(1/Q2l) +
    #   #                                    ( -1*exp(-1*K3l*Q3l*x/( 1 + 2*Q3l )) + 2*H3l/K3l )^(1/Q3l) ),

    #   #              Com.mal$Height[n] - ( ( -1*exp(-1*K1h*Q1h*x/( 1 + 2*Q1h )) + 2*H1h/K1h )^(1/Q1h) +
    #   #                                    ( -1*exp(-1*K2h*Q2h*x/( 1 + 2*Q2h )) + 2*H2h/K2h )^(1/Q2h) +
    #   #                                    ( -1*exp(-1*K3h*Q3h*x/( 1 + 2*Q3h )) + 2*H3h/K3h )^(1/Q3h) )
    #   #             ),
    #   #       col=MatLine_col, lwd=MatLine_lwd, lty=1)

      points( x = x,
              y = Com.mal$Weight[n] - ( ( -1*exp(-1*mean(K1m)*mean(Q1m)*x/( 1 + 2*mean(Q1m) )) + 2*mean(H1m)/mean(K1m)*pi^(mean(Q1m)/( 1 + 2*mean(Q1m) )) )^(( 1 + 2*mean(Q1m) )/mean(Q1m)) +
                                        ( -1*exp(-1*mean(K2m)*mean(Q2m)*x/( 1 + 2*mean(Q2m) )) + 2*mean(H2m)/mean(K2m)*pi^(mean(Q2m)/( 1 + 2*mean(Q2m) )) )^(( 1 + 2*mean(Q2m) )/mean(Q2m)) +
                                        ( -1*exp(-1*mean(K3m)*mean(Q3m)*x/( 1 + 2*mean(Q3m) )) + 2*mean(H3m)/mean(K3m)*pi^(mean(Q3m)/( 1 + 2*mean(Q3m) )) )^(( 1 + 2*mean(Q3m) )/mean(Q3m)) ),
              col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

     } # else

  } # for n

  text("Matsigenka weight", x=0, y=45, cex=1, adj=0)
  symbol.Male(centerx = 15, centery = 44, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package


  mtext("Age since conception (yrs)", side = 1, outer = T, cex = 0.8, line = 2, adj=0.5)
  mtext("Residual height or weight (observed - predicted) (cm or kg)", side = 2, outer = T, cex = 0.8, line = 2, adj=0.5)


graphics.off()





