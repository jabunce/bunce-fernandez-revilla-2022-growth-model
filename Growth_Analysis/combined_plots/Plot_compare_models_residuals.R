

######################################### Fig 2 #########################################


######## load posteriors from models, need to run PrepareData to plot raw data points

# Ber male height

# Composite model 5, height and weight
post6_comp <- readRDS("./analysis_hw_mal/post6.RDS")
#str(post6_comp)

# # Composite model 5, just height
# post6_comph <- readRDS("./analysis_height_mal/post6.RDS")
# #str(post6_comph)

# JPA1 model
post8_JPA <- readRDS("./analysis_height_mal_JPA1/post8.RDS")
#str(post8_JPA)

# SITAR model
sitar_model <- readRDS("./analysis_height_mal_SITAR/sitar_model.RDS")
sitar_mat_mean <- readRDS("./analysis_height_mal_SITAR/sitar_mat_mean.RDS")
sitar_ber_mean <- readRDS("./analysis_height_mal_SITAR/sitar_ber_mean.RDS")
sitar_mat_vmean <- readRDS("./analysis_height_mal_SITAR/sitar_mat_vmean.RDS")
sitar_ber_vmean <- readRDS("./analysis_height_mal_SITAR/sitar_ber_vmean.RDS")


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

# Comph_lwd <- 2
# Comph_lty <- 1
# Comph_col <- colorlist["pink"]

JPA_lwd <- 2
JPA_lty <- 1
JPA_col <- colorlist["blue"]

SITAR_lwd <- 2
SITAR_lty <- 1
SITAR_col <- colorlist["red"]




pdf(file="./combined_plots/Plots/Model_compare.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,25), xlab="Age since conception (years)", ylab="Height (cm)" )


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=Point_lwd, col=Point_col, pch=Point_pch, cex=Point_cex)



  # Plot composite-5 model, height and weight
  post <- post6_comp

  mQ <- rep(0, times=5) 
  mK <- rep(0, times=5) 
  mH <- rep(0, times=5) 
  mI <- rep(0, times=5) 

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) # 1 for Berkeley
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
        col=Comp_col, lwd=Comp_lwd, lty=Comp_lty)





  # plot JPA model 
  post <- post8_JPA

  berA = mean(post$berA)
  berC1 = mean(post$berC1)
  berC2 = mean(post$berC2)
  berC3 = mean(post$berC3)
  berD1 = mean(post$berD1)
  berD2 = mean(post$berD2)
  berD3 = mean(post$berD3)

  lines(x = x,
        y = berA*exp( -1/( berC1*log( x/berD1 + 1 ) + (x/berD2)^berC2 + (x/berD3)^berC3 ) ),
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


  # Plot composite-5 velocity, height and weight
  post <- post6_comp

  for ( z in 1:5 ){
    mQ[z] <- mean( pull(post, paste("mQ[", 1, ",", z, "]", sep="")) ) # 1 for Berkeley
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
  lines(x = x, y=y_primeBer, col=Comp_col, lwd=Comp_lwd, lty=Comp_lty)



  # plot JPA model 
  post <- post8_JPA

  berA = mean(post$berA)
  berC1 = mean(post$berC1)
  berC2 = mean(post$berC2)
  berC3 = mean(post$berC3)
  berD1 = mean(post$berD1)
  berD2 = mean(post$berD2)
  berD3 = mean(post$berD3)

  y_primeBer <- berA*exp( -1/( berC1*log( x/berD1 + 1 ) + (x/berD2)^berC2 + (x/berD3)^berC3 ) )*
                ( berC1*x + ( berD1 + x )*( berC2*(x/berD2)^berC2 + berC3*(x/berD3)^berC3 ) )*
                1/( x*( berD1 + x )*( (x/berD2)^berC2 + (x/berD3)^berC3 + berC1*log( 1 + x/berD1 ) )^2 )
  lines(x = x,
        y = y_primeBer,
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
                 "Composite model",
                 #"Composite model, just height",
                 "JPA-1 model",
                 "SITAR model"
                 ), bty="o", bg="white",
         col=c(Point_col,
               Comp_col,
               #Comph_col,
               JPA_col,
               SITAR_col),
         merge=FALSE,
         pch=c(Point_pch,
               NA,
               #NA,
               NA,
               NA),
         lty=c(0,
               Comp_lty,
               #Comph_lty,
               JPA_lty,
               SITAR_lty),
         lwd=c(Point_lwd,
               Comp_lwd,
               #Comph_lwd,
               JPA_lwd,
               SITAR_lwd),
         cex=0.6, seg.len=3)


graphics.off()




#################################################################





##############################################################################



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

JPA_lwd <- 0.75
JPA_lty <- 1
JPA_col <- colorlist["blue"]

SITAR_lwd <- 0.75
SITAR_lty <- 1
SITAR_col <- colorlist["red"]





pdf(file="./combined_plots/Plots/Residual_compare.pdf",
    height=5, width=5)
layout( mat=matrix(c(1,2,3),3,1,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
par(mar = c(1, 1, 1, 1), oma = c(5, 5, 1, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  ########################## plot composite model

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-25,25), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-20,0,20), labels=c(-20,0,20) )


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")

  post <- post6_comp
  samps <- length(post$lp__) #number of samples

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )
  meanHeight <- matrix(data=NA, nrow=length(Ber.mal$TotAge), ncol=samps )
  meanHeightL <- rep( NA, times=length(Ber.mal$TotAge) )
  meanHeightH <- rep( NA, times=length(Ber.mal$TotAge) )


  for ( z in 1:5 ){
    mQ[z,] <- pull(post, paste("mQ[", 1, ",", z, "]", sep="")) 
    mK[z,] <- pull(post, paste("mK[", 1, ",", z, "]", sep=""))
    mH[z,] <- pull(post, paste("mH[", 1, ",", z, "]", sep=""))
    mI[z,] <- pull(post, paste("mI[", 1, ",", z, "]", sep=""))

  } # for z


  for ( i in 1:length(Ber.mal$TotAge) ) {

    x <- Ber.mal$TotAge[i]

    meanHeight[i,] <- ifelse( x < mI[1,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]),

                      ifelse( x < mI[2,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]),

                      ifelse( x < mI[3,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]),

                      ifelse( x < mI[4,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]),

                                          0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]) +
                                                  ( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,])
                      ) ) ) )

    meanHeightL[i] <- HPDI(meanHeight[i,], prob=0.9 )[1]
    meanHeightH[i] <- HPDI(meanHeight[i,], prob=0.9 )[2]

    lines(x = c( x, x ),
          y = c( Ber.mal$Height[i] - meanHeightL[i],
                 Ber.mal$Height[i] - meanHeightH[i] ),
          col=Comp_col, lwd=Comp_lwd, lty=Comp_lty)

    points( x = x,
            y = Ber.mal$Height[i] - mean(meanHeight[i,]),
            col=Comp_col, lwd=Point_lwd, pch=Point_pch, cex=Point_cex )


  } # for i

  text("Composite model", x=0, y=20, cex=1, adj=0)




  ############################# plot JPA model

  #set up plot
  plot( x=0, y=10, type="n", ylim=c(-25,25), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE ) #(1=bottom, 2=left, 3=top, 4=right)
  axis( side=2, at=c(-20,0,20), labels=c(-20,0,20) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE )

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  post <- post8_JPA

  samps <- length(post$lp__) #number of samples

  mA = post$berA
  mC1 = post$berC1
  mC2 = post$berC2
  mC3 = post$berC3
  mD1 = post$berD1
  mD2 = post$berD2
  mD3 = post$berD3

  meanHeight <- matrix(data=NA, nrow=length(Ber.mal$TotAge), ncol=samps )
  meanHeightL <- rep( NA, times=length(Ber.mal$TotAge) )
  meanHeightH <- rep( NA, times=length(Ber.mal$TotAge) )


  for ( i in 1:length(Ber.mal$TotAge) ) {

    x <- Ber.mal$TotAge[i]

    meanHeight[i,] <- mA*exp( -1/( mC1*log( x/mD1 + 1 ) + (x/mD2)^mC2 + (x/mD3)^mC3 ) )

    meanHeightL[i] <- HPDI(meanHeight[i,], prob=0.9 )[1]
    meanHeightH[i] <- HPDI(meanHeight[i,], prob=0.9 )[2]

    lines(x = c( x, x ),
          y = c( Ber.mal$Height[i] - meanHeightL[i],
                 Ber.mal$Height[i] - meanHeightH[i] ),
          col=JPA_col, lwd=JPA_lwd, lty=JPA_lty)

    points( x = x,
            y = Ber.mal$Height[i] - mean(meanHeight[i,]),
            col=JPA_col, lwd=Point_lwd, pch=Point_pch, cex=Point_cex )


  } # for i

  text("JPA-1 Model", x=0, y=20, cex=1, adj=0)



  ############################ Plot SITAR model

  # # For SITAR: dataset without height set to 0 at conception, duplicating Berkeley data until age 25


  #set up plot
  plot( x=0, y=10, type="n", ylim=c(-25,25), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE ) #(1=bottom, 2=left, 3=top, 4=right)
  axis( side=2, at=c(-20,0,20), labels=c(-20,0,20) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE )

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")



  for ( i in 1:length(Ber.mal$TotAge) ) {

    x <- Ber.mal$TotAge[i]

    mean_height <- predict(sitar_model,
                           newdata=data.frame(TotAge=x, eth=0),
                           level=0
                           )

    points( x = x,
            y = Ber.mal$Height[i] - mean_height,
            col=SITAR_col, lwd=Point_lwd, pch=Point_pch, cex=Point_cex )

  } # for i



  text("SITAR Model", x=0, y=20, cex=1, adj=0)

  mtext("Age since conception (years)", side = 1, outer = T, cex = 0.8, line = 2, adj=0.5)
  mtext("Residual height (observed - predicted) (cm)", side = 2, outer = T, cex = 0.8, line = 2, adj=0.5)


graphics.off()







##################################################################



###############################################################################



####### Residuals multilevel models


post6_m <- readRDS("./analysis_hw_mal/post6.RDS")
post6_f <- readRDS("./analysis_hw_fem/post6.RDS")


colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.5)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)

BerLine_lwd <- 0.5
BerLine_lty <- 1
BerLine_col <- colorlist["2.3"]

MatLine_lwd <- 0.5
MatLine_lty <- 1
MatLine_col <- colorlist["1.3"]

BerPoint_col <- colorlist["2.3"]
BerPoint_pch <- 1
BerPoint_lwd <- 0.5
BerPoint_cex <- 1

MatPoint_col <- colorlist["1.3"]
MatPoint_pch <- 1
MatPoint_lwd <- 0.5
MatPoint_cex <- 1



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
  axis( side=2, at=c(-10,0,10), labels=c(-10,0,10) )
  axis( side=4, at=c(-10,0,10), labels=FALSE ) #right

  post <- post6_f

  samps <- length(post$lp__) #number of samples

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )
  meanHeight <- matrix(data=NA, nrow=length(Ber.fem$TotAge), ncol=samps )
  meanHeightL <- rep( NA, times=length(Ber.fem$TotAge) )
  meanHeightH <- rep( NA, times=length(Ber.fem$TotAge) )


  for ( z in 1:5 ){
    mQ[z,] <- pull(post, paste("mQ[", 1, ",", z, "]", sep="")) 
    mK[z,] <- pull(post, paste("mK[", 1, ",", z, "]", sep=""))
    mH[z,] <- pull(post, paste("mH[", 1, ",", z, "]", sep=""))
    mI[z,] <- pull(post, paste("mI[", 1, ",", z, "]", sep=""))

  } # for z


  for ( i in 1:length(Ber.fem$TotAge) ) {

    x <- Ber.fem$TotAge[i]

    meanHeight[i,] <- ifelse( x < mI[1,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]),

                      ifelse( x < mI[2,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]),

                      ifelse( x < mI[3,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]),

                      ifelse( x < mI[4,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]),

                                          0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]) +
                                                  ( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,])
                      ) ) ) )

    meanHeightL[i] <- HPDI(meanHeight[i,], prob=0.9 )[1]
    meanHeightH[i] <- HPDI(meanHeight[i,], prob=0.9 )[2]


    lines(x = c( x, x ),
          y = c( Ber.fem$Height[i] - meanHeightL[i],
                 Ber.fem$Height[i] - meanHeightH[i] ),
          col=BerLine_col, lwd=BerLine_lwd, lty=BerLine_lty)

    points( x = x,
            y = Ber.fem$Height[i] - mean(meanHeight[i,]),
            col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")

  text("U.S. height", x=0, y=23, cex=1, adj=0)
  symbol.Female(centerx = 10, centery = 24, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # U.S. male height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-25,25), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-10,0,10), labels=FALSE )


  post <- post6_m

  samps <- length(post$lp__) #number of samples

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )
  meanHeight <- matrix(data=NA, nrow=length(Ber.mal$TotAge), ncol=samps )
  meanHeightL <- rep( NA, times=length(Ber.mal$TotAge) )
  meanHeightH <- rep( NA, times=length(Ber.mal$TotAge) )


  for ( z in 1:5 ){
    mQ[z,] <- pull(post, paste("mQ[", 1, ",", z, "]", sep="")) 
    mK[z,] <- pull(post, paste("mK[", 1, ",", z, "]", sep=""))
    mH[z,] <- pull(post, paste("mH[", 1, ",", z, "]", sep=""))
    mI[z,] <- pull(post, paste("mI[", 1, ",", z, "]", sep=""))

  } # for z


  for ( i in 1:length(Ber.mal$TotAge) ) {

    x <- Ber.mal$TotAge[i]

    meanHeight[i,] <- ifelse( x < mI[1,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]),

                      ifelse( x < mI[2,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]),

                      ifelse( x < mI[3,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]),

                      ifelse( x < mI[4,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]),

                                          0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]) +
                                                  ( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,])
                      ) ) ) )

    meanHeightL[i] <- HPDI(meanHeight[i,], prob=0.9 )[1]
    meanHeightH[i] <- HPDI(meanHeight[i,], prob=0.9 )[2]

    lines(x = c( x, x ),
          y = c( Ber.mal$Height[i] - meanHeightL[i],
                 Ber.mal$Height[i] - meanHeightH[i] ),
          col=BerLine_col, lwd=BerLine_lwd, lty=BerLine_lty)

    points( x = x,
            y = Ber.mal$Height[i] - mean(meanHeight[i,]),
            col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

  } # for i


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")
  text("U.S. height", x=0, y=23, cex=1, adj=0)
  symbol.Male(centerx = 10, centery = 22, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # Matsigenka female height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-25,25), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-10,0,10), labels=c(-10,0,10) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-10,0,10), labels=FALSE ) #right


  post <- post6_f

  samps <- length(post$lp__) #number of samples

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )
  meanHeight <- matrix(data=NA, nrow=length(Mat.fem$TotAge), ncol=samps )
  meanHeightL <- rep( NA, times=length(Mat.fem$TotAge) )
  meanHeightH <- rep( NA, times=length(Mat.fem$TotAge) )


  for ( z in 1:5 ){
    mQ[z,] <- pull(post, paste("mQ[", 2, ",", z, "]", sep="")) # 1 for Ber, 2 for Mats
    mK[z,] <- pull(post, paste("mK[", 2, ",", z, "]", sep=""))
    mH[z,] <- pull(post, paste("mH[", 2, ",", z, "]", sep=""))
    mI[z,] <- pull(post, paste("mI[", 2, ",", z, "]", sep=""))

  } # for z


  for ( i in 1:length(Mat.fem$TotAge) ) {

    x <- Mat.fem$TotAge[i]

    meanHeight[i,] <- ifelse( x < mI[1,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]),

                      ifelse( x < mI[2,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]),

                      ifelse( x < mI[3,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]),

                      ifelse( x < mI[4,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]),

                                          0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]) +
                                                  ( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,])
                      ) ) ) )

    meanHeightL[i] <- HPDI(meanHeight[i,], prob=0.9 )[1]
    meanHeightH[i] <- HPDI(meanHeight[i,], prob=0.9 )[2]

    lines(x = c( x, x ),
          y = c( Mat.fem$Height[i] - meanHeightL[i],
                 Mat.fem$Height[i] - meanHeightH[i] ),
          col=MatLine_col, lwd=MatLine_lwd, lty=MatLine_lty)

    points( x = x,
            y = Mat.fem$Height[i] - mean(meanHeight[i,]),
            col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

  } # for i


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")
  text("Matsigenka height", x=0, y=23, cex=1, adj=0)
  symbol.Female(centerx = 15, centery = 24, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # Matsigenka male height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-25,25), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-10,0,10), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top

  post <- post6_m

  samps <- length(post$lp__) #number of samples

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )
  meanHeight <- matrix(data=NA, nrow=length(Mat.mal$TotAge), ncol=samps )
  meanHeightL <- rep( NA, times=length(Mat.mal$TotAge) )
  meanHeightH <- rep( NA, times=length(Mat.mal$TotAge) )


  for ( z in 1:5 ){
    mQ[z,] <- pull(post, paste("mQ[", 2, ",", z, "]", sep="")) # 1 for Ber, 2 for Mat
    mK[z,] <- pull(post, paste("mK[", 2, ",", z, "]", sep=""))
    mH[z,] <- pull(post, paste("mH[", 2, ",", z, "]", sep=""))
    mI[z,] <- pull(post, paste("mI[", 2, ",", z, "]", sep=""))

  } # for z


  for ( i in 1:length(Mat.mal$TotAge) ) {

    x <- Mat.mal$TotAge[i]

    meanHeight[i,] <- ifelse( x < mI[1,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]),

                      ifelse( x < mI[2,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]),

                      ifelse( x < mI[3,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]),

                      ifelse( x < mI[4,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]),

                                          0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]) +
                                                  ( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,])
                      ) ) ) )

    meanHeightL[i] <- HPDI(meanHeight[i,], prob=0.9 )[1]
    meanHeightH[i] <- HPDI(meanHeight[i,], prob=0.9 )[2]

    lines(x = c( x, x ),
          y = c( Mat.mal$Height[i] - meanHeightL[i],
                 Mat.mal$Height[i] - meanHeightH[i] ),
          col=MatLine_col, lwd=MatLine_lwd, lty=MatLine_lty)

    points( x = x,
            y = Mat.mal$Height[i] - mean(meanHeight[i,]),
            col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")
  text("Matsigenka height", x=0, y=23, cex=1, adj=0)
  symbol.Male(centerx = 15, centery = 22, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package




  # U.S. female weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-2800,2800), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-1000,0,1000), labels=c(-1000,0,1000) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-1000,0,1000), labels=FALSE )


  post <- post6_f

  samps <- length(post$lp__) #number of samples

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )
  meanWeight <- matrix(data=NA, nrow=length(Ber.fem$TotAge), ncol=samps )
  meanWeightL <- rep( NA, times=length(Ber.fem$TotAge) )
  meanWeightH <- rep( NA, times=length(Ber.fem$TotAge) )


  for ( z in 1:5 ){
    mQ[z,] <- pull(post, paste("mQ[", 1, ",", z, "]", sep="")) 
    mK[z,] <- pull(post, paste("mK[", 1, ",", z, "]", sep=""))
    mH[z,] <- pull(post, paste("mH[", 1, ",", z, "]", sep=""))
    mI[z,] <- pull(post, paste("mI[", 1, ",", z, "]", sep=""))

  } # for z


  for ( i in 1:length(Ber.fem$TotAge) ) {

    x <- Ber.fem$TotAge[i]

    meanWeight[i,] <- ifelse( x < mI[1,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2),

                      ifelse( x < mI[2,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2),

                      ifelse( x < mI[3,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2),

                      ifelse( x < mI[4,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2),                                                                                                                                                                  

                                          1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2) +
                                                   pi*( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,] + 2)
                      ) ) ) )

    meanWeightL[i] <- HPDI(meanWeight[i,], prob=0.9 )[1]
    meanWeightH[i] <- HPDI(meanWeight[i,], prob=0.9 )[2]

    lines(x = c( x, x ),
          y = c( Ber.fem$CellWeightg[i] - meanWeightL[i],
                 Ber.fem$CellWeightg[i] - meanWeightH[i] ),
          col=BerLine_col, lwd=BerLine_lwd, lty=BerLine_lty)

    points( x = x,
            y = Ber.fem$CellWeightg[i] - mean(meanWeight[i,]),
            col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")
  text("U.S. weight", x=0, y=2600, cex=1, adj=0)
  symbol.Female(centerx = 10, centery = 2600, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # U.S. male weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-2800,2800), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-1000,0,1000), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top


  post <- post6_m

  samps <- length(post$lp__) #number of samples

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )
  meanWeight <- matrix(data=NA, nrow=length(Ber.mal$TotAge), ncol=samps )
  meanWeightL <- rep( NA, times=length(Ber.mal$TotAge) )
  meanWeightH <- rep( NA, times=length(Ber.mal$TotAge) )


  for ( z in 1:5 ){
    mQ[z,] <- pull(post, paste("mQ[", 1, ",", z, "]", sep="")) 
    mK[z,] <- pull(post, paste("mK[", 1, ",", z, "]", sep=""))
    mH[z,] <- pull(post, paste("mH[", 1, ",", z, "]", sep=""))
    mI[z,] <- pull(post, paste("mI[", 1, ",", z, "]", sep=""))

  } # for z


  for ( i in 1:length(Ber.mal$TotAge) ) {

    x <- Ber.mal$TotAge[i]

    meanWeight[i,] <- ifelse( x < mI[1,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2),

                      ifelse( x < mI[2,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2),

                      ifelse( x < mI[3,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2),

                      ifelse( x < mI[4,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2),                                                                                                                                                                  

                                          1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2) +
                                                   pi*( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,] + 2)
                      ) ) ) )

    meanWeightL[i] <- HPDI(meanWeight[i,], prob=0.9 )[1]
    meanWeightH[i] <- HPDI(meanWeight[i,], prob=0.9 )[2]

    lines(x = c( x, x ),
          y = c( Ber.mal$CellWeightg[i] - meanWeightL[i],
                 Ber.mal$CellWeightg[i] - meanWeightH[i] ),
          col=BerLine_col, lwd=BerLine_lwd, lty=BerLine_lty)

    points( x = x,
            y = Ber.mal$CellWeightg[i] - mean(meanWeight[i,]),
            col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

  } # for i


  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")
  text("U.S. weight", x=0, y=2600, cex=1, adj=0)
  symbol.Male(centerx = 10, centery = 2500, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package




  # Matsigenka female weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-2800,2800), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE )
  axis( side=2, at=c(-1000,0,1000), labels=c(-1000,0,1000) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-1000,0,1000), labels=FALSE ) #right
  
  post <- post6_f

  samps <- length(post$lp__) #number of samples

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )
  meanWeight <- matrix(data=NA, nrow=length(Mat.fem$TotAge), ncol=samps )
  meanWeightL <- rep( NA, times=length(Mat.fem$TotAge) )
  meanWeightH <- rep( NA, times=length(Mat.fem$TotAge) )


  for ( z in 1:5 ){
    mQ[z,] <- pull(post, paste("mQ[", 2, ",", z, "]", sep="")) 
    mK[z,] <- pull(post, paste("mK[", 2, ",", z, "]", sep=""))
    mH[z,] <- pull(post, paste("mH[", 2, ",", z, "]", sep=""))
    mI[z,] <- pull(post, paste("mI[", 2, ",", z, "]", sep=""))

  } # for z


  for ( i in 1:length(Mat.fem$TotAge) ) {

    x <- Mat.fem$TotAge[i]

    meanWeight[i,] <- ifelse( x < mI[1,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2),

                      ifelse( x < mI[2,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2),

                      ifelse( x < mI[3,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2),

                      ifelse( x < mI[4,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2),                                                                                                                                                                  

                                          1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2) +
                                                   pi*( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,] + 2)
                      ) ) ) )

    meanWeightL[i] <- HPDI(meanWeight[i,], prob=0.9 )[1]
    meanWeightH[i] <- HPDI(meanWeight[i,], prob=0.9 )[2]

    lines(x = c( x, x ),
          y = c( Mat.fem$CellWeightg[i] - meanWeightL[i],
                 Mat.fem$CellWeightg[i] - meanWeightH[i] ),
          col=MatLine_col, lwd=MatLine_lwd, lty=MatLine_lty)

    points( x = x,
            y = Mat.fem$CellWeightg[i] - mean(meanWeight[i,]),
            col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")
  text("Matsigenka weight", x=0, y=2600, cex=1, adj=0)
  symbol.Female(centerx = 15, centery = 2600, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # Matsigenka male weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-2800,2800), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE )
  axis( side=2, at=c(-1000,0,1000), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top


  post <- post6_m

  samps <- length(post$lp__) #number of samples

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )
  meanWeight <- matrix(data=NA, nrow=length(Mat.mal$TotAge), ncol=samps )
  meanWeightL <- rep( NA, times=length(Mat.mal$TotAge) )
  meanWeightH <- rep( NA, times=length(Mat.mal$TotAge) )


  for ( z in 1:5 ){
    mQ[z,] <- pull(post, paste("mQ[", 2, ",", z, "]", sep="")) 
    mK[z,] <- pull(post, paste("mK[", 2, ",", z, "]", sep=""))
    mH[z,] <- pull(post, paste("mH[", 2, ",", z, "]", sep=""))
    mI[z,] <- pull(post, paste("mI[", 2, ",", z, "]", sep=""))

  } # for z


  for ( i in 1:length(Mat.mal$TotAge) ) {

    x <- Mat.mal$TotAge[i]

    meanWeight[i,] <- ifelse( x < mI[1,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2),

                      ifelse( x < mI[2,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2),

                      ifelse( x < mI[3,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2),

                      ifelse( x < mI[4,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2),                                                                                                                                                                  

                                          1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2) +
                                                   pi*( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,] + 2)
                      ) ) ) )

    meanWeightL[i] <- HPDI(meanWeight[i,], prob=0.9 )[1]
    meanWeightH[i] <- HPDI(meanWeight[i,], prob=0.9 )[2]

    lines(x = c( x, x ),
          y = c( Mat.mal$CellWeightg[i] - meanWeightL[i],
                 Mat.mal$CellWeightg[i] - meanWeightH[i] ),
          col=MatLine_col, lwd=MatLine_lwd, lty=MatLine_lty)

    points( x = x,
            y = Mat.mal$CellWeightg[i] - mean(meanWeight[i,]),
            col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")
  text("Matsigenka weight", x=0, y=2600, cex=1, adj=0)
  symbol.Male(centerx = 15, centery = 2500, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package


  mtext("Age since conception (years)", side = 1, outer = T, cex = 0.8, line = 2, adj=0.5)
  mtext("Residual height or weight (observed - predicted) (cm or g)", side = 2, outer = T, cex = 0.8, line = 2, adj=0.5)


graphics.off()







##########################################################


###############################################################################



# residuals from estimated individual trajectories

pdf(file="./combined_plots/Plots/Residual_mult_indiv.pdf",
    height=7, width=5)
layout( mat=matrix(c(1,2,3,4,5,6,7,8),4,2,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
par(mar = c(1, 1, 1, 1), oma = c(5, 5, 1, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)



  # U.S. female height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-6,6), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-3,0,3), labels=c(-3,0,3) )
  axis( side=4, at=c(-3,0,3), labels=FALSE ) #right


  post <- post6_f

  samps <- length(post$lp__)                                                  # number of samples
  J <- length(unique(Com.fem$ID))                                             # number of people measured
  #EthID <- Com.fem[match(unique(Com.fem$ID), Com.fem$ID),c("ID","Ethnicity")] # ethnicity for each ID

  Qf <- array( data=NA, dim=c(J,5,samps) ) 
  Kf <- array( data=NA, dim=c(J,5,samps) ) 
  Hf <- array( data=NA, dim=c(J,5,samps) )
  If <- array( data=NA, dim=c(J,5,samps) )

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )

  meanHeight <- rep( NA, times=samps )
  meanHeightL <- NA
  meanHeightH <- NA

  for ( j in 1:J ){ 
    for ( z in 1:5 ){
      Qf[j,z,] <- pull(post, paste("Q[", j, ",", z, "]", sep="")) #pull is a command from package dplyr to extract a vector from a tibble
      Kf[j,z,] <- pull(post, paste("K[", j, ",", z, "]", sep=""))
      Hf[j,z,] <- pull(post, paste("H[", j, ",", z, "]", sep=""))
      If[j,z,] <- pull(post, paste("I[", j, ",", z, "]", sep=""))
    } # for z
  } # for j



  for ( i in 1:nrow(Com.fem) ) {

    if ( Com.fem$Ethnicity[i] == 1 ) {            # if Berkeley

        mQ <- Qf[Com.fem$ID[i],,]
        mH <- Hf[Com.fem$ID[i],,]
        mK <- Kf[Com.fem$ID[i],,]
        mI <- If[Com.fem$ID[i],,]

        x <- Com.fem$TotAge[i]

        meanHeight <- ifelse( x < mI[1,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]),

                      ifelse( x < mI[2,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]),

                      ifelse( x < mI[3,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]),

                      ifelse( x < mI[4,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]),

                                          0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]) +
                                                  ( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,])
                      ) ) ) )

        meanHeightL <- HPDI(meanHeight, prob=0.9 )[1]
        meanHeightH <- HPDI(meanHeight, prob=0.9 )[2]

        lines(x = c( x, x ),
              y = c( Com.fem$Height[i] - meanHeightL,
                     Com.fem$Height[i] - meanHeightH ),
              col=BerLine_col, lwd=BerLine_lwd, lty=BerLine_lty)

        points( x = x,
                y = Com.fem$Height[i] - mean(meanHeight),
                col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

    } # if
  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")

  text("U.S. height", x=0, y=5.2, cex=1, adj=0)
  symbol.Female(centerx = 10, centery = 5.2, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # U.S. male height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-6,6), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-3,0,3), labels=FALSE )


  post <- post6_m

  samps <- length(post$lp__)          # number of samples
  J <- length(unique(Com.mal$ID))     # number of people measured
 
  Qm <- array( data=NA, dim=c(J,5,samps) ) 
  Km <- array( data=NA, dim=c(J,5,samps) ) 
  Hm <- array( data=NA, dim=c(J,5,samps) )
  Im <- array( data=NA, dim=c(J,5,samps) )

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )

  meanHeight <- rep( NA, times=samps )
  meanHeightL <- NA
  meanHeightH <- NA

  for ( j in 1:J ){ 
    for ( z in 1:5 ){
      Qm[j,z,] <- pull(post, paste("Q[", j, ",", z, "]", sep="")) #pull is a command from package dplyr to extract a vector from a tibble
      Km[j,z,] <- pull(post, paste("K[", j, ",", z, "]", sep=""))
      Hm[j,z,] <- pull(post, paste("H[", j, ",", z, "]", sep=""))
      Im[j,z,] <- pull(post, paste("I[", j, ",", z, "]", sep=""))
    } # for z
  } # for j



  for ( i in 1:nrow(Com.mal) ) {

    if ( Com.mal$Ethnicity[i] == 1 ) {            # if Berkeley

        mQ <- Qm[Com.mal$ID[i],,]
        mH <- Hm[Com.mal$ID[i],,]
        mK <- Km[Com.mal$ID[i],,]
        mI <- Im[Com.mal$ID[i],,]

        x <- Com.mal$TotAge[i]

        meanHeight <- ifelse( x < mI[1,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]),

                      ifelse( x < mI[2,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]),

                      ifelse( x < mI[3,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]),

                      ifelse( x < mI[4,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]),

                                          0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]) +
                                                  ( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,])
                      ) ) ) )

        meanHeightL <- HPDI(meanHeight, prob=0.9 )[1]
        meanHeightH <- HPDI(meanHeight, prob=0.9 )[2]

        lines(x = c( x, x ),
              y = c( Com.mal$Height[i] - meanHeightL,
                     Com.mal$Height[i] - meanHeightH ),
              col=BerLine_col, lwd=BerLine_lwd, lty=BerLine_lty)

        points( x = x,
                y = Com.mal$Height[i] - mean(meanHeight),
                col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

    } # if
  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")

  text("U.S. height", x=0, y=5.2, cex=1, adj=0)
  symbol.Male(centerx = 10, centery = 5.2, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package






  # Matsigenka female height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-6,6), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-3,0,3), labels=c(-3,0,3) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-3,0,3), labels=FALSE ) #right


  post <- post6_f

  samps <- length(post$lp__)                                                  # number of samples
  J <- length(unique(Com.fem$ID))                                             # number of people measured
 

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )

  meanHeight <- rep( NA, times=samps )
  meanHeightL <- NA
  meanHeightH <- NA




  for ( i in 1:nrow(Com.fem) ) {

    if ( Com.fem$Ethnicity[i] == 2 ) {            # if Matsigenka

        mQ <- Qf[Com.fem$ID[i],,]
        mH <- Hf[Com.fem$ID[i],,]
        mK <- Kf[Com.fem$ID[i],,]
        mI <- If[Com.fem$ID[i],,]

        x <- Com.fem$TotAge[i]

        meanHeight <- ifelse( x < mI[1,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]),

                      ifelse( x < mI[2,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]),

                      ifelse( x < mI[3,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]),

                      ifelse( x < mI[4,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]),

                                          0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]) +
                                                  ( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,])
                      ) ) ) )

        meanHeightL <- HPDI(meanHeight, prob=0.9 )[1]
        meanHeightH <- HPDI(meanHeight, prob=0.9 )[2]

        lines(x = c( x, x ),
              y = c( Com.fem$Height[i] - meanHeightL,
                     Com.fem$Height[i] - meanHeightH ),
              col=MatLine_col, lwd=MatLine_lwd, lty=MatLine_lty)

        points( x = x,
                y = Com.fem$Height[i] - mean(meanHeight),
                col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

    } # if
  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")

  text("Matsigenka height", x=0, y=5.2, cex=1, adj=0)
  symbol.Female(centerx = 15, centery = 5.2, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package




  # Matsigenka male height

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-6,6), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-3,0,3), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top

  post <- post6_m

  samps <- length(post$lp__)          # number of samples
  J <- length(unique(Com.mal$ID))     # number of people measured
 

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )

  meanHeight <- rep( NA, times=samps )
  meanHeightL <- NA
  meanHeightH <- NA




  for ( i in 1:nrow(Com.mal) ) {

    if ( Com.mal$Ethnicity[i] == 2 ) {            # if Matsigenka

        mQ <- Qm[Com.mal$ID[i],,]
        mH <- Hm[Com.mal$ID[i],,]
        mK <- Km[Com.mal$ID[i],,]
        mI <- Im[Com.mal$ID[i],,]

        x <- Com.mal$TotAge[i]

        meanHeight <- ifelse( x < mI[1,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]),

                      ifelse( x < mI[2,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]),

                      ifelse( x < mI[3,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]),

                      ifelse( x < mI[4,], 0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]),

                                          0.012 + ( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,]) +
                                                  ( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,]) +
                                                  ( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,]) +
                                                  ( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,]) +
                                                  ( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,])
                      ) ) ) )

        meanHeightL <- HPDI(meanHeight, prob=0.9 )[1]
        meanHeightH <- HPDI(meanHeight, prob=0.9 )[2]

        lines(x = c( x, x ),
              y = c( Com.mal$Height[i] - meanHeightL,
                     Com.mal$Height[i] - meanHeightH ),
              col=MatLine_col, lwd=MatLine_lwd, lty=MatLine_lty)

        points( x = x,
                y = Com.mal$Height[i] - mean(meanHeight),
                col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

    } # if
  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  text("Matsigenka height", x=0, y=5.2, cex=1, adj=0)
  symbol.Male(centerx = 15, centery = 5.2, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package




  # U.S. female weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-2500,2500), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-1000,0,1000), labels=c(-1000,0,1000) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-1000,0,1000), labels=FALSE )

  post <- post6_f

  samps <- length(post$lp__)                                                  # number of samples
  J <- length(unique(Com.fem$ID))                                             # number of people measured


  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )

  meanWeight <- rep( NA, times=samps )
  meanWeightL <- NA
  meanWeightH <- NA




  for ( i in 1:nrow(Com.fem) ) {

    if ( Com.fem$Ethnicity[i] == 1 ) {            # if Berkeley

        mQ <- Qf[Com.fem$ID[i],,]
        mH <- Hf[Com.fem$ID[i],,]
        mK <- Kf[Com.fem$ID[i],,]
        mI <- If[Com.fem$ID[i],,]

        x <- Com.fem$TotAge[i]

        meanWeight <- ifelse( x < mI[1,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2),

                      ifelse( x < mI[2,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2),

                      ifelse( x < mI[3,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2),

                      ifelse( x < mI[4,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2),                                                                                                                                                                  

                                          1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2) +
                                                   pi*( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,] + 2)
                      ) ) ) )

        meanWeightL <- HPDI(meanWeight, prob=0.9 )[1]
        meanWeightH <- HPDI(meanWeight, prob=0.9 )[2]

        lines(x = c( x, x ),
              y = c( Com.fem$CellWeightg[i] - meanWeightL,
                     Com.fem$CellWeightg[i] - meanWeightH ),
              col=BerLine_col, lwd=BerLine_lwd, lty=BerLine_lty)

        points( x = x,
                y = Com.fem$CellWeightg[i] - mean(meanWeight),
                col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

    } # if
  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  text("U.S. weight", x=0, y=2200, cex=1, adj=0)
  symbol.Female(centerx = 10, centery = 2200, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # U.S. male weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-2500,2500), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=c(-1000,0,1000), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top

  post <- post6_m

  samps <- length(post$lp__)          # number of samples
  J <- length(unique(Com.mal$ID))     # number of people measured

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )

  meanWeight <- rep( NA, times=samps )
  meanWeightL <- NA
  meanWeightH <- NA



  for ( i in 1:nrow(Com.mal) ) {

    if ( Com.mal$Ethnicity[i] == 1 ) {            # if Berkeley

        mQ <- Qm[Com.mal$ID[i],,]
        mH <- Hm[Com.mal$ID[i],,]
        mK <- Km[Com.mal$ID[i],,]
        mI <- Im[Com.mal$ID[i],,]

        x <- Com.mal$TotAge[i]

        meanWeight <- ifelse( x < mI[1,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2),

                      ifelse( x < mI[2,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2),

                      ifelse( x < mI[3,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2),

                      ifelse( x < mI[4,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2),                                                                                                                                                                  

                                          1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2) +
                                                   pi*( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,] + 2)
                      ) ) ) )

        meanWeightL <- HPDI(meanWeight, prob=0.9 )[1]
        meanWeightH <- HPDI(meanWeight, prob=0.9 )[2]

        lines(x = c( x, x ),
              y = c( Com.mal$CellWeightg[i] - meanWeightL,
                     Com.mal$CellWeightg[i] - meanWeightH ),
              col=BerLine_col, lwd=BerLine_lwd, lty=BerLine_lty)

        points( x = x,
                y = Com.mal$CellWeight[i] - mean(meanWeight),
                col=BerPoint_col, lwd=BerPoint_lwd, pch=BerPoint_pch, cex=BerPoint_cex)

    } # if
  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")


  text("U.S. weight", x=0, y=2200, cex=1, adj=0)
  symbol.Male(centerx = 10, centery = 2200, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package




  # Matsigenka female weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-2500,2500), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE )
  axis( side=2, at=c(-1000,0,1000), labels=c(-1000,0,1000) )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top
  axis( side=4, at=c(-1000,0,1000), labels=FALSE ) #right

  post <- post6_f

  samps <- length(post$lp__)                                                  # number of samples
  J <- length(unique(Com.fem$ID))                                             # number of people measured


  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )

  meanWeight <- rep( NA, times=samps )
  meanWeightL <- NA
  meanWeightH <- NA



  for ( i in 1:nrow(Com.fem) ) {

    if ( Com.fem$Ethnicity[i] == 2 ) {            # if Matsigenka

        mQ <- Qf[Com.fem$ID[i],,]
        mH <- Hf[Com.fem$ID[i],,]
        mK <- Kf[Com.fem$ID[i],,]
        mI <- If[Com.fem$ID[i],,]

        x <- Com.fem$TotAge[i]

        meanWeight <- ifelse( x < mI[1,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2),

                      ifelse( x < mI[2,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2),

                      ifelse( x < mI[3,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2),

                      ifelse( x < mI[4,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2),                                                                                                                                                                  

                                          1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2) +
                                                   pi*( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,] + 2)
                      ) ) ) )

        meanWeightL <- HPDI(meanWeight, prob=0.9 )[1]
        meanWeightH <- HPDI(meanWeight, prob=0.9 )[2]

        lines(x = c( x, x ),
              y = c( Com.fem$CellWeightg[i] - meanWeightL,
                     Com.fem$CellWeightg[i] - meanWeightH ),
              col=MatLine_col, lwd=MatLine_lwd, lty=MatLine_lty)

        points( x = x,
                y = Com.fem$CellWeightg[i] - mean(meanWeight),
                col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

    } # if
  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")

  text("Matsigenka weight", x=0, y=2200, cex=1, adj=0)
  symbol.Female(centerx = 15, centery = 2200, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package





  # Matsigenka male weight

  #set up plot
  #par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(-2500,2500), xlim=c(0,25), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=TRUE )
  axis( side=2, at=c(-1000,0,1000), labels=FALSE )
  axis( side=3, at=seq( 0, 25, by=5), labels=FALSE ) #top


  post <- post6_m

  samps <- length(post$lp__)          # number of samples
  J <- length(unique(Com.mal$ID))     # number of people measured

  mQ <- matrix(data=NA, nrow=5, ncol=samps ) 
  mK <- matrix(data=NA, nrow=5, ncol=samps ) 
  mH <- matrix(data=NA, nrow=5, ncol=samps )
  mI <- matrix(data=NA, nrow=5, ncol=samps )

  meanWeight <- rep( NA, times=samps )
  meanWeightL <- NA
  meanWeightH <- NA



  for ( i in 1:nrow(Com.mal) ) {

    if ( Com.mal$Ethnicity[i] == 2 ) {            # if Matsigenka

        mQ <- Qm[Com.mal$ID[i],,]
        mH <- Hm[Com.mal$ID[i],,]
        mK <- Km[Com.mal$ID[i],,]
        mI <- Im[Com.mal$ID[i],,]

        x <- Com.mal$TotAge[i]

        meanWeight <- ifelse( x < mI[1,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2),

                      ifelse( x < mI[2,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2),

                      ifelse( x < mI[3,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2),

                      ifelse( x < mI[4,], 1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2),                                                                                                                                                                  

                                          1.02e-6 + pi*( 2*mH[1,]/mK[1,] * ( 1 - exp(mK[1,]*mQ[1,]*( 0 -      x )/( 1 + 2*mQ[1,] )) ) )^(1/mQ[1,] + 2) +
                                                   pi*( 2*mH[2,]/mK[2,] * ( 1 - exp(mK[2,]*mQ[2,]*( mI[1,] - x )/( 1 + 2*mQ[2,] )) ) )^(1/mQ[2,] + 2) +
                                                   pi*( 2*mH[3,]/mK[3,] * ( 1 - exp(mK[3,]*mQ[3,]*( mI[2,] - x )/( 1 + 2*mQ[3,] )) ) )^(1/mQ[3,] + 2) +
                                                   pi*( 2*mH[4,]/mK[4,] * ( 1 - exp(mK[4,]*mQ[4,]*( mI[3,] - x )/( 1 + 2*mQ[4,] )) ) )^(1/mQ[4,] + 2) +
                                                   pi*( 2*mH[5,]/mK[5,] * ( 1 - exp(mK[5,]*mQ[5,]*( mI[4,] - x )/( 1 + 2*mQ[5,] )) ) )^(1/mQ[5,] + 2)
                      ) ) ) )

        meanWeightL <- HPDI(meanWeight, prob=0.9 )[1]
        meanWeightH <- HPDI(meanWeight, prob=0.9 )[2]

        lines(x = c( x, x ),
              y = c( Com.mal$CellWeightg[i] - meanWeightL,
                     Com.mal$CellWeightg[i] - meanWeightH ),
              col=MatLine_col, lwd=MatLine_lwd, lty=MatLine_lty)

        points( x = x,
                y = Com.mal$CellWeight[i] - mean(meanWeight),
                col=MatPoint_col, lwd=MatPoint_lwd, pch=MatPoint_pch, cex=MatPoint_cex)

    } # if
  } # for i

  lines(x = c(-1,26), y = c(0,0), lty=2, lwd=1, col="black")

  text("Matsigenka weight", x=0, y=2200, cex=1, adj=0)
  symbol.Male(centerx = 15, centery = 2200, rayonx=0.6, lwd=1, col="black") # gender symbol from HelpersMG package


  mtext("Age since conception (years)", side = 1, outer = T, cex = 0.8, line = 2, adj=0.5)
  mtext("Residual height or weight (observed - predicted) (cm or g)", side = 2, outer = T, cex = 0.8, line = 2, adj=0.5)


graphics.off()





