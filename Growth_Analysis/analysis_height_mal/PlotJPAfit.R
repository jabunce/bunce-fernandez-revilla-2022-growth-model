################## JPA-1 model

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


# BerIndivTraj_lwd <- 0.25
# BerIndivTraj_col <- colorlist["2.4"]
# BerIndivTraj_lty <- 1

# MatIndivTraj_lwd <- 0.25
# MatIndivTraj_col <- colorlist["1.4"]
# MatIndivTraj_lty <- 1


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



pdf(file="./Plots/JPA_Ber_Mat.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(27), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=1, col=BerPoint_col, cex=0.5)
  points(x=Mat.mal$TotAge, y=Mat.mal$Height, lwd=1, col=MatPoint_col, cex=0.5)

  # # plot real mean trajectory
  # lines( x=LMSmaleC$Age+0.75, y=LMSmaleC$Mu, col="green", lwd=3 )

  # # plot prior trajectory
  # muA <- start_list1$muA #175        
  # muD1 <- start_list1$muD1 #3.03        
  # muD2 <- start_list1$muD2 #9.98          
  # muD3 <- start_list1$muD3 #13.75      
  # muC1 <- start_list1$muC1 #0.59
  # muC2 <- start_list1$muC2 #3.63 
  # muC3 <- start_list1$muC3 #21.84 
  # lines(x = x,
  #       y=muA*exp( -1/( muC1*log( x/muD1 + 1 ) + (x/muD2)^muC2 + (x/muD3)^muC3 ) ),  #muA*( 1 - 1/( 1 + (x/muD1)^muC1 + (x/muD2)^muC2 + (x/muD3)^muC3 ) ),
  #       col="pink", lwd=3)


  # plot posterior mean trajectory
  post <- post5
  lines(x = x,
        y=mean(post$muA)*exp( -1/( mean(post$muC1)*log( x/mean(post$muD1) + 1 ) + (x/mean(post$muD2))^mean(post$muC2) + (x/mean(post$muD3))^mean(post$muC3) ) ),
        col=BerMeanTraj_col, lwd=3)

  post <- post6
  lines(x = x,
        y=mean(post$muA)*exp( -1/( mean(post$muC1)*log( x/mean(post$muD1) + 1 ) + (x/mean(post$muD2))^mean(post$muC2) + (x/mean(post$muD3))^mean(post$muC3) ) ),
        col=MatMeanTraj_col, lwd=3)


  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=2 ), las=2 )
  par(xpd=FALSE) # don't plot beyond the axes
  text("Growth velocity (cm/year)", x=33, y=8, srt=270, las=3)

  # first derivative from Mathematica
  post <- post5
  y_prime <- mean(post$muA)*exp( -1/( mean(post$muC1)*log( x/mean(post$muD1) + 1 ) + (x/mean(post$muD2))^mean(post$muC2) + (x/mean(post$muD3))^mean(post$muC3) ) )*
             ( mean(post$muC1)*x + ( mean(post$muD1) + x )*( mean(post$muC2)*(x/mean(post$muD2))^mean(post$muC2) + mean(post$muC3)*(x/mean(post$muD3))^mean(post$muC3) ) )*
              1/( x*( mean(post$muD1) + x )*( (x/mean(post$muD2))^mean(post$muC2) + (x/mean(post$muD3))^mean(post$muC3) + mean(post$muC1)*log( 1 + x/mean(post$muD1) ) )^2 )

  lines(x = x,
        y = y_prime,
        col=BerVelTraj_col, lwd=3, lty=1)

  post <- post6
  y_prime <- mean(post$muA)*exp( -1/( mean(post$muC1)*log( x/mean(post$muD1) + 1 ) + (x/mean(post$muD2))^mean(post$muC2) + (x/mean(post$muD3))^mean(post$muC3) ) )*
             ( mean(post$muC1)*x + ( mean(post$muD1) + x )*( mean(post$muC2)*(x/mean(post$muD2))^mean(post$muC2) + mean(post$muC3)*(x/mean(post$muD3))^mean(post$muC3) ) )*
              1/( x*( mean(post$muD1) + x )*( (x/mean(post$muD2))^mean(post$muC2) + (x/mean(post$muD3))^mean(post$muC3) + mean(post$muC1)*log( 1 + x/mean(post$muD1) ) )^2 )

  lines(x = x,
        y = y_prime,
        col=MatVelTraj_col, lwd=3, lty=1)


  par(xpd=TRUE)
  text("Growth velocity (cm/year)", x=33, y=8, srt=270, las=3)

  legend(-0.7, 16.5,
         legend=c("U.S. data", "Matsigenka data",
                 "Estimated U.S. height", "Est. Matsigenka height",
                 "Estimated U.S. velocity", "Est. Matigenka velocity"
                 ), bty="o", bg="white",
         col=c(BerPoint_col, MatPoint_col, BerMeanTraj_col, MatMeanTraj_col, BerVelTraj_col, MatVelTraj_col),
         merge=FALSE,
         pch=c(1,1,NA,NA,NA,NA),
         lty=c(0,0,1,1,1,1),
         lwd=c(1,1,3,3,3,3), cex=0.6, seg.len=3)

symbol.Male(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()


