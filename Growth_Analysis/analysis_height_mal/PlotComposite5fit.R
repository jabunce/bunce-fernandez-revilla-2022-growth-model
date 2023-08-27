
################## m4 composite-5 model, Berkeley

post <- post4

pdf(file="./Plots/Composite5_ber.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=1, col=grey(0.5), cex=0.5)

  # # plot prior trajectory
  # muQ1 = exp(start_list6$muLQ1)
  # muQ2 = exp(start_list6$muLQ2)
  # muQ3 = exp(start_list6$muLQ3)
  # muQ4 = exp(start_list6$muLQ4)
  # muQ5 = exp(start_list6$muLQ5)
  # muK1 = exp(start_list6$muLK1)
  # muK2 = exp(start_list6$muLK2)
  # muK3 = exp(start_list6$muLK3)
  # muK4 = exp(start_list6$muLK4)
  # muK5 = exp(start_list6$muLK5)
  # muH1 = muK1/2 + start_list6$muR1
  # muH2 = muK2/2 + start_list6$muR2
  # muH3 = muK3/2 + start_list6$muR3
  # muH4 = muK4/2 + start_list6$muR4
  # muH5 = muK5/2 + start_list6$muR5
  # lines(x = x,
  #       y = ( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1 )^(1/muQ1) +
  #           ( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2 )^(1/muQ2) +
  #           ( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3 )^(1/muQ3) +
  #           ( -1*exp(-1*muK4*muQ4*x/( 1 + 2*muQ4 )) + 2*muH4/muK4 )^(1/muQ4) +
  #           ( -1*exp(-1*muK5*muQ5*x/( 1 + 2*muQ5 )) + 2*muH5/muK5 )^(1/muQ5),
  #       col="pink", lwd=3)


  # plot posterior mean trajectory
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

  # plot posterior mean trajectory
  lines(x = x,
        y = ( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1 )^(1/muQ1) +
            ( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2 )^(1/muQ2) +
            ( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3 )^(1/muQ3) +
            ( -1*exp(-1*muK4*muQ4*x/( 1 + 2*muQ4 )) + 2*muH4/muK4 )^(1/muQ4) +
            ( -1*exp(-1*muK5*muQ5*x/( 1 + 2*muQ5 )) + 2*muH5/muK5 )^(1/muQ5),
        col="black", lwd=3)

  # plot individual component functions

  # infant
  x2 <- seq(from=(0), to=(26+0.75), by=0.1) # infant growth all the way back to conception
  lines(x = x2,
        y=( -1*exp(-1*muK1*muQ1*x2/( 1 + 2*muQ1 )) + 2*muH1/muK1 )^(1/muQ1),
        col="black", lwd=3)

  # child
  lines(x = x,
        y=( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2 )^(1/muQ2),
        col="black", lwd=3)

  # adolescent
  lines(x = x,
        y=( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3 )^(1/muQ3),
        col="black", lwd=3)

  # child
  lines(x = x,
        y=( -1*exp(-1*muK4*muQ4*x/( 1 + 2*muQ4 )) + 2*muH4/muK4 )^(1/muQ4),
        col="black", lwd=3)

  # adolescent
  lines(x = x,
        y=( -1*exp(-1*muK5*muQ5*x/( 1 + 2*muQ5 )) + 2*muH5/muK5 )^(1/muQ5),
        col="black", lwd=3)


  # plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=2 ), las=2 )
  par(xpd=FALSE) # don't plot beyond the axes
  
  # first derivative from mathematica
  y_prime <- 1/(1+2*muQ1)*muK1*exp((-1*muK1*muQ1*x)/(1+2*muQ1))*( -1*exp((-1*muK1*muQ1*x)/(1+2*muQ1)) + 2*muH1/muK1 )^(1/muQ1-1) + 
             1/(1+2*muQ2)*muK2*exp((-1*muK2*muQ2*x)/(1+2*muQ2))*( -1*exp((-1*muK2*muQ2*x)/(1+2*muQ2)) + 2*muH2/muK2 )^(1/muQ2-1) +
             1/(1+2*muQ3)*muK3*exp((-1*muK3*muQ3*x)/(1+2*muQ3))*( -1*exp((-1*muK3*muQ3*x)/(1+2*muQ3)) + 2*muH3/muK3 )^(1/muQ3-1) +
             1/(1+2*muQ4)*muK4*exp((-1*muK4*muQ4*x)/(1+2*muQ4))*( -1*exp((-1*muK4*muQ4*x)/(1+2*muQ4)) + 2*muH4/muK4 )^(1/muQ4-1) +
             1/(1+2*muQ5)*muK5*exp((-1*muK5*muQ5*x)/(1+2*muQ5))*( -1*exp((-1*muK5*muQ5*x)/(1+2*muQ5)) + 2*muH5/muK5 )^(1/muQ5-1)
  lines(x = x, y=y_prime, col="red", lwd=3)

  par(xpd=TRUE)
  text("Growth velocity (cm/year)", x=33, y=8, srt=270, las=3)

  legend(-0.7, 16.5,
         legend=c("U.S. data",
                 "Estimated height trajectory",
                 "Estimated growth velocity"), bty="o", bg="white",
         col=c(grey(0.5), "black", "red"),
         merge=FALSE,
         pch=c(1,NA,NA),
         lty=c(0,1,1),
         lwd=c(1,3,3), cex=0.6, seg.len=3)

#text(x=23,y=10,labels='\\MA',vfont=c("sans serif","bold"),cex = 4) #add Venus symbol

symbol.Male(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()


