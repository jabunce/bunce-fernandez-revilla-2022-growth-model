

################## m1 composite model

post <- post1

pdf(file="./Plots/Composite_ber_m1_wf.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,100), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Weight (kg)" )


  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$Weight, lwd=1, col=grey(0.5), cex=0.5)


  # plot prior trajectory
  muQ1 = exp(start_list1$muLQ1)
  muQ2 = exp(start_list1$muLQ2)
  muQ3 = exp(start_list1$muLQ3)
  muK1 = exp(start_list1$muLK1)
  muK2 = exp(start_list1$muLK2)
  muK3 = exp(start_list1$muLK3)
  muH1 = muK1/2 + start_list1$muR1
  muH2 = muK2/2 + start_list1$muR2
  muH3 = muK3/2 + start_list1$muR3
  lines(x = x,
        y = ( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1*pi^(muQ1/( 1 + 2*muQ1 )) )^(( 1 + 2*muQ1 )/muQ1) +
            ( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2*pi^(muQ2/( 1 + 2*muQ2 )) )^(( 1 + 2*muQ2 )/muQ2) +
            ( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3*pi^(muQ3/( 1 + 2*muQ3 )) )^(( 1 + 2*muQ3 )/muQ3),
            col="pink", lwd=3)


  # plot posterior mean trajectory
  muQ1 = exp(mean(post$muLQ1))
  muQ2 = exp(mean(post$muLQ2))
  muQ3 = exp(mean(post$muLQ3))
  muK1 = exp(mean(post$muLK1))
  muK2 = exp(mean(post$muLK2))
  muK3 = exp(mean(post$muLK3))
  muH1 = muK1/2 + mean(post$muR1)
  muH2 = muK2/2 + mean(post$muR2)
  muH3 = muK3/2 + mean(post$muR3)

  # plot posterior mean trajectory
  lines(x = x,
        y = ( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1*pi^(muQ1/( 1 + 2*muQ1 )) )^(( 1 + 2*muQ1 )/muQ1) +
            ( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2*pi^(muQ2/( 1 + 2*muQ2 )) )^(( 1 + 2*muQ2 )/muQ2) +
            ( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3*pi^(muQ3/( 1 + 2*muQ3 )) )^(( 1 + 2*muQ3 )/muQ3),
            col="black", lwd=3)


  # plot individual component functions

  # infant
  x2 <- seq(from=(0), to=(26+0.75), by=0.1) # infant growth all the way back to conception
  lines(x = x2,
        y=( -1*exp(-1*muK1*muQ1*x2/( 1 + 2*muQ1 )) + 2*muH1/muK1*pi^(muQ1/( 1 + 2*muQ1 )) )^(( 1 + 2*muQ1 )/muQ1),
        col="black", lwd=3)

  # child
  lines(x = x,
        y=( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2*pi^(muQ2/( 1 + 2*muQ2 )) )^(( 1 + 2*muQ2 )/muQ2),
        col="black", lwd=3)

  # adolescent
  lines(x = x,
        y=( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3*pi^(muQ3/( 1 + 2*muQ3 )) )^(( 1 + 2*muQ3 )/muQ3),
        col="black", lwd=3)


  # plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,10), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 10, by=2 ), las=2 )
  par(xpd=FALSE) # don't plot beyond the axes
  
  # first derivative from mathematica
  y_prime <- exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 ))*muK1*( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1*pi^(muQ1/( 1 + 2*muQ1 )) )^( 1 + 1/muQ1 ) + 
             exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 ))*muK2*( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2*pi^(muQ2/( 1 + 2*muQ2 )) )^( 1 + 1/muQ2 ) +
             exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 ))*muK3*( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3*pi^(muQ3/( 1 + 2*muQ3 )) )^( 1 + 1/muQ3 )

  lines(x = x, y=y_prime, col="red", lwd=3)

  par(xpd=TRUE)
  text("Growth velocity (kg/year)", x=33, y=5, srt=270, las=3)

  legend(-0.7, 10.3,
         legend=c("U.S. data",
                 "Estimated weight trajectory",
                 "Prior weight trajectory",
                 "Estimated growth velocity"), bty="o", bg="white",
         col=c(grey(0.5), "black", "pink", "red"),
         merge=FALSE,
         pch=c(1,NA,NA,NA),
         lty=c(0,1,1,1),
         lwd=c(1,3,3,3), cex=0.6, seg.len=3)

symbol.Female(centerx = 5, centery = 7, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package

graphics.off()




################################## modified velocity axis



pdf(file="./Plots/Composite_ber_m1_wf_vel.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,100), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Weight (kg)" )


  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$Weight, lwd=1, col=grey(0.5), cex=0.5)


  # plot prior trajectory
  muQ1 = exp(start_list1$muLQ1)
  muQ2 = exp(start_list1$muLQ2)
  muQ3 = exp(start_list1$muLQ3)
  muK1 = exp(start_list1$muLK1)
  muK2 = exp(start_list1$muLK2)
  muK3 = exp(start_list1$muLK3)
  muH1 = muK1/2 + start_list1$muR1
  muH2 = muK2/2 + start_list1$muR2
  muH3 = muK3/2 + start_list1$muR3
  lines(x = x,
        y = ( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1*pi^(muQ1/( 1 + 2*muQ1 )) )^(( 1 + 2*muQ1 )/muQ1) +
            ( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2*pi^(muQ2/( 1 + 2*muQ2 )) )^(( 1 + 2*muQ2 )/muQ2) +
            ( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3*pi^(muQ3/( 1 + 2*muQ3 )) )^(( 1 + 2*muQ3 )/muQ3),
            col="pink", lwd=3)


  # plot posterior mean trajectory
  muQ1 = exp(mean(post$muLQ1))
  muQ2 = exp(mean(post$muLQ2))
  muQ3 = exp(mean(post$muLQ3))
  muK1 = exp(mean(post$muLK1))
  muK2 = exp(mean(post$muLK2))
  muK3 = exp(mean(post$muLK3))
  muH1 = muK1/2 + mean(post$muR1)
  muH2 = muK2/2 + mean(post$muR2)
  muH3 = muK3/2 + mean(post$muR3)

  # plot posterior mean trajectory
  lines(x = x,
        y = ( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1*pi^(muQ1/( 1 + 2*muQ1 )) )^(( 1 + 2*muQ1 )/muQ1) +
            ( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2*pi^(muQ2/( 1 + 2*muQ2 )) )^(( 1 + 2*muQ2 )/muQ2) +
            ( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3*pi^(muQ3/( 1 + 2*muQ3 )) )^(( 1 + 2*muQ3 )/muQ3),
            col="black", lwd=3)


  # plot individual component functions

  # infant
  x2 <- seq(from=(0), to=(26+0.75), by=0.1) # infant growth all the way back to conception
  lines(x = x2,
        y=( -1*exp(-1*muK1*muQ1*x2/( 1 + 2*muQ1 )) + 2*muH1/muK1*pi^(muQ1/( 1 + 2*muQ1 )) )^(( 1 + 2*muQ1 )/muQ1),
        col="black", lwd=3)

  # child
  lines(x = x,
        y=( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2*pi^(muQ2/( 1 + 2*muQ2 )) )^(( 1 + 2*muQ2 )/muQ2),
        col="black", lwd=3)

  # adolescent
  lines(x = x,
        y=( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3*pi^(muQ3/( 1 + 2*muQ3 )) )^(( 1 + 2*muQ3 )/muQ3),
        col="black", lwd=3)


  # plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,17), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 17, by=5 ), las=2 )
  par(xpd=FALSE) # don't plot beyond the axes
  
  # first derivative from mathematica
  y_prime <- exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 ))*muK1*( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1*pi^(muQ1/( 1 + 2*muQ1 )) )^( 1 + 1/muQ1 ) + 
             exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 ))*muK2*( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2*pi^(muQ2/( 1 + 2*muQ2 )) )^( 1 + 1/muQ2 ) +
             exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 ))*muK3*( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3*pi^(muQ3/( 1 + 2*muQ3 )) )^( 1 + 1/muQ3 )

  lines(x = x, y=y_prime, col="red", lwd=3)

  par(xpd=TRUE)
  text("Growth velocity (kg/year)", x=33, y=8.5, srt=270, las=3)

  legend(3, 17,
         legend=c("U.S. data",
                 "Estimated weight trajectory",
                 "Prior weight trajectory",
                 "Estimated growth velocity"), bty="o", bg="white",
         col=c(grey(0.5), "black", "pink", "red"),
         merge=FALSE,
         pch=c(1,NA,NA,NA),
         lty=c(0,1,1,1),
         lwd=c(1,3,3,3), cex=0.6, seg.len=3)

symbol.Female(centerx = 5, centery = 12, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package

graphics.off()


