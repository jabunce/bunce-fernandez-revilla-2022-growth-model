
################## m4 composite-5 model, Berkeley

post <- post4

pdf(file="./Plots/Composite5_ber.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )


  # plot measurment points
  points(x=dat$TotAge, y=dat$Height, lwd=1, col=grey(0.5), cex=0.5)
  #points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=1, col="red", cex=0.5)



  muQ1 = mean(post$"muQ[1]")
  muQ2 = mean(post$"muQ[2]")
  muQ3 = mean(post$"muQ[3]")
  muQ4 = mean(post$"muQ[4]")
  muQ5 = mean(post$"muQ[5]")

  muK1 = mean(post$"muK[1]")
  muK2 = mean(post$"muK[2]")
  muK3 = mean(post$"muK[3]")
  muK4 = mean(post$"muK[4]")
  muK5 = mean(post$"muK[5]")

  muH1 = mean(post$"muH[1]")
  muH2 = mean(post$"muH[2]")
  muH3 = mean(post$"muH[3]")
  muH4 = mean(post$"muH[4]")
  muH5 = mean(post$"muH[5]")

  muI1 = mean(post$"muI[1]")
  muI2 = mean(post$"muI[2]")
  muI3 = mean(post$"muI[3]")
  muI4 = mean(post$"muI[4]")



  # plot posterior mean trajectory

  lines(x = x,
        y = ifelse( x < muI1, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( 0 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1),

            ifelse( x < muI2, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( 0 -    x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                      ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI1 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2),

            ifelse( x < muI3, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( 0 -    x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                      ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI1 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                      ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI2 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3),

            ifelse( x < muI4, 0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( 0 -    x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                      ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI1 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                      ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI2 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3) +
                                      ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI3 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4),

                              0.012 + ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( 0 -    x )/( 1 + 2*muQ1 )) ) )^(1/muQ1) +
                                      ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI1 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2) +
                                      ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI2 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3) +
                                      ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI3 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4) +
                                      ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI4 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5)

            ) ) ) ),
        col="black", lwd=3, lty=1)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ifelse( x > 0, ( 2*muH1/muK1 * ( 1 - exp(muK1*muQ1*( 0 - x )/( 1 + 2*muQ1 )) ) )^(1/muQ1), NA),
          col="black", lwd=3, lty=1)

  # child 1
  lines(x = x,
        y = ifelse( x > muI1, ( 2*muH2/muK2 * ( 1 - exp(muK2*muQ2*( muI1 - x )/( 1 + 2*muQ2 )) ) )^(1/muQ2), NA),
        col="black", lwd=3, lty=1)

  # child 2
  lines(x = x,
        y = ifelse( x > muI2, ( 2*muH3/muK3 * ( 1 - exp(muK3*muQ3*( muI2 - x )/( 1 + 2*muQ3 )) ) )^(1/muQ3), NA),
        col="black", lwd=3, lty=1)

  # child 3
  lines(x = x,
        y = ifelse( x > muI3, ( 2*muH4/muK4 * ( 1 - exp(muK4*muQ4*( muI3 - x )/( 1 + 2*muQ4 )) ) )^(1/muQ4), NA),
        col="black", lwd=3, lty=1)

  # adolescent
  lines(x = x,
        y = ifelse( x > muI4, ( 2*muH5/muK5 * ( 1 - exp(muK5*muQ5*( muI4 - x )/( 1 + 2*muQ5 )) ) )^(1/muQ5), NA),
        col="black", lwd=3, lty=1)





  # plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=2 ), las=2 )
  par(xpd=FALSE) # don't plot beyond the axes
  
  # first derivative from mathematica
  y_prime = ifelse( x < muI1, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( 0    - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( 0    - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ),

            ifelse( x < muI2, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( 0    - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( 0    - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                              1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI1 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI1 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ),

            ifelse( x < muI3, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( 0    - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( 0    - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                              1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI1 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI1 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                              1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI2 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI2 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ),

            ifelse( x < muI4, 1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( 0    - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( 0    - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                              1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI1 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI1 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                              1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI2 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI2 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ) +
                              1/( 1 + 2*muQ4 )*2^(1/muQ4)*muH4*exp((muK4*muQ4*( muI3 - x ))/( 1 + 2*muQ4 ))*(muH4/muK4*( 1 - exp((muK4*muQ4*( muI3 - x ))/( 1 + 2*muQ4 )) ))^( 1/muQ4 - 1 ),

                              1/( 1 + 2*muQ1 )*2^(1/muQ1)*muH1*exp((muK1*muQ1*( 0    - x ))/( 1 + 2*muQ1 ))*(muH1/muK1*( 1 - exp((muK1*muQ1*( 0    - x ))/( 1 + 2*muQ1 )) ))^( 1/muQ1 - 1 ) +
                              1/( 1 + 2*muQ2 )*2^(1/muQ2)*muH2*exp((muK2*muQ2*( muI1 - x ))/( 1 + 2*muQ2 ))*(muH2/muK2*( 1 - exp((muK2*muQ2*( muI1 - x ))/( 1 + 2*muQ2 )) ))^( 1/muQ2 - 1 ) +
                              1/( 1 + 2*muQ3 )*2^(1/muQ3)*muH3*exp((muK3*muQ3*( muI2 - x ))/( 1 + 2*muQ3 ))*(muH3/muK3*( 1 - exp((muK3*muQ3*( muI2 - x ))/( 1 + 2*muQ3 )) ))^( 1/muQ3 - 1 ) +
                              1/( 1 + 2*muQ4 )*2^(1/muQ4)*muH4*exp((muK4*muQ4*( muI3 - x ))/( 1 + 2*muQ4 ))*(muH4/muK4*( 1 - exp((muK4*muQ4*( muI3 - x ))/( 1 + 2*muQ4 )) ))^( 1/muQ4 - 1 ) +
                              1/( 1 + 2*muQ5 )*2^(1/muQ5)*muH5*exp((muK5*muQ5*( muI4 - x ))/( 1 + 2*muQ5 ))*(muH5/muK5*( 1 - exp((muK5*muQ5*( muI4 - x ))/( 1 + 2*muQ5 )) ))^( 1/muQ5 - 1 )

            ) ) ) )
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


symbol.Female(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()


