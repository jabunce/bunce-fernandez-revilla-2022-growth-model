
################## m4 composite model using high measurement density CDC trajectories, covarying random effects for individual

post <- post2

pdf(file="./Plots/Composite_Berkeley_m2.pdf",
    height=5, width=5)

  
  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,100), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Weight (kg)" )


  Ber.rows <- which( Com.fem$Ethnicity == 1 & Com.fem$TotAge < 0.1 ) # pick put one row corresponding to each indiv, other rows for each indiv have dublicate posteriors for Q, H, and K 

  # plot individual posterior trajectories
  for ( z in 1:length(Ber.rows) ){

    K1 <- pull(post, paste("K1[", Ber.rows[z], "]", sep="")) #pull is a command from package dplyr to extract a vector from a tibble
    K2 <- pull(post, paste("K2[", Ber.rows[z], "]", sep=""))
    K3 <- pull(post, paste("K3[", Ber.rows[z], "]", sep=""))
    Q1 <- pull(post, paste("Q1[", Ber.rows[z], "]", sep=""))
    Q2 <- pull(post, paste("Q2[", Ber.rows[z], "]", sep=""))
    Q3 <- pull(post, paste("Q3[", Ber.rows[z], "]", sep=""))
    H1 <- pull(post, paste("H1[", Ber.rows[z], "]", sep=""))
    H2 <- pull(post, paste("H2[", Ber.rows[z], "]", sep=""))
    H3 <- pull(post, paste("H3[", Ber.rows[z], "]", sep=""))

    lines(x = x,
          y=( -1*exp(-1*mean(K1)*mean(Q1)*x/( 1 + 2*mean(Q1) )) + 2*mean(H1)/mean(K1)*pi^(mean(Q1)/( 1 + 2*mean(Q1) )) )^(( 1 + 2*mean(Q1) )/mean(Q1)) +
            ( -1*exp(-1*mean(K2)*mean(Q2)*x/( 1 + 2*mean(Q2) )) + 2*mean(H2)/mean(K2)*pi^(mean(Q2)/( 1 + 2*mean(Q2) )) )^(( 1 + 2*mean(Q2) )/mean(Q2)) +
            ( -1*exp(-1*mean(K3)*mean(Q3)*x/( 1 + 2*mean(Q3) )) + 2*mean(H3)/mean(K3)*pi^(mean(Q3)/( 1 + 2*mean(Q3) )) )^(( 1 + 2*mean(Q3) )/mean(Q3)),
          col=grey(0.5), lwd=0.25, lty=1)
    
  } #for z


  # plot measurement points
  points(x=Ber.fem$TotAge, y=Ber.fem$Weight, lwd=1, col="red", cex=0.1)

  # plot baseline mean trajectory. This is the prior.
  post <- data_list2

  Q1hat = exp(mean(post$LQ1hat))
  Q2hat = exp(mean(post$LQ2hat))
  Q3hat = exp(mean(post$LQ3hat))
  K1hat = exp(mean(post$LK1hat))
  K2hat = exp(mean(post$LK2hat))
  K3hat = exp(mean(post$LK3hat))
  H1hat = K1hat/2 + mean(post$R1hat)
  H2hat = K2hat/2 + mean(post$R2hat)
  H3hat = K3hat/2 + mean(post$R3hat)

  lines(x = x,
        y = ( -1*exp(-1*K1hat*Q1hat*x/( 1 + 2*Q1hat )) + 2*H1hat/K1hat*pi^(Q1hat/( 1 + 2*Q1hat )) )^(( 1 + 2*Q1hat )/Q1hat) +
            ( -1*exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat )) + 2*H2hat/K2hat*pi^(Q2hat/( 1 + 2*Q2hat )) )^(( 1 + 2*Q2hat )/Q2hat) +
            ( -1*exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat )) + 2*H3hat/K3hat*pi^(Q3hat/( 1 + 2*Q3hat )) )^(( 1 + 2*Q3hat )/Q3hat),
        col="pink", lwd=3, lty=1)

  # plot posterior mean trajectory
  post <- post2

  mQ1 = exp(mean(post$mLQ1))
  mQ2 = exp(mean(post$mLQ2))
  mQ3 = exp(mean(post$mLQ3))
  mK1 = exp(mean(post$mLK1))
  mK2 = exp(mean(post$mLK2))
  mK3 = exp(mean(post$mLK3))
  mH1 = mK1/2 + mean(post$mR1)
  mH2 = mK2/2 + mean(post$mR2)
  mH3 = mK3/2 + mean(post$mR3)

  lines(x = x,
        y = ( -1*exp(-1*mK1*mQ1*x/( 1 + 2*mQ1 )) + 2*mH1/mK1*pi^(mQ1/( 1 + 2*mQ1 )) )^(( 1 + 2*mQ1 )/mQ1) +
            ( -1*exp(-1*mK2*mQ2*x/( 1 + 2*mQ2 )) + 2*mH2/mK2*pi^(mQ2/( 1 + 2*mQ2 )) )^(( 1 + 2*mQ2 )/mQ2) +
            ( -1*exp(-1*mK3*mQ3*x/( 1 + 2*mQ3 )) + 2*mH3/mK3*pi^(mQ3/( 1 + 2*mQ3 )) )^(( 1 + 2*mQ3 )/mQ3),
        col="black", lwd=3, lty=1)


  # plot individual component functions

  # infant
  x2 <- seq(from=(0), to=(25+0.75), by=0.1)
  post <- data_list2 # prior
  lines(x = x2,
        y = ( -1*exp(-1*K1hat*Q1hat*x2/( 1 + 2*Q1hat )) + 2*H1hat/K1hat*pi^(Q1hat/( 1 + 2*Q1hat )) )^(( 1 + 2*Q1hat )/Q1hat),
        col="pink", lwd=3, lty=1)
  post <- post2
  lines(x = x2,
        y = ( -1*exp(-1*mK1*mQ1*x2/( 1 + 2*mQ1 )) + 2*mH1/mK1*pi^(mQ1/( 1 + 2*mQ1 )) )^(( 1 + 2*mQ1 )/mQ1),
        col="black", lwd=3, lty=1)

  # child
  post <- data_list2 # prior
  lines(x = x,
        y = ( -1*exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat )) + 2*H2hat/K2hat*pi^(Q2hat/( 1 + 2*Q2hat )) )^(( 1 + 2*Q2hat )/Q2hat),
        col="pink", lwd=3, lty=1)
  post <- post2
  lines(x = x,
        y = ( -1*exp(-1*mK2*mQ2*x/( 1 + 2*mQ2 )) + 2*mH2/mK2*pi^(mQ2/( 1 + 2*mQ2 )) )^(( 1 + 2*mQ2 )/mQ2),
        col="black", lwd=3, lty=1)

  # adolescent
  post <- data_list2 # prior
  lines(x = x,
        y = ( -1*exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat )) + 2*H3hat/K3hat*pi^(Q3hat/( 1 + 2*Q3hat )) )^(( 1 + 2*Q3hat )/Q3hat),
        col="pink", lwd=3, lty=1)
  post <- post2
  lines(x = x,
        y = ( -1*exp(-1*mK3*mQ3*x/( 1 + 2*mQ3 )) + 2*mH3/mK3*pi^(mQ3/( 1 + 2*mQ3 )) )^(( 1 + 2*mQ3 )/mQ3),
        col="black", lwd=3, lty=1)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=2 ), las=2 )
  par(xpd=TRUE)
  text("Growth velocity (kg/year)", x=32, y=8, srt=270, las=3)

  # reference velocity (Tsimane boys). This is the prior.
  post <- data_list2
  y_prime <- exp(-1*K1hat*Q1hat*x/( 1 + 2*Q1hat ))*K1hat*( -1*exp(-1*K1hat*Q1hat*x/( 1 + 2*Q1hat )) + 2*H1hat/K1hat*pi^(Q1hat/( 1 + 2*Q1hat )) )^( 1 + 1/Q1hat ) + 
             exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat ))*K2hat*( -1*exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat )) + 2*H2hat/K2hat*pi^(Q2hat/( 1 + 2*Q2hat )) )^( 1 + 1/Q2hat ) +
             exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat ))*K3hat*( -1*exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat )) + 2*H3hat/K3hat*pi^(Q3hat/( 1 + 2*Q3hat )) )^( 1 + 1/Q3hat )
  lines(x = x, y=y_prime, col="pink", lwd=3, lty=1)

  # posterior mean velocity
  post <- post2
  y_primem <- exp(-1*mK1*mQ1*x/( 1 + 2*mQ1 ))*mK1*( -1*exp(-1*mK1*mQ1*x/( 1 + 2*mQ1 )) + 2*mH1/mK1*pi^(mQ1/( 1 + 2*mQ1 )) )^( 1 + 1/mQ1 ) + 
                exp(-1*mK2*mQ2*x/( 1 + 2*mQ2 ))*mK2*( -1*exp(-1*mK2*mQ2*x/( 1 + 2*mQ2 )) + 2*mH2/mK2*pi^(mQ2/( 1 + 2*mQ2 )) )^( 1 + 1/mQ2 ) +
                exp(-1*mK3*mQ3*x/( 1 + 2*mQ3 ))*mK3*( -1*exp(-1*mK3*mQ3*x/( 1 + 2*mQ3 )) + 2*mH3/mK3*pi^(mQ3/( 1 + 2*mQ3 )) )^( 1 + 1/mQ3 )
  lines(x = x, y=y_primem, col="black", lwd=3, lty=1)


graphics.off()



