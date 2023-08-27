

################## m7 JPA1 model for Berkeley and Matsigenka, covarying random effects for individual

post <- post7

pdf(file="./Plots/JPA1_model_sample_mat_ber_randeff_cov.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )

  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=0.5, col="red", cex=0.5)
  points(x=Mat.mal$TotAge, y=Mat.mal$Height, lwd=0.5, col="blue", cex=0.5)


  Ber.rows <- which( Com.mal$Ethnicity == 1 & Com.mal$TotAge < 0.1 ) # pick put one row corresponding to each indiv, other rows for each indiv have dublicate posteriors for Q, H, and K 
  Mat.rows <- which( Com.mal$Ethnicity == 2 & Com.mal$TotAge < 0.1 )


  # plot individual posterior trajectories for U.S.
  for ( z in 1:length(Ber.rows) ){

    A  <- pull(post, paste("A[", Ber.rows[z], "]", sep="")) #pull is a command from package dplyr to extract a vector from a tibble
    D1 <- pull(post, paste("D1[", Ber.rows[z], "]", sep=""))
    D2 <- pull(post, paste("D2[", Ber.rows[z], "]", sep=""))
    D3 <- pull(post, paste("D3[", Ber.rows[z], "]", sep=""))
    C1 <- pull(post, paste("C1[", Ber.rows[z], "]", sep=""))
    C2 <- pull(post, paste("C1[", Ber.rows[z], "]", sep=""))
    C3 <- pull(post, paste("C3[", Ber.rows[z], "]", sep=""))

    lines(x = x,
          y = mean(A)*exp( -1/( mean(C1)*log( x/mean(D1) + 1 ) + (x/mean(D2))^mean(C2) + (x/mean(D3))^mean(C3) ) ),
          col=rgb(1,0,0,max=1,alpha=0.5), lwd=0.25, lty=1)
  } #for z


  # plot individual posterior trajectories for Matsigenka
  for ( z in 1:length(Mat.rows) ){

    A  <- pull(post, paste("A[", Mat.rows[z], "]", sep="")) #pull is a command from package dplyr to extract a vector from a tibble
    D1 <- pull(post, paste("D1[", Mat.rows[z], "]", sep=""))
    D2 <- pull(post, paste("D2[", Mat.rows[z], "]", sep=""))
    D3 <- pull(post, paste("D3[", Mat.rows[z], "]", sep=""))
    C1 <- pull(post, paste("C1[", Mat.rows[z], "]", sep=""))
    C2 <- pull(post, paste("C1[", Mat.rows[z], "]", sep=""))
    C3 <- pull(post, paste("C3[", Mat.rows[z], "]", sep=""))

    lines(x = x,
          y = mean(A)*exp( -1/( mean(C1)*log( x/mean(D1) + 1 ) + (x/mean(D2))^mean(C2) + (x/mean(D3))^mean(C3) ) ),
          col=rgb(0,0,1,max=1,alpha=0.5), lwd=0.25, lty=1)
  } #for z



  # plot U.S. posterior mean trajectory
  post <- post7

  berA = mean(post$berA)
  berD1 = mean(post$berD1)
  berD2 = mean(post$berD2)
  berD3 = mean(post$berD3)
  berC1 = mean(post$berC1)
  berC2 = mean(post$berC2)
  berC3 = mean(post$berC3)


  lines(x = x,
        y = berA*exp( -1/( berC1*log( x/berD1 + 1 ) + (x/berD2)^berC2 + (x/berD3)^berC3 ) ),
        col="red", lwd=3, lty=1)


  # plot Matsigenka posterior mean trajectory
  matA = mean(post$matA)
  matD1 = mean(post$matD1)
  matD2 = mean(post$matD2)
  matD3 = mean(post$matD3)
  matC1 = mean(post$matC1)
  matC2 = mean(post$matC2)
  matC3 = mean(post$matC3)

  lines(x = x,
        y = matA*exp( -1/( matC1*log( x/matD1 + 1 ) + (x/matD2)^matC2 + (x/matD3)^matC3 ) ),
        col="blue", lwd=3, lty=1) 

 


  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=2 ), las=2 )
  par(xpd=FALSE)


  # ber posterior mean velocity
  y_primeBer <- berA*exp( -1/( berC1*log( x/berD1 + 1 ) + (x/berD2)^berC2 + (x/berD3)^berC3 ) )*
                ( berC1*x + ( berD1 + x )*( berC2*(x/berD2)^berC2 + berC3*(x/berD3)^berC3 ) )*
                1/( x*( berD1 + x )*( (x/berD2)^berC2 + (x/berD3)^berC3 + berC1*log( 1 + x/berD1 ) )^2 )

  lines(x = x, y=y_primeBer, col="orange", lwd=2, lty=1)

  # matsi posterior mean velocity
  y_primemat <- matA*exp( -1/( matC1*log( x/matD1 + 1 ) + (x/matD2)^matC2 + (x/matD3)^matC3 ) )*
                ( matC1*x + ( matD1 + x )*( matC2*(x/matD2)^matC2 + matC3*(x/matD3)^matC3 ) )*
                1/( x*( matD1 + x )*( (x/matD2)^matC2 + (x/matD3)^matC3 + matC1*log( 1 + x/matD1 ) )^2 )

  lines(x = x, y=y_primemat, col="cyan", lwd=2, lty=1)


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
                "red", "blue",
                "orange", "cyan"),
         merge=FALSE,
         pch=c(1,1, NA,NA, NA,NA, NA,NA),
         lty=c(0,0, 1,1, 1,1, 1,1),
         lwd=c(0.5,0.5, 0.25,0.25, 3,3, 2,2), cex=0.45, seg.len=4)

symbol.Male(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()


