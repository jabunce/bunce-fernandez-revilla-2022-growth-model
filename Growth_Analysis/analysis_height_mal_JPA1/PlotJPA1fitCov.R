

################## m8 JPA1 model for Berkeley and Matsigenka, covarying random effects for individual

post <- post8



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






pdf(file="./Plots/JPA1_model_sample_mat_ber_randeff_cov.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (years)", ylab="Height (cm)" )



  #Ber.rows <- which( Com.mal$Ethnicity == 1 & Com.mal$TotAge == 0.1 ) # pick out one row corresponding to each indiv
  #Mat.rows <- which( Com.mal$Ethnicity == 2 & Com.mal$TotAge == 0.1 )


  # plot individual posterior trajectories for U.S.
  for ( j in 1:J ){

    if ( EthID[j,2] == 1 ) {            # if Berkeley

      A  <- pull(post, paste("A[", j, "]", sep="")) #pull is a command from package dplyr to extract a vector from a tibble
      C1 <- pull(post, paste("C1[", j, "]", sep=""))
      C2 <- pull(post, paste("C2[", j, "]", sep=""))
      C3 <- pull(post, paste("C3[", j, "]", sep=""))
      D1 <- pull(post, paste("D1[", j, "]", sep=""))
      D2 <- pull(post, paste("D2[", j, "]", sep=""))
      D3 <- pull(post, paste("D3[", j, "]", sep=""))


      lines(x = x,
            y = mean(A)*exp( -1/( mean(C1)*log( x/mean(D1) + 1 ) + (x/mean(D2))^mean(C2) + (x/mean(D3))^mean(C3) ) ),
            col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)


    } else {            # if Matsigenka

      A  <- pull(post, paste("A[", j, "]", sep=""))
      C1 <- pull(post, paste("C1[", j, "]", sep=""))
      C2 <- pull(post, paste("C2[", j, "]", sep=""))
      C3 <- pull(post, paste("C3[", j, "]", sep=""))
      D1 <- pull(post, paste("D1[", j, "]", sep=""))
      D2 <- pull(post, paste("D2[", j, "]", sep=""))
      D3 <- pull(post, paste("D3[", j, "]", sep=""))


      lines(x = x,
            y = mean(A)*exp( -1/( mean(C1)*log( x/mean(D1) + 1 ) + (x/mean(D2))^mean(C2) + (x/mean(D3))^mean(C3) ) ),
            col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

    } # else

  } #for j



  # plot measurment points
  points(x=Ber.mal[which(Ber.mal$TotAge > 0),"TotAge"], y=Ber.mal[which(Ber.mal$TotAge > 0),"Height"], pch=BerPoint_pch, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex)
  points(x=Mat.mal[which(Mat.mal$TotAge > 0),"TotAge"], y=Mat.mal[which(Mat.mal$TotAge > 0),"Height"], pch=MatPoint_pch, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex)



  # plot U.S. posterior mean trajectory
  post <- post8

  berA = mean(post$berA)
  berC1 = mean(post$berC1)
  berC2 = mean(post$berC2)
  berC3 = mean(post$berC3)
  berD1 = mean(post$berD1)
  berD2 = mean(post$berD2)
  berD3 = mean(post$berD3)



  lines(x = x,
        y = berA*exp( -1/( berC1*log( x/berD1 + 1 ) + (x/berD2)^berC2 + (x/berD3)^berC3 ) ),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


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
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty) 

 


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

  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)

  # matsi posterior mean velocity
  y_primeMat <- matA*exp( -1/( matC1*log( x/matD1 + 1 ) + (x/matD2)^matC2 + (x/matD3)^matC3 ) )*
                ( matC1*x + ( matD1 + x )*( matC2*(x/matD2)^matC2 + matC3*(x/matD3)^matC3 ) )*
                1/( x*( matD1 + x )*( (x/matD2)^matC2 + (x/matD3)^matC3 + matC1*log( 1 + x/matD1 ) )^2 )

  lines(x = x, y=y_primeMat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)


  par(xpd=TRUE)
  text("Growth velocity (cm/year)", x=34, y=8, srt=270, las=3)

  legend(-1, 16.65,
         legend=c("U.S. data","Matsigenka",
                  "Estimated U.S. individual trajectories", "Matsigenka",
                 "Estimated U.S. mean trajectory","Matsigenka",
                 "Estimated U.S. velocity","Matsigenka"
                 ), bty="o", bg="white",
         col=c(BerPoint_col, MatPoint_col,
                BerIndivTraj_col,MatIndivTraj_col,
                BerMeanTraj_col,MatMeanTraj_col,
                BerVelTraj_col,MatVelTraj_col),
         merge=FALSE,
         pch=c(BerPoint_pch, MatPoint_pch, NA,NA, NA,NA, NA,NA),
         lty=c(0,0, BerIndivTraj_lty,MatIndivTraj_lty, BerMeanTraj_lty,MatMeanTraj_lty, BerVelTraj_lty,MatVelTraj_lty),
         lwd=c(BerPoint_lwd,MatPoint_lwd, BerIndivTraj_lwd,MatIndivTraj_lwd, BerMeanTraj_lwd,MatMeanTraj_lwd, BerVelTraj_lwd,MatVelTraj_lwd), cex=0.45, seg.len=4)

symbol.Male(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


graphics.off()


