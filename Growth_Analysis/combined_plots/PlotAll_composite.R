

######################################### Fig 3 #########################################


######## load posteriors from complete models (m3), need to run PrepareData to plot raw data points

# male height
post3_mh <- readRDS("./analysis_height_mal/post3.RDS")
#str(post3_mh)

# male weight
post3_mw <- readRDS("./analysis_weight_mal/post3.RDS")

# female height
post3_fh <- readRDS("./analysis_height_fem/post3.RDS")
#str(post3_mh)

# female weight
post3_fw <- readRDS("./analysis_weight_fem/post3.RDS")




######## combine trajectory plots

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



pdf(file="./combined_plots/Plots/Composite_model_combined.pdf",
    height=10, width=10)
layout( mat=matrix(c(1,2,3,4),2,2,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
#layout.show(4)
#par(xpd=TRUE) # clip at figure region, not plot region
#par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 5, 5)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  x <- seq(from=(0), to=(26+0.75), by=0.1)        # sequence for plotting functions, include gestation in age


  ################ female height
  post <- post3_fh

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=seq( 0, 200, by=50), labels=TRUE )

  Ber.rows <- which( Com.fem$Ethnicity == 1 & Com.fem$TotAge < 0.1 ) # pick put one row corresponding to each indiv, other rows for each indiv have dublicate posteriors for Q, H, and K 
  Mat.rows <- which( Com.fem$Ethnicity == 2 & Com.fem$TotAge < 0.1 )


  # plot individual posterior trajectories for U.S.
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
          y=( -1*exp(-1*mean(K1)*mean(Q1)*x/( 1 + 2*mean(Q1) )) + 2*mean(H1)/mean(K1) )^(1/mean(Q1)) +
            ( -1*exp(-1*mean(K2)*mean(Q2)*x/( 1 + 2*mean(Q2) )) + 2*mean(H2)/mean(K2) )^(1/mean(Q2)) +
            ( -1*exp(-1*mean(K3)*mean(Q3)*x/( 1 + 2*mean(Q3) )) + 2*mean(H3)/mean(K3) )^(1/mean(Q3)),
          col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)
  } #for z


  # plot individual posterior trajectories for Matsigenka
  for ( z in 1:length(Mat.rows) ){

    K1 <- pull(post, paste("K1[", Mat.rows[z], "]", sep="")) #pull is a command from package dplyr to extract a vector from a tibble
    K2 <- pull(post, paste("K2[", Mat.rows[z], "]", sep=""))
    K3 <- pull(post, paste("K3[", Mat.rows[z], "]", sep=""))
    Q1 <- pull(post, paste("Q1[", Mat.rows[z], "]", sep=""))
    Q2 <- pull(post, paste("Q2[", Mat.rows[z], "]", sep=""))
    Q3 <- pull(post, paste("Q3[", Mat.rows[z], "]", sep=""))
    H1 <- pull(post, paste("H1[", Mat.rows[z], "]", sep=""))
    H2 <- pull(post, paste("H2[", Mat.rows[z], "]", sep=""))
    H3 <- pull(post, paste("H3[", Mat.rows[z], "]", sep=""))

    lines(x = x,
          y=( -1*exp(-1*mean(K1)*mean(Q1)*x/( 1 + 2*mean(Q1) )) + 2*mean(H1)/mean(K1) )^(1/mean(Q1)) +
            ( -1*exp(-1*mean(K2)*mean(Q2)*x/( 1 + 2*mean(Q2) )) + 2*mean(H2)/mean(K2) )^(1/mean(Q2)) +
            ( -1*exp(-1*mean(K3)*mean(Q3)*x/( 1 + 2*mean(Q3) )) + 2*mean(H3)/mean(K3) )^(1/mean(Q3)),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)
  } #for z


  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$Height, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.fem$TotAge, y=Mat.fem$Height, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)


  # plot U.S. posterior mean trajectory
  berQ1 = exp(mean(post$berLQ1))
  berQ2 = exp(mean(post$berLQ2))
  berQ3 = exp(mean(post$berLQ3))
  berK1 = exp(mean(post$berLK1))
  berK2 = exp(mean(post$berLK2))
  berK3 = exp(mean(post$berLK3))
  berH1 = berK1/2 + mean(post$berR1)
  berH2 = berK2/2 + mean(post$berR2)
  berH3 = berK3/2 + mean(post$berR3)

  lines(x = x,
        y = ( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1 )^(1/berQ1) +
            ( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2 )^(1/berQ2) +
            ( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3 )^(1/berQ3),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot Matsigenka posterior mean trajectory
  matQ1 = exp(mean(post$matLQ1))
  matQ2 = exp(mean(post$matLQ2))
  matQ3 = exp(mean(post$matLQ3))
  matK1 = exp(mean(post$matLK1))
  matK2 = exp(mean(post$matLK2))
  matK3 = exp(mean(post$matLK3))
  matH1 = matK1/2 + mean(post$matR1)
  matH2 = matK2/2 + mean(post$matR2)
  matH3 = matK3/2 + mean(post$matR3)

  lines(x = x,
        y = ( -1*exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 )) + 2*matH1/matK1 )^(1/matQ1) +
            ( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2 )^(1/matQ2) +
            ( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3 )^(1/matQ3),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)

 


  # plot individual component functions

  # infant
  lines(x = x,
        y = ( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1 )^(1/berQ1),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x,
        y = ( -1*exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 )) + 2*matH1/matK1 )^(1/matQ1),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # child
  lines(x = x,
        y = ( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2 )^(1/berQ2),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x,
        y = ( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2 )^(1/matQ2),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # adolescent
  lines(x = x,
        y = ( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3 )^(1/berQ3),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x,
        y = ( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3 )^(1/matQ3),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=4 ), labels=FALSE, las=2 )
  par(xpd=FALSE) # clip to plot region


  # ber posterior mean velocity
  y_primeBer <- 1/(1+2*berQ1)*berK1*exp((-1*berK1*berQ1*x)/(1+2*berQ1))*( -1*exp((-1*berK1*berQ1*x)/(1+2*berQ1)) + 2*berH1/berK1 )^(1/berQ1-1) + 
                1/(1+2*berQ1)*berK2*exp((-1*berK2*berQ2*x)/(1+2*berQ2))*( -1*exp((-1*berK2*berQ2*x)/(1+2*berQ2)) + 2*berH2/berK2 )^(1/berQ2-1) +
                1/(1+2*berQ1)*berK3*exp((-1*berK3*berQ3*x)/(1+2*berQ3))*( -1*exp((-1*berK3*berQ3*x)/(1+2*berQ3)) + 2*berH3/berK3 )^(1/berQ3-1)

  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)

  # matsi posterior mean velocity
  y_primemat <- 1/(1+2*matQ1)*matK1*exp((-1*matK1*matQ1*x)/(1+2*matQ1))*( -1*exp((-1*matK1*matQ1*x)/(1+2*matQ1)) + 2*matH1/matK1 )^(1/matQ1-1) + 
                1/(1+2*matQ1)*matK2*exp((-1*matK2*matQ2*x)/(1+2*matQ2))*( -1*exp((-1*matK2*matQ2*x)/(1+2*matQ2)) + 2*matH2/matK2 )^(1/matQ2-1) +
                1/(1+2*matQ1)*matK3*exp((-1*matK3*matQ3*x)/(1+2*matQ3))*( -1*exp((-1*matK3*matQ3*x)/(1+2*matQ3)) + 2*matH3/matK3 )^(1/matQ3-1)

  lines(x = x, y=y_primemat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)


  par(xpd=NA) # clip plotting to device region
  

  # set horizontal spacing for legend text
  legtext <- c("Data",
               "Estimated individual trajectories", 
               "Estimated mean trajectory",
               "Estimated mean velocity"
               )
  xcoords <- c(0,
               4,
               19.5,
               32
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-13.6, y=21,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c("white",
               "white",
               "white", 
               "white"
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd,
               BerMeanTraj_lwd,
               BerVelTraj_lwd
               ),
         seg.len=4 )



  # set horizontal spacing for legend text
  # legtext <- c("U.S. data",
  #              "Estimated U.S. individual trajectories", 
  #              "Estimated U.S. mean trajectory",
  #              "Estimated U.S. mean velocity"
  #              )
  legtext <- c("U.S.",
               "U.S.", 
               "U.S.",
               "U.S."
               )
  xcoords <- c(0,
               8.5,  #5.5
               24.2, #23
               36.8  #38
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-12, y=20,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(BerPoint_col,
               BerIndivTraj_col,
               BerMeanTraj_col, 
               BerVelTraj_col
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty,
               BerMeanTraj_lty,
               BerVelTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd,
               BerMeanTraj_lwd,
               BerVelTraj_lwd
               ),
         seg.len=4 )


  # set horizontal spacing for legend text
  # legtext <- c("Matsigenka",
  #              "Est. Matsigenka indiv. traj.",
  #              "Est. Matsigenka mean traj.",
  #              "Est. Matsigenka mean vel."
  #              )
  legtext <-c("Matsigenka",
               "Matsigenka",
               "Matsigenka",
               "Matsigenka"
               )
  xcoords <- c(0,
               8.5,
               24.2,
               36.8
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)


  legend(x=-12, y=19,
         ncol=4,
         cex=1.3,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(MatPoint_col,
               MatIndivTraj_col,
               MatMeanTraj_col,
               MatVelTraj_col
              ),
         merge=FALSE,
         pch=c(MatPoint_pch,
               NA, 
               NA, 
               NA
               ),
         lty=c(0,
               MatIndivTraj_lty,
               MatMeanTraj_lty,
               MatVelTraj_lty
               ),
         lwd=c(MatPoint_lwd,
               MatIndivTraj_lwd,
               MatMeanTraj_lwd,
               MatVelTraj_lwd
               ),
         seg.len=4 )

  rect(xleft = -7.5,
       ybottom = 17.3,
       xright = 62,
       ytop = 20.9,
       lwd=1)

  symbol.Female(centerx = 3, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package




  ################ male height
  post <- post3_mh

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=FALSE )
  axis( side=2, at=seq( 0, 200, by=50), labels=FALSE )
  par(xpd=FALSE) # clip to plot region


  Ber.rows <- which( Com.mal$Ethnicity == 1 & Com.mal$TotAge < 0.1 ) # pick put one row corresponding to each indiv, other rows for each indiv have dublicate posteriors for Q, H, and K 
  Mat.rows <- which( Com.mal$Ethnicity == 2 & Com.mal$TotAge < 0.1 )


  # plot individual posterior trajectories for U.S.
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
          y=( -1*exp(-1*mean(K1)*mean(Q1)*x/( 1 + 2*mean(Q1) )) + 2*mean(H1)/mean(K1) )^(1/mean(Q1)) +
            ( -1*exp(-1*mean(K2)*mean(Q2)*x/( 1 + 2*mean(Q2) )) + 2*mean(H2)/mean(K2) )^(1/mean(Q2)) +
            ( -1*exp(-1*mean(K3)*mean(Q3)*x/( 1 + 2*mean(Q3) )) + 2*mean(H3)/mean(K3) )^(1/mean(Q3)),
          col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)
  } #for z


  # plot individual posterior trajectories for Matsigenka
  for ( z in 1:length(Mat.rows) ){

    K1 <- pull(post, paste("K1[", Mat.rows[z], "]", sep="")) #pull is a command from package dplyr to extract a vector from a tibble
    K2 <- pull(post, paste("K2[", Mat.rows[z], "]", sep=""))
    K3 <- pull(post, paste("K3[", Mat.rows[z], "]", sep=""))
    Q1 <- pull(post, paste("Q1[", Mat.rows[z], "]", sep=""))
    Q2 <- pull(post, paste("Q2[", Mat.rows[z], "]", sep=""))
    Q3 <- pull(post, paste("Q3[", Mat.rows[z], "]", sep=""))
    H1 <- pull(post, paste("H1[", Mat.rows[z], "]", sep=""))
    H2 <- pull(post, paste("H2[", Mat.rows[z], "]", sep=""))
    H3 <- pull(post, paste("H3[", Mat.rows[z], "]", sep=""))

    lines(x = x,
          y=( -1*exp(-1*mean(K1)*mean(Q1)*x/( 1 + 2*mean(Q1) )) + 2*mean(H1)/mean(K1) )^(1/mean(Q1)) +
            ( -1*exp(-1*mean(K2)*mean(Q2)*x/( 1 + 2*mean(Q2) )) + 2*mean(H2)/mean(K2) )^(1/mean(Q2)) +
            ( -1*exp(-1*mean(K3)*mean(Q3)*x/( 1 + 2*mean(Q3) )) + 2*mean(H3)/mean(K3) )^(1/mean(Q3)),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)
  } #for z


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Height, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.mal$TotAge, y=Mat.mal$Height, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)


  # plot U.S. posterior mean trajectory
  berQ1 = exp(mean(post$berLQ1))
  berQ2 = exp(mean(post$berLQ2))
  berQ3 = exp(mean(post$berLQ3))
  berK1 = exp(mean(post$berLK1))
  berK2 = exp(mean(post$berLK2))
  berK3 = exp(mean(post$berLK3))
  berH1 = berK1/2 + mean(post$berR1)
  berH2 = berK2/2 + mean(post$berR2)
  berH3 = berK3/2 + mean(post$berR3)

  lines(x = x,
        y = ( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1 )^(1/berQ1) +
            ( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2 )^(1/berQ2) +
            ( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3 )^(1/berQ3),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot Matsigenka posterior mean trajectory
  matQ1 = exp(mean(post$matLQ1))
  matQ2 = exp(mean(post$matLQ2))
  matQ3 = exp(mean(post$matLQ3))
  matK1 = exp(mean(post$matLK1))
  matK2 = exp(mean(post$matLK2))
  matK3 = exp(mean(post$matLK3))
  matH1 = matK1/2 + mean(post$matR1)
  matH2 = matK2/2 + mean(post$matR2)
  matH3 = matK3/2 + mean(post$matR3)

  lines(x = x,
        y = ( -1*exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 )) + 2*matH1/matK1 )^(1/matQ1) +
            ( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2 )^(1/matQ2) +
            ( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3 )^(1/matQ3),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty) 

 


  # plot individual component functions

  # infant
  lines(x = x,
        y = ( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1 )^(1/berQ1),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x,
        y = ( -1*exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 )) + 2*matH1/matK1 )^(1/matQ1),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty) 


  # child
  lines(x = x,
        y = ( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2 )^(1/berQ2),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x,
        y = ( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2 )^(1/matQ2),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # adolescent
  lines(x = x,
        y = ( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3 )^(1/berQ3),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x,
        y = ( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3 )^(1/matQ3),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=4 ), las=2, labels=FALSE )
  text( x=28.75, y=seq( 0, 16, by=4 ), labels=seq( 0, 16, by=4 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE)


  # ber posterior mean velocity
  y_primeBer <- 1/(1+2*berQ1)*berK1*exp((-1*berK1*berQ1*x)/(1+2*berQ1))*( -1*exp((-1*berK1*berQ1*x)/(1+2*berQ1)) + 2*berH1/berK1 )^(1/berQ1-1) + 
                1/(1+2*berQ1)*berK2*exp((-1*berK2*berQ2*x)/(1+2*berQ2))*( -1*exp((-1*berK2*berQ2*x)/(1+2*berQ2)) + 2*berH2/berK2 )^(1/berQ2-1) +
                1/(1+2*berQ1)*berK3*exp((-1*berK3*berQ3*x)/(1+2*berQ3))*( -1*exp((-1*berK3*berQ3*x)/(1+2*berQ3)) + 2*berH3/berK3 )^(1/berQ3-1)

  lines(x = x, y=y_primeBer, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)

  # matsi posterior mean velocity
  y_primemat <- 1/(1+2*matQ1)*matK1*exp((-1*matK1*matQ1*x)/(1+2*matQ1))*( -1*exp((-1*matK1*matQ1*x)/(1+2*matQ1)) + 2*matH1/matK1 )^(1/matQ1-1) + 
                1/(1+2*matQ1)*matK2*exp((-1*matK2*matQ2*x)/(1+2*matQ2))*( -1*exp((-1*matK2*matQ2*x)/(1+2*matQ2)) + 2*matH2/matK2 )^(1/matQ2-1) +
                1/(1+2*matQ1)*matK3*exp((-1*matK3*matQ3*x)/(1+2*matQ3))*( -1*exp((-1*matK3*matQ3*x)/(1+2*matQ3)) + 2*matH3/matK3 )^(1/matQ3-1)

  lines(x = x, y=y_primemat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)


  par(xpd=NA) # clip plotting to device region
  text("Growth velocity (cm/year)", x=31.5, y=8, srt=270, las=3, cex=1.8)

  symbol.Male(centerx = 3, centery = 15, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package




  ################ female weight
  post <- post3_fw

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,115), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 115, by=25), labels=TRUE )
  par(xpd=FALSE) # clip to plot region


  Ber.rows <- which( Com.fem$Ethnicity == 1 & Com.fem$TotAge < 0.1 ) # pick put one row corresponding to each indiv, other rows for each indiv have dublicate posteriors for Q, H, and K 
  Mat.rows <- which( Com.fem$Ethnicity == 2 & Com.fem$TotAge < 0.1 )

  # plot individual posterior trajectories for U.S.
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
          col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)
  } #for z

  # plot individual posterior trajectories for Matsigenka
  for ( z in 1:length(Mat.rows) ){

    K1 <- pull(post, paste("K1[", Mat.rows[z], "]", sep="")) #pull is a command from package dplyr to extract a vector from a tibble
    K2 <- pull(post, paste("K2[", Mat.rows[z], "]", sep=""))
    K3 <- pull(post, paste("K3[", Mat.rows[z], "]", sep=""))
    Q1 <- pull(post, paste("Q1[", Mat.rows[z], "]", sep=""))
    Q2 <- pull(post, paste("Q2[", Mat.rows[z], "]", sep=""))
    Q3 <- pull(post, paste("Q3[", Mat.rows[z], "]", sep=""))
    H1 <- pull(post, paste("H1[", Mat.rows[z], "]", sep=""))
    H2 <- pull(post, paste("H2[", Mat.rows[z], "]", sep=""))
    H3 <- pull(post, paste("H3[", Mat.rows[z], "]", sep=""))

    lines(x = x,
          y=( -1*exp(-1*mean(K1)*mean(Q1)*x/( 1 + 2*mean(Q1) )) + 2*mean(H1)/mean(K1)*pi^(mean(Q1)/( 1 + 2*mean(Q1) )) )^(( 1 + 2*mean(Q1) )/mean(Q1)) +
            ( -1*exp(-1*mean(K2)*mean(Q2)*x/( 1 + 2*mean(Q2) )) + 2*mean(H2)/mean(K2)*pi^(mean(Q2)/( 1 + 2*mean(Q2) )) )^(( 1 + 2*mean(Q2) )/mean(Q2)) +
            ( -1*exp(-1*mean(K3)*mean(Q3)*x/( 1 + 2*mean(Q3) )) + 2*mean(H3)/mean(K3)*pi^(mean(Q3)/( 1 + 2*mean(Q3) )) )^(( 1 + 2*mean(Q3) )/mean(Q3)),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)
  } #for z


  # plot measurment points
  points(x=Ber.fem$TotAge, y=Ber.fem$Weight, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.fem$TotAge, y=Mat.fem$Weight, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)


  # plot U.S. posterior mean trajectory
  berQ1 = exp(mean(post$berLQ1))
  berQ2 = exp(mean(post$berLQ2))
  berQ3 = exp(mean(post$berLQ3))
  berK1 = exp(mean(post$berLK1))
  berK2 = exp(mean(post$berLK2))
  berK3 = exp(mean(post$berLK3))
  berH1 = berK1/2 + mean(post$berR1)
  berH2 = berK2/2 + mean(post$berR2)
  berH3 = berK3/2 + mean(post$berR3)

  lines(x = x,
        y = ( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1*pi^(berQ1/( 1 + 2*berQ1 )) )^(( 1 + 2*berQ1 )/berQ1) +
            ( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2*pi^(berQ2/( 1 + 2*berQ2 )) )^(( 1 + 2*berQ2 )/berQ2) +
            ( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3*pi^(berQ3/( 1 + 2*berQ3 )) )^(( 1 + 2*berQ3 )/berQ3),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot matsigenka posterior mean trajectory
  matQ1 = exp(mean(post$matLQ1))
  matQ2 = exp(mean(post$matLQ2))
  matQ3 = exp(mean(post$matLQ3))
  matK1 = exp(mean(post$matLK1))
  matK2 = exp(mean(post$matLK2))
  matK3 = exp(mean(post$matLK3))
  matH1 = matK1/2 + mean(post$matR1)
  matH2 = matK2/2 + mean(post$matR2)
  matH3 = matK3/2 + mean(post$matR3)

  lines(x = x,
        y = ( -1*exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 )) + 2*matH1/matK1*pi^(matQ1/( 1 + 2*matQ1 )) )^(( 1 + 2*matQ1 )/matQ1) +
            ( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2*pi^(matQ2/( 1 + 2*matQ2 )) )^(( 1 + 2*matQ2 )/matQ2) +
            ( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3*pi^(matQ3/( 1 + 2*matQ3 )) )^(( 1 + 2*matQ3 )/matQ3),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # plot individual component functions

  # infant
  lines(x = x,
        y = ( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1*pi^(berQ1/( 1 + 2*berQ1 )) )^(( 1 + 2*berQ1 )/berQ1),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x, 
        y = ( -1*exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 )) + 2*matH1/matK1*pi^(matQ1/( 1 + 2*matQ1 )) )^(( 1 + 2*matQ1 )/matQ1),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # child
  lines(x = x,
        y = ( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2*pi^(berQ2/( 1 + 2*berQ2 )) )^(( 1 + 2*berQ2 )/berQ2),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x, 
        y = ( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2*pi^(matQ2/( 1 + 2*matQ2 )) )^(( 1 + 2*matQ2 )/matQ2),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  # adolescent
  lines(x = x,
        y = ( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3*pi^(berQ3/( 1 + 2*berQ3 )) )^(( 1 + 2*berQ3 )/berQ3),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x, 
        y = ( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3*pi^(matQ3/( 1 + 2*matQ3 )) )^(( 1 + 2*matQ3 )/matQ3),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=8, type="n", ylim=c(0,8), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 8, by=2 ), las=2, labels=FALSE )
  par(xpd=FALSE)


  # ber posterior mean velocity
  y_primeber <- exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 ))*berK1*( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1*pi^(berQ1/( 1 + 2*berQ1 )) )^( 1 + 1/berQ1 ) + 
                exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 ))*berK2*( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2*pi^(berQ2/( 1 + 2*berQ2 )) )^( 1 + 1/berQ2 ) +
                exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 ))*berK3*( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3*pi^(berQ3/( 1 + 2*berQ3 )) )^( 1 + 1/berQ3 )

  lines(x = x, y=y_primeber, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # matsi posterior mean velocity
  y_primemat <- exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 ))*matK1*( -1*exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 )) + 2*matH1/matK1*pi^(matQ1/( 1 + 2*matQ1 )) )^( 1 + 1/matQ1 ) + 
                exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 ))*matK2*( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2*pi^(matQ2/( 1 + 2*matQ2 )) )^( 1 + 1/matQ2 ) +
                exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 ))*matK3*( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3*pi^(matQ3/( 1 + 2*matQ3 )) )^( 1 + 1/matQ3 )

  lines(x = x, y=y_primemat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)



  par(xpd=TRUE)
  #text("Growth velocity (kg/year)", x=34, y=8, srt=270, las=3)

  symbol.Female(centerx = 3, centery = 7.5, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package



  ################ male weight
  post <- post3_mw

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,115), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 115, by=25), labels=FALSE )
  par(xpd=FALSE) # clip to plot region


  Ber.rows <- which( Com.mal$Ethnicity == 1 & Com.mal$TotAge < 0.1 ) # pick put one row corresponding to each indiv, other rows for each indiv have dublicate posteriors for Q, H, and K 
  Mat.rows <- which( Com.mal$Ethnicity == 2 & Com.mal$TotAge < 0.1 )

  # plot individual posterior trajectories for U.S.
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
          col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)
  } #for z

  # plot individual posterior trajectories for Matsigenka
  for ( z in 1:length(Mat.rows) ){

    K1 <- pull(post, paste("K1[", Mat.rows[z], "]", sep="")) #pull is a command from package dplyr to extract a vector from a tibble
    K2 <- pull(post, paste("K2[", Mat.rows[z], "]", sep=""))
    K3 <- pull(post, paste("K3[", Mat.rows[z], "]", sep=""))
    Q1 <- pull(post, paste("Q1[", Mat.rows[z], "]", sep=""))
    Q2 <- pull(post, paste("Q2[", Mat.rows[z], "]", sep=""))
    Q3 <- pull(post, paste("Q3[", Mat.rows[z], "]", sep=""))
    H1 <- pull(post, paste("H1[", Mat.rows[z], "]", sep=""))
    H2 <- pull(post, paste("H2[", Mat.rows[z], "]", sep=""))
    H3 <- pull(post, paste("H3[", Mat.rows[z], "]", sep=""))

    lines(x = x,
          y=( -1*exp(-1*mean(K1)*mean(Q1)*x/( 1 + 2*mean(Q1) )) + 2*mean(H1)/mean(K1)*pi^(mean(Q1)/( 1 + 2*mean(Q1) )) )^(( 1 + 2*mean(Q1) )/mean(Q1)) +
            ( -1*exp(-1*mean(K2)*mean(Q2)*x/( 1 + 2*mean(Q2) )) + 2*mean(H2)/mean(K2)*pi^(mean(Q2)/( 1 + 2*mean(Q2) )) )^(( 1 + 2*mean(Q2) )/mean(Q2)) +
            ( -1*exp(-1*mean(K3)*mean(Q3)*x/( 1 + 2*mean(Q3) )) + 2*mean(H3)/mean(K3)*pi^(mean(Q3)/( 1 + 2*mean(Q3) )) )^(( 1 + 2*mean(Q3) )/mean(Q3)),
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)
  } #for z


  # plot measurment points
  points(x=Ber.mal$TotAge, y=Ber.mal$Weight, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.mal$TotAge, y=Mat.mal$Weight, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)


  # plot U.S. posterior mean trajectory
  berQ1 = exp(mean(post$berLQ1))
  berQ2 = exp(mean(post$berLQ2))
  berQ3 = exp(mean(post$berLQ3))
  berK1 = exp(mean(post$berLK1))
  berK2 = exp(mean(post$berLK2))
  berK3 = exp(mean(post$berLK3))
  berH1 = berK1/2 + mean(post$berR1)
  berH2 = berK2/2 + mean(post$berR2)
  berH3 = berK3/2 + mean(post$berR3)

  lines(x = x,
        y = ( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1*pi^(berQ1/( 1 + 2*berQ1 )) )^(( 1 + 2*berQ1 )/berQ1) +
            ( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2*pi^(berQ2/( 1 + 2*berQ2 )) )^(( 1 + 2*berQ2 )/berQ2) +
            ( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3*pi^(berQ3/( 1 + 2*berQ3 )) )^(( 1 + 2*berQ3 )/berQ3),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)


  # plot matsigenka posterior mean trajectory
  matQ1 = exp(mean(post$matLQ1))
  matQ2 = exp(mean(post$matLQ2))
  matQ3 = exp(mean(post$matLQ3))
  matK1 = exp(mean(post$matLK1))
  matK2 = exp(mean(post$matLK2))
  matK3 = exp(mean(post$matLK3))
  matH1 = matK1/2 + mean(post$matR1)
  matH2 = matK2/2 + mean(post$matR2)
  matH3 = matK3/2 + mean(post$matR3)

  lines(x = x,
        y = ( -1*exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 )) + 2*matH1/matK1*pi^(matQ1/( 1 + 2*matQ1 )) )^(( 1 + 2*matQ1 )/matQ1) +
            ( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2*pi^(matQ2/( 1 + 2*matQ2 )) )^(( 1 + 2*matQ2 )/matQ2) +
            ( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3*pi^(matQ3/( 1 + 2*matQ3 )) )^(( 1 + 2*matQ3 )/matQ3),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty) 


  # plot individual component functions

  # infant
  lines(x = x,
        y = ( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1*pi^(berQ1/( 1 + 2*berQ1 )) )^(( 1 + 2*berQ1 )/berQ1),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x, 
        y = ( -1*exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 )) + 2*matH1/matK1*pi^(matQ1/( 1 + 2*matQ1 )) )^(( 1 + 2*matQ1 )/matQ1),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty) 


  # child
  lines(x = x,
        y = ( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2*pi^(berQ2/( 1 + 2*berQ2 )) )^(( 1 + 2*berQ2 )/berQ2),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x, 
        y = ( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2*pi^(matQ2/( 1 + 2*matQ2 )) )^(( 1 + 2*matQ2 )/matQ2),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty) 


  # adolescent
  lines(x = x,
        y = ( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3*pi^(berQ3/( 1 + 2*berQ3 )) )^(( 1 + 2*berQ3 )/berQ3),
            col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = x, 
        y = ( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3*pi^(matQ3/( 1 + 2*matQ3 )) )^(( 1 + 2*matQ3 )/matQ3),
            col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty) 



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=8, type="n", ylim=c(0,8), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 8, by=2 ), las=2, labels=FALSE )
  text( x=28.75, y=seq( 0, 8, by=2 ), labels=seq( 0, 8, by=2 ), srt=270, xpd=NA, cex=1)
  par(xpd=FALSE)


  # ber posterior mean velocity
  y_primeber <- exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 ))*berK1*( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1*pi^(berQ1/( 1 + 2*berQ1 )) )^( 1 + 1/berQ1 ) + 
                exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 ))*berK2*( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2*pi^(berQ2/( 1 + 2*berQ2 )) )^( 1 + 1/berQ2 ) +
                exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 ))*berK3*( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3*pi^(berQ3/( 1 + 2*berQ3 )) )^( 1 + 1/berQ3 )

  lines(x = x, y=y_primeber, col=BerVelTraj_col, lwd=BerVelTraj_lwd, lty=BerVelTraj_lty)


  # matsi posterior mean velocity
  y_primemat <- exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 ))*matK1*( -1*exp(-1*matK1*matQ1*x/( 1 + 2*matQ1 )) + 2*matH1/matK1*pi^(matQ1/( 1 + 2*matQ1 )) )^( 1 + 1/matQ1 ) + 
                exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 ))*matK2*( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2*pi^(matQ2/( 1 + 2*matQ2 )) )^( 1 + 1/matQ2 ) +
                exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 ))*matK3*( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3*pi^(matQ3/( 1 + 2*matQ3 )) )^( 1 + 1/matQ3 )

  lines(x = x, y=y_primemat, col=MatVelTraj_col, lwd=MatVelTraj_lwd, lty=MatVelTraj_lty)



  par(xpd=NA) # clip plotting to device region
  text("Growth velocity (kg/year)", x=31.5, y=4, srt=270, las=3, cex=1.8)

  symbol.Male(centerx = 3, centery = 7.5, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package


  mtext("Age since conception (years)", side = 1, outer = T, cex = 1.5, line = 2, adj=0.5)
  mtext("Height (cm)", side = 2, outer = T, cex = 1.5, line = 2, adj=0.8)
  mtext("Weight (kg)", side = 2, outer = T, cex = 1.5, line = 2, adj=0.2)



graphics.off()




