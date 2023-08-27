

################## m3 composite model for Berkeley and Matsigenka, covarying random effects for individual

post <- post3

pdf(file="./Plots/Composite_model_sample_mat_ber_randeff_cov.pdf",
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
          col=rgb(1,0,0,max=1,alpha=0.5), lwd=0.25, lty=1)
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
          col=rgb(0,0,1,max=1,alpha=0.5), lwd=0.25, lty=1)
  } #for z


  # # plot reference mean trajectory. This is essentially the prior.
  # post <- data_list3

  # Q1hat = exp(mean(post$LQ1hat))
  # Q2hat = exp(mean(post$LQ2hat))
  # Q3hat = exp(mean(post$LQ3hat))
  # K1hat = exp(mean(post$LK1hat))
  # K2hat = exp(mean(post$LK2hat))
  # K3hat = exp(mean(post$LK3hat))
  # H1hat = K1hat/2 + mean(post$R1hat)
  # H2hat = K2hat/2 + mean(post$R2hat)
  # H3hat = K3hat/2 + mean(post$R3hat)

  # lines(x = x,
  #       y = ( -1*exp(-1*K1hat*Q1hat*x/( 1 + 2*Q1hat )) + 2*H1hat/K1hat )^(1/Q1hat) +
  #           ( -1*exp(-1*K2hat*Q2hat*x/( 1 + 2*Q2hat )) + 2*H2hat/K2hat )^(1/Q2hat) +
  #           ( -1*exp(-1*K3hat*Q3hat*x/( 1 + 2*Q3hat )) + 2*H3hat/K3hat )^(1/Q3hat),
  #       col="pink", lwd=3, lty=1)


  # plot U.S. posterior mean trajectory
  post <- post3

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
        col="red", lwd=3, lty=1)


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
        col="blue", lwd=3, lty=1) 

 


  # plot individual component functions

  # infant
  post <- post3
  lines(x = x2,
        y = ( -1*exp(-1*berK1*berQ1*x2/( 1 + 2*berQ1 )) + 2*berH1/berK1 )^(1/berQ1),
            col="red", lwd=3, lty=1)
  lines(x = x2,
        y = ( -1*exp(-1*matK1*matQ1*x2/( 1 + 2*matQ1 )) + 2*matH1/matK1 )^(1/matQ1),
            col="blue", lwd=3, lty=1)


  # child
  lines(x = x,
        y = ( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2 )^(1/berQ2),
            col="red", lwd=3, lty=1)
  lines(x = x,
        y = ( -1*exp(-1*matK2*matQ2*x/( 1 + 2*matQ2 )) + 2*matH2/matK2 )^(1/matQ2),
            col="blue", lwd=3, lty=1)


  # adolescent
  lines(x = x,
        y = ( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3 )^(1/berQ3),
            col="red", lwd=3, lty=1)
  lines(x = x,
        y = ( -1*exp(-1*matK3*matQ3*x/( 1 + 2*matQ3 )) + 2*matH3/matK3 )^(1/matQ3),
            col="blue", lwd=3, lty=1)



  #plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=2 ), las=2 )
  par(xpd=FALSE)


  # ber posterior mean velocity
  y_primeBer <- 1/(1+2*berQ1)*berK1*exp((-1*berK1*berQ1*x)/(1+2*berQ1))*( -1*exp((-1*berK1*berQ1*x)/(1+2*berQ1)) + 2*berH1/berK1 )^(1/berQ1-1) + 
                1/(1+2*berQ1)*berK2*exp((-1*berK2*berQ2*x)/(1+2*berQ2))*( -1*exp((-1*berK2*berQ2*x)/(1+2*berQ2)) + 2*berH2/berK2 )^(1/berQ2-1) +
                1/(1+2*berQ1)*berK3*exp((-1*berK3*berQ3*x)/(1+2*berQ3))*( -1*exp((-1*berK3*berQ3*x)/(1+2*berQ3)) + 2*berH3/berK3 )^(1/berQ3-1)

  lines(x = x, y=y_primeBer, col="orange", lwd=2, lty=1)

  # matsi posterior mean velocity
  y_primemat <- 1/(1+2*matQ1)*matK1*exp((-1*matK1*matQ1*x)/(1+2*matQ1))*( -1*exp((-1*matK1*matQ1*x)/(1+2*matQ1)) + 2*matH1/matK1 )^(1/matQ1-1) + 
                1/(1+2*matQ1)*matK2*exp((-1*matK2*matQ2*x)/(1+2*matQ2))*( -1*exp((-1*matK2*matQ2*x)/(1+2*matQ2)) + 2*matH2/matK2 )^(1/matQ2-1) +
                1/(1+2*matQ1)*matK3*exp((-1*matK3*matQ3*x)/(1+2*matQ3))*( -1*exp((-1*matK3*matQ3*x)/(1+2*matQ3)) + 2*matH3/matK3 )^(1/matQ3-1)

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







######################## Plot parameter posteriors for composite model

post <- post3

mean_param_list <- list(
                       exp(post$matLQ1),
                       exp(post$berLQ1),

                       0.001*exp(post$matLK1), # assuming density = 1g/cm^3
                       0.001*exp(post$berLK1),

                       0.001*(exp(post$matLK1)/2 + post$matR1), # assuming density = 1g/cm^3
                       0.001*(exp(post$berLK1)/2 + post$berR1),

                       exp(post$matLQ2),
                       exp(post$berLQ2),

                       0.001*exp(post$matLK2),
                       0.001*exp(post$berLK2),

                       0.001*(exp(post$matLK2)/2 + post$matR2),
                       0.001*(exp(post$berLK2)/2 + post$berR2),

                       exp(post$matLQ3),
                       exp(post$berLQ3),

                       0.001*exp(post$matLK3),
                       0.001*exp(post$berLK3),

                       0.001*(exp(post$matLK3)/2 + post$matR3),
                       0.001*(exp(post$berLK3)/2 + post$berR3)
                      )


names(mean_param_list) <- c(
                          "matQ1",
                          "berQ1",

                          "matK1",
                          "berK1",

                          "matH1",
                          "berH1",

                          "matQ2",
                          "berQ2",

                          "matK2",
                          "berK2",

                          "matH2",
                          "berH2",

                          "matQ3",
                          "berQ3",

                          "matK3",
                          "berK3",

                          "matH3",
                          "berH3"
                        )

#str(mean_param_list)

contr_param_list <- list(
                       exp(post$berLQ1) - exp(post$matLQ1),
                       0.001*exp(post$berLK1) - 0.001*exp(post$matLK1),
                       0.001*(exp(post$berLK1)/2 + post$berR1) - 0.001*(exp(post$matLK1)/2 + post$matR1),

                       exp(post$berLQ2) - exp(post$matLQ2),
                       0.001*exp(post$berLK2) - 0.001*exp(post$matLK2),
                       0.001*(exp(post$berLK2)/2 + post$berR2) - 0.001*(exp(post$matLK2)/2 + post$matR2),

                       exp(post$berLQ3) - exp(post$matLQ3),
                       0.001*exp(post$berLK3) - 0.001*exp(post$matLK3),
                       0.001*(exp(post$berLK3)/2 + post$berR3) - 0.001*(exp(post$matLK3)/2 + post$matR3)
                      )


names(contr_param_list) <- c(
                          "ber.matQ1",
                          "ber.matK1",
                          "ber.matH1",

                          "ber.matQ2",
                          "ber.matK2",
                          "ber.matH2",

                          "ber.matQ3",
                          "ber.matK3",
                          "ber.matH3"
                        )

#str(contr_param_list)



pdf(file="./Plots/param_post_plot.pdf", 
height=14, width=7)
layout( matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), 9, 2, byrow = FALSE),
        heights=c(1,1,1,1,1,1,1,1,1),
        widths=c(1,1) )
par(mar = c(2, 1, 2, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


Q1_post_plot <- denschart3( mean_param_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$matLQ1)) - 0.003, mean(exp(post$berLQ1)) + 0.003 ), #( 0.155, 0.165 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean(exp(post$berLQ1)), mean(exp(post$berLQ1)) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)
lines(x=c( mean(exp(post$matLQ1)), mean(exp(post$matLQ1)) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)

axis(side=1, at=c(0.08,0.085), labels=c(0.08,0.085), cex.axis=1.5)
mtext(expression(paste("Infant ",italic("q")[1])), side = 1, outer = T, cex = 1.5, line = -69, adj=0.5)

#mtext("Composite model parameter posteriors", side = 3, outer = F, cex = 2, line = 1)
par(xpd=NA) # plotting clipped to device region

# legend
legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast", "")
xcoords <- c(0, 0.002, 0.0068, 0.011)
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x="topleft", inset=c(0,-0.5), text.width=textwidths, #x=c(0.0085,0.01), y=c(1,0.5), #mean(exp(post$berLQ1)) - 0.0005, 1.4,
         legend=legtext, bty="o", bg="white",
         col=c("red", "blue", "black","white"),
         lty=c(1,1,1,1),
         lwd=c(3,3,3,3),
         cex=1.4,
         x.intersp=0.5,
         seg.len=3,
         horiz=TRUE)

symbol.Male(centerx = 0.09, centery = 1.1, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


par(xpd=FALSE)



K1_post_plot <- denschart3( mean_param_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK1)) - 0.008, 0.001*mean(exp(post$matLK1)) + 0.008), #( 10, 10.6 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK1)), 0.001*mean(exp(post$berLK1)) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK1)), 0.001*mean(exp(post$matLK1)) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)

axis(side=1, at=c(0.04,0.05), labels=c(0.04,0.05), cex.axis=1.5)
mtext(expression(paste("Infant ",italic("K")[1]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -60, adj=0.5)


H1_post_plot <- denschart3( mean_param_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK1)/2 + post$berR1) - 0.005, 0.001*mean(exp(post$matLK1)/2 + post$matR1) + 0.005), #( 10.25, 10.75 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK1)/2 + post$berR1), 0.001*mean(exp(post$berLK1)/2 + post$berR1) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK1)/2 + post$matR1), 0.001*mean(exp(post$matLK1)/2 + post$matR1) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)

axis(side=1, at=c(0.03,0.04), labels=c(0.03,0.04), cex.axis=1.5)
mtext(expression(paste("Infant ",italic("H")[1]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = -51, adj=0.5)


Q2_post_plot <- denschart3( mean_param_list[c(7,8)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$berLQ2)) - 0.005, mean(exp(post$matLQ2)) + 0.005), #( 0.085, 0.095 ),
            yvals = c(#0.5,
                      0.5,
                      0.5)
 )

lines(x=c( mean(exp(post$berLQ2)), mean(exp(post$berLQ2)) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)
lines(x=c( mean(exp(post$matLQ2)), mean(exp(post$matLQ2)) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)

axis(side=1, at=c(0.095,0.1), labels=c(0.095,0.1), cex.axis=1.5)
mtext(expression(paste("Child ",italic("q")[2])), side = 1, outer = T, cex = 1.5, line = -42.5, adj=0.5)


K2_post_plot <- denschart3( mean_param_list[c(9,10)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK2)) - 0.00001, 0.001*mean(exp(post$berLK2)) + 0.00001), #( 3.6, 4 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK2)), 0.001*mean(exp(post$berLK2)) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK2)), 0.001*mean(exp(post$matLK2)) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)

axis(side=1, at=c(0.00475,0.00476), labels=c(0.00475,0.00476), cex.axis=1.5)
mtext(expression(paste("Child ",italic("K")[2]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -34, adj=0.5)


H2_post_plot <- denschart3( mean_param_list[c(11,12)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK2)/2 + post$matR2) - 0.00005, 0.001*mean(exp(post$berLK2)/2 + post$berR2) + 0.00005 ), #( 2.6, 2.8 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK2)/2 + post$berR2), 0.001*mean(exp(post$berLK2)/2 + post$berR2) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK2)/2 + post$matR2), 0.001*mean(exp(post$matLK2)/2 + post$matR2) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)

axis(side=1, at=c(0.00355,0.0036), labels=c(0.00355,0.0036), cex.axis=1.5)
mtext(expression(paste("Child ",italic("H")[2]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = -25, adj=0.5)



Q3_post_plot <- denschart3( mean_param_list[c(13,14)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$berLQ3)) - 1e-6, mean(exp(post$matLQ3)) + 1e-6), #( 0.0000056,  0.0000068 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean(exp(post$berLQ3)), mean(exp(post$berLQ3)) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)
lines(x=c( mean(exp(post$matLQ3)), mean(exp(post$matLQ3)) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)

axis(side=1, at=c(3.8e-05,4e-05), labels=c(3.8e-05,4e-05), cex.axis=1.5)
mtext(expression(paste("Adolescent ",italic("q")[3])), side = 1, outer = T, cex = 1.5, line = -16, adj=0.5)


K3_post_plot <- denschart3( mean_param_list[c(15,16)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK3)) - 0.1, 0.001*mean(exp(post$matLK3)) + 0.1 ), #( 120000, 145000 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK3)), 0.001*mean(exp(post$berLK3)) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK3)), 0.001*mean(exp(post$matLK3)) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)

axis(side=1, at=c(19.6,19.7), labels=c(19.6,19.7), cex.axis=1.5)
mtext(expression(paste("Adolescent ",italic("K")[3]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -7.5, adj=0.5)


H3_post_plot <- denschart3( mean_param_list[c(17,18)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK3)/2 + post$berR3) - 0.05, 0.001*mean(exp(post$matLK3)/2 + post$matR3) + 0.05 ), #( 60000, 75000 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK3)/2 + post$berR3), 0.001*mean(exp(post$berLK3)/2 + post$berR3) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK3)/2 + post$matR3), 0.001*mean(exp(post$matLK3)/2 + post$matR3) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)

axis(side=1, at=c(9.8,9.85), labels=c(9.8,9.85), cex.axis=1.5)
mtext(expression(paste("Adolescent ",italic("H")[3]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)



### parameter contrasts


Q1_cont_plot <- denschart3( contr_param_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.015,0.015),
                #range( min(contr_param_list$ber.matQ1), max(contr_param_list$ber.matQ1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ1), mean(contr_param_list$ber.matQ1) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.015,0,0.015), labels=c(-0.015,0,0.015), cex.axis=1.5)




K1_cont_plot <- denschart3( contr_param_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.03,0.03),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK1), mean(contr_param_list$ber.matK1) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.03,0,0.03), labels=c(-0.03,0,0.03), cex.axis=1.5)


H1_cont_plot <- denschart3( contr_param_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.02,0.02),
                #range( min(contr_param_list$ber.matH1), max(contr_param_list$ber.matH1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH1), mean(contr_param_list$ber.matH1) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.02,0,0.02), labels=c(-0.02,0,0.02), cex.axis=1.5)


Q2_cont_plot <- denschart3( contr_param_list[4],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.005,0.005),
                #range( min(contr_param_list$ber.matQ2), max(contr_param_list$ber.matQ2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ2), mean(contr_param_list$ber.matQ2) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.005,0,0.005), labels=c(-0.005,0,0.005), cex.axis=1.5)


K2_cont_plot <- denschart3( contr_param_list[5],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.00001,0.00001),
                #range( min(contr_param_list$ber.matK2), max(contr_param_list$ber.matK2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK2), mean(contr_param_list$ber.matK2) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.00001,0,0.00001), labels=c(-0.00005,0,0.00005), cex.axis=1.5)


H2_cont_plot <- denschart3( contr_param_list[6],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.00003,0.00003),
                #range( min(contr_param_list$ber.matH2), max(contr_param_list$ber.matH2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH2), mean(contr_param_list$ber.matH2) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.00003,0,0.00003), labels=c(-0.00005,0,0.00005), cex.axis=1.5)


Q3_cont_plot <- denschart3( contr_param_list[7],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.000002,0.000002),
                #range( min(contr_param_list$ber.matQ3), max(contr_param_list$ber.matQ3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ3), mean(contr_param_list$ber.matQ3) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.000002,0,0.000002), labels=c(-0.000002,0,0.000002), cex.axis=1.5)


K3_cont_plot <- denschart3( contr_param_list[8],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.1,0.1),
                #range( min(contr_param_list$ber.matK3), max(contr_param_list$ber.matK3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK3), mean(contr_param_list$ber.matK3) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.1,0,0.1), labels=c(-0.1,0,0.1), cex.axis=1.5)


H3_cont_plot <- denschart3( contr_param_list[9],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.05,0.05),
                #range( min(contr_param_list$ber.matH3), max(contr_param_list$ber.matH3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH3), mean(contr_param_list$ber.matH3) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.05,0,0.05), labels=c(-0.05,0,0.05), cex.axis=1.5)


graphics.off()








######################## Plot characteristic posteriors for composite model

post <- post3

Q1mat <- exp(post$matLQ1)
Q1ber <- exp(post$berLQ1)

Q2mat <- exp(post$matLQ2)
Q2ber <- exp(post$berLQ2)

Q3mat <- exp(post$matLQ3)
Q3ber <- exp(post$berLQ3)

K1mat <- exp(post$matLK1)
K1ber <- exp(post$berLK1)

K2mat <- exp(post$matLK2)
K2ber <- exp(post$berLK2)

K3mat <- exp(post$matLK3)
K3ber <- exp(post$berLK3)

H1mat <- exp(post$matLK1)/2 + post$matR1
H1ber <- exp(post$berLK1)/2 + post$berR1

H2mat <- exp(post$matLK2)/2 + post$matR2
H2ber <- exp(post$berLK2)/2 + post$berR2

H3mat <- exp(post$matLK3)/2 + post$matR3
H3ber <- exp(post$berLK3)/2 + post$berR3


# function to numerically solve to find age at max velocity and max velocity
maxagevel <- function(Q=1, K=1, H=1){
    D <- 1
    C <- -1

    secderiv <- function(t, Q=1, K=1, H=1){
        ( C*D*(K^3)*( C*D*K + 2*H*Q*exp(K*Q*t/( 1 + 2*Q )) )*
        ( C*exp(-1*K*Q*t/( 1 + 2*Q )) + 2*H/(D*K) )^(1/Q) )/
        ( ( ( C*D*K + 2*H*exp(K*Q*t/( 1 + 2*Q )) )^2 )*( 1 + 2*Q )^2 )
    } # secderiv
    tmax_sol <- uniroot( secderiv, Q=Q, K=K, H=H, lower=0, upper=21 )   # root between t=0 and t=21

    firderiv <- function(t=tmax_sol$root, Q=1, K=1, H=1){
        ( -1*K*C*exp(-1*K*Q*t/( 1 + 2*Q ))*
        ( C*exp(-1*K*Q*t/( 1 + 2*Q )) + 2*H/(D*K) )^( 1/Q - 1 ) )/( 1 + 2*Q )
    } # firderiv

    return( list(agemax=tmax_sol$root, maxvel=firderiv(tmax_sol$root,Q,K,H)) )
} # maxagevel

vmaxagevel <- Vectorize(maxagevel) # vectorize the function so that it can take vectors as arguments

# Q <- Q3ber[1:2]
# K <- K3ber[1:2]
# H <- H3ber[1:2]
# unlist(vmaxagevel(Q=Q, K=K, H=H)["agemax",])
# unlist(vmaxagevel(Q=Q, K=K, H=H)["maxvel",])



mean_char_list <- list(
                       (2*H1mat/K1mat)^(1/Q1mat),
                       (2*H1ber/K1ber)^(1/Q1ber),

                       ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat),
                       ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber),

                       (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2),
                       (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2),

                       (2*H2mat/K2mat)^(1/Q2mat),
                       (2*H2ber/K2ber)^(1/Q2ber),

                       ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat),
                       ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber),

                       (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2),
                       (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2),

                       (2*H3mat/K3mat)^(1/Q3mat),
                       (2*H3ber/K3ber)^(1/Q3ber),

                       ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat),
                       ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber),

                       #(2^(1/Q3cdc)*K3cdc*Q3cdc*((H3cdc - H3cdc*Q3cdc)/K3cdc)^(1/Q3cdc))/(1 + Q3cdc + 2*Q3cdc^2),
                       #(2^(1/Q3mat)*K3mat*Q3mat*((H3mat - H3mat*Q3mat)/K3mat)^(1/Q3mat))/(1 + Q3mat + 2*Q3mat^2),
                       #(2^(1/Q3ber)*K3ber*Q3ber*((H3ber - H3ber*Q3ber)/K3ber)^(1/Q3ber))/(1 + Q3ber + 2*Q3ber^2)

                       unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]),
                       unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",])
                      )


names(mean_char_list) <- c(
                          "matMaxHeight1",
                          "berMaxHeight1",

                          "matMaxVelAge1",
                          "berMaxVelAge1",

                          "matMaxVel1",
                          "berMaxVel1",

                          "matMaxHeight2",
                          "berMaxHeight2",

                          "matMaxVelAge2",
                          "berMaxVelAge2",

                          "matMaxVel2",
                          "berMaxVel2",

                          "matMaxHeight3",
                          "berMaxHeight3",

                          "matMaxVelAge3",
                          "berMaxVelAge3",

                          "matMaxVel3",
                          "berMaxVel3"
                        )

#str(mean_char_list)



contr_char_list <- list(
                       (2*H1ber/K1ber)^(1/Q1ber) - (2*H1mat/K1mat)^(1/Q1mat),
                       ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) - ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat),
                       (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) - (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2),
                       
                       (2*H2ber/K2ber)^(1/Q2ber) - (2*H2mat/K2mat)^(1/Q2mat),
                       ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) - ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat),
                       (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) - (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2),
                       
                       (2*H3ber/K3ber)^(1/Q3ber) - (2*H3mat/K3mat)^(1/Q3mat),
                       ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) - ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat),
                       #(2^(1/Q3ber)*K3ber*Q3ber*((H3ber - H3ber*Q3ber)/K3ber)^(1/Q3ber))/(1 + Q3ber + 2*Q3ber^2) - (2^(1/Q3mat)*K3mat*Q3mat*((H3mat - H3mat*Q3mat)/K3mat)^(1/Q3mat))/(1 + Q3mat + 2*Q3mat^2),
                       unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) - unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",])
                      )


names(contr_char_list) <- c(
                          "ber.matMaxWeight1",
                          "ber.matMaxVelAge1",
                          "ber.matMaxVel1",

                          "ber.matMaxWeight2",
                          "ber.matMaxVelAge2",
                          "ber.matMaxVel2",

                          "ber.matMaxWeight3",
                          "ber.matMaxVelAge3",
                          "ber.matMaxVel3"
                        )

#str(contr_char_list)



pdf(file="./Plots/char_post_plot.pdf", 
height=14, width=7)
layout( matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), 8, 2, byrow = FALSE),
        heights=c(1,1,1,1,1,1,1,1),
        widths=c(1,1) )
par(mar = c(2, 1, 2, 0), oma = c(5, 4, 4, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


MaxHeight1_post_plot <- denschart3( mean_char_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2*H1mat/K1mat)^(1/Q1mat) ), mean( (2*H1mat/K1mat)^(1/Q1mat) ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( (2*H1ber/K1ber)^(1/Q1ber) ), mean( (2*H1ber/K1ber)^(1/Q1ber) ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(75,85), labels=c(75,85), cex.axis=1.5)
mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)

#mtext("Composite model characteristic posteriors", side = 3, outer = F, cex = 2, line = 1)
par(xpd=NA) # plotting clipped to device region

# legend
legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast", "")
xcoords <- c(0, 2, 7, 11)
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x="topleft", inset=c(0,-0.5), text.width=textwidths, #x=c(0.0085,0.01), y=c(1,0.5), #mean(exp(post$berLQ1)) - 0.0005, 1.4,
         legend=legtext, bty="o", bg="white",
         col=c("red", "blue", "black","white"),
         lty=c(1,1,1,1),
         lwd=c(3,3,3,3),
         cex=1.4,
         x.intersp=0.5,
         seg.len=3,
         horiz=TRUE)


text(x=mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5 + 14,
     y=1.2,labels='\\MA',
     vfont=c("sans serif","bold"),
     cex = 6) #add Mars symbol

symbol.Male(centerx = mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5 + 14,
            centery = 1.1, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


par(xpd=FALSE)


MaxVelAge1_post_plot <- denschart3( mean_char_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ) - 0.1, mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ) + 0.1), #( 0.85, 0.95 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ), mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ), mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(0.6,0.7), labels=c(0.6,0.7), cex.axis=1.5)
mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)


MaxVel1_post_plot <- denschart3( mean_char_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2) ) - 20, mean( (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) ) + 20), #( 38, 46 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2) ), mean( (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2) ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) ), mean( (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(95,100,105), labels=c(95,100,105), cex.axis=1.5)
mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)


MaxHeight2_post_plot <- denschart3( mean_char_list[c(7,8)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( (2*H2mat/K2mat)^(1/Q2mat) ) - 5, mean( (2*H2ber/K2ber)^(1/Q2ber) ) + 5), #( 45, 65 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2*H2mat/K2mat)^(1/Q2mat) ), mean( (2*H2mat/K2mat)^(1/Q2mat) ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( (2*H2ber/K2ber)^(1/Q2ber) ), mean( (2*H2ber/K2ber)^(1/Q2ber) ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(50,60,70), labels=c(50,60,70), cex.axis=1.5)
mtext("Child max height (cm)", side = 1, outer = T, cex = 1.5, line = -38.5, adj=0.5)


MaxVelAge2_post_plot <- denschart3( mean_char_list[c(9,10)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) ) - 0.5, mean( ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat) ) + 0.5), #( 6.5, 7.5 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat) ), mean( ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat) ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) ), mean( ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(4.5,5), labels=c(4.5,5), cex.axis=1.5)
mtext("Child age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = -28.5, adj=0.5)


MaxVel2_post_plot <- denschart3( mean_char_list[c(11,12)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2) ) - 1, mean( (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) ) + 1), #( 5, 7 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2) ), mean( (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2) ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) ), mean( (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(8,9), labels=c(8,9), cex.axis=1.5)
mtext("Child max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -18.5, adj=0.5)



MaxHeight3_post_plot <- denschart3( mean_char_list[c(13,14)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( (2*H3ber/K3ber)^(1/Q3ber) ) - 5, mean( (2*H3mat/K3mat)^(1/Q3mat) ) + 5), #( 15, 25 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2*H3mat/K3mat)^(1/Q3mat) ), mean( (2*H3mat/K3mat)^(1/Q3mat) ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( (2*H3ber/K3ber)^(1/Q3ber) ), mean( (2*H3ber/K3ber)^(1/Q3ber) ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(30,35), labels=c(30,35), cex.axis=1.5)
mtext("Adolescent max height (cm)", side = 1, outer = T, cex = 1.5, line = -9, adj=0.5)


MaxVelAge3_post_plot <- denschart3( mean_char_list[c(15,16)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
                         rgb(1,0,0,max=1,alpha=0.5)
                        ),
            polyborder=c( "blue",
                          "red"
                        ),
            polyborderHPDI=c( "blue",
                              "red"
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean( ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat) ) - 1, mean( ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) ) + 1 ), #( 13, 16 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat) ), mean( ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat) ) ),
      y=c(0,0.75), col="blue", lwd=3, lty=1)
lines(x=c( mean( ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) ), mean( ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) ) ),
      y=c(0,0.75), col="red", lwd=3, lty=1)

axis(side=1, at=c(13,14), labels=c(13,14), cex.axis=1.5)
mtext("Adolescent age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)


# MaxVel3_post_plot <- denschart3( mean_char_list[c(17,18)],
#             #labels=rev(quest_names)
#             labels="",
#             adjust=1,
#             color=c( NA,
#                      NA
#                     ),
#             colorHPDI=c( rgb(0,0,1,max=1,alpha=0.5),          #rgb:  red, green, blue, alpha=0 is opaque
#                          rgb(1,0,0,max=1,alpha=0.5)
#                          ),
#             polyborder=c( "blue",
#                          "red"
#                         ),
#             polyborderHPDI=c( "blue",
#                              "red"
#                             ),
#             HPDI=0.9,
#             HPDI=0.9,
#             border=NA, yaxt="n",
#             cex=0.8, height=0.4,
#             #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
#             xlim=range( 2, 8 ),
#             yvals = c(0.5,0.5)
#  )

# lines(x=c( mean( (2^(1/Q3mat)*K3mat*Q3mat*((H3mat - H3mat*Q3mat)/K3mat)^(1/Q3mat))/(1 + Q3mat + 2*Q3mat^2) ), mean( (2^(1/Q3mat)*K3mat*Q3mat*((H3mat - H3mat*Q3mat)/K3mat)^(1/Q3mat))/(1 + Q3mat + 2*Q3mat^2) ) ),
#       y=c(0,0.75), col="blue", lwd=3, lty=1)
# lines(x=c( mean( (2^(1/Q3ber)*K3ber*Q3ber*((H3ber - H3ber*Q3ber)/K3ber)^(1/Q3ber))/(1 + Q3ber + 2*Q3ber^2) ), mean( (2^(1/Q3ber)*K3ber*Q3ber*((H3ber - H3ber*Q3ber)/K3ber)^(1/Q3ber))/(1 + Q3ber + 2*Q3ber^2) ) ),
#       y=c(0,0.75), col="red", lwd=3, lty=1)

# axis(side=1, at=c(2,4,6,8), labels=c(2,4,6,8), cex.axis=1.5)
# mtext("Adolescent Max Velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)



### parameter contrasts


MaxWeight1_cont_plot <- denschart3( contr_char_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-8,8),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxWeight1), mean(contr_char_list$ber.matMaxWeight1) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-8,0,8), labels=c(-8,0,8), cex.axis=1.5)




MaxVelAge1_cont_plot <- denschart3( contr_char_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.15,0.15),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge1), mean(contr_char_list$ber.matMaxVelAge1) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.15,0,0.15), labels=c(-0.15,0,0.15), cex.axis=1.5)


MaxVel1_cont_plot <- denschart3( contr_char_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-25,25),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel1), mean(contr_char_list$ber.matMaxVel1) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-25,0,25), labels=c(-25,0,25), cex.axis=1.5)


MaxWeight2_cont_plot <- denschart3( contr_char_list[4],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-15,15),
                #range( min(contr_char_list$ber.matMaxWeight2), max(contr_char_list$ber.matMaxWeight2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxWeight2), mean(contr_char_list$ber.matMaxWeight2) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-15,0,15), labels=c(-15,0,15), cex.axis=1.5)


MaxVelAge2_cont_plot <- denschart3( contr_char_list[5],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.4,0.4),
                #range( min(contr_char_list$ber.matMaxVelAge2), max(contr_char_list$ber.matMaxVelAge2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge2), mean(contr_char_list$ber.matMaxVelAge2) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-0.4,0,0.4), labels=c(-0.4,0,0.4), cex.axis=1.5)


MaxVel2_cont_plot <- denschart3( contr_char_list[6],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-2,2),
                #range( min(contr_char_list$ber.matMaxVel2), max(contr_char_list$ber.matMaxVel2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel2), mean(contr_char_list$ber.matMaxVel2) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-2,0,2), labels=c(-2,0,2), cex.axis=1.5)


MaxWeight3_cont_plot <- denschart3( contr_char_list[7],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-4,4),
                #range( min(contr_char_list$ber.matMaxWeight3), max(contr_char_list$ber.matMaxWeight3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxWeight3), mean(contr_char_list$ber.matMaxWeight3) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-4,0,4), labels=c(-4,0,4), cex.axis=1.5)


MaxVelAge3_cont_plot <- denschart3( contr_char_list[8],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=grey(0.5),
            polyborder="black",
            polyborderHPDI="black",
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-1,1),
                #range( min(contr_char_list$ber.matMaxVelAge3), max(contr_char_list$ber.matMaxVelAge3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge3), mean(contr_char_list$ber.matMaxVelAge3) ),
      y=c(0,0.75), col="black", lwd=3, lty=1)
lines(x=c(0,0),
      y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=1.5)


# MaxVel3_cont_plot <- denschart3( contr_char_list[9],
#             #labels=rev(quest_names)
#             labels="",
#             adjust=1,
#             color=NA,
#             colorHPDI=grey(0.5),
#             polyborder="black",
#             polyborderHPDI="black",
#             HPDI=0.9,
#             border=NA, yaxt="n",
#             cex=0.8, height=0.4,
#             #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
#             xlim=range(-1,1),
#                 #range( min(contr_char_list$ber.matMaxVel3), max(contr_char_list$ber.matMaxVel3) ),
#             yvals = 0.5
#  )

# lines(x=c( mean(contr_char_list$ber.matMaxVel3), mean(contr_char_list$ber.matMaxVel3) ),
#       y=c(0,0.75), col="black", lwd=3, lty=1)
# lines(x=c(0,0),
#       y=c(0,0.75), col="black", lwd=3, lty="11") #lty: first number in string is dash length, second is white space length

# axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=1.5)

graphics.off()

