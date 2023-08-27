

######################################### Fig 5 #########################################



######## load posteriors from complete models (m3), need to run Functions for densplot3 function

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



########## create lists of parameter posteriors

# male height
post <- post3_mh

mean_param_list_mh <- list(
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


names(mean_param_list_mh) <- c(
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

#str(mean_param_list_mh)

contr_param_list_mh <- list(
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


names(contr_param_list_mh) <- c(
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

#str(contr_param_list_mh)



# male weight
post <- post3_mw

mean_param_list_mw <- list(
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


names(mean_param_list_mw) <- c(
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

#str(mean_param_list_mw)

contr_param_list_mw <- list(
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


names(contr_param_list_mw) <- c(
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

#str(contr_param_list_mw)




# female height
post <- post3_fh

mean_param_list_fh <- list(
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


names(mean_param_list_fh) <- c(
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

#str(mean_param_list_fh)

contr_param_list_fh <- list(
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


names(contr_param_list_fh) <- c(
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

#str(contr_param_list_fh)





# female weight
post <- post3_fw

mean_param_list_fw <- list(
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


names(mean_param_list_fw) <- c(
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

#str(mean_param_list_fw)

contr_param_list_fw <- list(
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


names(contr_param_list_fw) <- c(
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

#str(contr_param_list_fw)










######## combine trajectory plots

colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.5)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)


# area and line colors
BerLine_lwd <- 4.5
BerLine_col <- colorlist["2.1"]

MatLine_lwd <- 4.5
MatLine_col <- colorlist["1.1"]

BerArea_col <- colorlist["2.3"]

MatArea_col <- colorlist["1.3"]

ConLine_lwd <- 4.5
ConLine_col <- "black"

ConArea_col <- grey(0.5)

ZerLine_lwd <- 4
ZerLine_lty <- "11"       #lty: first number in string is dash length, second is white space length
ZerLine_col <- "black"





pdf(file="./combined_plots/Plots/Params_combined.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                       19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                       37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                       55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1, 0.2, 1,1, 0.2, 1,1, 0.2, 1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels

# female height ###########################################################################################################################################################################
post <- post3_fh
mean_param_list <- mean_param_list_fh
contr_param_list <- contr_param_list_fh


Q1_post_plot <- denschart3( mean_param_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$matLQ1)) - 0.0003, mean(exp(post$berLQ1)) + 0.0003 ), #( 0.155, 0.165 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean(exp(post$berLQ1)), mean(exp(post$berLQ1)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ1)), mean(exp(post$matLQ1)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.08,0.085), labels=c(0.08,0.085),
    cex.axis=cex_axis)


#mtext("Composite model parameter posteriors", side = 3, outer = F, cex = 2, line = 1)



#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast")
xcoords <- c(0, 7, 23)
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x=10, y=21,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerArea_col, MatArea_col, ConArea_col),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=3.5,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = 9.2,
       ybottom = 14.5,
       xright = 86.8,
       ytop = 20,
       lwd=1)

# row labels
mtext(expression(bold("Infant")),     side = 1, outer = T, cex = 2.5, adj=-0.17, line = -108 )
mtext(expression(bold("Child")),      side = 1, outer = T, cex = 2.5, adj=-0.17, line = -70.5 )
mtext(expression(bold("Adolescent")), side = 1, outer = T, cex = 2.5, adj=-0.185, line = -33 )

mtext(expression(paste(bolditalic("q")[1])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -103 )
mtext(expression(paste(bolditalic("K")[1])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -90.5 )
mtext(expression(paste("(g/kg)")),           side = 1, outer = T, cex = 2,   adj=-0.096, line = -87.5 )
mtext(expression(paste(bolditalic("H")[1])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -78 )
mtext(expression(paste("(g/cm"^2,")")),      side = 1, outer = T, cex = 2,   adj=-0.105, line = -75 )


mtext(expression(paste(bolditalic("q")[2])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -65.5 )
mtext(expression(paste(bolditalic("K")[2])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -53 )
mtext(expression(paste("(g/kg)")),           side = 1, outer = T, cex = 2,   adj=-0.096, line = -50 )
mtext(expression(paste(bolditalic("H")[2])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -40.5 )
mtext(expression(paste("(g/cm"^2,")")),      side = 1, outer = T, cex = 2,   adj=-0.105, line = -37.5 )

mtext(expression(paste(bolditalic("q")[3])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -28 )
mtext(expression(paste(bolditalic("K")[3])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -15.5 )
mtext(expression(paste("(g/kg)")),           side = 1, outer = T, cex = 2,   adj=-0.096, line = -12.5 )
mtext(expression(paste(bolditalic("H")[3])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -3 )
mtext(expression(paste("(g/cm"^2,")")),      side = 1, outer = T, cex = 2,   adj=-0.105, line = 0 )

# column labels
symbol.Female(centerx = 7, centery = 10.8, rayonx=0.7, lwd=3, col="black")
mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.107, line = -110.5 )

symbol.Male(  centerx = 33.5, centery = 10.4, rayonx=0.7, lwd=3, col="black")
mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.385, line = -110.5 )

symbol.Female(centerx = 61, centery = 10.8, rayonx=0.7, lwd=3, col="black")
mtext(expression(bold("Weight")), side = 1, outer = T, cex = 2.5, adj=0.663, line = -110.5 )

symbol.Male(  centerx = 87.5, centery = 10.4, rayonx=0.7, lwd=3, col="black")
mtext(expression(bold("Weight")), side = 1, outer = T, cex = 2.5, adj=0.941, line = -110.5 )



# horizontal lines
lines(x=c(-18.5, 107),
      y=c(-38,-38),
      col="black", lwd=4, lty=1)

lines(x=c(-18.5, 107),
      y=c(-85.5,-85.5),
      col="black", lwd=4, lty=1)


# column brackets
lines(x=c(-2, 24),
      y=c(7.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(-2, -2),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(24, 24),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)


lines(x=c(26, 51),
      y=c(7.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(26, 26),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(51, 51),
     y=c(6.5,7.5),
      col="black", lwd=4, lty=1)


lines(x=c(53, 78),
      y=c(7.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(53, 53),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(78, 78),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)


lines(x=c(80, 105),
      y=c(7.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(80, 80),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)
lines(x=c(105, 105),
      y=c(6.5,7.5),
      col="black", lwd=4, lty=1)


# # vertical lines
# lines(x=c(0.115, 0.115),
#       y=c(1,-12),
#       col="black", lwd=3, lty=1)

# lines(x=c(0.156, 0.156),
#       y=c(1,-12),
#       col="black", lwd=3, lty=1)

# lines(x=c(0.198, 0.198),
#       y=c(1,-12),
#       col="black", lwd=3, lty=1)




par(xpd=FALSE)



K1_post_plot <- denschart3( mean_param_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK1)) - 0.001, 0.001*mean(exp(post$matLK1)) + 0.001), #( 10, 10.6 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK1)), 0.001*mean(exp(post$berLK1)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK1)), 0.001*mean(exp(post$matLK1)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
#as.vector(mean_param_list$matK1)
axis(side=1,
    #at=c(0.04,0.05), labels=c(0.04,0.05),
    cex.axis=cex_axis)


H1_post_plot <- denschart3( mean_param_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK1)/2 + post$berR1) - 0.001, 0.001*mean(exp(post$matLK1)/2 + post$matR1) + 0.001), #( 10.25, 10.75 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK1)/2 + post$berR1), 0.001*mean(exp(post$berLK1)/2 + post$berR1) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK1)/2 + post$matR1), 0.001*mean(exp(post$matLK1)/2 + post$matR1) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.03,0.04), labels=c(0.03,0.04),
    cex.axis=cex_axis)


Q2_post_plot <- denschart3( mean_param_list[c(7,8)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$berLQ2)) - 0.0008, mean(exp(post$matLQ2)) + 0.0008), #( 0.085, 0.095 ),
            yvals = c(#0.5,
                      0.5,
                      0.5)
 )

lines(x=c( mean(exp(post$berLQ2)), mean(exp(post$berLQ2)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ2)), mean(exp(post$matLQ2)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.095,0.1), labels=c(0.095,0.1),
    cex.axis=cex_axis)


K2_post_plot <- denschart3( mean_param_list[c(9,10)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK2)) - 0.000009, 0.001*mean(exp(post$berLK2)) + 0.000009), #( 3.6, 4 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK2)), 0.001*mean(exp(post$berLK2)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK2)), 0.001*mean(exp(post$matLK2)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    at=c(0.005593,0.005605), labels=c(0.005593,0.005605),
    cex.axis=cex_axis)


H2_post_plot <- denschart3( mean_param_list[c(11,12)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK2)/2 + post$matR2) - 0.00001, 0.001*mean(exp(post$berLK2)/2 + post$berR2) + 0.00001 ), #( 2.6, 2.8 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK2)/2 + post$berR2), 0.001*mean(exp(post$berLK2)/2 + post$berR2) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK2)/2 + post$matR2), 0.001*mean(exp(post$matLK2)/2 + post$matR2) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.00355,0.0036), labels=c(0.00355,0.0036),
    cex.axis=cex_axis)


Q3_post_plot <- denschart3( mean_param_list[c(13,14)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$berLQ3)) - 5e-7, mean(exp(post$matLQ3)) + 5e-7), #( 0.0000056,  0.0000068 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean(exp(post$berLQ3)), mean(exp(post$berLQ3)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ3)), mean(exp(post$matLQ3)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(3.8e-05,4e-05), labels=c(3.8e-05,4e-05),
    cex.axis=cex_axis)


K3_post_plot <- denschart3( mean_param_list[c(15,16)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK3)) - 0.003, 0.001*mean(exp(post$matLK3)) + 0.003 ), #( 120000, 145000 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK3)), 0.001*mean(exp(post$berLK3)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK3)), 0.001*mean(exp(post$matLK3)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(19.6,19.7), labels=c(19.6,19.7),
    cex.axis=cex_axis)


H3_post_plot <- denschart3( mean_param_list[c(17,18)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK3)/2 + post$berR3) - 0.002, 0.001*mean(exp(post$matLK3)/2 + post$matR3) + 0.002 ), #( 60000, 75000 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK3)/2 + post$berR3), 0.001*mean(exp(post$berLK3)/2 + post$berR3) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK3)/2 + post$matR3), 0.001*mean(exp(post$matLK3)/2 + post$matR3) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(9.8,9.85), labels=c(9.8,9.85),
    cex.axis=cex_axis)



### parameter contrasts


Q1_cont_plot <- denschart3( contr_param_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.001,0.001),
                #range( min(contr_param_list$ber.matQ1), max(contr_param_list$ber.matQ1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ1), mean(contr_param_list$ber.matQ1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-4,6e-4), labels=c(-6e-4,6e-4),
    cex.axis=cex_axis)




K1_cont_plot <- denschart3( contr_param_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.005,0.005),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK1), mean(contr_param_list$ber.matK1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.003,0.003), labels=c(-0.003,0.003),
    cex.axis=cex_axis)



H1_cont_plot <- denschart3( contr_param_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.003,0.003),
                #range( min(contr_param_list$ber.matH1), max(contr_param_list$ber.matH1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH1), mean(contr_param_list$ber.matH1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.002,0.002), labels=c(-0.002,0.002),
    cex.axis=cex_axis)


Q2_cont_plot <- denschart3( contr_param_list[4],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.003,0.003),
                #range( min(contr_param_list$ber.matQ2), max(contr_param_list$ber.matQ2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ2), mean(contr_param_list$ber.matQ2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.002,0.002), labels=c(-0.002,0.002),
    cex.axis=cex_axis)


K2_cont_plot <- denschart3( contr_param_list[5],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.00001,0.00001),
                #range( min(contr_param_list$ber.matK2), max(contr_param_list$ber.matK2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK2), mean(contr_param_list$ber.matK2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-6,6e-6), labels=c(-6e-6,6e-6),
    cex.axis=cex_axis)


H2_cont_plot <- denschart3( contr_param_list[6],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-1e-5,1e-5),
                #range( min(contr_param_list$ber.matH2), max(contr_param_list$ber.matH2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH2), mean(contr_param_list$ber.matH2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-6,6e-6), labels=c(-6e-6,6e-6),
    cex.axis=cex_axis)


Q3_cont_plot <- denschart3( contr_param_list[7],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-1e-6,1e-6),
                #range( min(contr_param_list$ber.matQ3), max(contr_param_list$ber.matQ3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ3), mean(contr_param_list$ber.matQ3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-7,6e-7), labels=c(-6e-7,6e-7),
    cex.axis=cex_axis)


K3_cont_plot <- denschart3( contr_param_list[8],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.007,0.007),
                #range( min(contr_param_list$ber.matK3), max(contr_param_list$ber.matK3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK3), mean(contr_param_list$ber.matK3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.004,0.004), labels=c(-0.004,0.004),
    cex.axis=cex_axis)


H3_cont_plot <- denschart3( contr_param_list[9],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.003,0.003),
                #range( min(contr_param_list$ber.matH3), max(contr_param_list$ber.matH3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH3), mean(contr_param_list$ber.matH3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.002,0.002), labels=c(-0.002,0.002),
    cex.axis=cex_axis)






# male height ###########################################################################################################################################################################
post <- post3_mh
mean_param_list <- mean_param_list_mh
contr_param_list <- contr_param_list_mh


Q1_post_plot <- denschart3( mean_param_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$matLQ1)) - 0.0001, mean(exp(post$berLQ1)) + 0.0001 ), #( 0.155, 0.165 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean(exp(post$berLQ1)), mean(exp(post$berLQ1)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ1)), mean(exp(post$matLQ1)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.0884,0.0888), labels=c(0.0884,0.0888),
    cex.axis=cex_axis)
#mtext(expression(paste("Infant ",italic("q")[1])), side = 1, outer = T, cex = 1.5, line = -69, adj=0.5)

#mtext("Composite model parameter posteriors", side = 3, outer = F, cex = 2, line = 1)
par(xpd=NA) # plotting clipped to device region


#symbol.Male(centerx = 0.09, centery = 1.1, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package


par(xpd=FALSE)



K1_post_plot <- denschart3( mean_param_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK1)) - 0.002, 0.001*mean(exp(post$matLK1)) + 0.002), #( 10, 10.6 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK1)), 0.001*mean(exp(post$berLK1)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK1)), 0.001*mean(exp(post$matLK1)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.04,0.05), labels=c(0.04,0.05),
    cex.axis=cex_axis)
#mtext(expression(paste("Infant ",italic("K")[1]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -60, adj=0.5)


H1_post_plot <- denschart3( mean_param_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK1)/2 + post$berR1) - 0.001, 0.001*mean(exp(post$matLK1)/2 + post$matR1) + 0.001), #( 10.25, 10.75 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK1)/2 + post$berR1), 0.001*mean(exp(post$berLK1)/2 + post$berR1) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK1)/2 + post$matR1), 0.001*mean(exp(post$matLK1)/2 + post$matR1) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
     #at=c(0.03,0.04), labels=c(0.03,0.04),
     cex.axis=cex_axis)
#mtext(expression(paste("Infant ",italic("H")[1]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = -51, adj=0.5)


Q2_post_plot <- denschart3( mean_param_list[c(7,8)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$berLQ2)) - 0.0008, mean(exp(post$matLQ2)) + 0.0008), #( 0.085, 0.095 ),
            yvals = c(#0.5,
                      0.5,
                      0.5)
 )

lines(x=c( mean(exp(post$berLQ2)), mean(exp(post$berLQ2)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ2)), mean(exp(post$matLQ2)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.095,0.1), labels=c(0.095,0.1),
    cex.axis=cex_axis)
#mtext(expression(paste("Child ",italic("q")[2])), side = 1, outer = T, cex = 1.5, line = -42.5, adj=0.5)


K2_post_plot <- denschart3( mean_param_list[c(9,10)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK2)) - 0.000007, 0.001*mean(exp(post$berLK2)) + 0.000007), #( 3.6, 4 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK2)), 0.001*mean(exp(post$berLK2)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK2)), 0.001*mean(exp(post$matLK2)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.004916,0.00493), labels=c(0.004916,0.00493),
    cex.axis=cex_axis)
#mtext(expression(paste("Child ",italic("K")[2]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -34, adj=0.5)


H2_post_plot <- denschart3( mean_param_list[c(11,12)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK2)/2 + post$matR2) - 0.000013, 0.001*mean(exp(post$berLK2)/2 + post$berR2) + 0.000013 ), #( 2.6, 2.8 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK2)/2 + post$berR2), 0.001*mean(exp(post$berLK2)/2 + post$berR2) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK2)/2 + post$matR2), 0.001*mean(exp(post$matLK2)/2 + post$matR2) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.00355,0.0036), labels=c(0.00355,0.0036),
    cex.axis=cex_axis)
#mtext(expression(paste("Child ",italic("H")[2]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = -25, adj=0.5)



Q3_post_plot <- denschart3( mean_param_list[c(13,14)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$berLQ3)) - 7e-7, mean(exp(post$matLQ3)) + 7e-7), #( 0.0000056,  0.0000068 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean(exp(post$berLQ3)), mean(exp(post$berLQ3)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ3)), mean(exp(post$matLQ3)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    at=c(3.9e-05,4.04e-05), labels=c(3.88e-05,4.04e-05),
    cex.axis=cex_axis)
#mtext(expression(paste("Adolescent ",italic("q")[3])), side = 1, outer = T, cex = 1.5, line = -16, adj=0.5)


K3_post_plot <- denschart3( mean_param_list[c(15,16)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK3)) - 0.05, 0.001*mean(exp(post$matLK3)) + 0.05 ), #( 120000, 145000 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK3)), 0.001*mean(exp(post$berLK3)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK3)), 0.001*mean(exp(post$matLK3)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(19.6,19.7), labels=c(19.6,19.7),
    cex.axis=cex_axis)
#mtext(expression(paste("Adolescent ",italic("K")[3]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -7.5, adj=0.5)


H3_post_plot <- denschart3( mean_param_list[c(17,18)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK3)/2 + post$berR3) - 0.02, 0.001*mean(exp(post$matLK3)/2 + post$matR3) + 0.02 ), #( 60000, 75000 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK3)/2 + post$berR3), 0.001*mean(exp(post$berLK3)/2 + post$berR3) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK3)/2 + post$matR3), 0.001*mean(exp(post$matLK3)/2 + post$matR3) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(9.8,9.85), labels=c(9.8,9.85),
    cex.axis=cex_axis)
#mtext(expression(paste("Adolescent ",italic("H")[3]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)



### parameter contrasts


Q1_cont_plot <- denschart3( contr_param_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.0002,0.0002),
                #range( min(contr_param_list$ber.matQ1), max(contr_param_list$ber.matQ1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ1), mean(contr_param_list$ber.matQ1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-1.5e-4,1.5e-4), labels=c(-1.5e-4,1.5e-4),
    cex.axis=cex_axis)




K1_cont_plot <- denschart3( contr_param_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.005,0.005),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK1), mean(contr_param_list$ber.matK1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.003,0.003), labels=c(-0.003,0.003),
    cex.axis=cex_axis)


H1_cont_plot <- denschart3( contr_param_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.003,0.003),
                #range( min(contr_param_list$ber.matH1), max(contr_param_list$ber.matH1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH1), mean(contr_param_list$ber.matH1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.002,0.002), labels=c(-0.002,0.002),
    cex.axis=cex_axis)


Q2_cont_plot <- denschart3( contr_param_list[4],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.005,0.005),
                #range( min(contr_param_list$ber.matQ2), max(contr_param_list$ber.matQ2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ2), mean(contr_param_list$ber.matQ2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.003,0.003), labels=c(-0.003,0.003),
    cex.axis=cex_axis)


K2_cont_plot <- denschart3( contr_param_list[5],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.00001,0.00001),
                #range( min(contr_param_list$ber.matK2), max(contr_param_list$ber.matK2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK2), mean(contr_param_list$ber.matK2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-8e-6,8e-6), labels=c(-8e-6,8e-6),
    cex.axis=cex_axis)


H2_cont_plot <- denschart3( contr_param_list[6],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.00003,0.00003),
                #range( min(contr_param_list$ber.matH2), max(contr_param_list$ber.matH2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH2), mean(contr_param_list$ber.matH2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-2e-5,2e-5), labels=c(-2e-5,2e-5),
    cex.axis=cex_axis)


Q3_cont_plot <- denschart3( contr_param_list[7],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.000001,0.000001),
                #range( min(contr_param_list$ber.matQ3), max(contr_param_list$ber.matQ3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ3), mean(contr_param_list$ber.matQ3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-7e-7,7e-7), labels=c(-7e-7,7e-7),
    cex.axis=cex_axis)


K3_cont_plot <- denschart3( contr_param_list[8],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.1,0.1),
                #range( min(contr_param_list$ber.matK3), max(contr_param_list$ber.matK3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK3), mean(contr_param_list$ber.matK3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.05,0.05), labels=c(-0.05,0.05),
    cex.axis=cex_axis)


H3_cont_plot <- denschart3( contr_param_list[9],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.05,0.05),
                #range( min(contr_param_list$ber.matH3), max(contr_param_list$ber.matH3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH3), mean(contr_param_list$ber.matH3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.02,0.02), labels=c(-0.02,0.02),
    cex.axis=cex_axis)




# female weight ###########################################################################################################################################################################
post <- post3_fw
mean_param_list <- mean_param_list_fw
contr_param_list <- contr_param_list_fw



Q1_post_plot <- denschart3( mean_param_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$berLQ1)) - 0.0008, mean(exp(post$matLQ1)) + 0.0008 ), #( 0.155, 0.165 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean(exp(post$berLQ1)), mean(exp(post$berLQ1)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ1)), mean(exp(post$matLQ1)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.0089,0.0091), labels=c(0.0089,0.0091),
    cex.axis=cex_axis)
#mtext(expression(paste("Infant ",italic("q")[1])), side = 1, outer = T, cex = 1.5, line = -69, adj=0.5)

#mtext("Composite model parameter posteriors", side = 3, outer = F, cex = 2, line = 1)
# par(xpd=NA) # plotting clipped to device region

#symbol.Female(centerx = 0.01, centery = 1.1, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package

par(xpd=FALSE) # plotting clipped to plot region



K1_post_plot <- denschart3( mean_param_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK1)) - 0.001, 0.001*mean(exp(post$matLK1)) + 0.001 ), #( 10, 10.6 ),
            yvals = c(0.5,
                      0.5)
            )
lines(x=c( 0.001*mean(exp(post$berLK1)), 0.001*mean(exp(post$berLK1)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK1)), 0.001*mean(exp(post$matLK1)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.7,0.8), labels=c(0.7,0.8),
    cex.axis=cex_axis)
#mtext(expression(paste("Infant ",italic("K")[1]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -60, adj=0.5)


H1_post_plot <- denschart3( mean_param_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK1)/2 + post$berR1) - 0.0005, 0.001*mean(exp(post$matLK1)/2 + post$matR1) + 0.0005), #( 10.25, 10.75 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK1)/2 + post$berR1), 0.001*mean(exp(post$berLK1)/2 + post$berR1) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK1)/2 + post$matR1), 0.001*mean(exp(post$matLK1)/2 + post$matR1) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.34,0.38), labels=c(0.34,0.38),
    cex.axis=cex_axis)
#mtext(expression(paste("Infant ",italic("H")[1]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = -51, adj=0.5)


Q2_post_plot <- denschart3( mean_param_list[c(7,8)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$berLQ2)) - 0.004, mean(exp(post$matLQ2)) + 0.004), #( 0.085, 0.095 ),
            yvals = c(0.5,0.5,0.5)
 )

lines(x=c( mean(exp(post$berLQ2)), mean(exp(post$berLQ2)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ2)), mean(exp(post$matLQ2)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.18, 0.19), labels=c(0.18, 0.19),
    cex.axis=cex_axis)
#mtext(expression(paste("Child ",italic("q")[2])), side = 1, outer = T, cex = 1.5, line = -42.5, adj=0.5)


K2_post_plot <- denschart3( mean_param_list[c(9,10)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK2)) - 0.0003, 0.001*mean(exp(post$berLK2)) + 0.0003), #( 3.6, 4 ),
            yvals = c(0.5,
                      0.5)
 )
lines(x=c( 0.001*mean(exp(post$berLK2)), 0.001*mean(exp(post$berLK2)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK2)), 0.001*mean(exp(post$matLK2)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.0045, 0.005), labels=c(0.0045, 0.005),
    cex.axis=cex_axis)
#mtext(expression(paste("Child ",italic("K")[2]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -34, adj=0.5)


H2_post_plot <- denschart3( mean_param_list[c(11,12)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK2)/2 + post$matR2) - 0.0002, 0.001*mean(exp(post$berLK2)/2 + post$berR2) + 0.0002 ), #( 2.6, 2.8 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK2)/2 + post$berR2), 0.001*mean(exp(post$berLK2)/2 + post$berR2) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK2)/2 + post$matR2), 0.001*mean(exp(post$matLK2)/2 + post$matR2) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.0025, 0.003), labels=c(0.0025, 0.003),
    cex.axis=cex_axis)
#mtext(expression(paste("Child ",italic("H")[2]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = -25, adj=0.5)



Q3_post_plot <- denschart3( mean_param_list[c(13,14)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$matLQ3)) - 0.0001, mean(exp(post$berLQ3)) + 0.0001 ), #( 0.0000056,  0.0000068 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean(exp(post$berLQ3)), mean(exp(post$berLQ3)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ3)), mean(exp(post$matLQ3)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.04, 0.044), labels=c(0.04, 0.044),
    cex.axis=cex_axis)
#mtext(expression(paste("Adolescent ",italic("q")[3])), side = 1, outer = T, cex = 1.5, line = -16, adj=0.5)


K3_post_plot <- denschart3( mean_param_list[c(15,16)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK3)) - 0.0001, 0.001*mean(exp(post$berLK3)) + 0.0001 ), #( 120000, 145000 ),
            yvals = c(0.5,
                      0.5)
 )
lines(x=c( 0.001*mean(exp(post$berLK3)), 0.001*mean(exp(post$berLK3)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK3)), 0.001*mean(exp(post$matLK3)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.006, 0.007), labels=c(0.006, 0.007),
    cex.axis=cex_axis)
#mtext(expression(paste("Adolescent ",italic("K")[3]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -7.5, adj=0.5)


H3_post_plot <- denschart3( mean_param_list[c(17,18)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK3)/2 + post$matR3) - 0.00005, 0.001*mean(exp(post$berLK3)/2 + post$berR3) + 0.00005 ), #( 60000, 75000 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK3)/2 + post$berR3), 0.001*mean(exp(post$berLK3)/2 + post$berR3) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK3)/2 + post$matR3), 0.001*mean(exp(post$matLK3)/2 + post$matR3) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.0035, 0.004), labels=c(0.0034, 0.004),
    cex.axis=cex_axis)
#mtext(expression(paste("Adolescent ",italic("H")[3]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)



### parameter contrasts


Q1_cont_plot <- denschart3( contr_param_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.0009,0.0009),
                #range( min(contr_param_list$ber.matQ1), max(contr_param_list$ber.matQ1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ1), mean(contr_param_list$ber.matQ1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-4,6e-4), labels=c(-6e-4,6e-4),
    cex.axis=cex_axis)




K1_cont_plot <- denschart3( contr_param_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.001,0.001),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK1), mean(contr_param_list$ber.matK1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-4,6e-4), labels=c(-6e-4,6e-4),
    cex.axis=cex_axis)


H1_cont_plot <- denschart3( contr_param_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.0005,0.0005),
                #range( min(contr_param_list$ber.matH1), max(contr_param_list$ber.matH1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH1), mean(contr_param_list$ber.matH1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-3e-4,3e-4), labels=c(-3e-4,3e-4),
    cex.axis=cex_axis)


Q2_cont_plot <- denschart3( contr_param_list[4],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.03,0.03),
                #range( min(contr_param_list$ber.matQ2), max(contr_param_list$ber.matQ2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ2), mean(contr_param_list$ber.matQ2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.02,0.02), labels=c(-0.02,0.02),
    cex.axis=cex_axis)


K2_cont_plot <- denschart3( contr_param_list[5],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.001,0.001),
                #range( min(contr_param_list$ber.matK2), max(contr_param_list$ber.matK2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK2), mean(contr_param_list$ber.matK2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-4,6e-4), labels=c(-6e-4,6e-4),
    cex.axis=cex_axis)


H2_cont_plot <- denschart3( contr_param_list[6],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.0006,0.0006),
                #range( min(contr_param_list$ber.matH2), max(contr_param_list$ber.matH2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH2), mean(contr_param_list$ber.matH2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-5e-4,5e-4), labels=c(-5e-4,5e-4),
    cex.axis=cex_axis)


Q3_cont_plot <- denschart3( contr_param_list[7],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.0001,0.0001),
                #range( min(contr_param_list$ber.matQ3), max(contr_param_list$ber.matQ3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ3), mean(contr_param_list$ber.matQ3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-5,6e-5), labels=c(-6e-5,6e-5),
    cex.axis=cex_axis)


K3_cont_plot <- denschart3( contr_param_list[8],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.0001,0.0001),
                #range( min(contr_param_list$ber.matK3), max(contr_param_list$ber.matK3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK3), mean(contr_param_list$ber.matK3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-5,6e-5), labels=c(-6e-5,6e-5),
    cex.axis=cex_axis)


H3_cont_plot <- denschart3( contr_param_list[9],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.00005,0.00005),
                #range( min(contr_param_list$ber.matH3), max(contr_param_list$ber.matH3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH3), mean(contr_param_list$ber.matH3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-3e-5,3e-5), labels=c(-3e-5,3e-5),
    cex.axis=cex_axis)





# male weight ###########################################################################################################################################################################
post <- post3_mw
mean_param_list <- mean_param_list_mw
contr_param_list <- contr_param_list_mw



Q1_post_plot <- denschart3( mean_param_list[c(1,2)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$berLQ1)) - 0.0005, mean(exp(post$matLQ1)) + 0.0005 ), #( 0.155, 0.165 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean(exp(post$berLQ1)), mean(exp(post$berLQ1)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ1)), mean(exp(post$matLQ1)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.0089,0.0091), labels=c(0.0089,0.0091),
    cex.axis=cex_axis)
#mtext(expression(paste("Infant ",italic("q")[1])), side = 1, outer = T, cex = 1.5, line = -69, adj=0.5)

#mtext("Composite model parameter posteriors", side = 3, outer = F, cex = 2, line = 1)
#par(xpd=NA) # plotting clipped to device region

#symbol.Male(centerx = 0.01, centery = 1.1, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package

par(xpd=FALSE) # plotting clipped to plot region



K1_post_plot <- denschart3( mean_param_list[c(3,4)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK1)) - 0.001, 0.001*mean(exp(post$matLK1)) + 0.001 ), #( 10, 10.6 ),
            yvals = c(0.5,
                      0.5)
            )
lines(x=c( 0.001*mean(exp(post$berLK1)), 0.001*mean(exp(post$berLK1)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK1)), 0.001*mean(exp(post$matLK1)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.7,0.8), labels=c(0.7,0.8),
    cex.axis=cex_axis)
#mtext(expression(paste("Infant ",italic("K")[1]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -60, adj=0.5)


H1_post_plot <- denschart3( mean_param_list[c(5,6)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$berLK1)/2 + post$berR1) - 0.0005, 0.001*mean(exp(post$matLK1)/2 + post$matR1) + 0.0005), #( 10.25, 10.75 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK1)/2 + post$berR1), 0.001*mean(exp(post$berLK1)/2 + post$berR1) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK1)/2 + post$matR1), 0.001*mean(exp(post$matLK1)/2 + post$matR1) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.34,0.38), labels=c(0.34,0.38),
    cex.axis=cex_axis)
#mtext(expression(paste("Infant ",italic("H")[1]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = -51, adj=0.5)


Q2_post_plot <- denschart3( mean_param_list[c(7,8)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$berLQ2)) - 0.003, mean(exp(post$matLQ2)) + 0.003), #( 0.085, 0.095 ),
            yvals = c(0.5,0.5,0.5)
 )

lines(x=c( mean(exp(post$berLQ2)), mean(exp(post$berLQ2)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ2)), mean(exp(post$matLQ2)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.18, 0.19), labels=c(0.18, 0.19),
    cex.axis=cex_axis)
#mtext(expression(paste("Child ",italic("q")[2])), side = 1, outer = T, cex = 1.5, line = -42.5, adj=0.5)


K2_post_plot <- denschart3( mean_param_list[c(9,10)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK2)) - 0.0003, 0.001*mean(exp(post$berLK2)) + 0.0003), #( 3.6, 4 ),
            yvals = c(0.5,
                      0.5)
 )
lines(x=c( 0.001*mean(exp(post$berLK2)), 0.001*mean(exp(post$berLK2)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK2)), 0.001*mean(exp(post$matLK2)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.0045, 0.005), labels=c(0.0045, 0.005),
    cex.axis=cex_axis)
#mtext(expression(paste("Child ",italic("K")[2]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -34, adj=0.5)


H2_post_plot <- denschart3( mean_param_list[c(11,12)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK2)/2 + post$matR2) - 0.0002, 0.001*mean(exp(post$berLK2)/2 + post$berR2) + 0.0002 ), #( 2.6, 2.8 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK2)/2 + post$berR2), 0.001*mean(exp(post$berLK2)/2 + post$berR2) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK2)/2 + post$matR2), 0.001*mean(exp(post$matLK2)/2 + post$matR2) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.0025, 0.003), labels=c(0.0025, 0.003),
    cex.axis=cex_axis)
#mtext(expression(paste("Child ",italic("H")[2]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = -25, adj=0.5)



Q3_post_plot <- denschart3( mean_param_list[c(13,14)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( mean(exp(post$matLQ3)) - 0.00005, mean(exp(post$berLQ3)) + 0.00005 ), #( 0.0000056,  0.0000068 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean(exp(post$berLQ3)), mean(exp(post$berLQ3)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean(exp(post$matLQ3)), mean(exp(post$matLQ3)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.04, 0.044), labels=c(0.04, 0.044),
    cex.axis=cex_axis)
#mtext(expression(paste("Adolescent ",italic("q")[3])), side = 1, outer = T, cex = 1.5, line = -16, adj=0.5)


K3_post_plot <- denschart3( mean_param_list[c(15,16)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK3)) - 0.000008, 0.001*mean(exp(post$berLK3)) + 0.000008 ), #( 120000, 145000 ),
            yvals = c(0.5,
                      0.5)
 )
lines(x=c( 0.001*mean(exp(post$berLK3)), 0.001*mean(exp(post$berLK3)) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK3)), 0.001*mean(exp(post$matLK3)) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    at=c(0.008940, 0.008956), labels=c(0.008940, 0.008956),
    cex.axis=cex_axis)
#mtext(expression(paste("Adolescent ",italic("K")[3]," (g/kg)")), side = 1, outer = T, cex = 1.5, line = -7.5, adj=0.5)


H3_post_plot <- denschart3( mean_param_list[c(17,18)],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=c( NA,
                     NA
                    ),
            colorHPDI=c( MatArea_col,
                         BerArea_col
                        ),
            polyborder=c( MatLine_col,
                          BerLine_col
                        ),
            polyborderHPDI=c( MatLine_col,
                              BerLine_col
                            ),
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range( 0.001*mean(exp(post$matLK3)/2 + post$matR3) - 0.00001, 0.001*mean(exp(post$berLK3)/2 + post$berR3) + 0.00001 ), #( 60000, 75000 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( 0.001*mean(exp(post$berLK3)/2 + post$berR3), 0.001*mean(exp(post$berLK3)/2 + post$berR3) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( 0.001*mean(exp(post$matLK3)/2 + post$matR3), 0.001*mean(exp(post$matLK3)/2 + post$matR3) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.0035, 0.004), labels=c(0.0034, 0.004),
    cex.axis=cex_axis)
#mtext(expression(paste("Adolescent ",italic("H")[3]," (g/cm"^2,")")), side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)



### parameter contrasts


Q1_cont_plot <- denschart3( contr_param_list[1],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.002,0.002),
                #range( min(contr_param_list$ber.matQ1), max(contr_param_list$ber.matQ1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ1), mean(contr_param_list$ber.matQ1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.001,0.001), labels=c(-0.001,0.001),
    cex.axis=cex_axis)




K1_cont_plot <- denschart3( contr_param_list[2],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.001,0.001),
                #range( min(contr_param_list$ber.matK1), max(contr_param_list$ber.matK1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK1), mean(contr_param_list$ber.matK1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-4,6e-4), labels=c(-6e-4,6e-4),
    cex.axis=cex_axis)


H1_cont_plot <- denschart3( contr_param_list[3],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.0005,0.0005),
                #range( min(contr_param_list$ber.matH1), max(contr_param_list$ber.matH1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH1), mean(contr_param_list$ber.matH1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-3e-4,3e-4), labels=c(-3e-4,3e-4),
    cex.axis=cex_axis)


Q2_cont_plot <- denschart3( contr_param_list[4],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.016,0.016),
                #range( min(contr_param_list$ber.matQ2), max(contr_param_list$ber.matQ2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ2), mean(contr_param_list$ber.matQ2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.01,0.01), labels=c(-0.01,0.01),
    cex.axis=cex_axis)


K2_cont_plot <- denschart3( contr_param_list[5],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.001,0.001),
                #range( min(contr_param_list$ber.matK2), max(contr_param_list$ber.matK2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK2), mean(contr_param_list$ber.matK2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-4,6e-4), labels=c(-6e-4,6e-4),
    cex.axis=cex_axis)


H2_cont_plot <- denschart3( contr_param_list[6],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.0008,0.0008),
                #range( min(contr_param_list$ber.matH2), max(contr_param_list$ber.matH2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH2), mean(contr_param_list$ber.matH2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-5e-4,5e-4), labels=c(-5e-4,5e-4),
    cex.axis=cex_axis)


Q3_cont_plot <- denschart3( contr_param_list[7],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-1e-4,1e-4),
                #range( min(contr_param_list$ber.matQ3), max(contr_param_list$ber.matQ3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matQ3), mean(contr_param_list$ber.matQ3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6e-5,6e-5), labels=c(-6e-5,6e-5),
    cex.axis=cex_axis)


K3_cont_plot <- denschart3( contr_param_list[8],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.00003,0.00003),
                #range( min(contr_param_list$ber.matK3), max(contr_param_list$ber.matK3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matK3), mean(contr_param_list$ber.matK3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-2e-5,2e-5), labels=c(-2e-5,2e-5),
    cex.axis=cex_axis)


H3_cont_plot <- denschart3( contr_param_list[9],
            #labels=rev(quest_names)
            labels="",
            adjust=1,
            color=NA,
            colorHPDI=ConArea_col,
            polyborder=ConLine_col,
            polyborderHPDI=ConLine_col,
            HPDI=0.9,
            border=NA, yaxt="n",
            cex=0.8, height=0.4,
            #clip(-0.5, 21, 0, 2), #clip(x1, x2, y1, y2) clips drawing beyond the rectangle
            xlim=range(-0.00004,0.00004),
                #range( min(contr_param_list$ber.matH3), max(contr_param_list$ber.matH3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_param_list$ber.matH3), mean(contr_param_list$ber.matH3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)
par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-3e-5,3e-5), labels=c(-3e-5,3e-5),
    cex.axis=cex_axis)




graphics.off()






