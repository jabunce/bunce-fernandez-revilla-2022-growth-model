

######################################### Fig 5 #########################################



######## load posteriors from complete models (m6), need to run Functions for densplot3 function

# male
post3_m <- readRDS("./analysis_hw_mal/post6.RDS")
#str(post3_m)

# female
post3_f <- readRDS("./analysis_hw_fem/post6.RDS")
#str(post3_f)





######## function to determine x-axis range for plotting
xrange <- function( D1=c(1,2,3), D2=c(2,3,4) ){

    # determine which distribution is to the right of the other
    if (mean(D1) > mean(D2)) {
        DR <- D1
        DL <- D2
    } else {
        DR <- D2
        DL <- D1
    } #else
    
    # boundaries for plotting
    Lbound <- HPDI(DL,0.9)[1] - 0.5*( mean(DL) - HPDI(DL,0.9)[1] )
    Rbound <- HPDI(DR,0.9)[2] + 0.5*( HPDI(DL,0.9)[2] - mean(DL) )

    # axis labels
    Llab <- HPDI(DL,0.9)[1]
    Rlab <- HPDI(DR,0.9)[2]

    x <- formatC(x=(Rlab - Llab), format = "f", digits = 10)
    decimals <- attr(regexpr("(?<=\\.)0+|$", x, perl = TRUE), "match.length") #number of zeros after decimal point before first integer

    Llabr <- round(Llab, digits=decimals+1)
    Rlabr <- round(Rlab, digits=decimals+1)

    if ( Llabr > Lbound && Rlabr < Rbound ) {
        Llabo <- Llabr
        Rlabo <- Rlabr
    } else {
        Llabo <- formatC(x=Llab, format = "f", digits = decimals+3)
        Rlabo <- formatC(x=Rlab, format = "f", digits = decimals+3)
    } #else

    return( list(Lbound=Lbound, Rbound=Rbound, Llab=Llabo, Rlab=Rlabo) )
} # xrange


#D1 <- (2*H1mat/K1mat)^(1/Q1mat)
#D2 <- (2*H1ber/K1ber)^(1/Q1ber)
#unlist(xrange(D1=D1, D2=D2)$Lbound)



######## function to determine x-axis range for plotting contrasts
xrangecon <- function( D1=c(1,2,3) ){
    
    # boundaries for plotting
    if ( abs(HPDI(D1,0.9)[1]) > abs(HPDI(D1,0.9)[2]) ) {
        bound <- abs(HPDI(D1,0.9)[1]) + 0.2*abs(HPDI(D1,0.9)[1])
        lab <- abs(HPDI(D1,0.9)[1])
    } else { 
        bound <- abs(HPDI(D1,0.9)[2]) + 0.2*abs(HPDI(D1,0.9)[2])
        lab <- abs(HPDI(D1,0.9)[2])
    } #else

    x <- formatC(x=lab, format = "f", digits = 10)
    decimals <- attr(regexpr("(?<=\\.)0+|$", x, perl = TRUE), "match.length") #number of zeros after decimal point before first integer

    labr <- round(lab, digits=decimals+1)

    if ( labr < bound ) {
        Llab <- -1*labr
        Rlab <- labr
    } else { 
        Llab <- formatC(x=-1*lab, format = "f", digits = decimals+3)
        Rlab <- formatC(x=lab, format = "f", digits = decimals+3)
    } #else

    Lbound <- -1*bound
    Rbound <- bound

    return( list(Lbound=Lbound, Rbound=Rbound, Llab=Llab, Rlab=Rlab) )
} # xrangecon


#D1 <- contr_param_list$ber.matMaxHeight1
#xrangecon(D1=D1)






########## create lists of parameter posteriors


##### female

post <- post3_f

mean_param_list_f <- list(
                            post$"mQ[2,1]",
                            post$"mQ[1,1]",

                            post$"mK[2,1]",
                            post$"mK[1,1]",

                            (post$"mH[2,1]")*(2/32), # original H in units of g/cm^2 of skin surface of function=~5% of body mass (**DO NOT need to multiply by (cm^2 skin surface area)/(g body mass)*1/0.05 **). Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
                            (post$"mH[1,1]")*(2/32),



                            post$"mQ[2,2]",
                            post$"mQ[1,2]",

                            post$"mK[2,2]",
                            post$"mK[1,2]",

                            (post$"mH[2,2]")*(2/32),
                            (post$"mH[1,2]")*(2/32),



                            post$"mQ[2,3]",
                            post$"mQ[1,3]", 

                            post$"mK[2,3]",
                            post$"mK[1,3]",

                            (post$"mH[2,3]")*(2/32),
                            (post$"mH[1,3]")*(2/32),



                            post$"mQ[2,4]",
                            post$"mQ[1,4]",                            

                            post$"mK[2,4]",
                            post$"mK[1,4]",

                            (post$"mH[2,4]")*(2/32),
                            (post$"mH[1,4]")*(2/32),



                            post$"mQ[2,5]",
                            post$"mQ[1,5]",

                            post$"mK[2,5]",
                            post$"mK[1,5]",

                            (post$"mH[2,5]")*(2/32),
                            (post$"mH[1,5]")*(2/32)
                          )


names(mean_param_list_f) <- c(
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
                                "berH3",


                                "matQ4",
                                "berQ4",

                                "matK4",
                                "berK4",

                                "matH4",
                                "berH4",


                                "matQ5",
                                "berQ5",

                                "matK5",
                                "berK5",

                                "matH5",
                                "berH5" 
                              )

#str(mean_param_list_f)



contr_param_list_f <- list(
                       mean_param_list_f$berQ1 - mean_param_list_f$matQ1,
                       mean_param_list_f$berK1 - mean_param_list_f$matK1,
                       mean_param_list_f$berH1 - mean_param_list_f$matH1,

                       mean_param_list_f$berQ2 - mean_param_list_f$matQ2,
                       mean_param_list_f$berK2 - mean_param_list_f$matK2,
                       mean_param_list_f$berH2 - mean_param_list_f$matH2,

                       mean_param_list_f$berQ3 - mean_param_list_f$matQ3,
                       mean_param_list_f$berK3 - mean_param_list_f$matK3,
                       mean_param_list_f$berH3 - mean_param_list_f$matH3,

                       mean_param_list_f$berQ4 - mean_param_list_f$matQ4,
                       mean_param_list_f$berK4 - mean_param_list_f$matK4,
                       mean_param_list_f$berH4 - mean_param_list_f$matH4,

                       mean_param_list_f$berQ5 - mean_param_list_f$matQ5,
                       mean_param_list_f$berK5 - mean_param_list_f$matK5,
                       mean_param_list_f$berH5 - mean_param_list_f$matH5
                      )


names(contr_param_list_f) <- c(
                          "ber.matQ1",
                          "ber.matK1",
                          "ber.matH1",

                          "ber.matQ2",
                          "ber.matK2",
                          "ber.matH2",

                          "ber.matQ3",
                          "ber.matK3",
                          "ber.matH3",

                          "ber.matQ4",
                          "ber.matK4",
                          "ber.matH4",

                          "ber.matQ5",
                          "ber.matK5",
                          "ber.matH5"                          
                        )

#str(contr_param_list_f)





##### male

post <- post3_m

mean_param_list_m <- list(
                            post$"mQ[2,1]",
                            post$"mQ[1,1]",

                            post$"mK[2,1]",
                            post$"mK[1,1]",

                            (post$"mH[2,1]")*(2/32), # original H in units of g/cm^2 of 5% of total body skin surface. Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
                            (post$"mH[1,1]")*(2/32),



                            post$"mQ[2,2]",
                            post$"mQ[1,2]",

                            post$"mK[2,2]",
                            post$"mK[1,2]",

                            (post$"mH[2,2]")*(2/32),
                            (post$"mH[1,2]")*(2/32),



                            post$"mQ[2,3]",
                            post$"mQ[1,3]", 

                            post$"mK[2,3]",
                            post$"mK[1,3]",

                            (post$"mH[2,3]")*(2/32),
                            (post$"mH[1,3]")*(2/32),



                            post$"mQ[2,4]",
                            post$"mQ[1,4]",                            

                            post$"mK[2,4]",
                            post$"mK[1,4]",

                            (post$"mH[2,4]")*(2/32),
                            (post$"mH[1,4]")*(2/32),



                            post$"mQ[2,5]",
                            post$"mQ[1,5]",

                            post$"mK[2,5]",
                            post$"mK[1,5]",

                            (post$"mH[2,5]")*(2/32),
                            (post$"mH[1,5]")*(2/32)
                          )


names(mean_param_list_m) <- c(
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
                                "berH3",


                                "matQ4",
                                "berQ4",

                                "matK4",
                                "berK4",

                                "matH4",
                                "berH4",


                                "matQ5",
                                "berQ5",

                                "matK5",
                                "berK5",

                                "matH5",
                                "berH5" 
                            )

#str(mean_param_list_m)



contr_param_list_m <- list(
                       mean_param_list_m$berQ1 - mean_param_list_m$matQ1,
                       mean_param_list_m$berK1 - mean_param_list_m$matK1,
                       mean_param_list_m$berH1 - mean_param_list_m$matH1,

                       mean_param_list_m$berQ2 - mean_param_list_m$matQ2,
                       mean_param_list_m$berK2 - mean_param_list_m$matK2,
                       mean_param_list_m$berH2 - mean_param_list_m$matH2,

                       mean_param_list_m$berQ3 - mean_param_list_m$matQ3,
                       mean_param_list_m$berK3 - mean_param_list_m$matK3,
                       mean_param_list_m$berH3 - mean_param_list_m$matH3,

                       mean_param_list_m$berQ4 - mean_param_list_m$matQ4,
                       mean_param_list_m$berK4 - mean_param_list_m$matK4,
                       mean_param_list_m$berH4 - mean_param_list_m$matH4,

                       mean_param_list_m$berQ5 - mean_param_list_m$matQ5,
                       mean_param_list_m$berK5 - mean_param_list_m$matK5,
                       mean_param_list_m$berH5 - mean_param_list_m$matH5
                      )


names(contr_param_list_m) <- c(
                          "ber.matQ1",
                          "ber.matK1",
                          "ber.matH1",

                          "ber.matQ2",
                          "ber.matK2",
                          "ber.matH2",

                          "ber.matQ3",
                          "ber.matK3",
                          "ber.matH3",

                          "ber.matQ4",
                          "ber.matK4",
                          "ber.matH4",

                          "ber.matQ5",
                          "ber.matK5",
                          "ber.matH5"                          
                        )

#str(contr_param_list_m)








######## define plotting colors

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





pdf(file="./combined_plots/Plots/Params_phases_combined.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16,17,18,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                       19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1, 0.2, 1,1, 0.2, 1,1, 0.2, 1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels

# female height ###########################################################################################################################################################################
post <- post3_f
mean_param_list <- mean_param_list_f
contr_param_list <- contr_param_list_f


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
            xlim= c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[1]) ), mean( unlist(mean_param_list[1]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[2]) ), mean( unlist(mean_param_list[2]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c(0.0835, 0.0855),#c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
        #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels=c(0.0835, 0.0855), #c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
             #   xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




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

  legend(x=-17, y=20.5,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerArea_col, MatArea_col, ConArea_col),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=3,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = -16.5, #9.2,
       ybottom = 14.5,
       xright = 51.1, #86.8,
       ytop = 20,
       lwd=1)

# row labels
mtext(expression(bold("In utero")),        side = 1, outer = T, cex = 2.5, adj=-0.17, line = -108 )
mtext(expression(bold("Infancy")),         side = 1, outer = T, cex = 2.5, adj=-0.17, line = -70.5 )
mtext(expression(bold("Early childhood")), side = 1, outer = T, cex = 2.5, adj=-0.19, line = -33 )

mtext(expression(paste(bolditalic("q")[1])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -103 )
mtext(expression(paste(bolditalic("K")[1])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -90.5 )
mtext(expression(paste("(g/g)")),            side = 1, outer = T, cex = 2,   adj=-0.09, line = -87.5 )
mtext(expression(paste(bolditalic("H")[1])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -78 )
mtext(expression(paste("(g/cm"^2,")")),      side = 1, outer = T, cex = 2,   adj=-0.105, line = -75 )


mtext(expression(paste(bolditalic("q")[2])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -65.5 )
mtext(expression(paste(bolditalic("K")[2])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -53 )
mtext(expression(paste("(g/g)")),            side = 1, outer = T, cex = 2,   adj=-0.09, line = -50 )
mtext(expression(paste(bolditalic("H")[2])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -40.5 )
mtext(expression(paste("(g/cm"^2,")")),      side = 1, outer = T, cex = 2,   adj=-0.105, line = -37.5 )

mtext(expression(paste(bolditalic("q")[3])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -28 )
mtext(expression(paste(bolditalic("K")[3])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -15.5 )
mtext(expression(paste("(g/g)")),            side = 1, outer = T, cex = 2,   adj=-0.09, line = -12.5 )
mtext(expression(paste(bolditalic("H")[3])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -3 )
mtext(expression(paste("(g/cm"^2,")")),      side = 1, outer = T, cex = 2,   adj=-0.105, line = 0 )

# column labels
symbol.Female(centerx = 11, centery = 10.8, rayonx=0.7, lwd=3, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.107, line = -110.5 )

symbol.Male(  centerx = 38.5, centery = 10.4, rayonx=0.7, lwd=3, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.385, line = -110.5 )



# horizontal lines
lines(x=c(-18.5, 51),
      y=c(-38,-38),
      col="black", lwd=4, lty=1)

lines(x=c(-18.5, 51),
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
            xlim= c( xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Rbound ),
            #range( mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ) - 0.04, mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ) + 0.04), #( 0.85, 0.95 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[3]) ), mean( unlist(mean_param_list[3]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[4]) ), mean( unlist(mean_param_list[4]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Llab,
           xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Rlab ),
     labels=c( xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Llab,
               xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Rlab ),     
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)


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
            xlim= c( xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[5]) ), mean( unlist(mean_param_list[5]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[6]) ), mean( unlist(mean_param_list[6]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Llab,
           xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Llab,
                xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)



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
            xlim= c( xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[7]) ), mean( unlist(mean_param_list[7]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[8]) ), mean( unlist(mean_param_list[8]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Llab,
           xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Llab,
                xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Child max height (cm)", side = 1, outer = T, cex = 1.5, line = -38.5, adj=0.5)


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
            xlim= c( xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[9]) ), mean( unlist(mean_param_list[9]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[10]) ), mean( unlist(mean_param_list[10]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Llab,
           xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Llab,
                xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Rlab ),
     #c(75,85), labels=c(75,85),
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
            xlim= c( xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[11]) ), mean( unlist(mean_param_list[11]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[12]) ), mean( unlist(mean_param_list[12]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Llab,
           xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Llab,
                xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Child max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -18.5, adj=0.5)




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
            xlim= c( xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[13]) ), mean( unlist(mean_param_list[13]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[14]) ), mean( unlist(mean_param_list[14]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Llab,
           xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Llab,
                xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Adolescent max height (cm)", side = 1, outer = T, cex = 1.5, line = -9, adj=0.5)



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
            xlim= c( xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[15]) ), mean( unlist(mean_param_list[15]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[16]) ), mean( unlist(mean_param_list[16]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Llab,
           xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Llab,
                xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Adolescent age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)




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
            xlim= c( xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[17]) ), mean( unlist(mean_param_list[17]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[18]) ), mean( unlist(mean_param_list[18]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Llab,
           xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Llab,
                xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)

#mtext("Adolescent Max Velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)




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
            xlim= range(xrangecon( D1=unlist(contr_param_list[1]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[1]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[1])), mean(unlist(contr_param_list[1])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[1]) )$Llab, xrangecon( D1=unlist(contr_param_list[1]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[1]) )$Llab, xrangecon( D1=unlist(contr_param_list[1]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[2]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[2]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[2])), mean(unlist(contr_param_list[2])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[2]) )$Llab, xrangecon( D1=unlist(contr_param_list[2]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[2]) )$Llab, xrangecon( D1=unlist(contr_param_list[2]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[3]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[3]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[3])), mean(unlist(contr_param_list[3])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[3]) )$Llab, xrangecon( D1=unlist(contr_param_list[3]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[3]) )$Llab, xrangecon( D1=unlist(contr_param_list[3]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[4]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[4]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[4])), mean(unlist(contr_param_list[4])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[4]) )$Llab, xrangecon( D1=unlist(contr_param_list[4]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[4]) )$Llab, xrangecon( D1=unlist(contr_param_list[4]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[5]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[5]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[5])), mean(unlist(contr_param_list[5])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[5]) )$Llab, xrangecon( D1=unlist(contr_param_list[5]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[5]) )$Llab, xrangecon( D1=unlist(contr_param_list[5]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[6]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[6]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[6])), mean(unlist(contr_param_list[6])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[6]) )$Llab, xrangecon( D1=unlist(contr_param_list[6]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[6]) )$Llab, xrangecon( D1=unlist(contr_param_list[6]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[7]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[7]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[7])), mean(unlist(contr_param_list[7])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[7]) )$Llab, xrangecon( D1=unlist(contr_param_list[7]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[7]) )$Llab, xrangecon( D1=unlist(contr_param_list[7]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[8]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[8]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[8])), mean(unlist(contr_param_list[8])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[8]) )$Llab, xrangecon( D1=unlist(contr_param_list[8]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[8]) )$Llab, xrangecon( D1=unlist(contr_param_list[8]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[9]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[9]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[9])), mean(unlist(contr_param_list[9])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[9]) )$Llab, xrangecon( D1=unlist(contr_param_list[9]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[9]) )$Llab, xrangecon( D1=unlist(contr_param_list[9]) )$Rlab),
    cex.axis=cex_axis)






# male height ###########################################################################################################################################################################
post <- post3_m
mean_param_list <- mean_param_list_m
contr_param_list <- contr_param_list_m


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
            xlim= c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[1]) ), mean( unlist(mean_param_list[1]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[2]) ), mean( unlist(mean_param_list[2]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
           xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Llab,
                xrange( D1=unlist(mean_param_list[1]),D2=unlist(mean_param_list[2]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)

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
            xlim= c( xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Rbound ),
            #range( mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ) - 0.04, mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ) + 0.04), #( 0.85, 0.95 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[3]) ), mean( unlist(mean_param_list[3]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[4]) ), mean( unlist(mean_param_list[4]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Llab,
           xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Rlab ),
     labels=c( xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Llab,
               xrange( D1=unlist(mean_param_list[3]),D2=unlist(mean_param_list[4]) )$Rlab ),     
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)



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
            xlim= c( xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[5]) ), mean( unlist(mean_param_list[5]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[6]) ), mean( unlist(mean_param_list[6]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Llab,
           xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Llab,
                xrange( D1=unlist(mean_param_list[5]),D2=unlist(mean_param_list[6]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)



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
            xlim= c( xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[7]) ), mean( unlist(mean_param_list[7]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[8]) ), mean( unlist(mean_param_list[8]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Llab,
           xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Llab,
                xrange( D1=unlist(mean_param_list[7]),D2=unlist(mean_param_list[8]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Child max height (cm)", side = 1, outer = T, cex = 1.5, line = -38.5, adj=0.5)



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
            xlim= c( xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[9]) ), mean( unlist(mean_param_list[9]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[10]) ), mean( unlist(mean_param_list[10]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Llab,
           xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Llab,
                xrange( D1=unlist(mean_param_list[9]),D2=unlist(mean_param_list[10]) )$Rlab ),
     #c(75,85), labels=c(75,85),
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
            xlim= c( xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[11]) ), mean( unlist(mean_param_list[11]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[12]) ), mean( unlist(mean_param_list[12]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Llab,
           xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Llab,
                xrange( D1=unlist(mean_param_list[11]),D2=unlist(mean_param_list[12]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Child max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -18.5, adj=0.5)



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
            xlim= c( xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[13]) ), mean( unlist(mean_param_list[13]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[14]) ), mean( unlist(mean_param_list[14]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Llab,
           xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Llab,
                xrange( D1=unlist(mean_param_list[13]),D2=unlist(mean_param_list[14]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Adolescent max height (cm)", side = 1, outer = T, cex = 1.5, line = -9, adj=0.5)


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
            xlim= c( xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[15]) ), mean( unlist(mean_param_list[15]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[16]) ), mean( unlist(mean_param_list[16]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Llab,
           xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Llab,
                xrange( D1=unlist(mean_param_list[15]),D2=unlist(mean_param_list[16]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Adolescent age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)


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
            xlim= c( xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[17]) ), mean( unlist(mean_param_list[17]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[18]) ), mean( unlist(mean_param_list[18]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Llab,
           xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Llab,
                xrange( D1=unlist(mean_param_list[17]),D2=unlist(mean_param_list[18]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)

#mtext("Adolescent Max Velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)



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
            xlim= range(xrangecon( D1=unlist(contr_param_list[1]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[1]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[1])), mean(unlist(contr_param_list[1])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[1]) )$Llab, xrangecon( D1=unlist(contr_param_list[1]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[1]) )$Llab, xrangecon( D1=unlist(contr_param_list[1]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[2]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[2]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[2])), mean(unlist(contr_param_list[2])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[2]) )$Llab, xrangecon( D1=unlist(contr_param_list[2]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[2]) )$Llab, xrangecon( D1=unlist(contr_param_list[2]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[3]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[3]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[3])), mean(unlist(contr_param_list[3])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[3]) )$Llab, xrangecon( D1=unlist(contr_param_list[3]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[3]) )$Llab, xrangecon( D1=unlist(contr_param_list[3]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[4]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[4]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[4])), mean(unlist(contr_param_list[4])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[4]) )$Llab, xrangecon( D1=unlist(contr_param_list[4]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[4]) )$Llab, xrangecon( D1=unlist(contr_param_list[4]) )$Rlab),
    cex.axis=cex_axis)


K2_cont_plot <- denschart3( contr_param_list[5],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[5]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[5]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[5])), mean(unlist(contr_param_list[5])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[5]) )$Llab, xrangecon( D1=unlist(contr_param_list[5]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[5]) )$Llab, xrangecon( D1=unlist(contr_param_list[5]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[6]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[6]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[6])), mean(unlist(contr_param_list[6])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[6]) )$Llab, xrangecon( D1=unlist(contr_param_list[6]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[6]) )$Llab, xrangecon( D1=unlist(contr_param_list[6]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[7]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[7]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[7])), mean(unlist(contr_param_list[7])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[7]) )$Llab, xrangecon( D1=unlist(contr_param_list[7]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[7]) )$Llab, xrangecon( D1=unlist(contr_param_list[7]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[8]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[8]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[8])), mean(unlist(contr_param_list[8])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[8]) )$Llab, xrangecon( D1=unlist(contr_param_list[8]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[8]) )$Llab, xrangecon( D1=unlist(contr_param_list[8]) )$Rlab),
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[9]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[9]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[9])), mean(unlist(contr_param_list[9])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[9]) )$Llab, xrangecon( D1=unlist(contr_param_list[9]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[9]) )$Llab, xrangecon( D1=unlist(contr_param_list[9]) )$Rlab),
    cex.axis=cex_axis)





graphics.off()














###########################################################################################################################################
###########################################################################################################################################


pdf(file="./combined_plots/Plots/Params_phases_combined2.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6, 0, 0, 0, 7, 8, 9,10,11,12,0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                       13,14,15,16,17,18, 0, 0, 0,19,20,21,22,23,24,0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1, 0.2, 1,1, 0.2, 1,1, 0.2, 1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels


# female height ###########################################################################################################################################################################
post <- post3_f
mean_param_list <- mean_param_list_f
contr_param_list <- contr_param_list_f


Q4_post_plot <- denschart3( mean_param_list[c(19,20)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[19]) ), mean( unlist(mean_param_list[19]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[20]) ), mean( unlist(mean_param_list[20]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Llab,
           xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Llab,
                xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




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

  legend(x=-17, y=20.5,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerArea_col, MatArea_col, ConArea_col),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=3,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = -16.5,
       ybottom = 14.5,
       xright = 51.1,
       ytop = 20,
       lwd=1)

# row labels
mtext(expression(bold("Late childhood")),   side = 1, outer = T, cex = 2.5, adj=-0.21, line = -106 ) #-108
mtext(expression(bold("Adolescence")),       side = 1, outer = T, cex = 2.5, adj=-0.18, line = -70.5 )
#mtext(expression(bold("Child 2")), side = 1, outer = T, cex = 2.5, adj=-0.185, line = -33 )

mtext(expression(paste(bolditalic("q")[4])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -102 ) #-103
mtext(expression(paste(bolditalic("K")[4])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -90.5 )
mtext(expression(paste("(g/g)")),            side = 1, outer = T, cex = 2,   adj=-0.09, line = -87.5 )
mtext(expression(paste(bolditalic("H")[4])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -78 )
mtext(expression(paste("(g/cm"^2,")")),      side = 1, outer = T, cex = 2,   adj=-0.105, line = -75 )


mtext(expression(paste(bolditalic("q")[5])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -65.5 )
mtext(expression(paste(bolditalic("K")[5])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -53 )
mtext(expression(paste("(g/g)")),            side = 1, outer = T, cex = 2,   adj=-0.09, line = -50 )
mtext(expression(paste(bolditalic("H")[5])), side = 1, outer = T, cex = 2.5, adj=-0.08, line = -40.5 )
mtext(expression(paste("(g/cm"^2,")")),      side = 1, outer = T, cex = 2,   adj=-0.105, line = -37.5 )


# column labels
symbol.Female(centerx = 11, centery = 10.8, rayonx=0.7, lwd=3, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.107, line = -110.5 )

symbol.Male(  centerx = 38.5, centery = 10.4, rayonx=0.7, lwd=3, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.385, line = -110.5 )




# horizontal lines
lines(x=c(-18.5, 51),
      y=c(-38,-38),
      col="black", lwd=4, lty=1)

# lines(x=c(-18.5, 107),
#       y=c(-85.5,-85.5),
#       col="black", lwd=4, lty=1)


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




par(xpd=FALSE)



K4_post_plot <- denschart3( mean_param_list[c(21,22)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Rbound ),
            #range( mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ) - 0.04, mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ) + 0.04), #( 0.85, 0.95 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[21]) ), mean( unlist(mean_param_list[21]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[22]) ), mean( unlist(mean_param_list[22]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Llab,
           xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Rlab ),
     labels=c( xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Llab,
               xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Rlab ),     
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)


H4_post_plot <- denschart3( mean_param_list[c(23,24)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[23]) ), mean( unlist(mean_param_list[23]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[24]) ), mean( unlist(mean_param_list[24]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Llab,
           xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Llab,
                xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)



Q5_post_plot <- denschart3( mean_param_list[c(25,26)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[25]) ), mean( unlist(mean_param_list[25]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[26]) ), mean( unlist(mean_param_list[26]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Llab,
           xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Llab,
                xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Child max height (cm)", side = 1, outer = T, cex = 1.5, line = -38.5, adj=0.5)


K5_post_plot <- denschart3( mean_param_list[c(27,28)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[27]) ), mean( unlist(mean_param_list[27]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[28]) ), mean( unlist(mean_param_list[28]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Llab,
           xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Llab,
                xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)



H5_post_plot <- denschart3( mean_param_list[c(29,30)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[29]) ), mean( unlist(mean_param_list[29]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[30]) ), mean( unlist(mean_param_list[30]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Llab,
           xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Llab,
                xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Child max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -18.5, adj=0.5)





### parameter contrasts


Q4_cont_plot <- denschart3( contr_param_list[10],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[10]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[10]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[10])), mean(unlist(contr_param_list[10])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[10]) )$Llab, xrangecon( D1=unlist(contr_param_list[10]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[10]) )$Llab, xrangecon( D1=unlist(contr_param_list[10]) )$Rlab),
    cex.axis=cex_axis)



K4_cont_plot <- denschart3( contr_param_list[11],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[11]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[11]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[11])), mean(unlist(contr_param_list[11])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[11]) )$Llab, xrangecon( D1=unlist(contr_param_list[11]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[11]) )$Llab, xrangecon( D1=unlist(contr_param_list[11]) )$Rlab),
    cex.axis=cex_axis)



H4_cont_plot <- denschart3( contr_param_list[12],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[12]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[12]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[12])), mean(unlist(contr_param_list[12])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[12]) )$Llab, xrangecon( D1=unlist(contr_param_list[12]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[12]) )$Llab, xrangecon( D1=unlist(contr_param_list[12]) )$Rlab),
    cex.axis=cex_axis)


Q5_cont_plot <- denschart3( contr_param_list[13],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[13]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[13]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[13])), mean(unlist(contr_param_list[13])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[13]) )$Llab, xrangecon( D1=unlist(contr_param_list[13]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[13]) )$Llab, xrangecon( D1=unlist(contr_param_list[13]) )$Rlab),
    cex.axis=cex_axis)



K5_cont_plot <- denschart3( contr_param_list[14],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[14]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[14]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[14])), mean(unlist(contr_param_list[14])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[14]) )$Llab, xrangecon( D1=unlist(contr_param_list[14]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[14]) )$Llab, xrangecon( D1=unlist(contr_param_list[14]) )$Rlab),
    cex.axis=cex_axis)



H5_cont_plot <- denschart3( contr_param_list[15],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[15]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[15]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[15])), mean(unlist(contr_param_list[15])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[15]) )$Llab, xrangecon( D1=unlist(contr_param_list[15]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[15]) )$Llab, xrangecon( D1=unlist(contr_param_list[15]) )$Rlab),
    cex.axis=cex_axis)







# male height ###########################################################################################################################################################################
post <- post3_m
mean_param_list <- mean_param_list_m
contr_param_list <- contr_param_list_m


Q4_post_plot <- denschart3( mean_param_list[c(19,20)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[19]) ), mean( unlist(mean_param_list[19]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[20]) ), mean( unlist(mean_param_list[20]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Llab,
           xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Llab,
                xrange( D1=unlist(mean_param_list[19]),D2=unlist(mean_param_list[20]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)




K4_post_plot <- denschart3( mean_param_list[c(21,22)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Rbound ),
            #range( mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ) - 0.04, mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ) + 0.04), #( 0.85, 0.95 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[21]) ), mean( unlist(mean_param_list[21]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[22]) ), mean( unlist(mean_param_list[22]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Llab,
           xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Rlab ),
     labels=c( xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Llab,
               xrange( D1=unlist(mean_param_list[21]),D2=unlist(mean_param_list[22]) )$Rlab ),     
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)


H4_post_plot <- denschart3( mean_param_list[c(23,24)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[23]) ), mean( unlist(mean_param_list[23]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[24]) ), mean( unlist(mean_param_list[24]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Llab,
           xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Llab,
                xrange( D1=unlist(mean_param_list[23]),D2=unlist(mean_param_list[24]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)



Q5_post_plot <- denschart3( mean_param_list[c(25,26)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[25]) ), mean( unlist(mean_param_list[25]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[26]) ), mean( unlist(mean_param_list[26]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Llab,
           xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Llab,
                xrange( D1=unlist(mean_param_list[25]),D2=unlist(mean_param_list[26]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Child max height (cm)", side = 1, outer = T, cex = 1.5, line = -38.5, adj=0.5)


K5_post_plot <- denschart3( mean_param_list[c(27,28)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[27]) ), mean( unlist(mean_param_list[27]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[28]) ), mean( unlist(mean_param_list[28]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Llab,
           xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Llab,
                xrange( D1=unlist(mean_param_list[27]),D2=unlist(mean_param_list[28]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)



H5_post_plot <- denschart3( mean_param_list[c(29,30)],
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
            xlim= c( xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Lbound,
                     xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Rbound ),
            #range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( unlist(mean_param_list[29]) ), mean( unlist(mean_param_list[29]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( unlist(mean_param_list[30]) ), mean( unlist(mean_param_list[30]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
     at=c( xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Llab,
           xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Llab,
                xrange( D1=unlist(mean_param_list[29]),D2=unlist(mean_param_list[30]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Child max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -18.5, adj=0.5)





### parameter contrasts


Q4_cont_plot <- denschart3( contr_param_list[10],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[10]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[10]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[10])), mean(unlist(contr_param_list[10])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[10]) )$Llab, xrangecon( D1=unlist(contr_param_list[10]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[10]) )$Llab, xrangecon( D1=unlist(contr_param_list[10]) )$Rlab),
    cex.axis=cex_axis)



K4_cont_plot <- denschart3( contr_param_list[11],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[11]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[11]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[11])), mean(unlist(contr_param_list[11])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[11]) )$Llab, xrangecon( D1=unlist(contr_param_list[11]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[11]) )$Llab, xrangecon( D1=unlist(contr_param_list[11]) )$Rlab),
    cex.axis=cex_axis)



H4_cont_plot <- denschart3( contr_param_list[12],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[12]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[12]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[12])), mean(unlist(contr_param_list[12])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[12]) )$Llab, xrangecon( D1=unlist(contr_param_list[12]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[12]) )$Llab, xrangecon( D1=unlist(contr_param_list[12]) )$Rlab),
    cex.axis=cex_axis)


Q5_cont_plot <- denschart3( contr_param_list[13],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[13]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[13]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[13])), mean(unlist(contr_param_list[13])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[13]) )$Llab, xrangecon( D1=unlist(contr_param_list[13]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[13]) )$Llab, xrangecon( D1=unlist(contr_param_list[13]) )$Rlab),
    cex.axis=cex_axis)



K5_cont_plot <- denschart3( contr_param_list[14],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[14]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[14]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[14])), mean(unlist(contr_param_list[14])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[14]) )$Llab, xrangecon( D1=unlist(contr_param_list[14]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[14]) )$Llab, xrangecon( D1=unlist(contr_param_list[14]) )$Rlab),
    cex.axis=cex_axis)



H5_cont_plot <- denschart3( contr_param_list[15],
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
            xlim= range(xrangecon( D1=unlist(contr_param_list[15]) )$Lbound,
                        xrangecon( D1=unlist(contr_param_list[15]) )$Rbound ),
                #range(-9,9),
                #range( min(contr_param_list$ber.matMaxHeight1), max(contr_param_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(unlist(contr_param_list[15])), mean(unlist(contr_param_list[15])) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_param_list[15]) )$Llab, xrangecon( D1=unlist(contr_param_list[15]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_param_list[15]) )$Llab, xrangecon( D1=unlist(contr_param_list[15]) )$Rlab),
    cex.axis=cex_axis)





graphics.off()






