

######################################### Fig 1 #########################################


######## load posteriors from model

# Ber female height
post3_h <- readRDS("./analysis_height_fem/post3.RDS")
#str(post3_h)

# Ber female weight
post3_w <- readRDS("./analysis_weight_fem/post3.RDS")
#str(post3_w)




######## plotting colors

colorlist <- palette.colors(palette = "Okabe-Ito", alpha=0.75) 
names(colorlist) <- c("black","orange","teal","green","yellow","blue","red","pink","grey")
colorlist
#pie(rep(1, 9), col = colorlist)


# line formats
cumu_lwd <- 4
cumu_lty <- 1
cumu_col <- "black"

inf_lwd <- 4
inf_lty <- 1
inf_col <- colorlist["blue"]

chil_lwd <- 4
chil_lty <- 1
chil_col <- colorlist["red"]

adol_lwd <- 4
adol_lty <- 1
adol_col <- colorlist["pink"]


pdf(file="./combined_plots/Plots/Composite_model_illustration.pdf",
    height=9, width=5)
layout( mat=matrix(c(1,2),2,1,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 3, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)



  x <- seq(from=(0), to=(25+0.75), by=0.1)        # sequence for plotting functions, include gestation in age



  ###### height

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,160), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=seq( 0, 160, by=40), labels=TRUE )


  post <- post3_h

  # plot posterior mean trajectory
  muQ1 = exp(mean(post$matLQ1))
  muQ2 = exp(mean(post$matLQ2))
  muQ3 = exp(mean(post$matLQ3))
  muK1 = exp(mean(post$matLK1))
  muK2 = exp(mean(post$matLK2))
  muK3 = exp(mean(post$matLK3))
  muH1 = muK1/2 + mean(post$matR1)
  muH2 = muK2/2 + mean(post$matR2)
  muH3 = muK3/2 + mean(post$matR3)

  # plot posterior mean trajectory
  lines(x = x,
        y = ( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1 )^(1/muQ1) +
            ( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2 )^(1/muQ2) +
            ( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3 )^(1/muQ3),
            col=cumu_col, lwd=cumu_lwd)

  # plot individual component functions

  # infant
  x <- seq(from=(0), to=(25+0.75), by=0.1) # infant growth all the way back to conception
  lines(x = x,
        y=( -1*exp(-1*muK1*muQ1*x/( 1 + 2*muQ1 )) + 2*muH1/muK1 )^(1/muQ1),
        col=inf_col, lwd=inf_lwd)

  # child
  lines(x = x,
        y=( -1*exp(-1*muK2*muQ2*x/( 1 + 2*muQ2 )) + 2*muH2/muK2 )^(1/muQ2),
        col=chil_col, lwd=chil_lwd)

  # adolescent
  lines(x = x,
        y=( -1*exp(-1*muK3*muQ3*x/( 1 + 2*muQ3 )) + 2*muH3/muK3 )^(1/muQ3),
        col=adol_col, lwd=adol_lwd)


  par(xpd=NA) # clip plotting to device region

  legend(-9.22, 200,
         legend=c("1. Growth in infancy",
                 "2. Growth in childhood",
                 "3. Growth in adolescence",
                 "Cumulative growth"), bty="o", bg="white",
         col=c(inf_col, chil_col, adol_col, cumu_col),
         lty=c(inf_lty, chil_lty, adol_lty, cumu_lty),
         lwd=c(inf_lwd, chil_lwd, adol_lwd, cumu_lwd),
         cex=0.8, seg.len=3, ncol=2) # ncol=number of columns of legend items



  ###### weight

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,65), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 60, by=20), labels=TRUE )
  par(xpd=FALSE) # clip to plot region
  
  post <- post3_w

  # plot posterior mean trajectory
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
        col=cumu_col, lwd=cumu_lwd)



  # plot individual component functions

  # infant
  lines(x = x,
        y = ( -1*exp(-1*berK1*berQ1*x/( 1 + 2*berQ1 )) + 2*berH1/berK1*pi^(berQ1/( 1 + 2*berQ1 )) )^(( 1 + 2*berQ1 )/berQ1),
            col=inf_col, lwd=inf_lwd)

  # child
  lines(x = x,
        y = ( -1*exp(-1*berK2*berQ2*x/( 1 + 2*berQ2 )) + 2*berH2/berK2*pi^(berQ2/( 1 + 2*berQ2 )) )^(( 1 + 2*berQ2 )/berQ2),
            col=chil_col, lwd=chil_lwd)

  # adolescent
  lines(x = x,
        y = ( -1*exp(-1*berK3*berQ3*x/( 1 + 2*berQ3 )) + 2*berH3/berK3*pi^(berQ3/( 1 + 2*berQ3 )) )^(( 1 + 2*berQ3 )/berQ3),
            col=adol_col, lwd=adol_lwd)



  par(xpd=NA) # clip plotting to device region
  mtext("Age since conception (years)", side = 1, outer = T, cex = 1.2, line = 2, adj=0.5)
  mtext("Height (cm)", side = 2, outer = T, cex = 1.2, line = 2, adj=0.8)
  mtext("Weight (kg)", side = 2, outer = T, cex = 1.2, line = 2, adj=0.2)


graphics.off()



















######################################### Fig A.4 #########################################

################## Illustration of power relationship between r and h

pdf(file="./combined_plots/Plots/Power_illustration.pdf",
    height=5, width=5)

  x <- seq(from=(0), to=(10), by=0.01)        # sequence for plotting functions, include gestation in age

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=10, type="n", ylim=c(0,10), xlim=c(0,10), cex=1.5,
        xlab=expression(paste("Time ", italic("t"))),
        ylab=expression(paste("Height ", italic("h"), " or Radius ", italic("r"))) )

  # height
  lines(x = x,
        y = x,
        col="black", lty=2, lwd=2)

  # r = 5
  lines(x = x,
        y = rep(5, times=length(x)),
        col="red", lty=1, lwd=3)

  # r = 0.5*h
  lines(x = x,
        y = 0.5*x,
        col="blue", lty=1, lwd=3)

  # r = h^0.5
  lines(x = x,
        y = x^0.5,
        col="black", lty=1, lwd=3)


  legend(0, 10,
         legend=c("Height",
                  expression(paste("Radius when ", italic("r"), " = 5")),
                  expression(paste("Radius when ", italic("r"), " = 1/2*", italic("h"))),
                  expression(paste("Radius when ", italic("r"), " = ", italic("h"), "^(1/2)"))),
         bty="o", bg="white",
         col=c("black", "red", "blue", "black"),
         lty=c(2,1,1,1),
         lwd=c(2,3,3,3), cex=0.8, seg.len=3)

graphics.off()



