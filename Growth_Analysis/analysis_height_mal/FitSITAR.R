

################## SITAR 


#install.packages("sitar")
#library(sitar) #load this before packages in RunAll



####################################################################################################################################
# dataset without height set to 0 at conception, duplicating Berkeley data until age 25

Mat.sub.e <- Mat.sub
Ber.clean.e <- Ber.clean
Mat.sub.e[1:20,]
Ber.clean.e[1:20,]

#### duplicate last height and weight measurements 8 times
Ber.exp.e <- Ber.clean.e
for ( x in 1:8 ) {
  for ( y in 1:length(unique(Ber.exp.e$PID)) ) {
    Ber.exp.e[nrow(Ber.exp.e)+1,] <- c( unique(Ber.exp.e$PID)[y],
                                    as.numeric(tail(Ber.exp.e[which(Ber.exp.e$PID == unique(Ber.exp.e$PID)[y]),"Ethnicity"],1)),
                                    as.numeric(tail(Ber.exp.e[which(Ber.exp.e$PID == unique(Ber.exp.e$PID)[y]),"Sex.1m"],1)),
                                    as.numeric(tail(Ber.exp.e[which(Ber.exp.e$PID == unique(Ber.exp.e$PID)[y]),"Age"],1)) + 1,
                                    as.numeric(tail(Ber.exp.e[which(Ber.exp.e$PID == unique(Ber.exp.e$PID)[y]),"Height"],1)),
                                    as.numeric(tail(Ber.exp.e[which(Ber.exp.e$PID == unique(Ber.exp.e$PID)[y]),"Weight"],1)) )
  } # for y
} # for x


#### Add gestational time to age
Mat.sub.e$TotAge <- as.numeric(Mat.sub.e$Age) + 0.75
Ber.exp.e$TotAge <- as.numeric(Ber.exp.e$Age) + 0.75

#### Make height, weight, and ethnicity numeric
Mat.sub.e$Height <- as.numeric(Mat.sub.e$Height)
Mat.sub.e$Weight <- as.numeric(Mat.sub.e$Weight)
Mat.sub.e$TotAge <- as.numeric(Mat.sub.e$TotAge)
Mat.sub.e$Ethnicity <- as.numeric(Mat.sub.e$Ethnicity)


#### Split dataset by sex
Ber.fem <- Ber.exp.e[which(Ber.exp.e$Sex.1m == 2),]
Ber.mal <- Ber.exp.e[which(Ber.exp.e$Sex.1m == 1),]
dim(Ber.fem)
dim(Ber.mal)

Mat.fem <- Mat.sub.e[which(Mat.sub.e$Sex.1m == 2),]
Mat.mal <- Mat.sub.e[which(Mat.sub.e$Sex.1m == 1),]
dim(Mat.fem)
dim(Mat.mal)


#### Make consecutive IDs
num_femB <- length( unique(Ber.fem$PID) )
Ber.fem$ID <- as.numeric( factor(Ber.fem$PID, levels=unique(Ber.fem$PID)) ) #trick to assign consecutive numbers to IDs

num_malB <- length( unique(Ber.mal$PID) )
Ber.mal$ID <- as.numeric( factor(Ber.mal$PID, levels=unique(Ber.mal$PID)) )


num_femM <- length( unique(Mat.fem$PID) )
Mat.fem$ID <- as.numeric( factor(Mat.fem$PID, levels=unique(Mat.fem$PID)) )

num_malM <- length( unique(Mat.mal$PID) )
Mat.mal$ID <- as.numeric( factor(Mat.mal$PID, levels=unique(Mat.mal$PID)) )



#### Combine ethnic groups and re-number consecutive IDs

Com.fem.e <- rbind(Ber.fem,Mat.fem)
Com.fem.e$ID <- as.numeric( factor(Com.fem.e$PID, levels=unique(Com.fem.e$PID)) )

Com.mal.e <- rbind(Ber.mal,Mat.mal)
Com.mal.e$ID <- as.numeric( factor(Com.mal.e$PID, levels=unique(Com.mal.e$PID)) )



############## Fit SITAR model


eth <- Com.mal.e$Ethnicity - 1  # Berkeley = 0, Matsigenka = 1
Com.mal.eth.e <- cbind(Com.mal.e, eth)


sitar.com.mal.9.e <- sitar( x=log(TotAge),                        # log age since conception recommended by Tim Cole
                           y=Height,
                           id=ID,
                           data=Com.mal.eth.e,
                           df=10,                                 # df sets the degrees of freedom for the mean spline curve, taking values from 1 (i.e. linear) upwards. 
                           #random="a+b+c",                       # The SITAR model usually has up to three random effects (a, b and c), termed size, timing and intensity respectively. In addition d can be fitted, along with a, b and c, to extend SITAR to model variability in the adult slope of the growth curve.
                           a.formula = ~eth,                      # fixed effect of ethnicity for each of the three indiv-level random effects
                           b.formula = ~eth,
                           c.formula = ~eth,
                           #xoffset="mean",                        # optional value of offset for x (either "mean" (default), "apv" or value). (also try 2)
                           #method="ML",                           # From nlme function: If "REML" the model is fit by maximizing the restricted loglikelihood. If "ML" the log-likelihood is maximized.
                           #control = nlmeControl(maxIter=100,     # maxIter: maximum number of iterations for the nlme optimization algorithm default=50 (also try 100)   
                           #                      msMaxIter=200,   # maximum number of iterations for nlminb (iter.max) or the nlm (iterlim, from the 10-th step) optimization step inside the nlme optimization. Default is 50 (which may be too small for e.g. for overparametrized cases).
                           #                      minScale=0.001), # minScale: minimum factor by which to shrink the default step size in an attempt to decrease the sum of squares in the PNLS step. Default=0.001 (also try 0.000001)
                           verbose=TRUE
                          )

sitar_model <- sitar.com.mal.9.e

#plot(sitar.com.mal.9.e, subset = eth == 0)
#lines(sitar.com.mal.9.e, subset = eth == 1, col = 2, y2par = list(col = 2))


# extracted predicted mean trajectories
mat_mean <- predict(sitar_model,
                          newdata=data.frame(TotAge=seq(from=0.001,
                            #from=min(Com.mal.e$TotAge), 
                            to=27, by=0.1), eth=1),
                          #abc=c(-954.8183 - 15.1761, -0.8274 - 0.6505, -0.1180 + 0.0431)
                          #asList = TRUE,
                          level=0
                          )
sitar_mat_mean <- cbind( seq(from=0.001,
                            #from=min(Com.mal.e$TotAge),
                            to=27, by=0.1), as.vector(mat_mean) )
colnames(sitar_mat_mean) <- c("TotAge", "Height")
saveRDS(sitar_mat_mean, "sitar_mat_mean.RDS")


ber_mean <- predict(sitar_model,
                          newdata=data.frame(TotAge=seq(from=0.001,
                            #from=min(Com.mal.e$TotAge),
                            to=27, by=0.1), eth=0),
                          #abc=c(-954.8183 - 15.1761, -0.8274 - 0.6505, -0.1180 + 0.0431)
                          #asList = TRUE,
                          level=0
                          )
sitar_ber_mean <- cbind( seq(from=0.001,
                            #from=min(Com.mal.e$TotAge),
                            to=27, by=0.1), as.vector(ber_mean) )
colnames(sitar_ber_mean) <- c("TotAge", "Height")
saveRDS(sitar_ber_mean, "sitar_ber_mean.RDS")


# extracted predicted mean velocities
mat_vmean <- predict(sitar_model,
                          newdata=data.frame(TotAge=seq(from=0.001,
                            #from=min(Com.mal.e$TotAge),
                            to=27, by=0.1), eth=1),
                          deriv = 1,
                          xfun = exp, # to undo log(age)
                          level=0
                          )
sitar_mat_vmean <- cbind( seq(from=0.001,
                            #from=min(Com.mal.e$TotAge),
                            to=27, by=0.1), as.vector(mat_vmean) )
colnames(sitar_mat_vmean) <- c("TotAge", "HeightVelocity")
saveRDS(sitar_mat_vmean, "sitar_mat_vmean.RDS")


ber_vmean <- predict(sitar_model,
                          newdata=data.frame(TotAge=seq(from=0.001,
                            #from=min(Com.mal.e$TotAge),
                            to=27, by=0.1), eth=0),
                          deriv = 1,
                          xfun = exp, # to undo log(age)
                          level=0
                          )
sitar_ber_vmean <- cbind( seq(from=0.001,
                            #from=min(Com.mal.e$TotAge),
                            to=27, by=0.1), as.vector(ber_vmean) )
colnames(sitar_ber_vmean) <- c("TotAge", "HeightVelocity")
saveRDS(sitar_ber_vmean, "sitar_ber_vmean.RDS")



# point and line colors and sizes

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

BerPoint_lwd <- 0.5
BerPoint_col <- colorlist["2.3"]
BerPoint_cex <- 0.5
BerPoint_pch <- 1

MatPoint_lwd <- 0.5
MatPoint_col <- colorlist["1.3"]
MatPoint_cex <- 0.5
MatPoint_pch <- 1


# BerIndivTraj_lwd <- 0.25
# BerIndivTraj_col <- colorlist["2.4"]
# BerIndivTraj_lty <- 1

# MatIndivTraj_lwd <- 0.25
# MatIndivTraj_col <- colorlist["1.4"]
# MatIndivTraj_lty <- 1


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





pdf(file="./Plots/sitar_ber_mat.e.pdf",
    height=5, width=5)

  #set up plot
  par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )


  points(x = Com.mal.e[which(Com.mal.e$Ethnicity == 1),"TotAge"],
         y = Com.mal.e[which(Com.mal.e$Ethnicity == 1),"Height"],
         lwd=1, col=BerPoint_col, cex=0.5)

  points(x = Com.mal.e[which(Com.mal.e$Ethnicity == 2),"TotAge"],
         y = Com.mal.e[which(Com.mal.e$Ethnicity == 2),"Height"],
         lwd=1, col=MatPoint_col, cex=0.5)


  # plot predicted mean trajectory
  points( x = sitar_ber_mean[,"TotAge"],
          y = sitar_ber_mean[,"Height"],
          type = "l",
           lwd=3, col=BerMeanTraj_col)

  points( x = sitar_mat_mean[,"TotAge"],
          y = sitar_mat_mean[,"Height"],
          type = "l",
           lwd=3, col=MatMeanTraj_col)


#plot mean velocity trajectory

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=seq( 0, 16, by=2 ), las=2 )
  par(xpd=FALSE) # don't plot beyond the axes
  #text("Growth velocity (cm/year)", x=33, y=8, srt=270, las=3)


  points( x = sitar_ber_vmean[,"TotAge"],
          y = sitar_ber_vmean[,"HeightVelocity"],
          type = "l",
           lwd=3, col=BerVelTraj_col)

  points( x = sitar_mat_vmean[,"TotAge"],
          y = sitar_mat_vmean[,"HeightVelocity"],
          type = "l",
           lwd=3, col=MatVelTraj_col)


  par(xpd=TRUE)
  text("Growth velocity (cm/year)", x=33, y=8, srt=270, las=3)


  legend(-0.7, 16.5,
         legend=c("U.S. data", "Matsigenka data",
                 "Estimated U.S. height", "Est. Matsigenka height",
                 "Estimated U.S. velocity", "Est. Matigenka velocity"
                 ), bty="o", bg="white",
         col=c(BerPoint_col, MatPoint_col, BerMeanTraj_col, MatMeanTraj_col, BerVelTraj_col, MatVelTraj_col),
         merge=FALSE,
         pch=c(1,1,NA,NA,NA,NA),
         lty=c(0,0,1,1,1,1),
         lwd=c(1,1,3,3,3,3), cex=0.6, seg.len=3)

symbol.Male(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package



graphics.off()







# #################################################################################################
# # Dataset with height 0 at conception and last height duplicated until age 25
# # convergence errors

# eth <- Com.mal$Ethnicity - 1  # Berkeley = 0, Matsigenka = 1
# Com.mal.eth <- cbind(Com.mal, eth)
# Com.mal.eth[1:20,]


# Com.mal.eth[which(Com.mal.eth$TotAge < 0.01),"TotAge"] <- 0
# Com.mal.eth[which(Com.mal.eth$Height < 0.01),"Height"] <- 0
# Com.mal.eth[which(Com.mal.eth$Weight < 0.01),"Weight"] <- 0

# sitar.com.mal.9 <- sitar( x=TotAge,
#                            y=Height,
#                            id=ID,
#                            data=Com.mal.eth,
#                            df=9,                                  # df sets the degrees of freedom for the mean spline curve, taking values from 1 (i.e. linear) upwards. 
#                            #random="a+b+c",                        # The SITAR model usually has up to three random effects (a, b and c), termed size, timing and intensity respectively. In addition d can be fitted, along with a, b and c, to extend SITAR to model variability in the adult slope of the growth curve.
#                            a.formula = ~eth,                      # fixed effect of ethnicity for each of the three indiv-level random effects
#                            b.formula = ~eth,
#                            c.formula = ~eth,
#                            #xoffset="mean",                        # optional value of offset for x (either "mean" (default), "apv" or value). (also try 2)
#                            #method="ML",                           # From nlme function: If "REML" the model is fit by maximizing the restricted loglikelihood. If "ML" the log-likelihood is maximized.
#                            control = nlmeControl(maxIter=100,     # maxIter: maximum number of iterations for the nlme optimization algorithm default=50 (also try 100)   
#                                                  msMaxIter=200,   # maximum number of iterations for nlminb (iter.max) or the nlm (iterlim, from the 10-th step) optimization step inside the nlme optimization. Default is 50 (which may be too small for e.g. for overparametrized cases).
#                                                  minScale=0.001), # minScale: minimum factor by which to shrink the default step size in an attempt to decrease the sum of squares in the PNLS step. Default=0.001 (also try 0.000001)
#                            verbose=TRUE
#                           )

# sitar_model <- sitar.com.mal.9



# # # does not converge with log(TotAge)
# # sitar.com.mal.9 <- sitar( x=log(TotAge),
# #                            y=Height,
# #                            id=ID,
# #                            data=Com.mal.eth,
# #                            df=9,                                  # df sets the degrees of freedom for the mean spline curve, taking values from 1 (i.e. linear) upwards. 
# #                            #random="a+b+c",                        # The SITAR model usually has up to three random effects (a, b and c), termed size, timing and intensity respectively. In addition d can be fitted, along with a, b and c, to extend SITAR to model variability in the adult slope of the growth curve.
# #                            a.formula = ~eth,                      # fixed effect of ethnicity for each of the three indiv-level random effects
# #                            b.formula = ~eth,
# #                            c.formula = ~eth,
# #                            #xoffset="mean",                        # optional value of offset for x (either "mean" (default), "apv" or value). (also try 2)
# #                            #method="ML",                           # From nlme function: If "REML" the model is fit by maximizing the restricted loglikelihood. If "ML" the log-likelihood is maximized.
# #                            control = nlmeControl(maxIter=100,     # maxIter: maximum number of iterations for the nlme optimization algorithm default=50 (also try 100)   
# #                                                  msMaxIter=200,   # maximum number of iterations for nlminb (iter.max) or the nlm (iterlim, from the 10-th step) optimization step inside the nlme optimization. Default is 50 (which may be too small for e.g. for overparametrized cases).
# #                                                  minScale=0.001), # minScale: minimum factor by which to shrink the default step size in an attempt to decrease the sum of squares in the PNLS step. Default=0.001 (also try 0.000001)
# #                            verbose=TRUE
# #                           )


# #sitar_model <- sitar.com.mal.9

# #plot(sitar.com.mal.9, subset = eth == 0)
# #lines(sitar.com.mal.9, subset = eth == 1, col = 2, y2par = list(col = 2))


# #plot(sitar.com.mal.9, y2par=list(col='blue'), apv=TRUE)


# # summary(sitar_model)


# # # fitted spline trajectories for each individual
# # plot(sitar_model,
# #      opt = "DV",          # 'd' for fitted Distance curve, 'v' for fitted Velocity curve, 'c' for fitted Crosssectional distance curve, 'D' for individual fitted Distance curves, 'V' for individual fitted Velocity curves, 'u' for Unadjusted individual growth curves, and 'a' for Adjusted individual growth curves. Options 'dvcDV' give spline curves, while 'ua' give data curves made up as line segments.
# #      y2par=list(col='blue'), # parameters for second y axis
# #      apv=TRUE                # vertical line at age of max velocity
# #      )


# # # actual trajectories for each individual
# # plot(sitar_model,
# #      opt = "ua"
# #      )

# # # extract mean fitted height trajectory
# # m_traj_com <- plot_d(sitar_model)
# # glimpse(m_traj_com)
# # mean_traj_com <- as.data.frame( cbind( m_traj_com$.x + 0.75, as.vector(m_traj_com$.y) ) )
# # names(mean_traj_com) <- c("TotAge", "Height" )
# # mean_traj_com[1:10,]


# # # extract individual fitted height trajectories
# # i_traj_com <- plot_D(sitar_model)
# # glimpse(i_traj_com)
# # indiv_traj_com <- as.data.frame( cbind( as.vector(as.numeric(i_traj_com$.id)), i_traj_com$.x, as.vector(i_traj_com$.y) ) )
# # names(indiv_traj_com) <- c("ID", "TotAge", "Height" )
# # indiv_traj_com[1:200,]
# # unique(indiv_traj_com$ID)

# # length(indiv_traj_com[which(indiv_traj_com$ID == 1),"ID"])

# # indiv_traj_com[which(indiv_traj_com$ID == 200),]


# #Com.mal[,c("ID","Ethnicity")]


# # extracted predicted mean trajectories
# mat_mean <- predict(sitar_model,
#                           newdata=data.frame(TotAge=seq(from=0, to=27, by=0.5), eth=1),
#                           #abc=c(-954.8183 - 15.1761, -0.8274 - 0.6505, -0.1180 + 0.0431)
#                           #asList = TRUE,
#                           level=0
#                           )
# sitar_mat_mean <- cbind( seq(from=0, to=27, by=0.5), as.vector(mat_mean) )
# colnames(sitar_mat_mean) <- c("TotAge", "Height")


# ber_mean <- predict(sitar_model,
#                           newdata=data.frame(TotAge=seq(from=0, to=27, by=0.5), eth=0),
#                           #abc=c(-954.8183 - 15.1761, -0.8274 - 0.6505, -0.1180 + 0.0431)
#                           #asList = TRUE,
#                           level=0
#                           )
# sitar_ber_mean <- cbind( seq(from=0, to=27, by=0.5), as.vector(ber_mean) )
# colnames(sitar_ber_mean) <- c("TotAge", "Height")


# # extracted predicted mean velocities
# mat_vmean <- predict(sitar_model,
#                           newdata=data.frame(TotAge=seq(from=0, to=27, by=0.1), eth=1),
#                           deriv = 1,
#                           level=0
#                           )
# sitar_mat_vmean <- cbind( seq(from=0, to=27, by=0.1), as.vector(mat_vmean) )
# colnames(sitar_mat_vmean) <- c("TotAge", "HeightVelocity")


# ber_vmean <- predict(sitar_model,
#                           newdata=data.frame(TotAge=seq(from=0, to=27, by=0.1), eth=0),
#                           deriv = 1,
#                           level=0
#                           )
# sitar_ber_vmean <- cbind( seq(from=0, to=27, by=0.1), as.vector(ber_vmean) )
# colnames(sitar_ber_vmean) <- c("TotAge", "HeightVelocity")



# # point and line colors and sizes

# colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
#                         alpha=0.75)
# names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
#                       "2.5","2.4","2.3","2.2","2.1")
# #pie(rep(1, 11), col = colorlist)

# colorlist2 <- hcl.colors(n=11, palette="Green-Brown",
#                         alpha=0.75)
# names(colorlist2) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
#                       "2.5","2.4","2.3","2.2","2.1")
# #pie(rep(1, 11), col = colorlist2)


# BerPoint_lwd <- 0.5
# BerPoint_col <- colorlist["2.3"]
# BerPoint_cex <- 0.5
# BerPoint_pch <- 1

# MatPoint_lwd <- 0.5
# MatPoint_col <- colorlist["1.3"]
# MatPoint_cex <- 0.5
# MatPoint_pch <- 1


# # BerIndivTraj_lwd <- 0.25
# # BerIndivTraj_col <- colorlist["2.4"]
# # BerIndivTraj_lty <- 1

# # MatIndivTraj_lwd <- 0.25
# # MatIndivTraj_col <- colorlist["1.4"]
# # MatIndivTraj_lty <- 1


# BerMeanTraj_lwd <- 3
# BerMeanTraj_col <- colorlist["2.1"]
# BerMeanTraj_lty <- 1

# MatMeanTraj_lwd <- 3
# MatMeanTraj_col <- colorlist["1.1"]
# MatMeanTraj_lty <- 1


# BerVelTraj_lwd <- 2
# BerVelTraj_col <- colorlist2["2.2"]
# BerVelTraj_lty <- 1

# MatVelTraj_lwd <- 2
# MatVelTraj_col <- colorlist2["1.2"]
# MatVelTraj_lty <- 1




# pdf(file="./Plots/sitar_ber_mat.pdf",
#     height=5, width=5)

#   #set up plot
#   par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
#   plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )



#   # # plot individual posterior trajectories for berkeley
#   # for ( z in 1:max(Com.mal[which(Com.mal$Ethnicity==1),"ID"]) ){


#   #    points( x = indiv_traj_com[which( indiv_traj_com$ID == z ),"TotAge"],
#   #            y = indiv_traj_com[which( indiv_traj_com$ID == z ),"Height"],
#   #            type = "l", col="red")


#   #   points( x = seq(from=0, to=26, by=0.5), #indiv_traj_com[which(indiv_traj_com$ID == x),"TotAge"],
#   #           y = as.vector( predict(sitar_model,
#   #                                  newdata=data.frame(TotAge=seq(from=0, to=26, by=0.5), ID=x),
#   #                                  level=1
#   #                                  )
#   #                         ), #indiv_traj_com[which(indiv_traj_com$ID == x),"Height"],
#   #           type = "l", col="red" )
    
#   # } #for z

#   # # plot individual posterior trajectories for matsi
#   # for ( z in 1:length(unique(Com.mal[which(Com.mal$Ethnicity==2),"ID"])) ){

#   #   x <- z + min(Com.mal[which(Com.mal$Ethnicity==2),"ID"]) - 1

#   #    points( x = indiv_traj_com[which( indiv_traj_com$ID == x ),"TotAge"],
#   #            y = indiv_traj_com[which( indiv_traj_com$ID == x ),"Height"],
#   #            type = "l", col="blue")

#   #   points( x = seq(from=0, to=26, by=0.5), #indiv_traj_com[which(indiv_traj_com$ID == x),"TotAge"],
#   #           y = as.vector( predict(sitar_model,
#   #                                  newdata=data.frame(TotAge=seq(from=0, to=26, by=0.5), ID=x),
#   #                                  #abc=c(a=-15.1727,b=-0.6496,c=0.0430),
#   #                                  level=1
#   #                                  )
#   #                         ), #indiv_traj_com[which(indiv_traj_com$ID == x),"Height"],
#   #           type = "l", col="blue" )
    
#   # } #for z


#   # plot data points
#   # points(x = Com.mal[which(Com.mal$Ethnicity == 1),"TotAge"],
#   #        y = Com.mal[which(Com.mal$Ethnicity == 1),"Height"],
#   #        lwd=1, col=BerPoint_col, cex=0.5)

#   # points(x = Com.mal[which(Com.mal$Ethnicity == 2),"TotAge"],
#   #        y = Com.mal[which(Com.mal$Ethnicity == 2),"Height"],
#   #        lwd=1, col=MatPoint_col, cex=0.5)


#   # # plot predicted mean trajectory
#   # points( x = sitar_ber_mean[,"TotAge"],
#   #         y = sitar_ber_mean[,"Height"],
#   #         type = "l",
#   #          lwd=3, col=BerMeanTraj_col)

#   # points( x = sitar_mat_mean[,"TotAge"],
#   #         y = sitar_mat_mean[,"Height"],
#   #         type = "l",
#   #          lwd=3, col=MatMeanTraj_col)


#   points(x = Com.mal[which(Com.mal$Ethnicity == 1),"TotAge"],
#          y = Com.mal[which(Com.mal$Ethnicity == 1),"Height"],
#          lwd=1, col=BerPoint_col, cex=0.5)

#   points(x = Com.mal[which(Com.mal$Ethnicity == 2),"TotAge"],
#          y = Com.mal[which(Com.mal$Ethnicity == 2),"Height"],
#          lwd=1, col=MatPoint_col, cex=0.5)


#   # plot predicted mean trajectory
#   points( x = sitar_ber_mean[,"TotAge"],
#           y = sitar_ber_mean[,"Height"],
#           type = "l",
#            lwd=3, col=BerMeanTraj_col)

#   points( x = sitar_mat_mean[,"TotAge"],
#           y = sitar_mat_mean[,"Height"],
#           type = "l",
#            lwd=3, col=MatMeanTraj_col)


# #plot mean velocity trajectory

#   #set up 
#   par(new=TRUE) #add to existing plot

#   plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
#   axis( side=4, at=seq( 0, 16, by=2 ), las=2 )
#   par(xpd=FALSE) # don't plot beyond the axes
#   #text("Growth velocity (cm/year)", x=33, y=8, srt=270, las=3)


#   points( x = sitar_ber_vmean[,"TotAge"],
#           y = sitar_ber_vmean[,"HeightVelocity"],
#           type = "l",
#            lwd=3, col=BerVelTraj_col)

#   points( x = sitar_mat_vmean[,"TotAge"],
#           y = sitar_mat_vmean[,"HeightVelocity"],
#           type = "l",
#            lwd=3, col=MatVelTraj_col)


#   par(xpd=TRUE)
#   text("Growth velocity (cm/year)", x=33, y=8, srt=270, las=3)


#   legend(-0.7, 16.5,
#          legend=c("U.S. data", "Matsigenka data",
#                  "Estimated U.S. height", "Est. Matsigenka height",
#                  "Estimated U.S. velocity", "Est. Matigenka velocity"
#                  ), bty="o", bg="white",
#          col=c(BerPoint_col, MatPoint_col, BerMeanTraj_col, MatMeanTraj_col, BerVelTraj_col, MatVelTraj_col),
#          merge=FALSE,
#          pch=c(1,1,NA,NA,NA,NA),
#          lty=c(0,0,1,1,1,1),
#          lwd=c(1,1,3,3,3,3), cex=0.6, seg.len=3)

# symbol.Male(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package



# graphics.off()






# ####################################################################################################################################
# # dataset without height set to 0 at conception, without duplicating Berkeley data until age 25
# # covergence errors

# Mat.sub.o <- Mat.sub
# Ber.clean.o <- Ber.clean
# Mat.sub.o[1:20,]
# Ber.clean.o[1:20,]


# #### Add gestational time to age
# Mat.sub.o$TotAge <- as.numeric(Mat.sub.o$Age) + 0.75
# Ber.clean.o$TotAge <- as.numeric(Ber.clean.o$Age) + 0.75

# #### Make height, weight, and ethnicity numeric
# Mat.sub.o$Height <- as.numeric(Mat.sub.o$Height)
# Mat.sub.o$Weight <- as.numeric(Mat.sub.o$Weight)
# Mat.sub.o$TotAge <- as.numeric(Mat.sub.o$TotAge)
# Mat.sub.o$Ethnicity <- as.numeric(Mat.sub.o$Ethnicity)


# #### Split dataset by sex
# Ber.fem <- Ber.clean.o[which(Ber.clean.o$Sex.1m == 2),]
# Ber.mal <- Ber.clean.o[which(Ber.clean.o$Sex.1m == 1),]
# dim(Ber.fem)
# dim(Ber.mal)

# Mat.fem <- Mat.sub.o[which(Mat.sub.o$Sex.1m == 2),]
# Mat.mal <- Mat.sub.o[which(Mat.sub.o$Sex.1m == 1),]
# dim(Mat.fem)
# dim(Mat.mal)


# #### Make consecutive IDs
# num_femB <- length( unique(Ber.fem$PID) )
# Ber.fem$ID <- as.numeric( factor(Ber.fem$PID, levels=unique(Ber.fem$PID)) ) #trick to assign consecutive numbers to IDs

# num_malB <- length( unique(Ber.mal$PID) )
# Ber.mal$ID <- as.numeric( factor(Ber.mal$PID, levels=unique(Ber.mal$PID)) )


# num_femM <- length( unique(Mat.fem$PID) )
# Mat.fem$ID <- as.numeric( factor(Mat.fem$PID, levels=unique(Mat.fem$PID)) )

# num_malM <- length( unique(Mat.mal$PID) )
# Mat.mal$ID <- as.numeric( factor(Mat.mal$PID, levels=unique(Mat.mal$PID)) )



# #### Combine ethnic groups and re-number consecutive IDs

# Com.fem.o <- rbind(Ber.fem,Mat.fem)
# Com.fem.o$ID <- as.numeric( factor(Com.fem.o$PID, levels=unique(Com.fem.o$PID)) )

# Com.mal.o <- rbind(Ber.mal,Mat.mal)
# Com.mal.o$ID <- as.numeric( factor(Com.mal.o$PID, levels=unique(Com.mal.o$PID)) )

# ##############


# eth <- Com.mal.o$Ethnicity - 1  # Berkeley = 0, Matsigenka = 1
# Com.mal.eth.o <- cbind(Com.mal.o, eth)


# sitar.com.mal.9.o <- sitar( x=log(TotAge),                          # log age since conception recommended by Tim Cole
#                            y=Height,
#                            id=ID,
#                            data=Com.mal.eth.o,
#                            df=9,                                  # df sets the degrees of freedom for the mean spline curve, taking values from 1 (i.e. linear) upwards. 
#                            #random="a+b+c",                        # The SITAR model usually has up to three random effects (a, b and c), termed size, timing and intensity respectively. In addition d can be fitted, along with a, b and c, to extend SITAR to model variability in the adult slope of the growth curve.
#                            a.formula = ~eth,                      # fixed effect of ethnicity for each of the three indiv-level random effects
#                            b.formula = ~eth,
#                            c.formula = ~eth,
#                            #xoffset="mean",                        # optional value of offset for x (either "mean" (default), "apv" or value). (also try 2)
#                            #method="ML",                           # From nlme function: If "REML" the model is fit by maximizing the restricted loglikelihood. If "ML" the log-likelihood is maximized.
#                            #control = nlmeControl(maxIter=100,     # maxIter: maximum number of iterations for the nlme optimization algorithm default=50 (also try 100)   
#                            #                      msMaxIter=200,   # maximum number of iterations for nlminb (iter.max) or the nlm (iterlim, from the 10-th step) optimization step inside the nlme optimization. Default is 50 (which may be too small for e.g. for overparametrized cases).
#                            #                      minScale=0.001), # minScale: minimum factor by which to shrink the default step size in an attempt to decrease the sum of squares in the PNLS step. Default=0.001 (also try 0.000001)
#                            verbose=TRUE
#                           )


# #plot(sitar.com.mal.9.o, subset = eth == 0)
# #lines(sitar.com.mal.9.o, subset = eth == 1, col = 2, y2par = list(col = 2))


# plot(sitar.com.mal.9.o, y2par=list(col='blue'), apv=TRUE)

# sitar_model <- sitar.com.mal.9.o
# summary(sitar_model)


# # fitted spline trajectories for each individual
# plot(sitar_model,
#      opt = "DV",          # 'd' for fitted Distance curve, 'v' for fitted Velocity curve, 'c' for fitted Crosssectional distance curve, 'D' for individual fitted Distance curves, 'V' for individual fitted Velocity curves, 'u' for Unadjusted individual growth curves, and 'a' for Adjusted individual growth curves. Options 'dvcDV' give spline curves, while 'ua' give data curves made up as line segments.
#      y2par=list(col='blue'), # parameters for second y axis
#      apv=TRUE                # vertical line at age of max velocity
#      )


# # actual trajectories for each individual
# plot(sitar_model,
#      opt = "ua"
#      )

# # extract mean fitted height trajectory
# m_traj_com <- plot_d(sitar_model)
# glimpse(m_traj_com)
# mean_traj_com <- as.data.frame( cbind( m_traj_com$.x + 0.75, as.vector(m_traj_com$.y) ) )
# names(mean_traj_com) <- c("TotAge", "Height" )
# mean_traj_com[1:10,]


# # extract individual fitted height trajectories
# i_traj_com <- plot_D(sitar_model)
# glimpse(i_traj_com)
# indiv_traj_com <- as.data.frame( cbind( as.vector(as.numeric(i_traj_com$.id)), i_traj_com$.x, as.vector(i_traj_com$.y) ) )
# names(indiv_traj_com) <- c("ID", "TotAge", "Height" )
# indiv_traj_com[1:200,]
# unique(indiv_traj_com$ID)

# length(indiv_traj_com[which(indiv_traj_com$ID == 1),"ID"])

# indiv_traj_com[which(indiv_traj_com$ID == 200),]


# #Com.mal[,c("ID","Ethnicity")]


# # extracted predicted mean trajectories
# mat_mean <- predict(sitar_model,
#                           newdata=data.frame(TotAge=seq(from=min(Com.mal.o$TotAge), to=max(Com.mal.o$TotAge), by=0.5), eth=1),
#                           #abc=c(-954.8183 - 15.1761, -0.8274 - 0.6505, -0.1180 + 0.0431)
#                           #asList = TRUE,
#                           level=0
#                           )
# sitar_mat_mean <- cbind( seq(from=min(Com.mal.o$TotAge), to=max(Com.mal.o$TotAge), by=0.5), as.vector(mat_mean) )
# colnames(sitar_mat_mean) <- c("TotAge", "Height")


# ber_mean <- predict(sitar_model,
#                           newdata=data.frame(TotAge=seq(from=min(Com.mal.o$TotAge), to=max(Com.mal.o$TotAge), by=0.5), eth=0),
#                           #abc=c(-954.8183 - 15.1761, -0.8274 - 0.6505, -0.1180 + 0.0431)
#                           #asList = TRUE,
#                           level=0
#                           )
# sitar_ber_mean <- cbind( seq(from=min(Com.mal.o$TotAge), to=max(Com.mal.o$TotAge), by=0.5), as.vector(ber_mean) )
# colnames(sitar_ber_mean) <- c("TotAge", "Height")


# # extracted predicted mean velocities
# mat_vmean <- predict(sitar_model,
#                           newdata=data.frame(TotAge=seq(from=min(Com.mal.o$TotAge), to=max(Com.mal.o$TotAge), by=0.1), eth=1),
#                           deriv = 1,
#                           xfun = exp, # to undo log(age)
#                           level=0
#                           )
# sitar_mat_vmean <- cbind( seq(from=min(Com.mal.o$TotAge), to=max(Com.mal.o$TotAge), by=0.1), as.vector(mat_vmean) )
# colnames(sitar_mat_vmean) <- c("TotAge", "HeightVelocity")


# ber_vmean <- predict(sitar_model,
#                           newdata=data.frame(TotAge=seq(from=min(Com.mal.o$TotAge), to=max(Com.mal.o$TotAge), by=0.1), eth=0),
#                           deriv = 1,
#                           xfun = exp, # to undo log(age)
#                           level=0
#                           )
# sitar_ber_vmean <- cbind( seq(from=min(Com.mal.o$TotAge), to=max(Com.mal.o$TotAge), by=0.1), as.vector(ber_vmean) )
# colnames(sitar_ber_vmean) <- c("TotAge", "HeightVelocity")



# # point and line colors and sizes

# colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
#                         alpha=0.75)
# names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
#                       "2.5","2.4","2.3","2.2","2.1")
# #pie(rep(1, 11), col = colorlist)

# colorlist2 <- hcl.colors(n=11, palette="Green-Brown",
#                         alpha=0.75)
# names(colorlist2) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
#                       "2.5","2.4","2.3","2.2","2.1")
# #pie(rep(1, 11), col = colorlist2)

# BerPoint_lwd <- 0.5
# BerPoint_col <- colorlist["2.3"]
# BerPoint_cex <- 0.5
# BerPoint_pch <- 1

# MatPoint_lwd <- 0.5
# MatPoint_col <- colorlist["1.3"]
# MatPoint_cex <- 0.5
# MatPoint_pch <- 1


# # BerIndivTraj_lwd <- 0.25
# # BerIndivTraj_col <- colorlist["2.4"]
# # BerIndivTraj_lty <- 1

# # MatIndivTraj_lwd <- 0.25
# # MatIndivTraj_col <- colorlist["1.4"]
# # MatIndivTraj_lty <- 1


# BerMeanTraj_lwd <- 3
# BerMeanTraj_col <- colorlist["2.1"]
# BerMeanTraj_lty <- 1

# MatMeanTraj_lwd <- 3
# MatMeanTraj_col <- colorlist["1.1"]
# MatMeanTraj_lty <- 1


# BerVelTraj_lwd <- 2
# BerVelTraj_col <- colorlist2["2.2"]
# BerVelTraj_lty <- 1

# MatVelTraj_lwd <- 2
# MatVelTraj_col <- colorlist2["1.2"]
# MatVelTraj_lty <- 1



# pdf(file="./Plots/sitar_ber_mat.o.pdf",
#     height=5, width=5)

#   #set up plot
#   par(mar=c(5, 4, 2, 6)) #c(bottom, left, top, right)
#   plot( x=0, y=60, type="n", ylim=c(0,200), xlim=c(0,26), xlab="Age since conception (yrs)", ylab="Height (cm)" )



#   # # plot individual posterior trajectories for berkeley
#   # for ( z in 1:max(Com.mal[which(Com.mal$Ethnicity==1),"ID"]) ){


#   #    points( x = indiv_traj_com[which( indiv_traj_com$ID == z ),"TotAge"],
#   #            y = indiv_traj_com[which( indiv_traj_com$ID == z ),"Height"],
#   #            type = "l", col="red")


#   #   points( x = seq(from=0, to=26, by=0.5), #indiv_traj_com[which(indiv_traj_com$ID == x),"TotAge"],
#   #           y = as.vector( predict(sitar_model,
#   #                                  newdata=data.frame(TotAge=seq(from=0, to=26, by=0.5), ID=x),
#   #                                  level=1
#   #                                  )
#   #                         ), #indiv_traj_com[which(indiv_traj_com$ID == x),"Height"],
#   #           type = "l", col="red" )
    
#   # } #for z

#   # # plot individual posterior trajectories for matsi
#   # for ( z in 1:length(unique(Com.mal[which(Com.mal$Ethnicity==2),"ID"])) ){

#   #   x <- z + min(Com.mal[which(Com.mal$Ethnicity==2),"ID"]) - 1

#   #    points( x = indiv_traj_com[which( indiv_traj_com$ID == x ),"TotAge"],
#   #            y = indiv_traj_com[which( indiv_traj_com$ID == x ),"Height"],
#   #            type = "l", col="blue")

#   #   points( x = seq(from=0, to=26, by=0.5), #indiv_traj_com[which(indiv_traj_com$ID == x),"TotAge"],
#   #           y = as.vector( predict(sitar_model,
#   #                                  newdata=data.frame(TotAge=seq(from=0, to=26, by=0.5), ID=x),
#   #                                  #abc=c(a=-15.1727,b=-0.6496,c=0.0430),
#   #                                  level=1
#   #                                  )
#   #                         ), #indiv_traj_com[which(indiv_traj_com$ID == x),"Height"],
#   #           type = "l", col="blue" )
    
#   # } #for z


#   # plot data points
#   # points(x = Com.mal[which(Com.mal$Ethnicity == 1),"TotAge"],
#   #        y = Com.mal[which(Com.mal$Ethnicity == 1),"Height"],
#   #        lwd=1, col=BerPoint_col, cex=0.5)

#   # points(x = Com.mal[which(Com.mal$Ethnicity == 2),"TotAge"],
#   #        y = Com.mal[which(Com.mal$Ethnicity == 2),"Height"],
#   #        lwd=1, col=MatPoint_col, cex=0.5)


#   # # plot predicted mean trajectory
#   # points( x = sitar_ber_mean[,"TotAge"],
#   #         y = sitar_ber_mean[,"Height"],
#   #         type = "l",
#   #          lwd=3, col=BerMeanTraj_col)

#   # points( x = sitar_mat_mean[,"TotAge"],
#   #         y = sitar_mat_mean[,"Height"],
#   #         type = "l",
#   #          lwd=3, col=MatMeanTraj_col)


#   points(x = Com.mal.o[which(Com.mal.o$Ethnicity == 1),"TotAge"],
#          y = Com.mal.o[which(Com.mal.o$Ethnicity == 1),"Height"],
#          lwd=1, col=BerPoint_col, cex=0.5)

#   points(x = Com.mal.o[which(Com.mal.o$Ethnicity == 2),"TotAge"],
#          y = Com.mal.o[which(Com.mal.o$Ethnicity == 2),"Height"],
#          lwd=1, col=MatPoint_col, cex=0.5)


#   # plot predicted mean trajectory
#   points( x = sitar_ber_mean[,"TotAge"],
#           y = sitar_ber_mean[,"Height"],
#           type = "l",
#            lwd=3, col=BerMeanTraj_col)

#   points( x = sitar_mat_mean[,"TotAge"],
#           y = sitar_mat_mean[,"Height"],
#           type = "l",
#            lwd=3, col=MatMeanTraj_col)


# #plot mean velocity trajectory

#   #set up 
#   par(new=TRUE) #add to existing plot

#   plot( x=0, y=16, type="n", ylim=c(0,16), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
#   axis( side=4, at=seq( 0, 16, by=2 ), las=2 )
#   par(xpd=FALSE) # don't plot beyond the axes
#   #text("Growth velocity (cm/year)", x=33, y=8, srt=270, las=3)


#   points( x = sitar_ber_vmean[,"TotAge"],
#           y = sitar_ber_vmean[,"HeightVelocity"],
#           type = "l",
#            lwd=3, col=BerVelTraj_col)

#   points( x = sitar_mat_vmean[,"TotAge"],
#           y = sitar_mat_vmean[,"HeightVelocity"],
#           type = "l",
#            lwd=3, col=MatVelTraj_col)


#   par(xpd=TRUE)
#   text("Growth velocity (cm/year)", x=33, y=8, srt=270, las=3)


#   legend(-0.7, 16.5,
#          legend=c("U.S. data", "Matsigenka data",
#                  "Estimated U.S. height", "Est. Matsigenka height",
#                  "Estimated U.S. velocity", "Est. Matigenka velocity"
#                  ), bty="o", bg="white",
#          col=c(BerPoint_col, MatPoint_col, BerMeanTraj_col, MatMeanTraj_col, BerVelTraj_col, MatVelTraj_col),
#          merge=FALSE,
#          pch=c(1,1,NA,NA,NA,NA),
#          lty=c(0,0,1,1,1,1),
#          lwd=c(1,1,3,3,3,3), cex=0.6, seg.len=3)

# symbol.Male(centerx = 23, centery = 10, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package



# graphics.off()





# summary(sitar_model)





