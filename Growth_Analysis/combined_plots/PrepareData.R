#Read the Berkeley and Matigenka data from the csv data file into R:
Ber.clean <- read.csv(file="./combined_plots/Data/Berkeley.csv", header=TRUE)
Mat.clean <- read.csv(file="./combined_plots/Data/Matsigenka.csv", header=TRUE)

#Check the variable names and dimensions in the data frame
# Age in years, height in cm, weight in kg
names(Ber.clean)
dim(Ber.clean)

names(Mat.clean)
dim(Mat.clean)


#### Remove all people with any NAs

Ber.clean <- Ber.clean[which(Ber.clean$height_cm != "NA" &
                             Ber.clean$weight_kg != "NA" &
                             Ber.clean$w_underline == "FALSE" &
                             Ber.clean$w_parentheses == "FALSE" &
                             Ber.clean$h_underline == "FALSE" &
                             Ber.clean$h_parentheses == "FALSE"), ]
names(Ber.clean)[4:6] <- c("Age","Height","Weight")
Ber.clean = Ber.clean [, 1:6]
Ber.clean[1:10,]
dim(Ber.clean)


#plot Berkeley indiv height trajectories
pdf(file="./combined_plots/Plots/Berkeley_indiv_height_traj.pdf",
  height=10, width=12)
par(mfrow=c(2,2), oma=c(5,7,5,5), mar=c(3,2,2,2))

for ( Indiv in 1:length(unique(Ber.clean$PID)) ) {
  plot( x=Ber.clean[which( Ber.clean$PID == unique(Ber.clean$PID)[Indiv] ), "Age"],
        y=Ber.clean[which( Ber.clean$PID == unique(Ber.clean$PID)[Indiv] ), "Height"],
        type="b",
      col="red", ylim=c(0,200), xlim=c(-1,25), 
      ylab="",
      xlab="",
      main=c(unique(Ber.clean$PID)[Indiv]),
      cex.main=1.2,
      cex.axis=1.2
      #xaxp=c(-2, 2, 2),
      #yaxp=c(0, 1, 2)
      )
} #for
mtext(text="Height (cm)", side=2, outer=TRUE, line=2, cex=2, las=3, adj=0.5)
mtext(text="Age since birth (years)", side=1, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
graphics.off()

# Remove height outliers (probable measurement error)
Ber.clean[which(Ber.clean$PID == 304),]
Ber.clean[which(Ber.clean$PID == 304 & Ber.clean$Age == 5), "Height"] <- NA
Ber.clean[which(Ber.clean$PID == 304),]


#plot Berkeley indiv weight trajectories
pdf(file="./combined_plots/Plots/Berkeley_indiv_weight_traj.pdf", 
  height=10, width=12)
par(mfrow=c(2,2), oma=c(5,7,5,5), mar=c(3,2,2,2))

for ( Indiv in 1:length(unique(Ber.clean$PID)) ) {
  plot( x=Ber.clean[which( Ber.clean$PID == unique(Ber.clean$PID)[Indiv] ), "Age"],
        y=Ber.clean[which( Ber.clean$PID == unique(Ber.clean$PID)[Indiv] ), "Weight"],
        type="b",
      col="red", ylim=c(0,130), xlim=c(-1,25), 
      ylab="",
      xlab="",
      main=c(unique(Ber.clean$PID)[Indiv]),
      cex.main=1.2,
      cex.axis=1.2
      #xaxp=c(-2, 2, 2),
      #yaxp=c(0, 1, 2)
      )
} #for
mtext(text="Weight (kg)", side=2, outer=TRUE, line=2, cex=2, las=3, adj=0.5)
mtext(text="Age since birth (years)", side=1, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
graphics.off()

Ber.clean <- Ber.clean[complete.cases(Ber.clean), ]


#plot Matsigenka height
pdf(file="./combined_plots/Plots/Matsi_height.pdf",
  height=5, width=5)
plot( x=Mat.clean$Age, y=Mat.clean$height,
      type="p", ylim=c(0,180), xlim=c(0,26),
      xlab="Age since birth (yrs)", ylab="Height (cm)",
      cex=0.5 )
graphics.off()

# Remove two outliers (probable measurement error)
Mat.clean[which(Mat.clean$height == min(Mat.clean$height,na.rm=TRUE)),"height"] <- NA
Mat.clean[which(Mat.clean$height == max(Mat.clean$height[which(Mat.clean$Age < 5)],na.rm=TRUE)),"height"] <- NA

#plot Matsigenka weight
pdf(file="./combined_plots/Plots/Matsi_weight.pdf",
  height=5, width=5)
plot( x=Mat.clean$Age, y=Mat.clean$weight,
      type="p", ylim=c(0,80), xlim=c(0,26),
      xlab="Age since birth (yrs)", ylab="Weight (kg)",
      cex=0.5 )
graphics.off()


#plot Matsigenka indiv height trajectories
pdf(file="./combined_plots/Plots/Matsi_indiv_height_traj.pdf", 
  height=10, width=12)
par(mfrow=c(4,4), oma=c(5,7,5,5), mar=c(3,2,2,2))

for ( Indiv in 1:length(unique(Mat.clean$PID)) ) {
  plot( x=Mat.clean[which( Mat.clean$PID == unique(Mat.clean$PID)[Indiv] ), "Age"],
        y=Mat.clean[which( Mat.clean$PID == unique(Mat.clean$PID)[Indiv] ), "height"],
        type="b",
      col="red", ylim=c(0,200), xlim=c(-1,25), 
      ylab="",
      xlab="",
      main=c(Indiv, unique(Mat.clean$PID)[Indiv]),
      cex.main=1.2,
      cex.axis=1.2
      #xaxp=c(-2, 2, 2),
      #yaxp=c(0, 1, 2)
      )
} #for
mtext(text="Height (cm)", side=2, outer=TRUE, line=2, cex=2, las=3, adj=0.5)
mtext(text="Age since birth (years)", side=1, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
graphics.off()

#Mat.clean[which(Mat.clean$PID=="AAMUT"),]

Mat.clean <- Mat.clean[order(Mat.clean$PID),] #order by PID



Mat.clean <- Mat.clean[complete.cases(Mat.clean), ]
names(Mat.clean)[5:6] <- c("Height","Weight")
dim(Mat.clean)


# when height at time 1 is greater than height at time 2, replace height at time 2 with height at time 1
testID <- NULL

for ( x in 1:( nrow(Mat.clean) - 1 ) ) {
  if ( Mat.clean$PID[x] == Mat.clean$PID[x+1] & 
       Mat.clean$Height[x] > Mat.clean$Height[x+1] ) {

       testID <- c(testID, Mat.clean$PID[x])
  } #if

} #for x

Mat.clean[which(Mat.clean$PID %in% testID),]


for ( x in 1:( nrow(Mat.clean) - 1 ) ) {
  if ( Mat.clean$PID[x] == Mat.clean$PID[x+1] & 
       Mat.clean$Height[x] > Mat.clean$Height[x+1] ) {

       Mat.clean$Height[x+1] <- Mat.clean$Height[x]
  } #if

} #for x

Mat.clean[which(Mat.clean$PID %in% testID),]





# delete two height and weight measures for one indiv in the same year
for ( x in 1:( nrow(Mat.clean) - 1 ) ) {
  if ( #Mat.clean$PID[x+1] != NA & Mat.clean$Age[x+1] != NA &
       Mat.clean$PID[x] == Mat.clean$PID[x+1] & 
       Mat.clean$Age[x] == Mat.clean$Age[x+1] ) {

       Mat.clean$Height[x] <- NA
       Mat.clean$Weight[x] <- NA
  } #if

} #for x




#plot Matsigenka indiv weight trajectories
pdf(file="./combined_plots/Plots/Matsi_indiv_weight_traj.pdf", 
  height=10, width=12)
par(mfrow=c(4,4), oma=c(5,7,5,5), mar=c(3,2,2,2))

for ( Indiv in 1:length(unique(Mat.clean$PID)) ) {
  plot( x=Mat.clean[which( Mat.clean$PID == unique(Mat.clean$PID)[Indiv] ), "Age"],
        y=Mat.clean[which( Mat.clean$PID == unique(Mat.clean$PID)[Indiv] ), "weight"],
        type="b",
      col="red", ylim=c(0,200), xlim=c(-1,25), 
      ylab="",
      xlab="",
      main=c(unique(Mat.clean$PID)[Indiv]),
      cex.main=1.2,
      cex.axis=1.2
      #xaxp=c(-2, 2, 2),
      #yaxp=c(0, 1, 2)
      )
} #for
mtext(text="Weight (kg)", side=2, outer=TRUE, line=2, cex=2, las=3, adj=0.5)
mtext(text="Age since birth (years)", side=1, outer=TRUE, line=1, cex=2, las=1, adj=0.5)
graphics.off()


Mat.clean <- Mat.clean[complete.cases(Mat.clean), ]
dim(Mat.clean)



#### Make height, weight, age, and ethnicity numeric
Mat.clean$Height <- as.numeric(Mat.clean$Height)
Mat.clean$Weight <- as.numeric(Mat.clean$Weight)
Mat.clean$Ethnicity <- as.numeric(Mat.clean$Ethnicity)
Mat.clean$Age <- as.numeric(Mat.clean$Age)


#### Add column for weight of cells contributing to longitudinal growth
Ber.clean$Weightg <- 1000*Ber.clean$Weight
Ber.clean$CellWeightg <- 0.01*(5 + 95*exp(-4*(Ber.clean$Age + 0.75)))*Ber.clean$Weightg


Mat.clean$Weightg <- 1000*Mat.clean$Weight
Mat.clean$CellWeightg <- 0.01*(5 + 95*exp(-4*(Mat.clean$Age + 0.75)))*Mat.clean$Weightg




#### Keep only people younger than 25 yrs

Mat.sub <- Mat.clean[which(Mat.clean$Age < 25),]
dim(Mat.sub)


##### Extend age range to 25 by duplicating last height and weight entries

Ber.exp <- Ber.clean
for ( x in 1:8 ) {
  for ( y in 1:length(unique(Ber.exp$PID)) ) {
    Ber.exp[nrow(Ber.exp)+1,] <- c( unique(Ber.exp$PID)[y],
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Ethnicity"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Sex.1m"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Age"],1)) + 1,
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Height"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Weight"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Weightg"],1)),
                                    as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"CellWeightg"],1))
                                   )
  } # for y
} # for x

Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[6]),]



##### Set height and weight to ~0 at ~conception (0.75 years before birth)
for ( y in 1:length(unique(Ber.exp$PID)) ) {
  Ber.exp[nrow(Ber.exp)+1,] <- c( unique(Ber.exp$PID)[y],
                                  as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Ethnicity"],1)),
                                  as.numeric(tail(Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[y]),"Sex.1m"],1)),
                                  -0.75,        # TotAge
                                  0.012,        # height
                                  1.02e-9,      # weight in Kg
                                  1.02e-6,      # weight in g
                                  1.02e-6       # cell weight in g
                                 )
} # for y

Ber.exp[which(Ber.exp$PID == unique(Ber.exp$PID)[6]),]



Mat.exp <- Mat.sub
for ( y in 1:length(unique(Mat.exp$PID)) ) {
  Mat.exp[nrow(Mat.exp)+1,] <- c( unique(Mat.exp$PID)[y],
                                  as.numeric(tail(Mat.exp[which(Mat.exp$PID == unique(Mat.exp$PID)[y]),"Ethnicity"],1)),
                                  as.numeric(tail(Mat.exp[which(Mat.exp$PID == unique(Mat.exp$PID)[y]),"Sex.1m"],1)),
                                  -0.75,        # TotAge
                                  0.012,        # height
                                  1.02e-9,      # weight in Kg
                                  1.02e-6,      # weight in g
                                  1.02e-6       # cell weight in g
                                 )
} # for y

Mat.exp$Ethnicity <- as.numeric(Mat.exp$Ethnicity)
Mat.exp$Sex.1m <- as.numeric(Mat.exp$Sex.1m)
Mat.exp$Age <- as.numeric(Mat.exp$Age)
Mat.exp$Height <- as.numeric(Mat.exp$Height)
Mat.exp$Weight <- as.numeric(Mat.exp$Weight)
Mat.exp$Weightg <- as.numeric(Mat.exp$Weightg)
Mat.exp$CellWeightg <- as.numeric(Mat.exp$CellWeightg)

Mat.exp[which(Mat.exp$PID == unique(Mat.exp$PID)[6]),]


#### Add gestational time to age
Ber.exp$TotAge <- as.numeric(Ber.exp$Age) + 0.75
Mat.exp$TotAge <- as.numeric(Mat.exp$Age) + 0.75




#### Split dataset by sex
Ber.fem <- Ber.exp[which(Ber.exp$Sex.1m == 2),]
Ber.mal <- Ber.exp[which(Ber.exp$Sex.1m == 1),]
dim(Ber.fem)
dim(Ber.mal)

Mat.fem <- Mat.exp[which(Mat.exp$Sex.1m == 2),]
Mat.mal <- Mat.exp[which(Mat.exp$Sex.1m == 1),]
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

Com.fem <- rbind(Ber.fem,Mat.fem)
Com.fem$ID <- as.numeric( factor(Com.fem$PID, levels=unique(Com.fem$PID)) )

Com.mal <- rbind(Ber.mal,Mat.mal)
Com.mal$ID <- as.numeric( factor(Com.mal$PID, levels=unique(Com.mal$PID)) )




#plot female cell weight
pdf(file="./combined_plots/Plots/fem_cell_weight.pdf",
  height=5, width=5)
plot( x=Ber.fem$Age, y=Ber.fem$CellWeightg,
      type="p", ylim=c(0,5000), xlim=c(0,26),
      xlab="Age since birth (yrs)", ylab="Weight (g)",
      cex=0.5, col="red" )
points(x=Mat.fem$Age, y=Mat.fem$CellWeightg, lwd=1, col="blue", cex=0.5)
graphics.off()

#plot male cell weight
pdf(file="./combined_plots/Plots/mal_cell_weight.pdf",
  height=5, width=5)
plot( x=Ber.mal$Age, y=Ber.mal$CellWeightg,
      type="p", ylim=c(0,5000), xlim=c(0,26),
      xlab="Age since birth (yrs)", ylab="Weight (g)",
      cex=0.5, col="red" )
points(x=Mat.mal$Age, y=Mat.mal$CellWeightg, lwd=1, col="blue", cex=0.5)
graphics.off()





##################################################################################################



#### plot measured points, male height

pdf(file="./combined_plots/Plots/Mal_height.pdf",
  height=5, width=5)
plot( x=Ber.mal$TotAge, y=Ber.mal$Height,
      type="p", ylim=c(0,200), xlim=c(0,25),
      xlab="Age since conception (yrs)", ylab="Height (cm)",
      cex=0.5 )
#points(x=Ber.mal$TotAge, y=Ber.mal$Height, col=gray(0.75), cex=0.5)
points(x=Mat.mal$TotAge, y=Mat.mal$Height, col="red", cex=0.5)
symbol.Male(centerx = 20, centery = 80, rayonx=0.8, lwd=2, col="black") # Male symbol from HelpersMG package
graphics.off()


# female height
pdf(file="./combined_plots/Plots/Fem_height.pdf",
  height=5, width=5)
plot( x=Ber.fem$TotAge, y=Ber.fem$Height,
      type="p", ylim=c(0,180), xlim=c(0,25),
      xlab="Age since conception (yrs)", ylab="Height (cm)",
      cex=0.5 )
#points(x=Ber.fem$TotAge, y=Ber.fem$Height, col=gray(0.75), cex=0.5)
points(x=Mat.fem$TotAge, y=Mat.fem$Height, col="red", cex=0.5)
symbol.Female(centerx = 20, centery = 80, rayonx=0.8, lwd=2, col="black")
graphics.off()


# male weight
pdf(file="./combined_plots/Plots/Mal_weight.pdf",
  height=5, width=5)
plot( x=Ber.mal$TotAge, y=Ber.mal$Weight,
      type="p", ylim=c(0,100), xlim=c(0,25),
      xlab="Age since conception (yrs)", ylab="Weight (kg)",
      cex=0.5 )
#points(x=Ber.mal$TotAge, y=Ber.mal$Weight, col=gray(0.75), cex=0.5)
points(x=Mat.mal$TotAge, y=Mat.mal$Weight, col="red", cex=0.5)
symbol.Male(centerx = 5, centery = 60, rayonx=0.8, lwd=2, col="black")
graphics.off()

# female weight
pdf(file="./combined_plots/Plots/Fem_weight.pdf",
  height=5, width=5)
plot( x=Ber.fem$TotAge, y=Ber.fem$Weight,
      type="p", ylim=c(0,100), xlim=c(0,25),
      xlab="Age since conception (yrs)", ylab="Weight (kg)",
      cex=0.5 )
#points(x=Ber.fem$TotAge, y=Ber.fem$Weight, col=gray(0.75), cex=0.5)
points(x=Mat.fem$TotAge, y=Mat.fem$Weight, col="red", cex=0.5)
symbol.Female(centerx = 5, centery = 60, rayonx=0.8, lwd=2, col="black")
graphics.off()





colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.75)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)

BerPoint_lwd <- 1
BerPoint_col <- colorlist["2.3"]
BerPoint_cex <- 0.5
BerPoint_pch <- 1

MatPoint_lwd <- 1
MatPoint_col <- "blue"  #colorlist["1.3"] 
MatPoint_cex <- 0.5
MatPoint_pch <- 1


pdf(file="./combined_plots/Plots/WeightForHeight.pdf",
  height=5, width=8)
layout( mat=matrix(c(1,2),1,2,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 3, 1)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

  ###### females

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=40, y=60, type="n", ylim=c(0,115), xlim=c(40,200), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 50, 200, by=50), labels=TRUE )
  axis( side=2, at=seq( 0, 100, by=25), labels=TRUE )
  axis( side=4, at=seq( 0, 100, by=25), labels=FALSE )


  points(x=Ber.fem$Height, y=Ber.fem$Weight, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.fem$Height, y=Mat.fem$Weight, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)
  symbol.Female(centerx = 140, centery = 100, rayonx=4, lwd=2, col="black") # Female symbol from HelpersMG package


  legend(45, 110,
         legend=c("U.S.   ",
                 "Matsigenka   "
                 ), bty="o", bg="white",
         col=c(BerPoint_col, MatPoint_col),
         merge=FALSE,
         pch=c(BerPoint_pch, MatPoint_pch),
         lwd=c(BerPoint_lwd, MatPoint_lwd),
         lty=c(0,0),
         cex=0.8)


  ###### males

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=40, y=60, type="n", ylim=c(0,115), xlim=c(40,200), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 50, 200, by=50), labels=TRUE )
  axis( side=2, at=seq( 0, 100, by=25), labels=FALSE )


  points(x=Ber.mal$Height, y=Ber.mal$Weight, lwd=BerPoint_lwd, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch)
  points(x=Mat.mal$Height, y=Mat.mal$Weight, lwd=MatPoint_lwd, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch)
  symbol.Male(centerx = 60, centery = 100, rayonx=4, lwd=2, col="black") # male symbol from HelpersMG package

  par(xpd=NA) # clip plotting to device region
  mtext("Height (cm)", side = 1, outer = T, cex = 1.3, line = 2, adj=0.5)
  mtext("Weight (kg)", side = 2, outer = T, cex = 1.3, line = 2, adj=0.5)


graphics.off()





################ plot repeat measures

Ber.clean.mal <- Ber.clean[which(Ber.clean$Sex.1m == 1),]
Mat.sub.mal <- Mat.sub[which(Mat.sub$Sex.1m == 1),]

Ber.clean.fem <- Ber.clean[which(Ber.clean$Sex.1m == 2),]
Mat.sub.fem <- Mat.sub[which(Mat.sub$Sex.1m == 2),]



Ber.mal.PID <- unique(Ber.clean.mal$PID)
Mat.mal.PID <- unique(Mat.sub.mal$PID)

Ber.fem.PID <- unique(Ber.clean.fem$PID)
Mat.fem.PID <- unique(Mat.sub.fem$PID)



Ber.mal.sort <- Ber.clean.mal[order(Ber.clean.mal$PID, Ber.clean.mal$Age),] # order by ID then by age
Mat.mal.sort <- Mat.sub.mal[order(Mat.sub.mal$PID, Mat.sub.mal$Age),]

Ber.fem.sort <- Ber.clean.fem[order(Ber.clean.fem$PID, Ber.clean.fem$Age),]
Mat.fem.sort <- Mat.sub.fem[order(Mat.sub.fem$PID, Mat.sub.fem$Age),]


### repeat measure characteristics
num_measures_malB <- as.data.frame(table(Ber.mal.sort$PID))
avg_num_measures_malB <- mean(num_measures_malB$Freq)
max_num_measures_malB <- max(num_measures_malB$Freq)
num_single_measures_malB <- nrow(num_measures_malB[which(num_measures_malB$Freq==1),])
length(unique(Ber.mal.sort$PID))
min(Ber.mal.sort$Age)
max(Ber.mal.sort$Age)

num_measures_malM <- as.data.frame(table(Mat.mal.sort$PID))
avg_num_measures_malM <- mean(num_measures_malM$Freq)
max_num_measures_malM <- max(num_measures_malM$Freq)
num_single_measures_malM <- nrow(num_measures_malM[which(num_measures_malM$Freq==1),])
length(unique(Mat.mal.sort$PID))
min(Mat.mal.sort$Age)
max(Mat.mal.sort$Age)


num_measures_femB <- as.data.frame(table(Ber.fem.sort$PID))
avg_num_measures_femB <- mean(num_measures_femB$Freq)
max_num_measures_femB <- max(num_measures_femB$Freq)
num_single_measures_femB <- nrow(num_measures_femB[which(num_measures_femB$Freq==1),])
length(unique(Ber.fem.sort$PID))
min(Ber.fem.sort$Age)
max(Ber.fem.sort$Age)

num_measures_femM <- as.data.frame(table(Mat.fem.sort$PID))
avg_num_measures_femM <- mean(num_measures_femM$Freq)
max_num_measures_femM <- max(num_measures_femM$Freq)
num_single_measures_femM <- nrow(num_measures_femM[which(num_measures_femM$Freq==1),])
length(unique(Mat.fem.sort$PID))
min(Mat.fem.sort$Age)
max(Mat.fem.sort$Age)



###### point and line colors
colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.75)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)

BerPoint_lwd <- 0.5
BerPoint_col <- colorlist["2.3"]
BerPoint_cex <- 0.5
BerPoint_pch <- 1

MatPoint_lwd <- 0.75
MatPoint_col <- colorlist["1.1"]
MatPoint_cex <- 0.5
MatPoint_pch <- 1


BerIndivTraj_lwd <- 0.25
BerIndivTraj_col <- colorlist["2.4"]
BerIndivTraj_lty <- 1

MatIndivTraj_lwd <- 0.75
MatIndivTraj_col <- colorlist["1.2"]
MatIndivTraj_lty <- 1





pdf(file="./combined_plots/Plots/Repeat_measure_illustration.pdf",
  height=10, width=10)
layout( mat=matrix(c(1,2,3,4),2,2,byrow = TRUE), heights=c(1,1), widths=c(1,1) )
par(mar = c(0, 0, 0, 0), oma = c(5, 5, 5, 5)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)


  ################ female height

  #set up plot
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(35,200), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5), labels=FALSE )
  axis( side=2, at=seq( 0, 200, by=50), labels=TRUE )


  for ( i in 1:length(Ber.fem.PID) ) {

    lines(x=Ber.fem.sort[which(Ber.fem.sort$PID == Ber.fem.PID[i]),"Age"],
          y=Ber.fem.sort[which(Ber.fem.sort$PID == Ber.fem.PID[i]),"Height"],
          col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, type="l")

  } # for

  points(x=Ber.fem.sort$Age, y=Ber.fem.sort$Height, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch, lwd=BerPoint_lwd)

  for ( j in 1:length(Mat.fem.PID) ) {

    lines(x=Mat.fem.sort[which(Mat.fem.sort$PID == Mat.fem.PID[j]),"Age"],
          y=Mat.fem.sort[which(Mat.fem.sort$PID == Mat.fem.PID[j]),"Height"],
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, type="l")

  } # for

  points(x=Mat.fem.sort$Age, y=Mat.fem.sort$Height, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch, lwd=MatPoint_lwd)



  par(xpd=NA) # clip plotting to device region
  
  # set horizontal spacing for legend text
  legtext <- c("U.S. data",
               "Repeated measures of U.S. individuals" 
               )
  xcoords <- c(0,
               13 #11
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)



  legend(x=-4, y=245,
         ncol=4,
         cex=1.5,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(BerPoint_col,
               BerIndivTraj_col
              ),
         merge=FALSE,
         pch=c(BerPoint_pch,
               NA
               ),
         lty=c(0,
               BerIndivTraj_lty
               ),
         lwd=c(BerPoint_lwd,
               BerIndivTraj_lwd
               ),
         seg.len=4 )


  # set horizontal spacing for legend text
  legtext <- c("Matsigenka data",
               "Repeated measures of Matsigenka indivs"
               )
  xcoords <- c(0,
               13
               )
  secondvector <- (1:length(legtext))-1
  textwidths <- xcoords/secondvector # this works for all but the first element
  textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)


  legend(x=-4, y=235,
         ncol=4,
         cex=1.5,
         text.width=textwidths,
         legend=legtext,
         bty="n",
         #bg="white",
         col=c(MatPoint_col,
               MatIndivTraj_col
              ),
         merge=FALSE,
         pch=c(MatPoint_pch,
               NA
               ),
         lty=c(0,
               MatIndivTraj_lty
               ),
         lwd=c(MatPoint_lwd,
               MatIndivTraj_lwd
               ),
         seg.len=4 )

  rect(xleft = 1.5,
       ybottom = 216,
       xright = 54,
       ytop = 242,
       lwd=1)

  symbol.Female(centerx = 3, centery = 190, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package



  ################ Male height

  #set up plot
  par(xpd=FALSE) # clip to plot region
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(35,200), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=FALSE )
  axis( side=2, at=seq( 0, 200, by=50), labels=FALSE )


  for ( i in 1:length(Ber.mal.PID) ) {

    lines(x=Ber.mal.sort[which(Ber.mal.sort$PID == Ber.mal.PID[i]),"Age"],
          y=Ber.mal.sort[which(Ber.mal.sort$PID == Ber.mal.PID[i]),"Height"],
          col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, type="l")

  } # for

  points(x=Ber.mal.sort$Age, y=Ber.mal.sort$Height, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch, lwd=BerPoint_lwd)

  for ( j in 1:length(Mat.mal.PID) ) {

    lines(x=Mat.mal.sort[which(Mat.mal.sort$PID == Mat.mal.PID[j]),"Age"],
          y=Mat.mal.sort[which(Mat.mal.sort$PID == Mat.mal.PID[j]),"Height"],
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, type="l")

  } # for

  points(x=Mat.mal.sort$Age, y=Mat.mal.sort$Height, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch, lwd=MatPoint_lwd)


  symbol.Male(centerx = 3, centery = 190, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package




  ################ female weight

  #set up plot
  par(xpd=FALSE) # clip to plot region
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,115), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 115, by=25), labels=TRUE )


  for ( i in 1:length(Ber.fem.PID) ) {

    lines(x=Ber.fem.sort[which(Ber.fem.sort$PID == Ber.fem.PID[i]),"Age"],
          y=Ber.fem.sort[which(Ber.fem.sort$PID == Ber.fem.PID[i]),"Weight"],
          col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, type="l")

  } # for

  points(x=Ber.fem.sort$Age, y=Ber.fem.sort$Weight, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch, lwd=BerPoint_lwd)

  for ( j in 1:length(Mat.fem.PID) ) {

    lines(x=Mat.fem.sort[which(Mat.fem.sort$PID == Mat.fem.PID[j]),"Age"],
          y=Mat.fem.sort[which(Mat.fem.sort$PID == Mat.fem.PID[j]),"Weight"],
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, type="l")

  } # for

  points(x=Mat.fem.sort$Age, y=Mat.fem.sort$Weight, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch, lwd=MatPoint_lwd)


  symbol.Female(centerx = 3, centery = 105, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package




  ################ male weight

  #set up plot
  par(xpd=FALSE) # clip to plot region
  par(mar=c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0,115), xlim=c(0,26), axes=FALSE, ylab=NA, xlab=NA)
  box(which = "plot", lty = "solid")
  axis( side=1, at=seq( 0, 25, by=5 ), labels=TRUE )
  axis( side=2, at=seq( 0, 115, by=25), labels=FALSE )


  for ( i in 1:length(Ber.mal.PID) ) {

    lines(x=Ber.mal.sort[which(Ber.mal.sort$PID == Ber.mal.PID[i]),"Age"],
          y=Ber.mal.sort[which(Ber.mal.sort$PID == Ber.mal.PID[i]),"Weight"],
          col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, type="l")

  } # for

  points(x=Ber.mal.sort$Age, y=Ber.mal.sort$Weight, col=BerPoint_col, cex=BerPoint_cex, pch=BerPoint_pch, lwd=BerPoint_lwd)

  for ( j in 1:length(Mat.mal.PID) ) {

    lines(x=Mat.mal.sort[which(Mat.mal.sort$PID == Mat.mal.PID[j]),"Age"],
          y=Mat.mal.sort[which(Mat.mal.sort$PID == Mat.mal.PID[j]),"Weight"],
          col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, type="l")

  } # for

  points(x=Mat.mal.sort$Age, y=Mat.mal.sort$Weight, col=MatPoint_col, cex=MatPoint_cex, pch=MatPoint_pch, lwd=MatPoint_lwd)


  symbol.Male(centerx = 3, centery = 105, rayonx=0.8, lwd=2, col="black") # gender symbol from HelpersMG package

  par(xpd=NA) # clip plotting to device region
  mtext("Age since birth (years)", side = 1, outer = T, cex = 1.5, line = 2, adj=0.5)
  mtext("Height (cm)", side = 2, outer = T, cex = 1.5, line = 2, adj=0.8)
  mtext("Weight (kg)", side = 2, outer = T, cex = 1.5, line = 2, adj=0.2)


  graphics.off()






# For SITAR: dataset without height set to 0 at conception, duplicating Berkeley data until age 25

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
                                    as.numeric(tail(Ber.exp.e[which(Ber.exp.e$PID == unique(Ber.exp.e$PID)[y]),"Weight"],1)),
                                    as.numeric(tail(Ber.exp.e[which(Ber.exp.e$PID == unique(Ber.exp.e$PID)[y]),"Weightg"],1)),
                                    as.numeric(tail(Ber.exp.e[which(Ber.exp.e$PID == unique(Ber.exp.e$PID)[y]),"CellWeightg"],1))
                                    )
  } # for y
} # for x


#### Add gestational time to age
Mat.sub.e$TotAge <- as.numeric(Mat.sub.e$Age) + 0.75
Ber.exp.e$TotAge <- as.numeric(Ber.exp.e$Age) + 0.75

#### Make height, weight, and ethnicity numeric
Mat.sub.e$Height <- as.numeric(Mat.sub.e$Height)
Mat.sub.e$Weight <- as.numeric(Mat.sub.e$Weight)
Mat.sub.e$Weightg <- as.numeric(Mat.sub.e$Weightg)
Mat.sub.e$CellWeightg <- as.numeric(Mat.sub.e$CellWeightg)
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


eth <- Com.mal.e$Ethnicity - 1  # Berkeley = 0, Matsigenka = 1
Com.mal.eth.e <- cbind(Com.mal.e, eth)



