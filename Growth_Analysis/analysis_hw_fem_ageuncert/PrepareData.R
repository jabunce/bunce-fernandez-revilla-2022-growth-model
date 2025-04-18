#Read the Berkeley and Matigenka data from the csv data file into R:
Ber.clean <- read.csv(file="./Data/Berkeley.csv", header=TRUE)
Mat.clean <- read.csv(file="./Data/Matsigenka.csv", header=TRUE)

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
pdf(file="./Plots/Berkeley_indiv_height_traj.pdf",
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
pdf(file="./Plots/Berkeley_indiv_weight_traj.pdf", 
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
pdf(file="./Plots/Matsi_height.pdf",
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
pdf(file="./Plots/Matsi_weight.pdf",
  height=5, width=5)
plot( x=Mat.clean$Age, y=Mat.clean$weight,
      type="p", ylim=c(0,80), xlim=c(0,26),
      xlab="Age since birth (yrs)", ylab="Weight (kg)",
      cex=0.5 )
graphics.off()


#plot Matsigenka indiv height trajectories
pdf(file="./Plots/Matsi_indiv_height_traj.pdf", 
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
pdf(file="./Plots/Matsi_indiv_weight_traj.pdf", 
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
pdf(file="./Plots/fem_cell_weight.pdf",
  height=5, width=5)
plot( x=Ber.fem$Age, y=Ber.fem$CellWeightg,
      type="p", ylim=c(0,5000), xlim=c(0,26),
      xlab="Age since birth (yrs)", ylab="Weight (g)",
      cex=0.5, col="red" )
points(x=Mat.fem$Age, y=Mat.fem$CellWeightg, lwd=1, col="blue", cex=0.5)
graphics.off()

#plot male cell weight
pdf(file="./Plots/mal_cell_weight.pdf",
  height=5, width=5)
plot( x=Ber.mal$Age, y=Ber.mal$CellWeightg,
      type="p", ylim=c(0,5000), xlim=c(0,26),
      xlab="Age since birth (yrs)", ylab="Weight (g)",
      cex=0.5, col="red" )
points(x=Mat.mal$Age, y=Mat.mal$CellWeightg, lwd=1, col="blue", cex=0.5)
graphics.off()


# add random variance to female Matsigenka age

# sample from normal with Total Age as mean and SD as 0.25 year
Com.fem$RandAge <- mvrnorm( n=1, mu=Com.fem$TotAge, Sigma=diag(rep(0.25,length(Com.fem$TotAge))) ) # stdev of 0.5 around reported age, zeros on off-diagonal

Com.fem[which(Com.fem$TotAge == 0),"RandAge"] <- 0
Com.fem[which(Com.fem$RandAge < 0),"RandAge"] <- 0

Com.fem[which(Com.fem$Ethnicity == 1),"RandAge"] <- Com.fem[which(Com.fem$Ethnicity == 1),"TotAge"]

# # US 
# plot( x=Com.fem$RandAge[which(Com.fem$Ethnicity==1)], y=Com.fem$Height[which(Com.fem$Ethnicity==1)],
#       type="p", ylim=c(0,200), xlim=c(0,26),
#       xlab="Age since conception (years)", ylab="Height (cm)",
#       cex=0.5, col="black", pch=16 )
# points(x=Com.fem$TotAge[which(Com.fem$Ethnicity==1)], y=Com.fem$Height[which(Com.fem$Ethnicity==1)], lwd=1, col="blue", cex=0.5)

# # Matsigenka
# plot( x=Com.fem$RandAge[which(Com.fem$Ethnicity==2)], y=Com.fem$Height[which(Com.fem$Ethnicity==2)],
#       type="p", ylim=c(0,200), xlim=c(0,26),
#       xlab="Age since conception (years)", ylab="Height (cm)",
#       cex=0.5, col="black", pch=16 )
# points(x=Com.fem$TotAge[which(Com.fem$Ethnicity==2)], y=Com.fem$Height[which(Com.fem$Ethnicity==2)], lwd=1, col="blue", cex=0.5)

