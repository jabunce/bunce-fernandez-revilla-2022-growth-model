

######################################### Fig 3 #########################################


######## load posteriors from complete models, need to run PrepareData to plot raw data points

# male
post3_m <- readRDS("./analysis_hw_mal/post6.RDS")
#str(post3_m)

# female
post3_f <- readRDS("./analysis_hw_fem/post6.RDS")
#str(post3_f)



# function to calculate proportions of phases for weighting by age
propparam <- function(x=1, Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                           K1=1, K2=1, K3=1, K4=1, K5=1,
                           H1=1, H2=1, H3=1, H4=1, H5=1,
                           I1=1, I2=1, I3=1, I4=1){
      
      I0 <- rep(0, times=length(I1))

      # total height grown since egg size at age x
      totHeight <- ifelse( x <= I1, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1),

                   ifelse( x <= I2, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2),

                   ifelse( x <= I3, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2) +
                                            ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3),

                   ifelse( x <= I4, 0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2) +
                                            ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3) +
                                            ( 2*H4/K4 * ( 1 - exp(K4*Q4*( I3 - x )/( 1 + 2*Q4 )) ) )^(1/Q4),

                                    0.012 + ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1) +
                                            ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2) +
                                            ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3) +
                                            ( 2*H4/K4 * ( 1 - exp(K4*Q4*( I3 - x )/( 1 + 2*Q4 )) ) )^(1/Q4) +
                                            ( 2*H5/K5 * ( 1 - exp(K5*Q5*( I4 - x )/( 1 + 2*Q5 )) ) )^(1/Q5)

                   ) ) ) ) - 0.012

      # heights of each process at time x
      Height1 <- ifelse( x >= I0 , ( 2*H1/K1 * ( 1 - exp(K1*Q1*( I0 - x )/( 1 + 2*Q1 )) ) )^(1/Q1), 0) 

      Height2 <- ifelse( x >= I1 , ( 2*H2/K2 * ( 1 - exp(K2*Q2*( I1 - x )/( 1 + 2*Q2 )) ) )^(1/Q2), 0) 

      Height3 <- ifelse( x >= I2 , ( 2*H3/K3 * ( 1 - exp(K3*Q3*( I2 - x )/( 1 + 2*Q3 )) ) )^(1/Q3), 0) 

      Height4 <- ifelse( x >= I3 , ( 2*H4/K4 * ( 1 - exp(K4*Q4*( I3 - x )/( 1 + 2*Q4 )) ) )^(1/Q4), 0) 

      Height5 <- ifelse( x >= I4 , ( 2*H5/K5 * ( 1 - exp(K5*Q5*( I4 - x )/( 1 + 2*Q5 )) ) )^(1/Q5), 0) 

      # vector of proportions of total height contributed by each process at time x
      p <- rbind(Height1/totHeight,
                 Height2/totHeight,
                 Height3/totHeight,
                 Height4/totHeight,
                 Height5/totHeight)


      # max asymptotic height of each process
      maxHeight1 <- (2*H1/K1)^(1/Q1)
      maxHeight2 <- (2*H2/K2)^(1/Q2)
      maxHeight3 <- (2*H3/K3)^(1/Q3)
      maxHeight4 <- (2*H4/K4)^(1/Q4)
      maxHeight5 <- (2*H5/K5)^(1/Q5)

      # vector of proportions of original metabolic activity for each process at time x.
      # On average, cells lose 75% of their metabolic activity by the time an individual growth process finishes at asymptote, or 75% of cells convert to things that don't divide.
      # Mostly because active bone marrow converted to marrow fat, and bone cells converted to bone matrix.
      m <- rbind( 1 - 0.75*(Height1/maxHeight1),
                  1 - 0.75*(Height2/maxHeight2),
                  1 - 0.75*(Height3/maxHeight3),
                  1 - 0.75*(Height4/maxHeight4),
                  1 - 0.75*(Height5/maxHeight5) )


      return( list(p=p,m=m) )

} # propparam


vpropparam <- Vectorize(propparam) # vectorize the function so that it can take vectors as arguments


# e <- 5

# Q1 <- Q1ber[1:e]
# Q2 <- Q2ber[1:e]
# Q3 <- Q3ber[1:e]
# Q4 <- Q4ber[1:e]
# Q5 <- Q5ber[1:e]

# K1 <- K1ber[1:e]
# K2 <- K2ber[1:e]
# K3 <- K3ber[1:e]
# K4 <- K4ber[1:e]
# K5 <- K5ber[1:e]

# H1 <- H1ber[1:e]
# H2 <- H2ber[1:e]
# H3 <- H3ber[1:e]
# H4 <- H4ber[1:e]
# H5 <- H5ber[1:e]

# I1 <- I1ber[1:e]
# I2 <- I2ber[1:e]
# I3 <- I3ber[1:e]
# I4 <- I4ber[1:e]

# d <- 5

# p <- matrix(unlist(vpropparam(x=d, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                                    K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                                    H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                                    I1=I1, I2=I2, I3=I3, I4=I4)["p",]), nrow=5, byrow=FALSE)
# p
# colSums(p)


# m <- matrix(unlist(vpropparam(x=d, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                                    K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                                    H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                                    I1=I1, I2=I2, I3=I3, I4=I4)["m",]), nrow=5, byrow=FALSE)
# m
# colSums(m)


a <- seq(from=(0.1), to=(26.75), by=0.1)  #c(0.1,1,2)




##### female

post <- post3_f

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

Q1ber <- post$"mQ[1,1]"
Q2ber <- post$"mQ[1,2]" 
Q3ber <- post$"mQ[1,3]" 
Q4ber <- post$"mQ[1,4]" 
Q5ber <- post$"mQ[1,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

K1ber <- post$"mK[1,1]"
K2ber <- post$"mK[1,2]" 
K3ber <- post$"mK[1,3]" 
K4ber <- post$"mK[1,4]" 
K5ber <- post$"mK[1,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]"
H3mat <- post$"mH[2,3]"
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

H1ber <- post$"mH[1,1]"
H2ber <- post$"mH[1,2]"
H3ber <- post$"mH[1,3]"
H4ber <- post$"mH[1,4]" 
H5ber <- post$"mH[1,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

I1ber <- post$"mI[1,1]"
I2ber <- post$"mI[1,2]" 
I3ber <- post$"mI[1,3]" 
I4ber <- post$"mI[1,4]" 


tQmat_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKmat_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHmat_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

tQber_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKber_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHber_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

conQ_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conK_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conH_f <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))


# don't multiply Q's by m. Metabolism doesn't affect allometry. H and K remain fairly constant after adolescence and adulthood, as reporterd for human metabolism

for ( t in 1:length(a) ){
      Pmat <- matrix(unlist(vpropparam(x=a[t],Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                              K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                              H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                              I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["p",]), nrow=5, byrow=FALSE)
      Pber <- matrix(unlist(vpropparam(x=a[t],Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                              K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                              H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                              I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["p",]), nrow=5, byrow=FALSE)

      Mmat <- matrix(unlist(vpropparam(x=a[t],Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                              K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                              H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                              I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["m",]), nrow=5, byrow=FALSE)
      Mber <- matrix(unlist(vpropparam(x=a[t],Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                              K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                              H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                              I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["m",]), nrow=5, byrow=FALSE)

      tQmat_f[t,] <- Pmat[1,]*Q1mat +
                     Pmat[2,]*Q2mat +
                     Pmat[3,]*Q3mat +
                     Pmat[4,]*Q4mat +
                     Pmat[5,]*Q5mat

      tKmat_f[t,] <- Pmat[1,]*Mmat[1,]*K1mat +
                     Pmat[2,]*Mmat[2,]*K2mat +
                     Pmat[3,]*Mmat[3,]*K3mat +
                     Pmat[4,]*Mmat[4,]*K4mat +
                     Pmat[5,]*Mmat[5,]*K5mat

      tHmat_f[t,] <- (Pmat[1,]*Mmat[1,]*H1mat*(2/32) + # original H in units of g/cm^2 of 5% of total body skin surface. Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
                      Pmat[2,]*Mmat[2,]*H2mat*(2/32) +
                      Pmat[3,]*Mmat[3,]*H3mat*(2/32) +
                      Pmat[4,]*Mmat[4,]*H4mat*(2/32) +
                      Pmat[5,]*Mmat[5,]*H5mat*(2/32) )



      tQber_f[t,] <- Pber[1,]*Q1ber +
                     Pber[2,]*Q2ber +
                     Pber[3,]*Q3ber +
                     Pber[4,]*Q4ber +
                     Pber[5,]*Q5ber

      tKber_f[t,] <- Pber[1,]*Mber[1,]*K1ber +
                     Pber[2,]*Mber[2,]*K2ber +
                     Pber[3,]*Mber[3,]*K3ber +
                     Pber[4,]*Mber[4,]*K4ber +
                     Pber[5,]*Mber[5,]*K5ber
                   
      tHber_f[t,] <- (Pber[1,]*Mber[1,]*H1ber*(2/32) +
                      Pber[2,]*Mber[2,]*H2ber*(2/32) +
                      Pber[3,]*Mber[3,]*H3ber*(2/32) +
                      Pber[4,]*Mber[4,]*H4ber*(2/32) +
                      Pber[5,]*Mber[5,]*H5ber*(2/32) )

      #contrasts
      conQ_f[t,] <- tQber_f[t,] - tQmat_f[t,]
      conK_f[t,] <- tKber_f[t,] - tKmat_f[t,] 
      conH_f[t,] <- tHber_f[t,] - tHmat_f[t,]  

} # for t




##### male

post <- post3_m

Q1mat <- post$"mQ[2,1]"
Q2mat <- post$"mQ[2,2]" 
Q3mat <- post$"mQ[2,3]" 
Q4mat <- post$"mQ[2,4]" 
Q5mat <- post$"mQ[2,5]"

Q1ber <- post$"mQ[1,1]"
Q2ber <- post$"mQ[1,2]" 
Q3ber <- post$"mQ[1,3]" 
Q4ber <- post$"mQ[1,4]" 
Q5ber <- post$"mQ[1,5]"

K1mat <- post$"mK[2,1]"
K2mat <- post$"mK[2,2]" 
K3mat <- post$"mK[2,3]" 
K4mat <- post$"mK[2,4]" 
K5mat <- post$"mK[2,5]"

K1ber <- post$"mK[1,1]"
K2ber <- post$"mK[1,2]" 
K3ber <- post$"mK[1,3]" 
K4ber <- post$"mK[1,4]" 
K5ber <- post$"mK[1,5]"

H1mat <- post$"mH[2,1]"
H2mat <- post$"mH[2,2]" 
H3mat <- post$"mH[2,3]"
H4mat <- post$"mH[2,4]" 
H5mat <- post$"mH[2,5]"

H1ber <- post$"mH[1,1]"
H2ber <- post$"mH[1,2]" 
H3ber <- post$"mH[1,3]" 
H4ber <- post$"mH[1,4]" 
H5ber <- post$"mH[1,5]"

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

I1ber <- post$"mI[1,1]"
I2ber <- post$"mI[1,2]" 
I3ber <- post$"mI[1,3]" 
I4ber <- post$"mI[1,4]" 


tQmat_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKmat_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHmat_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

tQber_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tKber_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
tHber_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))

conQ_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conK_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))
conH_m <- matrix(data=NA, nrow=length(a), ncol=length(Q1mat))


# don't multiply Q's by m. Metabolism doesn't affect allometry. H and K remain fairly constant after adolescence and adulthood, as reporterd for human metabolism

for ( t in 1:length(a) ){
      Pmat <- matrix(unlist(vpropparam(x=a[t],Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                              K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                              H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                              I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["p",]), nrow=5, byrow=FALSE)
      Pber <- matrix(unlist(vpropparam(x=a[t],Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                              K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                              H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                              I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["p",]), nrow=5, byrow=FALSE)

      Mmat <- matrix(unlist(vpropparam(x=a[t],Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                              K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                              H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                              I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["m",]), nrow=5, byrow=FALSE)
      Mber <- matrix(unlist(vpropparam(x=a[t],Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                              K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                              H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                              I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["m",]), nrow=5, byrow=FALSE)

      tQmat_m[t,] <- Pmat[1,]*Q1mat +
                     Pmat[2,]*Q2mat +
                     Pmat[3,]*Q3mat +
                     Pmat[4,]*Q4mat +
                     Pmat[5,]*Q5mat

      tKmat_m[t,] <- Pmat[1,]*Mmat[1,]*K1mat +
                     Pmat[2,]*Mmat[2,]*K2mat +
                     Pmat[3,]*Mmat[3,]*K3mat +
                     Pmat[4,]*Mmat[4,]*K4mat +
                     Pmat[5,]*Mmat[5,]*K5mat

      tHmat_m[t,] <- (Pmat[1,]*Mmat[1,]*H1mat*(2/32) + # original H in units of g/cm^2 of 5% of total body skin surface. Multiply by (2cm^2 skin surface)/(32cm^2 intestine surface) (Mosteller1987, Helander2014)
                      Pmat[2,]*Mmat[2,]*H2mat*(2/32) +
                      Pmat[3,]*Mmat[3,]*H3mat*(2/32) +
                      Pmat[4,]*Mmat[4,]*H4mat*(2/32) +
                      Pmat[5,]*Mmat[5,]*H5mat*(2/32) )



      tQber_m[t,] <- Pber[1,]*Q1ber +
                     Pber[2,]*Q2ber +
                     Pber[3,]*Q3ber +
                     Pber[4,]*Q4ber +
                     Pber[5,]*Q5ber

      tKber_m[t,] <- Pber[1,]*Mber[1,]*K1ber +
                     Pber[2,]*Mber[2,]*K2ber +
                     Pber[3,]*Mber[3,]*K3ber +
                     Pber[4,]*Mber[4,]*K4ber +
                     Pber[5,]*Mber[5,]*K5ber
                   
      tHber_m[t,] <- (Pber[1,]*Mber[1,]*H1ber*(2/32) +
                      Pber[2,]*Mber[2,]*H2ber*(2/32) +
                      Pber[3,]*Mber[3,]*H3ber*(2/32) +
                      Pber[4,]*Mber[4,]*H4ber*(2/32) +
                      Pber[5,]*Mber[5,]*H5ber*(2/32) )

      #contrasts
      conQ_m[t,] <- tQber_m[t,] - tQmat_m[t,]
      conK_m[t,] <- tKber_m[t,] - tKmat_m[t,] 
      conH_m[t,] <- tHber_m[t,] - tHmat_m[t,]  

} # for t






################################################################# example male age 15

Pmat <- matrix(unlist(vpropparam(x=15,Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                      K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                      H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                      I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["p",]), nrow=5, byrow=FALSE)
Pber <- matrix(unlist(vpropparam(x=15,Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                      K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                      H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                      I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["p",]), nrow=5, byrow=FALSE)

Mmat <- matrix(unlist(vpropparam(x=15,Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                      K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                      H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                      I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["m",]), nrow=5, byrow=FALSE)
Mber <- matrix(unlist(vpropparam(x=15,Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                      K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                      H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                      I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["m",]), nrow=5, byrow=FALSE)

mPmat <- rowMeans(Pmat)
mQmat <- c( mean(Q1mat), mean(Q2mat), mean(Q3mat), mean(Q4mat), mean(Q5mat) )
prodPQmat <- mPmat*mQmat
sumprodPQmat <- sum(prodPQmat)

mPber <- rowMeans(Pber)
mQber <- c( mean(Q1ber), mean(Q2ber), mean(Q3ber), mean(Q4ber), mean(Q5ber) )
prodPQber <- mPber*mQber
sumprodPQber <- sum(prodPQber)

matQsumm <- as.data.frame(cbind(mQmat, mPmat, prodPQmat, c(NA,NA,NA,NA,sumprodPQmat)))
berQsumm <- as.data.frame(cbind(mQber, mPber, prodPQber, c(NA,NA,NA,NA,sumprodPQber)))

names(matQsumm) <- c("Q","PropTotHeight","Product","Sum")
names(berQsumm) <- c("Q","PropTotHeight","Product","Sum")
matQsumm
berQsumm



mPmat <- rowMeans(Pmat)
mMmat <- rowMeans(Mmat)
mKmat <- c( mean(K1mat), mean(K2mat), mean(K3mat), mean(K4mat), mean(K5mat) )
prodPKmat <- mPmat*mKmat*mMmat
sumprodPKmat <- sum(prodPKmat)

mPber <- rowMeans(Pber)
mMber <- rowMeans(Mber)
mKber <- c( mean(K1ber), mean(K2ber), mean(K3ber), mean(K4ber), mean(K5ber) )
prodPKber <- mPber*mKber*mMber
sumprodPKber <- sum(prodPKber)

matKsumm <- as.data.frame(cbind(mKmat, mPmat, mMmat, prodPKmat, c(NA,NA,NA,NA,sumprodPKmat)))
berKsumm <- as.data.frame(cbind(mKber, mPber, mMber, prodPKber, c(NA,NA,NA,NA,sumprodPKber)))
names(matKsumm) <- c("K","PropTotHeight","PropOrigMetab","Product","Sum")
names(berKsumm) <- c("K","PropTotHeight","PropOrigMetab","Product","Sum")
matKsumm
berKsumm


mPmat <- rowMeans(Pmat)
mMmat <- rowMeans(Mmat)
mHmat <- c( mean(H1mat), mean(H2mat), mean(H3mat), mean(H4mat), mean(H5mat) )*(2/32)
prodPHmat <- mPmat*mHmat*mMmat
sumprodPHmat <- sum(prodPHmat)

mPber <- rowMeans(Pber)
mMber <- rowMeans(Mber)
mHber <- c( mean(H1ber), mean(H2ber), mean(H3ber), mean(H4ber), mean(H5ber) )*(2/32)
prodPHber <- mPber*mHber*mMber
sumprodPHber <- sum(prodPHber)

matHsumm <- as.data.frame(cbind(mHmat, mPmat, mMmat, prodPHmat, c(NA,NA,NA,NA,sumprodPHmat)))
berHsumm <- as.data.frame(cbind(mHber, mPber, mMber, prodPHber, c(NA,NA,NA,NA,sumprodPHber)))
names(matHsumm) <- c("H","PropTotHeight","PropOrigMetab","Product","Sum")
names(berHsumm) <- c("H","PropTotHeight","PropOrigMetab","Product","Sum")
matHsumm
berHsumm





################################################


# line colors and sizes
colorlist <- hcl.colors(n=11, palette="Blue-Red 3",
                        alpha=0.5)
names(colorlist) <- c("1.1","1.2","1.3","1.4","1.5", "neutral",
                      "2.5","2.4","2.3","2.2","2.1")
#pie(rep(1, 11), col = colorlist)

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


BerArea_col <- colorlist["2.3"]

MatArea_col <- colorlist["1.3"]

ConLine_lwd <- 3
ConLine_col <- "black"
ConArea_col <- grey(0.5,alpha=0.5)

ZerLine_lwd <- 1
ZerLine_lty <- "33"       #lty: first number in string is dash length, second is white space length
ZerLine_col <- "black"


# values for plotting
Nsims <- length(post$"mQ[2,1]")
n_samps <- 100 # how many samples to draw from posterior for plotting
samps <- sample.int(Nsims, n_samps, replace=FALSE) #random draws from the posterior












pdf(file="./combined_plots/Plots/params_combined.pdf",
    height=10, width=10)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6
                      ),
        nrow=3, ncol=2, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1,1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 8)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels


# female  ###########################################################################################################################################################################

post <- post3_f

tQmat <- tQmat_f
tKmat <- tKmat_f
tHmat <- tHmat_f

tQber <- tQber_f
tKber <- tKber_f
tHber <- tHber_f

conQ <- conQ_f
conK <- conK_f
conH <- conH_f



################ Q

 #set up plot
  #par(mar=c(5, 6, 2, 6), oma = c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0.07, 0.18), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.10,0.18,0.02), las=1 )
  axis( side=1, at=seq(0,25,5), labels=NA, las=1 )
  box(col = "black") 
  par(xpd=FALSE)

  # for ( z in 1:n_samps ){

  #   # US
  #   lines(x = a,
  #         y = tQber[,samps[z]],
  #         col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

  #   # Matsigenka
  #   lines(x = a,
  #         y = tQmat[,samps[z]],
  #         col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

  # } #for z


  ints <-  t(apply(tQber, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = BerArea_col, border=NA)

  ints <-  t(apply(tQmat, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)


  lines(x = a,
        y = rowMeans(tQber),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = a,
        y = rowMeans(tQmat),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  #par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(0,0.2),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region


  #par(xpd=TRUE)
  #text(expression(paste(bolditalic("q"))), x=-7, y=0.13, srt=0, las=3, cex=1.5)
  #text("Age since conception (years)", x=13, y=0.055, srt=0, las=3)



  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.005,0.005+0.05), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.005, 0.005), labels=NA, las=2 )
  par(xpd=FALSE)


  # for ( z in 1:n_samps ){

  #   lines(x = a,
  #         y = conQ[,samps[z]],
  #         col=grey(0.5), lwd=0.25, lty=1)

  # } #for z


  ints <-  t(apply(conQ, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = a,
        y = rowMeans(conQ),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region

  # lines(x = c(min(a),max(a)),
  #       y = c(0,0),
  #       col="black", lwd=1, lty="33") #lty: first number in string is dash length, second is white space length


  par(xpd=TRUE)
  #text("Contrast", x=33, y=0, srt=270, las=3)



#set up new plot for legend placement
par(new=TRUE) #add to existing plot
plot( x=0, y=8, type="n", ylim=c(0,10), xlim=c(0,10), axes=FALSE, ylab=NA, xlab=NA)
par(xpd=NA) # plotting clipped to device region


# legend
legtext <- c("U.S.", "Matsigenka", "U.S. - Matsigenka contrast")
xcoords <- c(0, 5, 12) # 0, 7, 23
secondvector <- (1:length(legtext))-1
textwidths <- xcoords/secondvector # this works for all but the first element
textwidths[1] <- 0 # so replace element 1 with a finite number (any will do)

  legend(x=-5, y=15.2,
         #inset=c(0,-0.5),       # inset is distance from x and y margins
         text.width=textwidths,  
         legend=legtext,
         bty="n",
         bg="white",
         col=c(BerMeanTraj_col, MatMeanTraj_col, ConLine_col),
         lty=c(1,1,1), 
         lwd=c(9,9,9),
         cex=1.8,
         x.intersp=0.5,
         seg.len=1.5,
         horiz=TRUE)

  rect(xleft = -5,
       ybottom = 13.3,
       xright = 21.91,
       ytop = 14.9,
       lwd=1)


# row labels

mtext(expression(paste(bolditalic("q"))),   side = 1, outer = T, cex = 1.5, adj=-0.12, line = -55 ) #side = c(bottom, left, top, right)

mtext(expression(paste(bolditalic("K"))),   side = 1, outer = T, cex = 1.5, adj=-0.12, line = -32 )
mtext(expression(paste("(g/g)")),           side = 1, outer = T, cex = 1.5, adj=-0.16, line = -29 )

mtext(expression(paste(bolditalic("H"))),   side = 1, outer = T, cex = 1.5, adj=-0.12, line = -12 )
mtext(expression(paste("(g/cm"^2,")")),     side = 1, outer = T, cex = 1.5, adj=-0.20, line = -9 )


mtext("Age since conception (years)",       side = 1, outer = T, cex = 1.5, adj=0.6, line = 3.8 )




# column labels
symbol.Female(centerx = 5, centery = 12, rayonx=0.4, lwd=1.5, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.107, line = -110.5 )

symbol.Male(  centerx = 16.5, centery = 11.6, rayonx=0.4, lwd=1.5, col="black")
#mtext(expression(bold("Height")), side = 1, outer = T, cex = 2.5, adj=0.385, line = -110.5 )


par(xpd=FALSE)


################### K

plot( x=0, y=60, type="n", ylim=c(4,24), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(8,24,4), labels=seq(8,24,4), las=1 )
  axis( side=1, at=seq(0,25,5), labels = NA, las=1 )
  box(col = "black") 
  par(xpd=FALSE)


  # for ( z in 1:n_samps ){

  #   # US
  #   lines(x = a,
  #         y = tKber[,samps[z]],
  #         col=colorlist["2.3"], lwd=0.25, lty=1)

  #   # Matsigenka
  #   lines(x = a,
  #         y = tKmat[,samps[z]],
  #         col=colorlist["1.3"], lwd=0.25, lty=1)

  # } #for z

  # all values of vertices passed to polygon() must be within the plotting range

  mina <- 0.75
  indexmina <- min(which(a > mina))

  ints <-  t(apply(tKber, 1, HPDI, prob=0.9))[,]  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = BerArea_col, border=NA)

  ints <-  t(apply(tKmat, 1, HPDI, prob=0.9))
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = MatArea_col, border=NA)


  lines(x = a,
        y = rowMeans(tKber),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = a,
        y = rowMeans(tKmat),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(4,50),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region


  par(xpd=TRUE)
  #text(expression(paste(bolditalic("K"))), x=-7, y=16, srt=0, las=3, cex=1.5)
  #text(expression(paste("(g/g)")), x=-7, y=15, srt=0, las=3, cex=1)
  #text("Age since conception (years)", x=13, y=2, srt=0, las=3)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.7-0.2,0.2+5), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.4, 0.4), labels=NA, las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conK, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)

  lines(x = a,
        y = rowMeans(conK),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region



################ H


plot( x=0, y=60, type="n", ylim=c(0.33-0.15,1), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.3,1,0.1), labels=seq(0.3,1,0.1), las=1 )
  axis( side=1, at=seq(0,25,5), labels=seq(0,25,5), las=1 )
  box(col = "black") 
  par(xpd=FALSE)


  # for ( z in 1:n_samps ){

  #   # US
  #   lines(x = a,
  #         y = tHber[,samps[z]],
  #         col=colorlist["2.3"], lwd=0.25, lty=1)

  #   # Matsigenka
  #   lines(x = a,
  #         y = tHmat[,samps[z]],
  #         col=colorlist["1.3"], lwd=0.25, lty=1)

  # } #for z

  # all values of vertices passed to polygon() must be within the plotting range

  mina <- 0.75
  indexmina <- min(which(a > mina))

  ints <-  t(apply(tHber, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = BerArea_col, border=NA)

  ints <-  t(apply(tHmat, 1, HPDI, prob=0.9))
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = MatArea_col, border=NA)


  lines(x = a,
        y = rowMeans(tHber),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = a,
        y = rowMeans(tHmat),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(0,2),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region


  par(xpd=NA)
  #text(expression(paste(bolditalic("H"))), x=-7, y=12, srt=0, las=3, cex=1.5)
  #text(expression(paste("(g/cm"^2,")")), x=-7, y=11, srt=0, las=3, cex=1)
  #text("Age since conception (years)", x=13, y=1.5, srt=0, las=3)

  text("birth", x=0.75, y=-0.05, srt=0, las=3, cex=1.5)
  par(xpd=FALSE)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.03-0.005,0.005+0.15), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.02, 0.02), labels=NA, las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conH, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = a,
        y = rowMeans(conH),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region



# male  ###########################################################################################################################################################################

post <- post3_m

tQmat <- tQmat_m
tKmat <- tKmat_m
tHmat <- tHmat_m

tQber <- tQber_m
tKber <- tKber_m
tHber <- tHber_m

conQ <- conQ_m
conK <- conK_m
conH <- conH_m



################ Q

 #set up plot
  #par(mar=c(5, 6, 2, 6), oma = c(1, 1, 1, 1)) #c(bottom, left, top, right)
  plot( x=0, y=60, type="n", ylim=c(0.07, 0.18), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.10,0.18,0.02), labels=NA, las=1 )
  axis( side=1, at=seq(0,25,5), labels=NA, las=1 )
  box(col = "black") 
  par(xpd=FALSE)

  # for ( z in 1:n_samps ){

  #   # US
  #   lines(x = a,
  #         y = tQber[,samps[z]],
  #         col=BerIndivTraj_col, lwd=BerIndivTraj_lwd, lty=BerIndivTraj_lty)

  #   # Matsigenka
  #   lines(x = a,
  #         y = tQmat[,samps[z]],
  #         col=MatIndivTraj_col, lwd=MatIndivTraj_lwd, lty=MatIndivTraj_lty)

  # } #for z

  ints <-  t(apply(tQber, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = BerArea_col, border=NA)

  ints <-  t(apply(tQmat, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = MatArea_col, border=NA)


  lines(x = a,
        y = rowMeans(tQber),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = a,
        y = rowMeans(tQmat),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  #par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(0,0.2),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region


  par(xpd=TRUE)
  #text(expression(paste(bolditalic("q"))), x=-7, y=0.13, srt=0, las=3, cex=1.5)
  #text("Age since conception (years)", x=13, y=0.055, srt=0, las=3)



  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.005,0.005+0.05), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.005, 0, 0.005), labels=c(-0.005, 0, 0.005), las=2 )
  par(xpd=FALSE)


  # for ( z in 1:n_samps ){

  #   lines(x = a,
  #         y = conQ[,samps[z]],
  #         col=grey(0.5), lwd=0.25, lty=1)

  # } #for z


  ints <-  t(apply(conQ, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = a,
        y = rowMeans(conQ),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  #par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region

  # lines(x = c(min(a),max(a)),
  #       y = c(0,0),
  #       col="black", lwd=1, lty="33") #lty: first number in string is dash length, second is white space length




par(xpd=FALSE)



################### K

plot( x=0, y=60, type="n", ylim=c(4,24), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(8,24,4), labels=NA, las=1 )
  axis( side=1, at=seq(0,25,5), labels=NA, las=1 )
  box(col = "black") 
  par(xpd=FALSE)


  # for ( z in 1:n_samps ){

  #   # US
  #   lines(x = a,
  #         y = tKber[,samps[z]],
  #         col=colorlist["2.3"], lwd=0.25, lty=1)

  #   # Matsigenka
  #   lines(x = a,
  #         y = tKmat[,samps[z]],
  #         col=colorlist["1.3"], lwd=0.25, lty=1)

  # } #for z

  # all values of vertices passed to polygon() must be within the plotting range

  mina <- 0.75
  indexmina <- min(which(a > mina))

  ints <-  t(apply(tKber, 1, HPDI, prob=0.9))[,]  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = BerArea_col, border=NA)

  ints <-  t(apply(tKmat, 1, HPDI, prob=0.9))
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = MatArea_col, border=NA)


  lines(x = a,
        y = rowMeans(tKber),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = a,
        y = rowMeans(tKmat),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(4,50),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region


  #par(xpd=TRUE)
  #text(expression(paste(bolditalic("K"))), x=-7, y=16, srt=0, las=3, cex=1.5)
  #text(expression(paste("(g/g)")), x=-7, y=15, srt=0, las=3, cex=1)
  #text("Age since conception (years)", x=13, y=2, srt=0, las=3)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.7-0.2,0.2+5), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.4, 0, 0.4), labels=c(-0.4, 0, 0.4), las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conK, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)

  lines(x = a,
        y = rowMeans(conK),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)

  #par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region


  par(xpd=NA)  # plotting clipped to devise region
  text("Contrast", x=33, y=2.5, srt=270, las=2, cex=2.3)
  par(xpd=FALSE) # plotting clipped to plot region



################ H


plot( x=0, y=60, type="n", ylim=c(0.33-0.15,1), xlim=c(0,26), axes=F, ylab=NA, xlab=NA )
  axis( side=2, at=seq(0.3,1,0.1), labels=NA, las=1 )
  axis( side=1, at=seq(0,25,5), labels=seq(0,25,5), las=1 )
  box(col = "black") 
  par(xpd=FALSE)


  # for ( z in 1:n_samps ){

  #   # US
  #   lines(x = a,
  #         y = tHber[,samps[z]],
  #         col=colorlist["2.3"], lwd=0.25, lty=1)

  #   # Matsigenka
  #   lines(x = a,
  #         y = tHmat[,samps[z]],
  #         col=colorlist["1.3"], lwd=0.25, lty=1)

  # } #for z

  # all values of vertices passed to polygon() must be within the plotting range

  mina <- 0.75
  indexmina <- min(which(a > mina))

  ints <-  t(apply(tHber, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of matrix
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = BerArea_col, border=NA)

  ints <-  t(apply(tHmat, 1, HPDI, prob=0.9))
  polygon(c(a[which(a >= mina)], rev(a[which(a >= mina)])), c(ints[indexmina:length(a),2], rev(ints[indexmina:length(a),1])), 
        col = MatArea_col, border=NA)


  lines(x = a,
        y = rowMeans(tHber),
        col=BerMeanTraj_col, lwd=BerMeanTraj_lwd, lty=BerMeanTraj_lty)
  lines(x = a,
        y = rowMeans(tHmat),
        col=MatMeanTraj_col, lwd=MatMeanTraj_lwd, lty=MatMeanTraj_lty)


  par(xpd=NA) # plotting clipped to device region
  lines(x = c(0.75,0.75),
        y = c(0,2),
        col="black", lwd=1, lty=1)
  par(xpd=FALSE) # plotting clipped to plot region

  #par(xpd=TRUE)
  #text(expression(paste(bolditalic("H"))), x=-7, y=12, srt=0, las=3, cex=1.5)
  #text(expression(paste("(g/cm"^2,")")), x=-7, y=11, srt=0, las=3, cex=1)
  #text("Age since conception (years)", x=13, y=1.5, srt=0, las=3)

  par(xpd=NA)
  text("birth", x=0.75, y=-0.05, srt=0, las=3, cex=1.5)
  par(xpd=FALSE)


  #plot contrast

  #set up 
  par(new=TRUE) #add to existing plot

  plot( x=0, y=0, type="n", ylim=c(-0.03-0.005,0.005+0.15), xlim=c(0,26), axes=F, ylab=NA, xlab=NA)
  axis( side=4, at=c(-0.02, 0, 0.02), labels=c(-0.01, 0, 0.01), las=2 )
  par(xpd=FALSE)


  ints <-  t(apply(conH, 1, HPDI, prob=0.9))  #apply HPDI function with argument prob=0.9 to the rows (dim 1) of conQ
  polygon(c(a, rev(a)), c(ints[,2], rev(ints[,1])), 
        col = ConArea_col, border=NA)


  lines(x = a,
        y = rowMeans(conH),
        col=ConLine_col, lwd=ConLine_lwd, lty=1)


  #par(xpd=NA) # plotting clipped to device region
  lines(x=c(min(a),35),
        y=c(0,0), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
  par(xpd=FALSE) # plotting clipped to plot region


graphics.off()







#################################################################################################################################

############### I


# male
post3_m <- readRDS("./analysis_hw_mal/post6.RDS")
#str(post3_m)

# female
post3_f <- readRDS("./analysis_hw_fem/post6.RDS")
#str(post3_f)



######## function to determine x-axis range for plotting
xrange <- function( D1=c(1,2,3), D2=c(2,3,4) ){

    # determine which distribution is to the right of the other
    if (mean(D1) > mean(D2)) {DR <- D1} else {DR <- D2} 
    if (mean(D1) > mean(D2)) {DL <- D2} else {DL <- D1}
    
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
        Llabo <- formatC(x=Llab, format = "f", digits = 3)
        Rlabo <- formatC(x=Rlab, format = "f", digits = 3)
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
        labo <- 
        Llab <- formatC(x=-1*lab, format = "f", digits = 3)
        Rlab <- formatC(x=lab, format = "f", digits = 3)
    } #else

    Lbound <- -1*bound
    Rbound <- bound

    return( list(Lbound=Lbound, Rbound=Rbound, Llab=Llab, Rlab=Rlab) )
} # xrangecon


#D1 <- contr_char_list$ber.matMaxHeight1
#xrangecon(D1=D1)



##### female

post <- post3_f

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

I1ber <- post$"mI[1,1]"
I2ber <- post$"mI[1,2]" 
I3ber <- post$"mI[1,3]" 
I4ber <- post$"mI[1,4]" 

mean_I_list_f <- list( I1mat, I1ber,
                       I2mat, I2ber,
                       I3mat, I3ber,
                       I4mat, I4ber )


names(mean_I_list_f) <- c( "I1mat", "I1ber",
                           "I2mat", "I2ber",
                           "I3mat", "I3ber",
                           "I4mat", "I4ber" )

#str(mean_I_list_f)



contr_I_list_f <- list( I1ber - I1mat,
                        I2ber - I2mat,
                        I3ber - I3mat,
                        I4ber - I4mat )


names(contr_I_list_f) <- c( "ber.mat1",
                            "ber.mat2",
                            "ber.mat3",
                            "ber.mat4" )

#str(contr_I_list_f)



##### male

post <- post3_m

I1mat <- post$"mI[2,1]"
I2mat <- post$"mI[2,2]" 
I3mat <- post$"mI[2,3]" 
I4mat <- post$"mI[2,4]" 

I1ber <- post$"mI[1,1]"
I2ber <- post$"mI[1,2]" 
I3ber <- post$"mI[1,3]" 
I4ber <- post$"mI[1,4]" 

mean_I_list_m <- list( I1mat, I1ber,
                       I2mat, I2ber,
                       I3mat, I3ber,
                       I4mat, I4ber )


names(mean_I_list_m) <- c( "I1mat", "I1ber",
                           "I2mat", "I2ber",
                           "I3mat", "I3ber",
                           "I4mat", "I4ber" )

#str(mean_I_list_m)



contr_I_list_m <- list( I1ber - I1mat,
                        I2ber - I2mat,
                        I3ber - I3mat,
                        I4ber - I4mat )


names(contr_I_list_m) <- c( "ber.mat1",
                            "ber.mat2",
                            "ber.mat3",
                            "ber.mat4" )

#str(contr_I_list_m)





######## plotting colors

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





pdf(file="./combined_plots/Plots/I_combined.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 2, 3, 4, 0, 0, 0, 0, 0, 5, 6, 7, 8,0,0,0,0,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        9,10,11,12, 0, 0, 0, 0, 0,13,14,15,16,0,0,0,0,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1, 0.2, 1,1, 0.2, 1,1, 0.2, 1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels


# female  ###########################################################################################################################################################################

post <- post3_f
mean_I_list <- mean_I_list_f
contr_I_list <- contr_I_list_f


I1_plot <- denschart3( mean_I_list[c(1,2)],
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
            xlim=c( xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Lbound,
                    xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Rbound ),     #range(0.12,0.13),        #( mean( mean_I_list$I1mat ) - 1, mean( mean_I_list$I1ber ) + 1 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I1mat ), mean( mean_I_list$I1mat ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_I_list$I1ber ), mean( mean_I_list$I1ber ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)



axis(side=1,
     at=c( xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Llab,
           xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Llab,
                xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)



#axis(side=1, at=c(0.12,0.13), labels=c(0.12,0.13), cex.axis=1.5)
#mtext("Age (years since conception)", side = 1, outer = T, cex = 1.5, line = 3, adj=1)

#mtext("Composite model characteristic posteriors", side = 3, outer = F, cex = 2, line = 1)
#par(xpd=NA) # plotting clipped to device region



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

mtext(expression(paste(bolditalic("i")[2])), side = 1, outer = T, cex = 2.5, adj=-0.05, line = -103 )
mtext(expression(paste(bolditalic("i")[3])), side = 1, outer = T, cex = 2.5, adj=-0.05, line = -90.5 )
mtext(expression(paste(bolditalic("i")[4])), side = 1, outer = T, cex = 2.5, adj=-0.05, line = -78 )
mtext(expression(paste(bolditalic("i")[5])), side = 1, outer = T, cex = 2.5, adj=-0.05, line = -65.5 )

mtext("Age since conception (years)", side = 1, outer = T, cex = 2.2, adj=0.15, line = -58)


# column labels
symbol.Female(centerx = 11, centery = 10.8, rayonx=0.7, lwd=3, col="black")

symbol.Male(  centerx = 38.5, centery = 10.4, rayonx=0.7, lwd=3, col="black")




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



I2_plot <- denschart3( mean_I_list[c(3,4)],
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
            xlim=c( xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Lbound,
                    xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Rbound ),   #range(0.7,0.8),        #( mean( mean_I_list$I2mat ) - 0.2, mean( mean_I_list$I2ber ) + 0.2),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I2mat ), mean( mean_I_list$I2mat ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_I_list$I2ber ), mean( mean_I_list$I2ber ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Llab,
           xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Llab,
                xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)

#axis(side=1, at=c(0.7,0.8), labels=c(0.7,0.8), cex.axis=1.5)
#mtext("Age (years since conception)", side = 1, outer = T, cex = 1.5, line = 3, adj=0.5)



I3_plot <- denschart3( mean_I_list[c(5,6)],
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
            xlim=c( xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Lbound,
                    xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Rbound ),     #range(2.3,2.8),        #( mean( mean_I_list$I3mat ) - 1, mean( mean_I_list$I3ber ) + 1),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I3mat ), mean( mean_I_list$I3mat ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_I_list$I3ber ), mean( mean_I_list$I3ber ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Llab,
           xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Llab,
                xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)


#axis(side=1, at=c(2.3,2.8), labels=c(2.3,2.8), cex.axis=1.5)
#mtext("Age (years since conception)", side = 1, outer = T, cex = 1.5, line = 3, adj=0.5)


I4_plot <- denschart3( mean_I_list[c(7,8)],
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
            xlim=c( xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Lbound,
                    xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Rbound ),     #range(10,12),        #( mean( mean_I_list$I4mat ) - 1, mean( mean_I_list$I4ber ) + 1),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I4mat ), mean( mean_I_list$I4mat ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_I_list$I4ber ), mean( mean_I_list$I4ber ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Llab,
           xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Llab,
                xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)


#axis(side=1, at=c(10,12), labels=c(10,12), cex.axis=1.5)
#mtext("Age since conception (years)", side = 1, outer = T, cex = 1.3, line = 3, adj=-3)



### parameter contrasts


I1_cont_plot <- denschart3( contr_I_list[1],
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
            xlim=range(xrangecon( D1=unlist(contr_I_list[1]) )$Lbound,
                       xrangecon( D1=unlist(contr_I_list[1]) )$Rbound ),   #range(-0.01,0.01),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat1), mean(contr_I_list$ber.mat1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region


axis(side=1,
    at=c(xrangecon( D1=unlist(contr_I_list[1]) )$Llab, xrangecon( D1=unlist(contr_I_list[1]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_I_list[1]) )$Llab, xrangecon( D1=unlist(contr_I_list[1]) )$Rlab),
    cex.axis=cex_axis)





I2_cont_plot <- denschart3( contr_I_list[2],
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
            xlim=range(xrangecon( D1=unlist(contr_I_list[2]) )$Lbound,
                       xrangecon( D1=unlist(contr_I_list[2]) )$Rbound ),  #range(-0.05,0.05),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat2), mean(contr_I_list$ber.mat2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_I_list[2]) )$Llab, xrangecon( D1=unlist(contr_I_list[2]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_I_list[2]) )$Llab, xrangecon( D1=unlist(contr_I_list[2]) )$Rlab),
    cex.axis=cex_axis)

#axis(side=1, at=c(-0.05,0.05), labels=c(-0.05,0.05), cex.axis=1.5)


I3_cont_plot <- denschart3( contr_I_list[3],
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
            xlim=range(xrangecon( D1=unlist(contr_I_list[3]) )$Lbound,
                       xrangecon( D1=unlist(contr_I_list[3]) )$Rbound ),  #range(-0.2,0.2),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat3), mean(contr_I_list$ber.mat3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_I_list[3]) )$Llab, xrangecon( D1=unlist(contr_I_list[3]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_I_list[3]) )$Llab, xrangecon( D1=unlist(contr_I_list[3]) )$Rlab),
    cex.axis=cex_axis)
#axis(side=1, at=c(-0.2,0.2), labels=c(-0.2,0.2), cex.axis=1.5)



I4_cont_plot <- denschart3( contr_I_list[4],
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
            xlim=range(xrangecon( D1=unlist(contr_I_list[4]) )$Lbound,
                       xrangecon( D1=unlist(contr_I_list[4]) )$Rbound ),  #range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat4), mean(contr_I_list$ber.mat4) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_I_list[4]) )$Llab, xrangecon( D1=unlist(contr_I_list[4]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_I_list[4]) )$Llab, xrangecon( D1=unlist(contr_I_list[4]) )$Rlab),
    cex.axis=cex_axis)
#axis(side=1, at=c(-0.5,0.5), labels=c(-0.5,0.5), cex.axis=1.5)




# male  ###########################################################################################################################################################################

post <- post3_m
mean_I_list <- mean_I_list_m
contr_I_list <- contr_I_list_m


I1_plot <- denschart3( mean_I_list[c(1,2)],
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
            xlim=c( xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Lbound,
                    xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Rbound ),     #range(0.12,0.13),        #( mean( mean_I_list$I1mat ) - 1, mean( mean_I_list$I1ber ) + 1 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I1mat ), mean( mean_I_list$I1mat ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_I_list$I1ber ), mean( mean_I_list$I1ber ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Llab,
           xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Llab,
                xrange( D1=unlist(mean_I_list[1]),D2=unlist(mean_I_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)



I2_plot <- denschart3( mean_I_list[c(3,4)],
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
            xlim=c( xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Lbound,
                    xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Rbound ),   #range(0.7,0.8),        #( mean( mean_I_list$I2mat ) - 0.2, mean( mean_I_list$I2ber ) + 0.2),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I2mat ), mean( mean_I_list$I2mat ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_I_list$I2ber ), mean( mean_I_list$I2ber ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Llab,
           xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Llab,
                xrange( D1=unlist(mean_I_list[3]),D2=unlist(mean_I_list[4]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)

#axis(side=1, at=c(0.7,0.8), labels=c(0.7,0.8), cex.axis=1.5)
#mtext("Age (years since conception)", side = 1, outer = T, cex = 1.5, line = 3, adj=0.5)



I3_plot <- denschart3( mean_I_list[c(5,6)],
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
            xlim=c( xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Lbound,
                    xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Rbound ),     #range(2.3,2.8),        #( mean( mean_I_list$I3mat ) - 1, mean( mean_I_list$I3ber ) + 1),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I3mat ), mean( mean_I_list$I3mat ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_I_list$I3ber ), mean( mean_I_list$I3ber ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Llab,
           xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Llab,
                xrange( D1=unlist(mean_I_list[5]),D2=unlist(mean_I_list[6]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)


#axis(side=1, at=c(2.3,2.8), labels=c(2.3,2.8), cex.axis=1.5)
#mtext("Age (years since conception)", side = 1, outer = T, cex = 1.5, line = 3, adj=0.5)


I4_plot <- denschart3( mean_I_list[c(7,8)],
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
            xlim=c( xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Lbound,
                    xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Rbound ),     #range(10,12),        #( mean( mean_I_list$I4mat ) - 1, mean( mean_I_list$I4ber ) + 1),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( mean_I_list$I4mat ), mean( mean_I_list$I4mat ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_I_list$I4ber ), mean( mean_I_list$I4ber ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Llab,
           xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Llab,
                xrange( D1=unlist(mean_I_list[7]),D2=unlist(mean_I_list[8]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)



### parameter contrasts



I1_cont_plot <- denschart3( contr_I_list[1],
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
            xlim=range(xrangecon( D1=unlist(contr_I_list[1]) )$Lbound,
                       xrangecon( D1=unlist(contr_I_list[1]) )$Rbound ),   #range(-0.01,0.01),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat1), mean(contr_I_list$ber.mat1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region


axis(side=1,
    at=c(xrangecon( D1=unlist(contr_I_list[1]) )$Llab, xrangecon( D1=unlist(contr_I_list[1]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_I_list[1]) )$Llab, xrangecon( D1=unlist(contr_I_list[1]) )$Rlab),
    cex.axis=cex_axis)





I2_cont_plot <- denschart3( contr_I_list[2],
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
            xlim=range(xrangecon( D1=unlist(contr_I_list[2]) )$Lbound,
                       xrangecon( D1=unlist(contr_I_list[2]) )$Rbound ),  #range(-0.05,0.05),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat2), mean(contr_I_list$ber.mat2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_I_list[2]) )$Llab, xrangecon( D1=unlist(contr_I_list[2]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_I_list[2]) )$Llab, xrangecon( D1=unlist(contr_I_list[2]) )$Rlab),
    cex.axis=cex_axis)

#axis(side=1, at=c(-0.05,0.05), labels=c(-0.05,0.05), cex.axis=1.5)


I3_cont_plot <- denschart3( contr_I_list[3],
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
            xlim=range(xrangecon( D1=unlist(contr_I_list[3]) )$Lbound,
                       xrangecon( D1=unlist(contr_I_list[3]) )$Rbound ),  #range(-0.2,0.2),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat3), mean(contr_I_list$ber.mat3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_I_list[3]) )$Llab, xrangecon( D1=unlist(contr_I_list[3]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_I_list[3]) )$Llab, xrangecon( D1=unlist(contr_I_list[3]) )$Rlab),
    cex.axis=cex_axis)
#axis(side=1, at=c(-0.2,0.2), labels=c(-0.2,0.2), cex.axis=1.5)



I4_cont_plot <- denschart3( contr_I_list[4],
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
            xlim=range(xrangecon( D1=unlist(contr_I_list[4]) )$Lbound,
                       xrangecon( D1=unlist(contr_I_list[4]) )$Rbound ),  #range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_I_list$ber.mat4), mean(contr_I_list$ber.mat4) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_I_list[4]) )$Llab, xrangecon( D1=unlist(contr_I_list[4]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_I_list[4]) )$Llab, xrangecon( D1=unlist(contr_I_list[4]) )$Rlab),
    cex.axis=cex_axis)
#axis(side=1, at=c(-0.5,0.5), labels=c(-0.5,0.5), cex.axis=1.5)



graphics.off()






