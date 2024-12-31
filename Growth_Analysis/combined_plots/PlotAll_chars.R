

##################################################################################


######## load posteriors from complete models, need to run PrepareData to plot raw data points

# male
post3_m <- readRDS("./analysis_hw_mal/post6.RDS")
#str(post3_m)

# female
post3_f <- readRDS("./analysis_hw_fem/post6.RDS")
#str(post3_f)




######## function to determine max velocity and age at max velocity during puberty
maxagevel <- function(Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                      K1=1, K2=1, K3=1, K4=1, K5=1,
                      H1=1, H2=1, H3=1, H4=1, H5=1,
                      I1=1, I2=1, I3=1, I4=1){


    secderiv <- function(x, Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                            K1=1, K2=1, K3=1, K4=1, K5=1,
                            H1=1, H2=1, H3=1, H4=1, H5=1,
                            I1=1, I2=1, I3=1, I4=1){

      I0 <- rep(0, times=length(I1))

      ifelse( x < I1, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ),

      ifelse( x < I2, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ),

      ifelse( x < I3, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ) +
                      2^(1/Q3)*( 1/( 1 + 2*Q3 )^2*H3^2*Q3*( 1/Q3 - 1 )*exp((2*K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 2 ) - 1/( 1 + 2*Q3 )^2*H3*K3*Q3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))))^( 1/Q3 - 1 ) ),

      ifelse( x < I4, 2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ) +
                      2^(1/Q3)*( 1/( 1 + 2*Q3 )^2*H3^2*Q3*( 1/Q3 - 1 )*exp((2*K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 2 ) - 1/( 1 + 2*Q3 )^2*H3*K3*Q3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))))^( 1/Q3 - 1 ) ) +
                      2^(1/Q4)*( 1/( 1 + 2*Q4 )^2*H4^2*Q4*( 1/Q4 - 1 )*exp((2*K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 2 ) - 1/( 1 + 2*Q4 )^2*H4*K4*Q4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))))^( 1/Q4 - 1 ) ),

                      2^(1/Q1)*( 1/( 1 + 2*Q1 )^2*H1^2*Q1*( 1/Q1 - 1 )*exp((2*K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 2 ) - 1/( 1 + 2*Q1 )^2*H1*K1*Q1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))))^( 1/Q1 - 1 ) ) +
                      2^(1/Q2)*( 1/( 1 + 2*Q2 )^2*H2^2*Q2*( 1/Q2 - 1 )*exp((2*K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 2 ) - 1/( 1 + 2*Q2 )^2*H2*K2*Q2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))))^( 1/Q2 - 1 ) ) +
                      2^(1/Q3)*( 1/( 1 + 2*Q3 )^2*H3^2*Q3*( 1/Q3 - 1 )*exp((2*K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 2 ) - 1/( 1 + 2*Q3 )^2*H3*K3*Q3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))))^( 1/Q3 - 1 ) ) +
                      2^(1/Q4)*( 1/( 1 + 2*Q4 )^2*H4^2*Q4*( 1/Q4 - 1 )*exp((2*K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 2 ) - 1/( 1 + 2*Q4 )^2*H4*K4*Q4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))))^( 1/Q4 - 1 ) ) +
                      2^(1/Q5)*( 1/( 1 + 2*Q5 )^2*H5^2*Q5*( 1/Q5 - 1 )*exp((2*K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))*(H5/K5*( 1 - exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 )) ))^( 1/Q5 - 2 ) - 1/( 1 + 2*Q5 )^2*H5*K5*Q5*exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))*(H5/K5*( 1 - exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))))^( 1/Q5 - 1 ) ) 
      ) ) ) ) 
    } # secderiv


    # find childhood age at which height velocity starts to increase for puberty
    a <- seq(from=10, to=24, by=0.1) # choose "from" such that velocity is increasing at puberty
    lowerlim <- 10.5
    found <- FALSE
    for ( t in 1:length(a) ){
        if ( found == FALSE ) {
            if ( secderiv(x=a[t], Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
                                  K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
                                  H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
                                  I1=I1, I2=I2, I3=I3, I4=I4) > 0 ) {
                found <- TRUE
                lowerlim <- a[t]
            } # if
        } # if
    } # for t 

    tmax_sol <- uniroot( secderiv, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
                                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
                                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
                                   I1=I1, I2=I2, I3=I3, I4=I4, lower=lowerlim, upper=25 )   # root between t=10.5 and t=21, to ignore high fetal/infant growth rate. Choose lower bound where slope of velocity is positive, because upper bound has negative slope.


    firderiv <- function(x=1, Q1=1, Q2=1, Q3=1, Q4=1, Q5=1,
                              K1=1, K2=1, K3=1, K4=1, K5=1,
                              H1=1, H2=1, H3=1, H4=1, H5=1,
                              I1=1, I2=1, I3=1, I4=1){

      I0 <- rep(0, times=length(I1))

      ifelse( x < I1, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ),

      ifelse( x < I2, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ),

      ifelse( x < I3, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ) +
                      1/( 1 + 2*Q3 )*2^(1/Q3)*H3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 1 ),

      ifelse( x < I4, 1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ) +
                      1/( 1 + 2*Q3 )*2^(1/Q3)*H3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 1 ) +
                      1/( 1 + 2*Q4 )*2^(1/Q4)*H4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 1 ),

                      1/( 1 + 2*Q1 )*2^(1/Q1)*H1*exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 ))*(H1/K1*( 1 - exp((K1*Q1*( I0 - x ))/( 1 + 2*Q1 )) ))^( 1/Q1 - 1 ) +
                      1/( 1 + 2*Q2 )*2^(1/Q2)*H2*exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 ))*(H2/K2*( 1 - exp((K2*Q2*( I1 - x ))/( 1 + 2*Q2 )) ))^( 1/Q2 - 1 ) +
                      1/( 1 + 2*Q3 )*2^(1/Q3)*H3*exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 ))*(H3/K3*( 1 - exp((K3*Q3*( I2 - x ))/( 1 + 2*Q3 )) ))^( 1/Q3 - 1 ) +
                      1/( 1 + 2*Q4 )*2^(1/Q4)*H4*exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 ))*(H4/K4*( 1 - exp((K4*Q4*( I3 - x ))/( 1 + 2*Q4 )) ))^( 1/Q4 - 1 ) +
                      1/( 1 + 2*Q5 )*2^(1/Q5)*H5*exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 ))*(H5/K5*( 1 - exp((K5*Q5*( I4 - x ))/( 1 + 2*Q5 )) ))^( 1/Q5 - 1 )
      ) ) ) )  
    } # firderiv

  # curve(firderiv(x, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
  #                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
  #                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
  #                   I1=I1, I2=I2, I3=I3, I4=I4), from=5, to=21)
  # curve(secderiv(x, Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
  #                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
  #                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
  #                   I1=I1, I2=I2, I3=I3, I4=I4), from=5, to=21, add = TRUE, col = "red")

    return( list(agemax=tmax_sol$root, maxvel=firderiv(x=tmax_sol$root,Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
                                                                       K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
                                                                       H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
                                                                       I1=I1, I2=I2, I3=I3, I4=I4)) )
} # maxagevel


vmaxagevel <- Vectorize(maxagevel) # vectorize the function so that it can take vectors as arguments

# x <- 1

# Q1 <- Q1ber[1:x]
# Q2 <- Q2ber[1:x]
# Q3 <- Q3ber[1:x]
# Q4 <- Q4ber[1:x]
# Q5 <- Q5ber[1:x]

# K1 <- K1ber[1:x]
# K2 <- K2ber[1:x]
# K3 <- K3ber[1:x]
# K4 <- K4ber[1:x]
# K5 <- K5ber[1:x]

# H1 <- H1ber[1:x]
# H2 <- H2ber[1:x]
# H3 <- H3ber[1:x]
# H4 <- H4ber[1:x]
# H5 <- H5ber[1:x]

# I1 <- I1ber[1:x]
# I2 <- I2ber[1:x]
# I3 <- I3ber[1:x]
# I4 <- I4ber[1:x]

# unlist(vmaxagevel(Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                   I1=I1, I2=I2, I3=I3, I4=I4)["agemax",])
# unlist(vmaxagevel(Q1=Q1, Q2=Q2, Q3=Q3, Q4=Q4, Q5=Q5,
#                   K1=K1, K2=K2, K3=K3, K4=K4, K5=K5,
#                   H1=H1, H2=H2, H3=H3, H4=H4, H5=H5,
#                   I1=I1, I2=I2, I3=I3, I4=I4)["maxvel",])






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




######## female characteristics

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



mean_char_list_f <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       (2*H1ber/K1ber)^(1/Q1ber) +
                       (2*H2ber/K2ber)^(1/Q2ber) +
                       (2*H3ber/K3ber)^(1/Q3ber) +
                       (2*H4ber/K4ber)^(1/Q4ber) +
                       (2*H5ber/K5ber)^(1/Q5ber),


                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                         K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                         H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                         I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["agemax",]),


                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",]),

                       unlist(vmaxagevel(Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                         K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                         H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                         I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["maxvel",])
                      )


names(mean_char_list_f) <- c(
                          "matMaxHeight",
                          "berMaxHeight",

                          "matMaxVelAge",
                          "berMaxVelAge",

                          "matMaxVel",
                          "berMaxVel"
                        )

#str(mean_char_list_f)



contr_char_list_f <- list(mean_char_list_f$berMaxHeight - mean_char_list_f$matMaxHeight,
                          mean_char_list_f$berMaxVelAge - mean_char_list_f$matMaxVelAge,
                          mean_char_list_f$berMaxVel - mean_char_list_f$matMaxVel)


names(contr_char_list_f) <- c(
                          "ber.matMaxHeight",
                          "ber.matMaxVelAge",
                          "ber.matMaxVel"
                        )

#str(contr_char_list_f)




######## male characteristics

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



mean_char_list_m <- list(
                       (2*H1mat/K1mat)^(1/Q1mat) +
                       (2*H2mat/K2mat)^(1/Q2mat) +
                       (2*H3mat/K3mat)^(1/Q3mat) +
                       (2*H4mat/K4mat)^(1/Q4mat) +
                       (2*H5mat/K5mat)^(1/Q5mat),

                       (2*H1ber/K1ber)^(1/Q1ber) +
                       (2*H2ber/K2ber)^(1/Q2ber) +
                       (2*H3ber/K3ber)^(1/Q3ber) +
                       (2*H4ber/K4ber)^(1/Q4ber) +
                       (2*H5ber/K5ber)^(1/Q5ber),


                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["agemax",]),

                       unlist(vmaxagevel(Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                         K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                         H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                         I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["agemax",]),


                       unlist(vmaxagevel(Q1=Q1mat, Q2=Q2mat, Q3=Q3mat, Q4=Q4mat, Q5=Q5mat,
                                         K1=K1mat, K2=K2mat, K3=K3mat, K4=K4mat, K5=K5mat,
                                         H1=H1mat, H2=H2mat, H3=H3mat, H4=H4mat, H5=H5mat,
                                         I1=I1mat, I2=I2mat, I3=I3mat, I4=I4mat)["maxvel",]),

                       unlist(vmaxagevel(Q1=Q1ber, Q2=Q2ber, Q3=Q3ber, Q4=Q4ber, Q5=Q5ber,
                                         K1=K1ber, K2=K2ber, K3=K3ber, K4=K4ber, K5=K5ber,
                                         H1=H1ber, H2=H2ber, H3=H3ber, H4=H4ber, H5=H5ber,
                                         I1=I1ber, I2=I2ber, I3=I3ber, I4=I4ber)["maxvel",])
                      )


names(mean_char_list_m) <- c(
                          "matMaxHeight",
                          "berMaxHeight",

                          "matMaxVelAge",
                          "berMaxVelAge",

                          "matMaxVel",
                          "berMaxVel"
                        )

#str(mean_char_list_m)



contr_char_list_m <- list(mean_char_list_m$berMaxHeight - mean_char_list_m$matMaxHeight,
                          mean_char_list_m$berMaxVelAge - mean_char_list_m$matMaxVelAge,
                          mean_char_list_m$berMaxVel - mean_char_list_m$matMaxVel)


names(contr_char_list_m) <- c(
                          "ber.matMaxHeight",
                          "ber.matMaxVelAge",
                          "ber.matMaxVel"
                        )

#str(contr_char_list_m)







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





pdf(file="./combined_plots/Plots/Chars_combined.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 2, 3, 0, 0, 0, 0, 0, 0, 4, 5, 6, 0,0,0,0,0,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                        7, 8, 9, 0, 0, 0, 0, 0, 0, 10,11,12,0,0,0,0,0,0,
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
mean_char_list <- mean_char_list_f
contr_char_list <- contr_char_list_f


MaxHeight_post_plot <- denschart3( mean_char_list[c(1,2)],
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
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxHeight ), mean( mean_char_list$matMaxHeight ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxHeight ), mean( mean_char_list$berMaxHeight ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
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

mtext("Max height",           side = 1, outer = T, cex = 2, adj=-0.17, line = -103 )
mtext("(cm)",                 side = 1, outer = T, cex = 2, adj=-0.13, line = -101 )

mtext("Age at max velocity",  side = 1, outer = T, cex = 2, adj=-0.23, line = -90.5 )
mtext("(years)",              side = 1, outer = T, cex = 2, adj=-0.14, line = -88.5 )

mtext("Max velocity",         side = 1, outer = T, cex = 2, adj=-0.17, line = -78 )
mtext("(cm/year)",            side = 1, outer = T, cex = 2, adj=-0.15, line = -76 )



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




MaxVelAge_post_plot <- denschart3( mean_char_list[c(3,4)],
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
            #xlim=range( mean( mean_char_list$matMaxVelAge ) - 0.5, mean( mean_char_list$berMaxVelAge ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVelAge ), mean( mean_char_list$matMaxVelAge ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVelAge ), mean( mean_char_list$berMaxVelAge ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
           xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),
     labels=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
               xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),     
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)




MaxVel_post_plot <- denschart3( mean_char_list[c(5,6)],
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
            #xlim=range( mean( mean_char_list$matMaxVel ) - 0.5, mean( mean_char_list$berMaxVel ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVel ), mean( mean_char_list$matMaxVel ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVel ), mean( mean_char_list$berMaxVel ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
           xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
                xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)



### parameter contrasts


MaxWeight_cont_plot <- denschart3( contr_char_list[1],
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
            #xlim=range(-20,20),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[1]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[1]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxHeight), mean(contr_char_list$ber.matMaxHeight) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    cex.axis=cex_axis)



MaxVelAge_cont_plot <- denschart3( contr_char_list[2],
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
            #xlim=range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[2]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[2]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVelAge), mean(contr_char_list$ber.matMaxVelAge) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    cex.axis=cex_axis)



MaxVel_cont_plot <- denschart3( contr_char_list[3],
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
            #xlim=range(-1.5,1.5),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[3]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[3]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVel), mean(contr_char_list$ber.matMaxVel) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    cex.axis=cex_axis)





# male ###########################################################################################################################################################################

post <- post3_m
mean_char_list <- mean_char_list_m
contr_char_list <- contr_char_list_m


MaxHeight_post_plot <- denschart3( mean_char_list[c(1,2)],
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
            #xlim=range( mean( mean_char_list$matMaxHeight ) - 5, mean( mean_char_list$berMaxHeight ) + 5 ), #( 85, 100 ),
            xlim= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxHeight ), mean( mean_char_list$matMaxHeight ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxHeight ), mean( mean_char_list$berMaxHeight ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
           xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Llab,
                xrange( D1=unlist(mean_char_list[1]),D2=unlist(mean_char_list[2]) )$Rlab ),
     #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)



par(xpd=FALSE)


MaxVelAge_post_plot <- denschart3( mean_char_list[c(3,4)],
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
            #xlim=range( mean( mean_char_list$matMaxVelAge ) - 0.5, mean( mean_char_list$berMaxVelAge ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVelAge ), mean( mean_char_list$matMaxVelAge ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVelAge ), mean( mean_char_list$berMaxVelAge ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
           xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),
     labels=c( xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Llab,
               xrange( D1=unlist(mean_char_list[3]),D2=unlist(mean_char_list[4]) )$Rlab ),     
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)




MaxVel_post_plot <- denschart3( mean_char_list[c(5,6)],
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
            #xlim=range( mean( mean_char_list$matMaxVel ) - 0.5, mean( mean_char_list$berMaxVel ) + 0.5),
            xlim= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Lbound,
                     xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rbound ),
            yvals = c(0.5,
                      0.5)
 )


lines(x=c( mean( mean_char_list$matMaxVel ), mean( mean_char_list$matMaxVel ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( mean_char_list$berMaxVel ), mean( mean_char_list$berMaxVel ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)


axis(side=1,
     at=c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
           xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     labels= c( xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Llab,
                xrange( D1=unlist(mean_char_list[5]),D2=unlist(mean_char_list[6]) )$Rlab ),
     #c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)



### parameter contrasts


MaxWeight_cont_plot <- denschart3( contr_char_list[1],
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
            #xlim=range(-20,20),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[1]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[1]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxHeight), mean(contr_char_list$ber.matMaxHeight) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[1]) )$Llab, xrangecon( D1=unlist(contr_char_list[1]) )$Rlab),
    cex.axis=cex_axis)



MaxVelAge_cont_plot <- denschart3( contr_char_list[2],
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
            #xlim=range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[2]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[2]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVelAge), mean(contr_char_list$ber.matMaxVelAge) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[2]) )$Llab, xrangecon( D1=unlist(contr_char_list[2]) )$Rlab),
    cex.axis=cex_axis)



MaxVel_cont_plot <- denschart3( contr_char_list[3],
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
            #xlim=range(-1.5,1.5),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            xlim= range(xrangecon( D1=unlist(contr_char_list[3]) )$Lbound,
                        xrangecon( D1=unlist(contr_char_list[3]) )$Rbound ),
            yvals = 0.5
 )


lines(x=c( mean(contr_char_list$ber.matMaxVel), mean(contr_char_list$ber.matMaxVel) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    labels=c(xrangecon( D1=unlist(contr_char_list[3]) )$Llab, xrangecon( D1=unlist(contr_char_list[3]) )$Rlab),
    cex.axis=cex_axis)




graphics.off()











