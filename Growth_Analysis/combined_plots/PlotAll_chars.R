

######################################### Fig 4 #########################################


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




####### function to numerically solve to find age at max velocity and max velocity for Weight
maxagevel <- function(Q=1, K=1, H=1){
    D <- 1
    C <- -1

    secderiv <- function(t, Q=1, K=1, H=1){
        ( K*C*exp(( -2*K*Q*t )/( 2*Q + 1 )) )/D*
        ( ( 2*H*Q*exp( ( K*Q*t )/( 2*Q + 1 ) )*(D*pi)^( Q/( 2*Q + 1 ) ) )/( 2*Q + 1 ) + C*D*K )*
        ( C*exp(( -1*K*Q*t )/( 2*Q + 1 )) + 2*H/(D*K)*(D*pi)^( Q/( 2*Q + 1 ) ) )^(1/Q)
    } # secderiv
    tmax_sol <- uniroot( secderiv, Q=Q, K=K, H=H, lower=0, upper=21 )   # root between t=0 and t=21

    firderiv <- function(t=tmax_sol$root, Q=1, K=1, H=1){
        -1*K*C*exp(( -1*K*Q*t )/( 2*Q + 1 ))*
        ( C*exp(( -1*K*Q*t )/( 2*Q + 1 )) + 2*H/(D*K)*(D*pi)^( Q/( 2*Q + 1 ) ) )^( ( Q + 1 )/Q )
    } # firderiv

    return( list(agemax=tmax_sol$root, maxvel=firderiv(tmax_sol$root,Q,K,H)) )
} # maxagevel

vmaxagevel <- Vectorize(maxagevel) # vectorize the function so that it can take vectors as arguments

# Q <- Q1ber[1:2]
# K <- K1ber[1:2]
# H <- H1ber[1:2]
# unlist(vmaxagevel(Q=Q, K=K, H=H)["agemax",])
# unlist(vmaxagevel(Q=Q, K=K, H=H)["maxvel",])



######## function to numerically solve to find age at max velocity and max velocity for Height
maxagevelh <- function(Q=1, K=1, H=1){
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

vmaxagevelh <- Vectorize(maxagevelh) # vectorize the function so that it can take vectors as arguments

# Q <- Q3ber[1:2]
# K <- K3ber[1:2]
# H <- H3ber[1:2]
# unlist(vmaxagevelh(Q=Q, K=K, H=H)["agemax",])
# unlist(vmaxagevelh(Q=Q, K=K, H=H)["maxvel",])




########## create lists of characteristic posteriors

# male height
post <- post3_mh

Q1mat_mh <- exp(post$matLQ1)
Q1ber_mh <- exp(post$berLQ1)

Q2mat_mh <- exp(post$matLQ2)
Q2ber_mh <- exp(post$berLQ2)

Q3mat_mh <- exp(post$matLQ3)
Q3ber_mh <- exp(post$berLQ3)

K1mat_mh <- exp(post$matLK1)
K1ber_mh <- exp(post$berLK1)

K2mat_mh <- exp(post$matLK2)
K2ber_mh <- exp(post$berLK2)

K3mat_mh <- exp(post$matLK3)
K3ber_mh <- exp(post$berLK3)

H1mat_mh <- exp(post$matLK1)/2 + post$matR1
H1ber_mh <- exp(post$berLK1)/2 + post$berR1

H2mat_mh <- exp(post$matLK2)/2 + post$matR2
H2ber_mh <- exp(post$berLK2)/2 + post$berR2

H3mat_mh <- exp(post$matLK3)/2 + post$matR3
H3ber_mh <- exp(post$berLK3)/2 + post$berR3


Q1mat <- Q1mat_mh
Q1ber <- Q1ber_mh

Q2mat <- Q2mat_mh
Q2ber <- Q2ber_mh

Q3mat <- Q3mat_mh
Q3ber <- Q3ber_mh

K1mat <- K1mat_mh
K1ber <- K1ber_mh

K2mat <- K2mat_mh
K2ber <- K2ber_mh

K3mat <- K3mat_mh
K3ber <- K3ber_mh

H1mat <- H1mat_mh
H1ber <- H1ber_mh

H2mat <- H2mat_mh
H2ber <- H2ber_mh

H3mat <- H3mat_mh
H3ber <- H3ber_mh


mean_char_list_mh <- list(
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

                       unlist(vmaxagevelh(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]),
                       unlist(vmaxagevelh(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",])
                      )


names(mean_char_list_mh) <- c(
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

#str(mean_char_list_mh)



contr_char_list_mh <- list(
                       (2*H1ber/K1ber)^(1/Q1ber) - (2*H1mat/K1mat)^(1/Q1mat),
                       ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) - ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat),
                       (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) - (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2),
                       
                       (2*H2ber/K2ber)^(1/Q2ber) - (2*H2mat/K2mat)^(1/Q2mat),
                       ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) - ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat),
                       (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) - (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2),
                       
                       (2*H3ber/K3ber)^(1/Q3ber) - (2*H3mat/K3mat)^(1/Q3mat),
                       ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) - ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat),
                       #(2^(1/Q3ber)*K3ber*Q3ber*((H3ber - H3ber*Q3ber)/K3ber)^(1/Q3ber))/(1 + Q3ber + 2*Q3ber^2) - (2^(1/Q3mat)*K3mat*Q3mat*((H3mat - H3mat*Q3mat)/K3mat)^(1/Q3mat))/(1 + Q3mat + 2*Q3mat^2),
                       unlist(vmaxagevelh(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) - unlist(vmaxagevelh(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",])
                      )


names(contr_char_list_mh) <- c(
                          "ber.matMaxHeight1",
                          "ber.matMaxVelAge1",
                          "ber.matMaxVel1",

                          "ber.matMaxHeight2",
                          "ber.matMaxVelAge2",
                          "ber.matMaxVel2",

                          "ber.matMaxHeight3",
                          "ber.matMaxVelAge3",
                          "ber.matMaxVel3"
                        )

#str(contr_char_list_mh)




# female height
post <- post3_fh

Q1mat_fh <- exp(post$matLQ1)
Q1ber_fh <- exp(post$berLQ1)

Q2mat_fh <- exp(post$matLQ2)
Q2ber_fh <- exp(post$berLQ2)

Q3mat_fh <- exp(post$matLQ3)
Q3ber_fh <- exp(post$berLQ3)

K1mat_fh <- exp(post$matLK1)
K1ber_fh <- exp(post$berLK1)

K2mat_fh <- exp(post$matLK2)
K2ber_fh <- exp(post$berLK2)

K3mat_fh <- exp(post$matLK3)
K3ber_fh <- exp(post$berLK3)

H1mat_fh <- exp(post$matLK1)/2 + post$matR1
H1ber_fh <- exp(post$berLK1)/2 + post$berR1

H2mat_fh <- exp(post$matLK2)/2 + post$matR2
H2ber_fh <- exp(post$berLK2)/2 + post$berR2

H3mat_fh <- exp(post$matLK3)/2 + post$matR3
H3ber_fh <- exp(post$berLK3)/2 + post$berR3

Q1mat <- Q1mat_fh
Q1ber <- Q1ber_fh

Q2mat <- Q2mat_fh
Q2ber <- Q2ber_fh

Q3mat <- Q3mat_fh
Q3ber <- Q3ber_fh

K1mat <- K1mat_fh
K1ber <- K1ber_fh

K2mat <- K2mat_fh
K2ber <- K2ber_fh

K3mat <- K3mat_fh
K3ber <- K3ber_fh

H1mat <- H1mat_fh
H1ber <- H1ber_fh

H2mat <- H2mat_fh
H2ber <- H2ber_fh

H3mat <- H3mat_fh
H3ber <- H3ber_fh


mean_char_list_fh <- list(
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

                       unlist(vmaxagevelh(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]),
                       unlist(vmaxagevelh(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",])
                      )


names(mean_char_list_fh) <- c(
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

#str(mean_char_list_fh)



contr_char_list_fh <- list(
                       (2*H1ber/K1ber)^(1/Q1ber) - (2*H1mat/K1mat)^(1/Q1mat),
                       ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) - ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat),
                       (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) - (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2),
                       
                       (2*H2ber/K2ber)^(1/Q2ber) - (2*H2mat/K2mat)^(1/Q2mat),
                       ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) - ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat),
                       (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) - (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2),
                       
                       (2*H3ber/K3ber)^(1/Q3ber) - (2*H3mat/K3mat)^(1/Q3mat),
                       ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) - ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat),
                       #(2^(1/Q3ber)*K3ber*Q3ber*((H3ber - H3ber*Q3ber)/K3ber)^(1/Q3ber))/(1 + Q3ber + 2*Q3ber^2) - (2^(1/Q3mat)*K3mat*Q3mat*((H3mat - H3mat*Q3mat)/K3mat)^(1/Q3mat))/(1 + Q3mat + 2*Q3mat^2),
                       unlist(vmaxagevelh(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) - unlist(vmaxagevelh(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",])
                      )


names(contr_char_list_fh) <- c(
                          "ber.matMaxHeight1",
                          "ber.matMaxVelAge1",
                          "ber.matMaxVel1",

                          "ber.matMaxHeight2",
                          "ber.matMaxVelAge2",
                          "ber.matMaxVel2",

                          "ber.matMaxHeight3",
                          "ber.matMaxVelAge3",
                          "ber.matMaxVel3"
                        )

#str(contr_char_list_fh)




# male weight
post <- post3_mw

Q1mat_mw <- exp(post$matLQ1)
Q1ber_mw <- exp(post$berLQ1)

Q2mat_mw <- exp(post$matLQ2)
Q2ber_mw <- exp(post$berLQ2)

Q3mat_mw <- exp(post$matLQ3)
Q3ber_mw <- exp(post$berLQ3)

K1mat_mw <- exp(post$matLK1)
K1ber_mw <- exp(post$berLK1)

K2mat_mw <- exp(post$matLK2)
K2ber_mw <- exp(post$berLK2)

K3mat_mw <- exp(post$matLK3)
K3ber_mw <- exp(post$berLK3)

H1mat_mw <- exp(post$matLK1)/2 + post$matR1
H1ber_mw <- exp(post$berLK1)/2 + post$berR1

H2mat_mw <- exp(post$matLK2)/2 + post$matR2
H2ber_mw <- exp(post$berLK2)/2 + post$berR2

H3mat_mw <- exp(post$matLK3)/2 + post$matR3
H3ber_mw <- exp(post$berLK3)/2 + post$berR3


Q1mat <- Q1mat_mw
Q1ber <- Q1ber_mw

Q2mat <- Q2mat_mw
Q2ber <- Q2ber_mw

Q3mat <- Q3mat_mw
Q3ber <- Q3ber_mw

K1mat <- K1mat_mw
K1ber <- K1ber_mw

K2mat <- K2mat_mw
K2ber <- K2ber_mw

K3mat <- K3mat_mw
K3ber <- K3ber_mw

H1mat <- H1mat_mw
H1ber <- H1ber_mw

H2mat <- H2mat_mw
H2ber <- H2ber_mw

H3mat <- H3mat_mw
H3ber <- H3ber_mw


mean_char_list_mw <- list(
                       pi*(2*H1mat/K1mat)^( ( 2*Q1mat + 1 )/Q1mat ),
                       pi*(2*H1ber/K1ber)^( ( 2*Q1ber + 1 )/Q1ber ),

                       unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["agemax",]),

                       unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["maxvel",]),
                       unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["maxvel",]),

                       pi*(2*H2mat/K2mat)^( ( 2*Q2mat + 1 )/Q2mat ),
                       pi*(2*H2ber/K2ber)^( ( 2*Q2ber + 1 )/Q2ber ),

                       unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["agemax",]),

                       unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["maxvel",]),
                       unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["maxvel",]),

                       pi*(2*H3mat/K3mat)^( ( 2*Q3mat + 1 )/Q3mat ),
                       pi*(2*H3ber/K3ber)^( ( 2*Q3ber + 1 )/Q3ber ),

                       unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["agemax",]),

                       unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]),
                       unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",])
                      )


names(mean_char_list_mw) <- c(
                          "matMaxWeight1",
                          "berMaxWeight1",

                          "matMaxVelAge1",
                          "berMaxVelAge1",

                          "matMaxVel1",
                          "berMaxVel1",

                          "matMaxWeight2",
                          "berMaxWeight2",

                          "matMaxVelAge2",
                          "berMaxVelAge2",

                          "matMaxVel2",
                          "berMaxVel2",

                          "matMaxWeight3",
                          "berMaxWeight3",

                          "matMaxVelAge3",
                          "berMaxVelAge3",

                          "matMaxVel3",
                          "berMaxVel3"
                        )

#str(mean_char_list_mw)


contr_char_list_mw <- list(
                       pi*(2*H1ber/K1ber)^( ( 2*Q1ber + 1 )/Q1ber ) - pi*(2*H1mat/K1mat)^( ( 2*Q1mat + 1 )/Q1mat ),
                       unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["agemax",]) - unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["maxvel",]) - unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["maxvel",]),

                       pi*(2*H2ber/K2ber)^( ( 2*Q2ber + 1 )/Q2ber ) - pi*(2*H2mat/K2mat)^( ( 2*Q2mat + 1 )/Q2mat ),
                       unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["agemax",]) - unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["maxvel",]) - unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["maxvel",]),

                       pi*(2*H3ber/K3ber)^( ( 2*Q3ber + 1 )/Q3ber ) - pi*(2*H3mat/K3mat)^( ( 2*Q3mat + 1 )/Q3mat ),
                       unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["agemax",]) - unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) - unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",])
                      )


names(contr_char_list_mw) <- c(
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

#str(contr_char_list_mw)





# female weight
post <- post3_fw

Q1mat_fw <- exp(post$matLQ1)
Q1ber_fw <- exp(post$berLQ1)

Q2mat_fw <- exp(post$matLQ2)
Q2ber_fw <- exp(post$berLQ2)

Q3mat_fw <- exp(post$matLQ3)
Q3ber_fw <- exp(post$berLQ3)

K1mat_fw <- exp(post$matLK1)
K1ber_fw <- exp(post$berLK1)

K2mat_fw <- exp(post$matLK2)
K2ber_fw <- exp(post$berLK2)

K3mat_fw <- exp(post$matLK3)
K3ber_fw <- exp(post$berLK3)

H1mat_fw <- exp(post$matLK1)/2 + post$matR1
H1ber_fw <- exp(post$berLK1)/2 + post$berR1

H2mat_fw <- exp(post$matLK2)/2 + post$matR2
H2ber_fw <- exp(post$berLK2)/2 + post$berR2

H3mat_fw <- exp(post$matLK3)/2 + post$matR3
H3ber_fw <- exp(post$berLK3)/2 + post$berR3


Q1mat <- Q1mat_fw
Q1ber <- Q1ber_fw

Q2mat <- Q2mat_fw
Q2ber <- Q2ber_fw

Q3mat <- Q3mat_fw
Q3ber <- Q3ber_fw

K1mat <- K1mat_fw
K1ber <- K1ber_fw

K2mat <- K2mat_fw
K2ber <- K2ber_fw

K3mat <- K3mat_fw
K3ber <- K3ber_fw

H1mat <- H1mat_fw
H1ber <- H1ber_fw

H2mat <- H2mat_fw
H2ber <- H2ber_fw

H3mat <- H3mat_fw
H3ber <- H3ber_fw

mean_char_list_fw <- list(
                       pi*(2*H1mat/K1mat)^( ( 2*Q1mat + 1 )/Q1mat ),
                       pi*(2*H1ber/K1ber)^( ( 2*Q1ber + 1 )/Q1ber ),
                       
                       unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["agemax",]),
                       
                       unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["maxvel",]),
                       unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["maxvel",]),

                       pi*(2*H2mat/K2mat)^( ( 2*Q2mat + 1 )/Q2mat ),
                       pi*(2*H2ber/K2ber)^( ( 2*Q2ber + 1 )/Q2ber ),

                       unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["agemax",]),

                       unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["maxvel",]),
                       unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["maxvel",]),

                       pi*(2*H3mat/K3mat)^( ( 2*Q3mat + 1 )/Q3mat ),
                       pi*(2*H3ber/K3ber)^( ( 2*Q3ber + 1 )/Q3ber ),

                       unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["agemax",]),

                       unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]),
                       unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",])
                      )


names(mean_char_list_fw) <- c(
                          "matMaxWeight1",
                          "berMaxWeight1",

                          "matMaxVelAge1",
                          "berMaxVelAge1",

                          "matMaxVel1",
                          "berMaxVel1",

                          "matMaxWeight2",
                          "berMaxWeight2",

                          "matMaxVelAge2",
                          "berMaxVelAge2",

                          "matMaxVel2",
                          "berMaxVel2",

                          "matMaxWeight3",
                          "berMaxWeight3",

                          "matMaxVelAge3",
                          "berMaxVelAge3",

                          "matMaxVel3",
                          "berMaxVel3"
                        )

#str(mean_char_list_fw)


contr_char_list_fw <- list(
                       pi*(2*H1ber/K1ber)^( ( 2*Q1ber + 1 )/Q1ber ) - pi*(2*H1mat/K1mat)^( ( 2*Q1mat + 1 )/Q1mat ),
                       unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["agemax",]) - unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["maxvel",]) - unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["maxvel",]),

                       pi*(2*H2ber/K2ber)^( ( 2*Q2ber + 1 )/Q2ber ) - pi*(2*H2mat/K2mat)^( ( 2*Q2mat + 1 )/Q2mat ),
                       unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["agemax",]) - unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["maxvel",]) - unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["maxvel",]),

                       pi*(2*H3ber/K3ber)^( ( 2*Q3ber + 1 )/Q3ber ) - pi*(2*H3mat/K3mat)^( ( 2*Q3mat + 1 )/Q3mat ),
                       unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["agemax",]) - unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["agemax",]),
                       unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) - unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",])
                      )


names(contr_char_list_fw) <- c(
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

#str(contr_char_list_fw_fw)






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





pdf(file="./combined_plots/Plots/Chars_combined.pdf",
    height=20, width=20)
layout( matrix(data=c(  1, 2, 3, 4, 5, 6, 7, 8, 0, 9,10,11,12,13,14,15,16,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                       17,18,19,20,21,22,23,24, 0,25,26,27,28,29,30,31,32,0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                       33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
                        0, 0, 0, 0, 0, 0, 0, 0, 0,
                       51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68
                      ),
        nrow=9, ncol=11, byrow = FALSE),
        heights=rep(1,times=9),
        widths=c(1,1, 0.2, 1,1, 0.2, 1,1, 0.2, 1,1)
      )
par(mar = c(2, 1, 2, 0.5), oma = c(6, 20, 6, 4)) #margins for indiv plot, oma for outer margins (bottom, left, top, right)

cex_axis <- 1.75 # size of x-axis labels

# female height ###########################################################################################################################################################################

post <- post3_fh
mean_char_list <- mean_char_list_fh
contr_char_list <- contr_char_list_fh

Q1mat <- Q1mat_fh
Q1ber <- Q1ber_fh

Q2mat <- Q2mat_fh
Q2ber <- Q2ber_fh

Q3mat <- Q3mat_fh
Q3ber <- Q3ber_fh

K1mat <- K1mat_fh
K1ber <- K1ber_fh

K2mat <- K2mat_fh
K2ber <- K2ber_fh

K3mat <- K3mat_fh
K3ber <- K3ber_fh

H1mat <- H1mat_fh
H1ber <- H1ber_fh

H2mat <- H2mat_fh
H2ber <- H2ber_fh

H3mat <- H3mat_fh
H3ber <- H3ber_fh


MaxHeight1_post_plot <- denschart3( mean_char_list[c(1,2)],
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
            xlim=range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 5, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 5 ), #( 64, 82 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2*H1mat/K1mat)^(1/Q1mat) ), mean( (2*H1mat/K1mat)^(1/Q1mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( (2*H1ber/K1ber)^(1/Q1ber) ), mean( (2*H1ber/K1ber)^(1/Q1ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
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

mtext("Max height or weight", side = 1, outer = T, cex = 2, adj=-0.24, line = -103 )
mtext("(cm or kg)",           side = 1, outer = T, cex = 2, adj=-0.16, line = -101 )

mtext("Age at max velocity",  side = 1, outer = T, cex = 2, adj=-0.23, line = -90.5 )
mtext("(years)",              side = 1, outer = T, cex = 2, adj=-0.14, line = -88.5 )

mtext("Max velocity",         side = 1, outer = T, cex = 2, adj=-0.17, line = -78 )
mtext("(cm/yr or kg/yr)",     side = 1, outer = T, cex = 2, adj=-0.19, line = -76 )

mtext("Max height or weight", side = 1, outer = T, cex = 2, adj=-0.24, line = -65.5 )
mtext("(cm or kg)",           side = 1, outer = T, cex = 2, adj=-0.16, line = -63.5 )

mtext("Age at max velocity",  side = 1, outer = T, cex = 2, adj=-0.23, line = -53 )
mtext("(years)",              side = 1, outer = T, cex = 2, adj=-0.14, line = -51 )

mtext("Max velocity",         side = 1, outer = T, cex = 2, adj=-0.17, line = -40.5 )
mtext("(cm/yr or kg/yr)",     side = 1, outer = T, cex = 2, adj=-0.19, line = -38.5 )

mtext("Max height or weight", side = 1, outer = T, cex = 2, adj=-0.24, line = -28 )
mtext("(cm or kg)",           side = 1, outer = T, cex = 2, adj=-0.16, line = -26 )

mtext("Age at max velocity",  side = 1, outer = T, cex = 2, adj=-0.23, line = -15.5 )
mtext("(years)",              side = 1, outer = T, cex = 2, adj=-0.14, line = -13.5 )

mtext("Max velocity",         side = 1, outer = T, cex = 2, adj=-0.17, line = -3 )
mtext("(kg/yr)",              side = 1, outer = T, cex = 2, adj=-0.14, line = -1 )



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




par(xpd=FALSE)


MaxVelAge1_post_plot <- denschart3( mean_char_list[c(3,4)],
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
            xlim=range( mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ) - 0.04, mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ) + 0.04), #( 0.85, 0.95 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ), mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ), mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)


MaxVel1_post_plot <- denschart3( mean_char_list[c(5,6)],
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
            xlim=range( mean( (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2) ) - 5, mean( (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) ) + 5), #( 38, 46 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2) ), mean( (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) ), mean( (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(95,100,105), labels=c(95,100,105),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)


MaxHeight2_post_plot <- denschart3( mean_char_list[c(7,8)],
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
            xlim=range( mean( (2*H2mat/K2mat)^(1/Q2mat) ) - 3, mean( (2*H2ber/K2ber)^(1/Q2ber) ) + 3), #( 45, 65 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2*H2mat/K2mat)^(1/Q2mat) ), mean( (2*H2mat/K2mat)^(1/Q2mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( (2*H2ber/K2ber)^(1/Q2ber) ), mean( (2*H2ber/K2ber)^(1/Q2ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(50,60,70), labels=c(50,60,70),
    cex.axis=cex_axis)
#mtext("Child max height (cm)", side = 1, outer = T, cex = 1.5, line = -38.5, adj=0.5)


MaxVelAge2_post_plot <- denschart3( mean_char_list[c(9,10)],
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
            xlim=range( mean( ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) ) - 0.15, mean( ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat) ) + 0.15), #( 6.5, 7.5 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat) ), mean( ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) ), mean( ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(4.5,5), labels=c(4.5,5),
    cex.axis=cex_axis)
#mtext("Child age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = -28.5, adj=0.5)


MaxVel2_post_plot <- denschart3( mean_char_list[c(11,12)],
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
            xlim=range( mean( (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2) ) - 0.3, mean( (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) ) + 0.3), #( 5, 7 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2) ), mean( (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) ), mean( (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(8,9), labels=c(8,9),
    cex.axis=cex_axis)
#mtext("Child max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -18.5, adj=0.5)



MaxHeight3_post_plot <- denschart3( mean_char_list[c(13,14)],
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
            xlim=range( mean( (2*H3ber/K3ber)^(1/Q3ber) ) - 3, mean( (2*H3mat/K3mat)^(1/Q3mat) ) + 3), #( 15, 25 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2*H3mat/K3mat)^(1/Q3mat) ), mean( (2*H3mat/K3mat)^(1/Q3mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( (2*H3ber/K3ber)^(1/Q3ber) ), mean( (2*H3ber/K3ber)^(1/Q3ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(30,35), labels=c(30,35),
    cex.axis=cex_axis)
#mtext("Adolescent max height (cm)", side = 1, outer = T, cex = 1.5, line = -9, adj=0.5)


MaxVelAge3_post_plot <- denschart3( mean_char_list[c(15,16)],
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
            xlim=range( mean( ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat) ) - 0.2, mean( ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) ) + 0.2 ), #( 13, 16 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat) ), mean( ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) ), mean( ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(13,14), labels=c(13,14),
    cex.axis=cex_axis)
#mtext("Adolescent age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)


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
#       y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
# lines(x=c( mean( (2^(1/Q3ber)*K3ber*Q3ber*((H3ber - H3ber*Q3ber)/K3ber)^(1/Q3ber))/(1 + Q3ber + 2*Q3ber^2) ), mean( (2^(1/Q3ber)*K3ber*Q3ber*((H3ber - H3ber*Q3ber)/K3ber)^(1/Q3ber))/(1 + Q3ber + 2*Q3ber^2) ) ),
#       y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

# axis(side=1, at=c(2,4,6,8), labels=c(2,4,6,8), cex.axis=cex_axis)
# mtext("Adolescent Max Velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)



### parameter contrasts


MaxHeight1_cont_plot <- denschart3( contr_char_list[1],
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
            xlim=range(-12,12),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxHeight1), mean(contr_char_list$ber.matMaxHeight1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-8,8), labels=c(-8,8),
    cex.axis=cex_axis)




MaxVelAge1_cont_plot <- denschart3( contr_char_list[2],
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
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge1), mean(contr_char_list$ber.matMaxVelAge1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.03,0.03), labels=c(-0.03,0.03),
    cex.axis=cex_axis)


MaxVel1_cont_plot <- denschart3( contr_char_list[3],
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
            xlim=range(-15,15),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel1), mean(contr_char_list$ber.matMaxVel1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-10,10), labels=c(-10,10),
    cex.axis=cex_axis)


MaxHeight2_cont_plot <- denschart3( contr_char_list[4],
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
            xlim=range(-7,7),
                #range( min(contr_char_list$ber.matHaxWeight2), max(contr_char_list$ber.matMaxHeight2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxHeight2), mean(contr_char_list$ber.matMaxHeight2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-5,5), labels=c(-5,5),
    cex.axis=cex_axis)


MaxVelAge2_cont_plot <- denschart3( contr_char_list[5],
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
            xlim=range(-0.15,0.15),
                #range( min(contr_char_list$ber.matMaxVelAge2), max(contr_char_list$ber.matMaxVelAge2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge2), mean(contr_char_list$ber.matMaxVelAge2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.1,0.1), labels=c(-0.1,0.1),
    cex.axis=cex_axis)


MaxVel2_cont_plot <- denschart3( contr_char_list[6],
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
            xlim=range(-1,1),
                #range( min(contr_char_list$ber.matMaxVel2), max(contr_char_list$ber.matMaxVel2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel2), mean(contr_char_list$ber.matMaxVel2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.7,0.7), labels=c(-0.7,0.7),
    cex.axis=cex_axis)


MaxHeight3_cont_plot <- denschart3( contr_char_list[7],
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
            xlim=range(-3,3),
                #range( min(contr_char_list$ber.matMaxHeight3), max(contr_char_list$ber.matMaxHeight3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxHeight3), mean(contr_char_list$ber.matMaxHeight3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-2,2), labels=c(-2,2),
    cex.axis=cex_axis)


MaxVelAge3_cont_plot <- denschart3( contr_char_list[8],
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
            xlim=range(-0.3,0.3),
                #range( min(contr_char_list$ber.matMaxVelAge3), max(contr_char_list$ber.matMaxVelAge3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge3), mean(contr_char_list$ber.matMaxVelAge3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.2,0.2), labels=c(-0.2,0.2),
    cex.axis=cex_axis)


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

# axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=cex_axis)





# male height ###########################################################################################################################################################################

post <- post3_mh
mean_char_list <- mean_char_list_mh
contr_char_list <- contr_char_list_mh

Q1mat <- Q1mat_mh
Q1ber <- Q1ber_mh

Q2mat <- Q2mat_mh
Q2ber <- Q2ber_mh

Q3mat <- Q3mat_mh
Q3ber <- Q3ber_mh

K1mat <- K1mat_mh
K1ber <- K1ber_mh

K2mat <- K2mat_mh
K2ber <- K2ber_mh

K3mat <- K3mat_mh
K3ber <- K3ber_mh

H1mat <- H1mat_mh
H1ber <- H1ber_mh

H2mat <- H2mat_mh
H2ber <- H2ber_mh

H3mat <- H3mat_mh
H3ber <- H3ber_mh

MaxHeight1_post_plot <- denschart3( mean_char_list[c(1,2)],
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
            xlim=range( mean( (2*H1mat/K1mat)^(1/Q1mat) ) - 2, mean( (2*H1ber/K1ber)^(1/Q1ber) ) + 2 ), #( 85, 100 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2*H1mat/K1mat)^(1/Q1mat) ), mean( (2*H1mat/K1mat)^(1/Q1mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( (2*H1ber/K1ber)^(1/Q1ber) ), mean( (2*H1ber/K1ber)^(1/Q1ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(75,85), labels=c(75,85),
    cex.axis=cex_axis)
#mtext("Infant max height (cm)", side = 1, outer = T, cex = 1.5, line = -68, adj=0.5)

par(xpd=FALSE)


MaxVelAge1_post_plot <- denschart3( mean_char_list[c(3,4)],
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
            xlim=range( mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ) - 0.05, mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ) + 0.05), #( 0.85, 0.95 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ), mean( ((1 + 2*Q1mat)*log(K1mat/(2*H1mat*Q1mat)))/(K1mat*Q1mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ), mean( ((1 + 2*Q1ber)*log(K1ber/(2*H1ber*Q1ber)))/(K1ber*Q1ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(0.6,0.7), labels=c(0.6,0.7),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -58, adj=0.5)


MaxVel1_post_plot <- denschart3( mean_char_list[c(5,6)],
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
            xlim=range( mean( (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2) ) - 5, mean( (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) ) + 5), #( 38, 46 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2) ), mean( (2^(1/Q1mat)*K1mat*Q1mat*((H1mat - H1mat*Q1mat)/K1mat)^(1/Q1mat))/(1 + Q1mat + 2*Q1mat^2) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) ), mean( (2^(1/Q1ber)*K1ber*Q1ber*((H1ber - H1ber*Q1ber)/K1ber)^(1/Q1ber))/(1 + Q1ber + 2*Q1ber^2) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(95,100,105), labels=c(95,100,105),
    cex.axis=cex_axis)
#mtext("Infant max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -48, adj=0.5)


MaxHeight2_post_plot <- denschart3( mean_char_list[c(7,8)],
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
            xlim=range( mean( (2*H2mat/K2mat)^(1/Q2mat) ) - 3, mean( (2*H2ber/K2ber)^(1/Q2ber) ) + 3), #( 45, 65 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2*H2mat/K2mat)^(1/Q2mat) ), mean( (2*H2mat/K2mat)^(1/Q2mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( (2*H2ber/K2ber)^(1/Q2ber) ), mean( (2*H2ber/K2ber)^(1/Q2ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(50,60,70), labels=c(50,60,70),
    cex.axis=cex_axis)
#mtext("Child max height (cm)", side = 1, outer = T, cex = 1.5, line = -38.5, adj=0.5)


MaxVelAge2_post_plot <- denschart3( mean_char_list[c(9,10)],
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
            xlim=range( mean( ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) ) - 0.25, mean( ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat) ) + 0.25), #( 6.5, 7.5 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat) ), mean( ((1 + 2*Q2mat)*log(K2mat/(2*H2mat*Q2mat)))/(K2mat*Q2mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) ), mean( ((1 + 2*Q2ber)*log(K2ber/(2*H2ber*Q2ber)))/(K2ber*Q2ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(4.5,5), labels=c(4.5,5),
    cex.axis=cex_axis)
#mtext("Child age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = -28.5, adj=0.5)


MaxVel2_post_plot <- denschart3( mean_char_list[c(11,12)],
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
            xlim=range( mean( (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2) ) - 0.5, mean( (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) ) + 0.5), #( 5, 7 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2) ), mean( (2^(1/Q2mat)*K2mat*Q2mat*((H2mat - H2mat*Q2mat)/K2mat)^(1/Q2mat))/(1 + Q2mat + 2*Q2mat^2) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) ), mean( (2^(1/Q2ber)*K2ber*Q2ber*((H2ber - H2ber*Q2ber)/K2ber)^(1/Q2ber))/(1 + Q2ber + 2*Q2ber^2) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(8,9), labels=c(8,9),
    cex.axis=cex_axis)
#mtext("Child max velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = -18.5, adj=0.5)



MaxHeight3_post_plot <- denschart3( mean_char_list[c(13,14)],
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
            xlim=range( mean( (2*H3ber/K3ber)^(1/Q3ber) ) - 3, mean( (2*H3mat/K3mat)^(1/Q3mat) ) + 3), #( 15, 25 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( (2*H3mat/K3mat)^(1/Q3mat) ), mean( (2*H3mat/K3mat)^(1/Q3mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( (2*H3ber/K3ber)^(1/Q3ber) ), mean( (2*H3ber/K3ber)^(1/Q3ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(30,35), labels=c(30,35),
    cex.axis=cex_axis)
#mtext("Adolescent max height (cm)", side = 1, outer = T, cex = 1.5, line = -9, adj=0.5)


MaxVelAge3_post_plot <- denschart3( mean_char_list[c(15,16)],
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
            xlim=range( mean( ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat) ) - 0.3, mean( ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) ) + 0.3 ), #( 13, 16 ),
            yvals = c(0.5,
                      0.5)
 )

lines(x=c( mean( ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat) ), mean( ((1 + 2*Q3mat)*log(K3mat/(2*H3mat*Q3mat)))/(K3mat*Q3mat) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
lines(x=c( mean( ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) ), mean( ((1 + 2*Q3ber)*log(K3ber/(2*H3ber*Q3ber)))/(K3ber*Q3ber) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

axis(side=1,
    #at=c(13,14), labels=c(13,14),
    cex.axis=cex_axis)
#mtext("Adolescent age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)


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
#       y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)
# lines(x=c( mean( (2^(1/Q3ber)*K3ber*Q3ber*((H3ber - H3ber*Q3ber)/K3ber)^(1/Q3ber))/(1 + Q3ber + 2*Q3ber^2) ), mean( (2^(1/Q3ber)*K3ber*Q3ber*((H3ber - H3ber*Q3ber)/K3ber)^(1/Q3ber))/(1 + Q3ber + 2*Q3ber^2) ) ),
#       y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)

# axis(side=1, at=c(2,4,6,8), labels=c(2,4,6,8), cex.axis=cex_axis)
# mtext("Adolescent Max Velocity (cm/year)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)



### parameter contrasts


MaxHeight1_cont_plot <- denschart3( contr_char_list[1],
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
            xlim=range(-9,9),
                #range( min(contr_char_list$ber.matMaxHeight1), max(contr_char_list$ber.matMaxHeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxHeight1), mean(contr_char_list$ber.matMaxHeight1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6,6), labels=c(-6,6),
    cex.axis=cex_axis)




MaxVelAge1_cont_plot <- denschart3( contr_char_list[2],
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
            xlim=range(-0.06,0.06),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge1), mean(contr_char_list$ber.matMaxVelAge1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.04,0.04), labels=c(-0.04,0.04),
    cex.axis=cex_axis)


MaxVel1_cont_plot <- denschart3( contr_char_list[3],
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
            xlim=range(-9,9),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel1), mean(contr_char_list$ber.matMaxVel1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-5,5), labels=c(-5,5),
    cex.axis=cex_axis)


MaxHeight2_cont_plot <- denschart3( contr_char_list[4],
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
            xlim=range(-13,13),
                #range( min(contr_char_list$ber.matMaxHeight2), max(contr_char_list$ber.matMaxHeight2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxHeight2), mean(contr_char_list$ber.matMaxHeight2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-8,8), labels=c(-8,8),
    cex.axis=cex_axis)


MaxVelAge2_cont_plot <- denschart3( contr_char_list[5],
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
            xlim=range(-0.22,0.22),
                #range( min(contr_char_list$ber.matMaxVelAge2), max(contr_char_list$ber.matMaxVelAge2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge2), mean(contr_char_list$ber.matMaxVelAge2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.1,0.1), labels=c(-0.1,0.1),
    cex.axis=cex_axis)


MaxVel2_cont_plot <- denschart3( contr_char_list[6],
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
            xlim=range(-1.7,1.7),
                #range( min(contr_char_list$ber.matMaxVel2), max(contr_char_list$ber.matMaxVel2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel2), mean(contr_char_list$ber.matMaxVel2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-1,1), labels=c(-1,1),
    cex.axis=cex_axis)


MaxHeight3_cont_plot <- denschart3( contr_char_list[7],
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
            xlim=range(-3,3),
                #range( min(contr_char_list$ber.matMaxHeight3), max(contr_char_list$ber.matMaxHeight3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxHeight3), mean(contr_char_list$ber.matMaxHeight3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-2,2), labels=c(-2,2),
    cex.axis=cex_axis)


MaxVelAge3_cont_plot <- denschart3( contr_char_list[8],
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
            xlim=range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVelAge3), max(contr_char_list$ber.matMaxVelAge3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge3), mean(contr_char_list$ber.matMaxVelAge3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.3,0.3), labels=c(-0.3,0.3),
    cex.axis=cex_axis)


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

# axis(side=1, at=c(-1,0,1), labels=c(-1,0,1), cex.axis=cex_axis)






# female weight ###########################################################################################################################################################################
post <- post3_fw
mean_char_list <- mean_char_list_fw
contr_char_list <- contr_char_list_fw

Q1mat <- Q1mat_fw
Q1ber <- Q1ber_fw

Q2mat <- Q2mat_fw
Q2ber <- Q2ber_fw

Q3mat <- Q3mat_fw
Q3ber <- Q3ber_fw

K1mat <- K1mat_fw
K1ber <- K1ber_fw

K2mat <- K2mat_fw
K2ber <- K2ber_fw

K3mat <- K3mat_fw
K3ber <- K3ber_fw

H1mat <- H1mat_fw
H1ber <- H1ber_fw

H2mat <- H2mat_fw
H2ber <- H2ber_fw

H3mat <- H3mat_fw
H3ber <- H3ber_fw


MaxWeight1_post_plot <- denschart3( mean_char_list[c(1,2)],
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
            xlim=range( mean( pi*(2*H1mat/K1mat)^( ( 2*Q1mat + 1 )/Q1mat ) ) - 0.3, mean( pi*(2*H1ber/K1ber)^( ( 2*Q1ber + 1 )/Q1ber ) ) + 0.3 ), #( 85, 100 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean( pi*(2*H1ber/K1ber)^( ( 2*Q1ber + 1 )/Q1ber ) ), mean( pi*(2*H1ber/K1ber)^( ( 2*Q1ber + 1 )/Q1ber ) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( pi*(2*H1mat/K1mat)^( ( 2*Q1mat + 1 )/Q1mat ) ), mean( pi*(2*H1mat/K1mat)^( ( 2*Q1mat + 1 )/Q1mat ) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(7,8), labels=c(7,8),
    cex.axis=cex_axis)
#mtext("Infant max weight (kg)", side = 1, outer = T, cex = 1.5, line = -69, adj=0.5)

par(xpd=FALSE)


MaxVelAge1_post_plot <- denschart3( mean_char_list[c(3,4)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["agemax",]) ) - 0.05, mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["agemax",]) ) + 0.05 ), #( 0.85, 0.95 ),
            yvals = c(0.5,0.5,0.5)
 )
lines(x=c( mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["agemax",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["agemax",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.7, 0.8), labels=c(0.7, 0.8),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -60, adj=0.5)


MaxVel1_post_plot <- denschart3( mean_char_list[c(5,6)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["maxvel",]) ) - 0.1, mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["maxvel",]) ) + 0.1 ), #( 38, 46 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["maxvel",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["maxvel",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(17, 17.5), labels=c(17, 17.5),
    cex.axis=cex_axis)
#mtext("Infant max velocity (kg/year)", side = 1, outer = T, cex = 1.5, line = -51, adj=0.5)


MaxWeight2_post_plot <- denschart3( mean_char_list[c(7,8)],
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
            xlim=range( mean( pi*(2*H2mat/K2mat)^( ( 2*Q2mat + 1 )/Q2mat ) ) - 0.8, mean( pi*(2*H2ber/K2ber)^( ( 2*Q2ber + 1 )/Q2ber ) ) + 0.8), #( 45, 65 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean( pi*(2*H2ber/K2ber)^( ( 2*Q2ber + 1 )/Q2ber ) ), mean( pi*(2*H2ber/K2ber)^( ( 2*Q2ber + 1 )/Q2ber ) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( pi*(2*H2mat/K2mat)^( ( 2*Q2mat + 1 )/Q2mat ) ), mean( pi*(2*H2mat/K2mat)^( ( 2*Q2mat + 1 )/Q2mat ) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(10,14), labels=c(10,14),
    cex.axis=cex_axis)
#mtext("Child max weight (kg)", side = 1, outer = T, cex = 1.5, line = -42.5, adj=0.5)


MaxVelAge2_post_plot <- denschart3( mean_char_list[c(9,10)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["agemax",]) ) - 0.18, mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["agemax",]) ) + 0.18), #( 6.5, 7.5 ),
            yvals = c(0.5,0.5)
 )
lines(x=c( mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["agemax",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["agemax",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(2,3), labels=c(2,3),
    cex.axis=cex_axis)
#mtext("Child age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = -34, adj=0.5)


MaxVel2_post_plot <- denschart3( mean_char_list[c(11,12)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["maxvel",]) ) - 0.15, mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["maxvel",]) ) + 0.15), #( 5, 7 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["maxvel",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["maxvel",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(2,3,4), labels=c(2,3,4),
    cex.axis=cex_axis)
#mtext("Child max velocity (kg/year)", side = 1, outer = T, cex = 1.5, line = -25, adj=0.5)



MaxWeight3_post_plot <- denschart3( mean_char_list[c(13,14)],
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
            xlim=range( mean( pi*(2*H3ber/K3ber)^( ( 2*Q3ber + 1 )/Q3ber ) ) - 3, mean( pi*(2*H3mat/K3mat)^( ( 2*Q3mat + 1 )/Q3mat ) ) + 3), #( 15, 25 ),
            yvals = c(0.5,0.5,0.5)
 )

lines(x=c( mean( pi*(2*H3ber/K3ber)^( ( 2*Q3ber + 1 )/Q3ber ) ), mean( pi*(2*H3ber/K3ber)^( ( 2*Q3ber + 1 )/Q3ber ) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( pi*(2*H3mat/K3mat)^( ( 2*Q3mat + 1 )/Q3mat ) ), mean( pi*(2*H3mat/K3mat)^( ( 2*Q3mat + 1 )/Q3mat ) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(45,55), labels=c(45,55),
    cex.axis=cex_axis)
#mtext("Adolescent max weight (kg)", side = 1, outer = T, cex = 1.5, line = -16, adj=0.5)


MaxVelAge3_post_plot <- denschart3( mean_char_list[c(15,16)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["agemax",]) ) - 0.09, mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["agemax",]) ) + 0.09 ), #( 13, 16 ),
            yvals = c(0.5,0.5)
 )
lines(x=c( mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["agemax",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["agemax",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(11,12,13), labels=c(11,12,13),
    cex.axis=cex_axis)
#mtext("Adolescent age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = -7.5, adj=0.5)


MaxVel3_post_plot <- denschart3( mean_char_list[c(17,18)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) ) - 0.4, mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]) ) + 0.4 ),
            yvals = c(0.5,0.5,0.5)
 )

lines(x=c( mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(4,4.5,5), labels=c(4,4.5,5),
    cex.axis=cex_axis)
#mtext("Adolescent Max Velocity (kg/year)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)




### parameter contrasts


MaxWeight1_cont_plot <- denschart3( contr_char_list[1],
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
            xlim=range(-0.4,0.4),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxWeight1), mean(contr_char_list$ber.matMaxWeight1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.3,0.3), labels=c(-0.3,0.3),
    cex.axis=cex_axis)




MaxVelAge1_cont_plot <- denschart3( contr_char_list[2],
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
            xlim=range(-0.07,0.07),
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge1), mean(contr_char_list$ber.matMaxVelAge1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.04,0.04), labels=c(-0.04,0.04),
    cex.axis=cex_axis)


MaxVel1_cont_plot <- denschart3( contr_char_list[3],
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
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel1), mean(contr_char_list$ber.matMaxVel1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.06,0.06), labels=c(-0.06,0.06),
    cex.axis=cex_axis)


MaxWeight2_cont_plot <- denschart3( contr_char_list[4],
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
            xlim=range(-5.5,5.5),
                #range( min(contr_char_list$ber.matMaxWeight2), max(contr_char_list$ber.matMaxWeight2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxWeight2), mean(contr_char_list$ber.matMaxWeight2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-4,4), labels=c(-4,4),
    cex.axis=cex_axis)


MaxVelAge2_cont_plot <- denschart3( contr_char_list[5],
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
            xlim=range(-0.3,0.3),
                #range( min(contr_char_list$ber.matMaxVelAge2), max(contr_char_list$ber.matMaxVelAge2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge2), mean(contr_char_list$ber.matMaxVelAge2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.2,0.2), labels=c(-0.2,0.2),
    cex.axis=cex_axis)


MaxVel2_cont_plot <- denschart3( contr_char_list[6],
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
            xlim=range(-1.35,1.35),
                #range( min(contr_char_list$ber.matMaxVel2), max(contr_char_list$ber.matMaxVel2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel2), mean(contr_char_list$ber.matMaxVel2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-1,1), labels=c(-1,1),
    cex.axis=cex_axis)


MaxWeight3_cont_plot <- denschart3( contr_char_list[7],
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
            xlim=range(-3,3),
                #range( min(contr_char_list$ber.matMaxWeight3), max(contr_char_list$ber.matMaxWeight3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxWeight3), mean(contr_char_list$ber.matMaxWeight3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-2,2), labels=c(-2,2),
    cex.axis=cex_axis)


MaxVelAge3_cont_plot <- denschart3( contr_char_list[8],
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
            xlim=range(-0.3,0.3),
                #range( min(contr_char_list$ber.matMaxVelAge3), max(contr_char_list$ber.matMaxVelAge3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge3), mean(contr_char_list$ber.matMaxVelAge3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.2,0.2), labels=c(-0.2,0.2),
    cex.axis=cex_axis)


MaxVel3_cont_plot <- denschart3( contr_char_list[9],
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
            xlim=range(-0.4,0.4),
                #range( min(contr_char_list$ber.matMaxVel3), max(contr_char_list$ber.matMaxVel3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel3), mean(contr_char_list$ber.matMaxVel3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.3,0.3), labels=c(-0.3,0.3),
    cex.axis=cex_axis)






# male weight ###########################################################################################################################################################################
post <- post3_mw
mean_char_list <- mean_char_list_mw
contr_char_list <- contr_char_list_mw

Q1mat <- Q1mat_mw
Q1ber <- Q1ber_mw

Q2mat <- Q2mat_mw
Q2ber <- Q2ber_mw

Q3mat <- Q3mat_mw
Q3ber <- Q3ber_mw

K1mat <- K1mat_mw
K1ber <- K1ber_mw

K2mat <- K2mat_mw
K2ber <- K2ber_mw

K3mat <- K3mat_mw
K3ber <- K3ber_mw

H1mat <- H1mat_mw
H1ber <- H1ber_mw

H2mat <- H2mat_mw
H2ber <- H2ber_mw

H3mat <- H3mat_mw
H3ber <- H3ber_mw




MaxWeight1_post_plot <- denschart3( mean_char_list[c(1,2)],
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
            xlim=range( mean( pi*(2*H1mat/K1mat)^( ( 2*Q1mat + 1 )/Q1mat ) ) - 0.4, mean( pi*(2*H1ber/K1ber)^( ( 2*Q1ber + 1 )/Q1ber ) ) + 0.4 ), #( 85, 100 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean( pi*(2*H1ber/K1ber)^( ( 2*Q1ber + 1 )/Q1ber ) ), mean( pi*(2*H1ber/K1ber)^( ( 2*Q1ber + 1 )/Q1ber ) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( pi*(2*H1mat/K1mat)^( ( 2*Q1mat + 1 )/Q1mat ) ), mean( pi*(2*H1mat/K1mat)^( ( 2*Q1mat + 1 )/Q1mat ) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(7,8), labels=c(7,8),
    cex.axis=cex_axis)
#mtext("Infant max weight (kg)", side = 1, outer = T, cex = 1.5, line = -69, adj=0.5)

par(xpd=FALSE)


MaxVelAge1_post_plot <- denschart3( mean_char_list[c(3,4)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["agemax",]) ) - 0.05, mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["agemax",]) ) + 0.05 ), #( 0.85, 0.95 ),
            yvals = c(0.5,0.5,0.5)
 )
lines(x=c( mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["agemax",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["agemax",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(0.7, 0.8), labels=c(0.7, 0.8),
    cex.axis=cex_axis)
#mtext("Infant age at maximum velocity (years)", side = 1, outer = T, cex = 1.5, line = -60, adj=0.5)


MaxVel1_post_plot <- denschart3( mean_char_list[c(5,6)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["maxvel",]) ) - 0.15, mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["maxvel",]) ) + 0.15 ), #( 38, 46 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q1ber, K=K1ber, H=H1ber)["maxvel",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q1mat, K=K1mat, H=H1mat)["maxvel",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(17, 17.5), labels=c(17, 17.5),
    cex.axis=cex_axis)
#mtext("Infant max velocity (kg/year)", side = 1, outer = T, cex = 1.5, line = -51, adj=0.5)


MaxWeight2_post_plot <- denschart3( mean_char_list[c(7,8)],
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
            xlim=range( mean( pi*(2*H2mat/K2mat)^( ( 2*Q2mat + 1 )/Q2mat ) ) - 0.8, mean( pi*(2*H2ber/K2ber)^( ( 2*Q2ber + 1 )/Q2ber ) ) + 0.8), #( 45, 65 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean( pi*(2*H2ber/K2ber)^( ( 2*Q2ber + 1 )/Q2ber ) ), mean( pi*(2*H2ber/K2ber)^( ( 2*Q2ber + 1 )/Q2ber ) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( pi*(2*H2mat/K2mat)^( ( 2*Q2mat + 1 )/Q2mat ) ), mean( pi*(2*H2mat/K2mat)^( ( 2*Q2mat + 1 )/Q2mat ) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(10,14), labels=c(10,14),
    cex.axis=cex_axis)
#mtext("Child max weight (kg)", side = 1, outer = T, cex = 1.5, line = -42.5, adj=0.5)


MaxVelAge2_post_plot <- denschart3( mean_char_list[c(9,10)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["agemax",]) ) - 0.4, mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["agemax",]) ) + 0.4), #( 6.5, 7.5 ),
            yvals = c(0.5,0.5)
 )
lines(x=c( mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["agemax",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["agemax",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(2,3), labels=c(2,3),
    cex.axis=cex_axis)
#mtext("Child age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = -34, adj=0.5)


MaxVel2_post_plot <- denschart3( mean_char_list[c(11,12)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["maxvel",]) ) - 0.15, mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["maxvel",]) ) + 0.15), #( 5, 7 ),
            yvals = c(0.5,0.5)
 )

lines(x=c( mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q2ber, K=K2ber, H=H2ber)["maxvel",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q2mat, K=K2mat, H=H2mat)["maxvel",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(2,3,4), labels=c(2,3,4),
    cex.axis=cex_axis)
#mtext("Child max velocity (kg/year)", side = 1, outer = T, cex = 1.5, line = -25, adj=0.5)



MaxWeight3_post_plot <- denschart3( mean_char_list[c(13,14)],
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
            xlim=range( mean( pi*(2*H3ber/K3ber)^( ( 2*Q3ber + 1 )/Q3ber ) ) - 9, mean( pi*(2*H3mat/K3mat)^( ( 2*Q3mat + 1 )/Q3mat ) ) + 9), #( 15, 25 ),
            yvals = c(0.5,0.5,0.5)
 )

lines(x=c( mean( pi*(2*H3ber/K3ber)^( ( 2*Q3ber + 1 )/Q3ber ) ), mean( pi*(2*H3ber/K3ber)^( ( 2*Q3ber + 1 )/Q3ber ) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( pi*(2*H3mat/K3mat)^( ( 2*Q3mat + 1 )/Q3mat ) ), mean( pi*(2*H3mat/K3mat)^( ( 2*Q3mat + 1 )/Q3mat ) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(45,55), labels=c(45,55),
    cex.axis=cex_axis)
#mtext("Adolescent max weight (kg)", side = 1, outer = T, cex = 1.5, line = -16, adj=0.5)


MaxVelAge3_post_plot <- denschart3( mean_char_list[c(15,16)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["agemax",]) ) - 0.04, mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["agemax",]) ) + 0.04 ), #( 13, 16 ),
            yvals = c(0.5,0.5)
 )
lines(x=c( mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["agemax",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["agemax",]) ), mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["agemax",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(11,12,13), labels=c(11,12,13),
    cex.axis=cex_axis)
#mtext("Adolescent age at max velocity (years)", side = 1, outer = T, cex = 1.5, line = -7.5, adj=0.5)


MaxVel3_post_plot <- denschart3( mean_char_list[c(17,18)],
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
            xlim=range( mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) ) - 0.9, mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]) ) + 0.9 ),
            yvals = c(0.5,0.5,0.5)
 )

lines(x=c( mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q3ber, K=K3ber, H=H3ber)["maxvel",]) ) ),
      y=c(0,0.75), col=BerLine_col, lwd=BerLine_lwd, lty=1)
lines(x=c( mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]) ), mean( unlist(vmaxagevel(Q=Q3mat, K=K3mat, H=H3mat)["maxvel",]) ) ),
      y=c(0,0.75), col=MatLine_col, lwd=MatLine_lwd, lty=1)

axis(side=1,
    #at=c(4,4.5,5), labels=c(4,4.5,5),
    cex.axis=cex_axis)
#mtext("Adolescent Max Velocity (kg/year)", side = 1, outer = T, cex = 1.5, line = 1, adj=0.5)




### parameter contrasts


MaxWeight1_cont_plot <- denschart3( contr_char_list[1],
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
            xlim=range(-0.8,0.8),
                #range( min(contr_char_list$ber.matMaxWeight1), max(contr_char_list$ber.matMaxWeight1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxWeight1), mean(contr_char_list$ber.matMaxWeight1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,0.75), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.5,0.5), labels=c(-0.5,0.5),
    cex.axis=cex_axis)




MaxVelAge1_cont_plot <- denschart3( contr_char_list[2],
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
                #range( min(contr_char_list$ber.matMaxVelAge1), max(contr_char_list$ber.matMaxVelAge1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge1), mean(contr_char_list$ber.matMaxVelAge1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.06,0.06), labels=c(-0.06,0.06),
    cex.axis=cex_axis)


MaxVel1_cont_plot <- denschart3( contr_char_list[3],
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
            xlim=range(-0.2,0.2),
                #range( min(contr_char_list$ber.matMaxVel1), max(contr_char_list$ber.matMaxVel1) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel1), mean(contr_char_list$ber.matMaxVel1) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.15,0.15), labels=c(-0.15,0.15),
    cex.axis=cex_axis)


MaxWeight2_cont_plot <- denschart3( contr_char_list[4],
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
            xlim=range(-3.5,3.5),
                #range( min(contr_char_list$ber.matMaxWeight2), max(contr_char_list$ber.matMaxWeight2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxWeight2), mean(contr_char_list$ber.matMaxWeight2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-3,3), labels=c(-3,3),
    cex.axis=cex_axis)


MaxVelAge2_cont_plot <- denschart3( contr_char_list[5],
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
            xlim=range(-0.5,0.5),
                #range( min(contr_char_list$ber.matMaxVelAge2), max(contr_char_list$ber.matMaxVelAge2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge2), mean(contr_char_list$ber.matMaxVelAge2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.3,0.3), labels=c(-0.3,0.3),
    cex.axis=cex_axis)


MaxVel2_cont_plot <- denschart3( contr_char_list[6],
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
            xlim=range(-1.2,1.2),
                #range( min(contr_char_list$ber.matMaxVel2), max(contr_char_list$ber.matMaxVel2) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel2), mean(contr_char_list$ber.matMaxVel2) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-1,1), labels=c(-1,1),
    cex.axis=cex_axis)


MaxWeight3_cont_plot <- denschart3( contr_char_list[7],
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
            xlim=range(-10,10),
                #range( min(contr_char_list$ber.matMaxWeight3), max(contr_char_list$ber.matMaxWeight3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxWeight3), mean(contr_char_list$ber.matMaxWeight3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-6,6), labels=c(-6,6),
    cex.axis=cex_axis)


MaxVelAge3_cont_plot <- denschart3( contr_char_list[8],
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
                #range( min(contr_char_list$ber.matMaxVelAge3), max(contr_char_list$ber.matMaxVelAge3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVelAge3), mean(contr_char_list$ber.matMaxVelAge3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.05,0.05), labels=c(-0.05,0.05),
    cex.axis=cex_axis)


MaxVel3_cont_plot <- denschart3( contr_char_list[9],
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
            xlim=range(-1,1),
                #range( min(contr_char_list$ber.matMaxVel3), max(contr_char_list$ber.matMaxVel3) ),
            yvals = 0.5
 )

lines(x=c( mean(contr_char_list$ber.matMaxVel3), mean(contr_char_list$ber.matMaxVel3) ),
      y=c(0,0.75), col=ConLine_col, lwd=ConLine_lwd, lty=1)

par(xpd=NA) # plotting clipped to device region
lines(x=c(0,0),
      y=c(0,1.5), col=ZerLine_col, lwd=ZerLine_lwd, lty=ZerLine_lty)
par(xpd=FALSE) # plotting clipped to plot region

axis(side=1,
    at=c(-0.6,0.6), labels=c(-0.6,0.6),
    cex.axis=cex_axis)



graphics.off()











