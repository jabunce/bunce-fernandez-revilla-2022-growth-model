# clear the workspace
rm (list = ls(all=TRUE))

# Load packages
# if you don't have one or more of these packages, you can install them like this: install.packages(c("lattice", "MASS"))
library(rstan)
library(rethinking)
library(sitar)
library(lattice)
library(MASS) #for mvrnorm function
library(graphics)
library(grid)
library(boot) #for inverse logit function
library(plyr) #for Catalina's simulation code

library(cmdstanr)
#install_cmdstan()  # use this to install CmdStan if it's not already installed
library(posterior)
library(bayesplot)
library(dplyr) # pull function to extract data from model fit "tibbles"
library(stats) # uniroot function to solve equations
library(HelpersMG) # for male and female symbols

# Set working directory: the path to the folder containing the subfolders Code, Plots, and Data
# on mac
#setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/growth_fitting_analysis/growth_github/analysis_height_mal")
# on pc
#setwd("C:/Users/John/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/growth_fitting_analysis/growth_github/analysis_height_mal")
# on server
setwd("~/growth_github/analysis_height_mal")

# Load helper functions
source("./Code/Functions.R")

# Prepare data (this can take a minute to process), on the server run this by hand 
source("./Code/PrepareData.R")



# Below, Sections 1 - 4 can be run indpendently, and in any order



###################################### Section 1: Fit composite model with three components

# design priors for reasonable variance about mean trajectory
# means and stdevs from this used for m1
# makes Figure A.5
source("./Code/PriorPredictInitial.R")

samps <- 2000 	#number of mcmc samples
num_chains <- 4 #number of mcmc chains

# estimate baseline mean parameter values by fitting model to Berkeley data
source("./Code/GetBaselineParams.R")
#readRDS("m1_fit.rds")

# plot baseline model posterior growth curves
# makes Figure A.8
source("./Code/PlotBaselineFit.R")


# design priors for reasonable indiv-level stdev
# stdevs from this used for m2
source("./Code/PriorPredictIndivStdev.R")

# samps <- 1000 	#number of mcmc samples
# num_chains <- 2 #number of mcmc chains

# estimate indiv-level stdev by fitting model with indiv-level random effects to Berkeley data
# output of m2 used for prior group sims
source("./Code/GetIndivStdev.R")
#readRDS("m2_fit.rds")

# plot model posterior trajectory against LMS trajectory and indiv trajectories 
source("./Code/PlotIndivFit.R")


# design priors for reasonable indiv-level and and group-level stdev
# makes Figure A.10
source("./Code/PriorPredictGroupStdev.R")
 
samps <- 4000 	#number of mcmc samples
num_chains <- 4 #number of mcmc chains

# fit main stan model to simulated dataset (m3)
source("./Code/FitMainModel.R")
#readRDS("m3_fit.rds")

# plot model posterior posterior trajectories and parameter comparisons
# makes male height components of Figures 3, 4, and 5
source("./Code/PlotMainModelOutput.R")



######################################### Section 2: composite model with five components

# makes Figure A.17
source("./Code/PriorPredict5.R")

samps <- 2000 	#number of mcmc samples
num_chains <- 4 #number of mcmc chains

source("./Code/Fit5Composite.R")
#readRDS("m4_fit.rds")

# makes Figure A.18
source("./Code/PlotComposite5fit.R")



########################################### Section 3: JPA-1 model

# Ber and Matsi separate

# makes Figure A.19
source("./Code/PriorPredictJPA.R")

samps <- 3000 	#number of mcmc samples
num_chains <- 4 #number of mcmc chains

source("./Code/FitJPA1model.R")
#readRDS("m5_fit.rds")
#readRDS("m6_fit.rds")

# makes Figure A.20
source("./Code/PlotJPAfit.R")


# # Ber and Matsi together - doesn't fit

# source("./Code/PriorPredictGroupStdevJPA.R")

# source("./Code/FitJPA1modelCov.R")
# #readRDS("m7_fit.rds")

# source("./Code/PlotJPAfitCov.R")



############################################ Section 4: SITAR model

# makes Figure A.21
source("./Code/FitSITAR.R")




