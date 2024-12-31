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
#setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/growth_fitting_analysis/growth_11dec24/Matsigenka/analysis_height_mal_JPA1")
# on pc
setwd("C:/Users/John/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/growth_fitting_analysis/growth_11dec24/Matsigenka/analysis_height_mal_JPA1")
# on server
#setwd("~/Matsigenka/analysis_height_mal_JPA1")

# Load helper functions
source("./Code/Functions.R")

# Prepare data (this can take a minute to process), on the server run this by hand 
source("./Code/PrepareData.R")





########################################### 


source("./Code/PriorPredictJPA1.R")

samps <- 1000 	#number of mcmc samples
num_chains <- 2 #number of mcmc chains

source("./Code/FitJPA1model.R")
#readRDS("m7_fit.RDS")


source("./Code/PlotJPA1fit.R")




# Ber and Matsi together

source("./Code/PriorPredictGroupStdevJPA1.R")

samps <- 9000 	#6000 4000 number of mcmc samples
num_chains <- 8 #6 4 number of mcmc chains


source("./Code/FitJPA1modelCov.R")
#readRDS("m8_fit.RDS")

source("./Code/PlotJPA1fitCov.R")






