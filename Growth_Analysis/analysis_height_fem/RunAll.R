# clear the workspace
rm (list = ls(all=TRUE))

# Load packages
# if you don't have one or more of these packages, you can install them like this: install.packages(c("lattice", "MASS"))
library(rstan)
library(rethinking)
library(lattice)
library(MASS) #for mvrnorm function
library(graphics)
library(grid)
library(boot) #for inverse logit function
library(plyr) #for Catalina's simulation code

library(cmdstanr)
library(posterior)
library(bayesplot)
library(dplyr) # pull function to extract data from model fit "tibbles"
library(stats) # uniroot function to solve equations
library(HelpersMG) # for male and female symbols

# Set working directory: the path to the folder containing the subfolders Code, Plots, and Data
# on mac
#setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/growth_fitting_analysis/growth_github/analysis_height_fem")
# on pc
#setwd("C:/Users/John/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/growth_fitting_analysis/growth_github/analysis_height_fem")
# on server
setwd("~/growth_github/analysis_height_fem")

# Load helper functions
source("./Code/Functions.R")

# Prepare data (this can take a minute to process) 
source("./Code/PrepareData.R")



# design priors for reasonable variance about mean trajectory
# means and stdevs from this used for m1
source("./Code/PriorPredictInitial.R")

samps <- 2000 	#number of mcmc samples
num_chains <- 4 #number of mcmc chains

# estimate baseline mean parameter values by fitting model to Berkeley data
source("./Code/GetBaselineParams.R")
#readRDS("m1_fit.rds")

# plot baseline model posterior growth curves
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
source("./Code/PriorPredictGroupStdev.R")
 
samps <- 4000 	#number of mcmc samples
num_chains <- 4 #number of mcmc chains

# fit main stan model to simulated dataset (m3)
source("./Code/FitMainModel.R")
#readRDS("m3_fit.rds")

#plot model posterior posterior trajectories and parameter comparisons
source("./Code/PlotMainModelOutput.R")


