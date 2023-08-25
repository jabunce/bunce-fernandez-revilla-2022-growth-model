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

# Set working directory: the path to the folder containing the subfolders Code, Plots, and Data
# on mac
setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/growth_fitting_analysis/waistheight_github")
# on pc
#setwd("C:/Users/jabunce/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/growth_fitting_analysis/waistheight_github")
# on server
#setwd("~/waistheight_github")


# compare alternative relationships between radius and height to head cicumference LMS data
# makes appendix figure A.2
source("./Code/HeadCircumference_predictions.R")


# compare alternative relationships between radius and height to waist cicumference LMS data
# makes appendix figure A.3
source("./Code/WaistCircumference_predictions.R")

