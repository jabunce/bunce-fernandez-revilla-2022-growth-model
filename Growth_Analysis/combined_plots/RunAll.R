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
library(splines) # spline for SITAR

library(cmdstanr)
library(posterior)
library(bayesplot)
library(plyr)
library(dplyr) # pull function to extract data from model fit "tibbles"
library(stats) # uniroot function to solve equations
library(HelpersMG) # for male and female symbols

library(shape) # for drawing cylinder
library(plotrix)


# Set working directory: the path to the folder containing the subfolders for height and weight by sex
# on mac
#setwd("/Users/johnbunce/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/growth_fitting_analysis/growth_11dec24/Matsigenka")
# on pc
setwd("C:/Users/John/Dropbox/Matsigenka-Mestizo_project_2014/health_analysis/growth_fitting_analysis/growth_11dec24/Matsigenka")
# on server
#setwd("~/Matsigenka")

# Load helper functions
source("./combined_plots/Code/Functions.R")

# Prepare data (this can take a minute to process)
# Also makes Figure (raw data repeat measures) and Figure (weight for height)
source("./combined_plots/Code/PrepareData.R")



#########################################################
# May need to close and re-open a new R session if computer doesn't have enough memory to load model fit (.RDS) files.
# For instance, a memory error might look like: "cannot allocate vector of size 62 Kb"
# After opening a new session, run script up to here, and then just the desired section below.
# Each individual section can take a few minutes to run.


# Figures example trajectories
source("./combined_plots/Code/Plot_composite_example.R")


# Figures comparison with other models, and residuals
source("./combined_plots/Code/Plot_compare_models_residuals.R")


# Figure combined trajectory estimates
source("./combined_plots/Code/PlotAll_composite.R")


# Figure combined trajectory characteristic estimates
source("./combined_plots/Code/PlotAll_chars.R")


# Figure combined parameter estimates
source("./combined_plots/Code/PlotAll_params.R")


# Figure combined parameter estimates by process
source("./combined_plots/Code/PlotAll_params_phases.R")


# Figure intervention trajectories
source("./combined_plots/Code/Intervention_trajectories.R")


# Figure cylinder diagram
source("./combined_plots/Code/cylinder_draw.R")
