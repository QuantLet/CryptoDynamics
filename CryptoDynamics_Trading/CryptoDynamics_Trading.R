rm(list = ls())

###############################
# Set directory and load data #
###############################

setwd("~/Dropbox/Cointegration and CC Dynamics/New Code")
data = read.csv("logprice.csv",header=T)
date = as.Date(read.csv("logprice.csv")[,2])
beta = read.csv("beta.csv")

############################
# Calculate the thresholds #
############################

#Long term stochastic trends

#Calculate mean and standard deviation

#Set lower and upper threshold


###################################
# Implementation Trading Strategy #
###################################

#Implementation
#Calculate performance measures


#########################
# Plot Trading Strategy #
#########################

#One stochastic trend, lower/upper bounds
#Change colors (0 black, -1 red, +1 blue)


###########################
# Plot Cumulative Profits #
###########################

#Compare with SP500 and CRIX
