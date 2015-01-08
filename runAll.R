##############################################
## Author: Eduardo Clark
## Title: Homicides and temperature in Mexico
## Date: January 2015
##############################################

##Use this file to run all

#Unzip data
unzip("data.zip")

##Load libraries
source("src/loadLibraries.R")

##PrepareData
source("src/HomicidePanel.R")
source("src/TemperaturePanel.R")
source("src/CreateFinalPanel.R")

#Modeling
source("src/EffectsEstimation.R")