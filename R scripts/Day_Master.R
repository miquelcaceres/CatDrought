#Working directory
setwd("D:/Rservices/CatDrought/")

#Sets sink
sink("CatDrought.log")

#Download current day meteo
source("D:/Recerca/Datasets/Climate/R scripts/AEMET_download.R")

#In case it has changed
setwd("D:/Rservices/CatDrought/")

#Interpolate meteo
source("R scripts/Day_0_MeteorologyInterpolation.R")

#Soil water balance
source("R scripts/Day_1_SWB.R")

#Returns to normal console output
sink()