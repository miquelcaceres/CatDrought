#########################################################################
#  Runs weather station data downloading,
#  weather interpolation and soil water balance,
#  and process outputs for the current date
#########################################################################

#Working directory
setwd("D:/Rservices/CatDrought/")

date = Sys.Date()

# #Sets sink
# sink("CatDrought.log")

#Download current day meteo
source("D:/Recerca/Datasets/Climate/R scripts/AEMET_download.R")

#In case it has changed
setwd("D:/Rservices/CatDrought/")

#Interpolate meteo
source("R scripts/Day_0_MeteorologyInterpolation.R")
interpolateCat() #current day

#Soil water balance
source("R scripts/Day_1_SWB.R")
swbCat()

# #Calculates SWB maps
source("R scripts/Day_2_DaySWBMaps.R")
swbPointMapsCat(date) #Current day
# 
# #Update plot 1-year trends
source("R scripts/Day_3_UpdatePlotSWBYearTrends.R")
updatePlotSWBYearTrends(date)
# 
# #Calculates Drought stress maps
source("R scripts/Day_4_DayDroughtStressMaps.R")
droughtStressMapsCat(date)
# 
# #Update plot 1-year trends
source("R scripts/Day_5_UpdatePlotDroughtStressYearTrends.R")
updatePlotDroughtStressYearTrends(date)

#Returns to normal console output
# sink()