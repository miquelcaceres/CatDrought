#####################################################################################################
#  Runs weather interpolation, soil water balance and processes outputs for a set of dates,         #
#  assuming that weather station data is already available                                          #
#####################################################################################################

#Working directory
setwd("D:/Rservices/CatDrought/")

source("R scripts/Day_0_MeteorologyInterpolation.R")
source("R scripts/Day_1_SWB.R")
source("R scripts/Day_2_DaySWBMaps.R")
source("R scripts/Day_4_DayDroughtStressMaps.R")

dates = seq(as.Date("2017-01-22"), as.Date("2017-04-26"), by="day")

for(i in 1:length(dates)) {
  cat(paste0("\n\n [ ",i,"/",length(dates)," ] Processing: ",dates[i],"\n"))
  #Interpolate meteo
  cat(paste("  Interpolation -"))
  interpolateCat(dates[i])
  #Soil water balance
  cat(paste(" Water balance -"))
  swbCat()
  #Create maps
  swbPointMapsCat(dates[i])
  #Update plot SWB trends
  # updatePlotSWBYearTrends(dates[i])
  #Create drought stress maps
  droughtStressMapsCat(dates[i])
  cat(paste(" done \n"))
}
