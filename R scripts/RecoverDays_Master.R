#########################################################################
#  Runs weather interpolation and soil water balance for a set of dates, 
#  assuming that weather station data is already available
#########################################################################

#Working directory
setwd("D:/Rservices/CatDrought/")

# source("R scripts/Day_0_MeteorologyInterpolation.R")
# source("R scripts/Day_1_SWB.R")
source("R scripts/Day_2_DaySWBMaps.R")
source("R scripts/Day_3_UpdatePlotSWBYearTrends.R")
source("R scripts/Day_4_DayDroughtStressMaps.R")
source("R scripts/Day_5_UpdatePlotDroughtStressYearTrends.R")

dates = seq(as.Date("2017-03-28"), as.Date("2017-04-24"), by="day")

for(i in 1:length(dates)) {
  print(dates[i])
  #Interpolate meteo
  # interpolateCat(dates[i]) #current day
  #Soil water balance
  # swbCat()
  #Create maps
  swbPointMapsCat(dates[i])
  #Update plot SWB trends
  # updatePlotSWBYearTrends(dates[i])
  #Create drought stress maps
  droughtStressMapsCat(dates[i])
  #Update plot drought stress trends
  # updatePlotDroughtStressYearTrends(dates[i])
}
