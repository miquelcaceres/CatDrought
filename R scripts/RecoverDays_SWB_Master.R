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

dates = seq(as.Date("2017-01-01"), as.Date("2017-04-30"), by="day")
recoverLastDaySoil = FALSE

if(recoverLastDaySoil){
  load("Rdata/IFN3_SPT_cat.rdata")
  plotIDs = rownames(IFN3_SPT@coords)
  npoints = length(plotIDs)
  dates_prev = dates[1]-1
  cat(paste0("Recovering soil from: ", dates_prev,"\n"))
  #Set soil moisture to previous day
  load(paste0("Rdata/DailySWB/", as.character(dates_prev), ".rda"))  
  pb = txtProgressBar(0, npoints, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb,i)
    resday = swbres[[i]]
    plotID = plotIDs[i]
    load(file=paste0("Rdata/Plots/",plotID,".rda"))
    ## Use soil texture to calculate REW from 
    nlayers = length(soil$W)
    for(l in 1:nlayers) {
      soil$W[l] = soil.psi2theta(soil$clay[l], soil$sand[l], resday$psiVec[l])/soil$Theta_FC[l]
    }
    save(x, soil, file=paste0("Rdata/Plots/",plotID,".rda"))
  }
}

for(i in 1:length(dates)) {
  cat(paste0("\n\n [ ",i,"/",length(dates)," ] Processing: ",dates[i],"\n"))
  #Interpolate meteo
  cat(paste("  Interpolation -"))
  interpolateCat(dates[i], excludeRainFromStations=c("0433D", "9590D", "9988B", "9677"))
  #Soil water balance
  cat(paste(" Water balance (parallelized) \n"))
  swbCat()
  cat(paste("  Maps\n"))
  #Create maps
  swbPointMapsCat(dates[i])
  #Create drought stress maps
  droughtStressMapsCat(dates[i])
}
