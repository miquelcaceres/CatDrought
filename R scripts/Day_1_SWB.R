library(medfate)

load("Rdata/IFN3_SPT_cat.rdata")
load("Rdata/temp_spm.rdata")

IFN3_latlon = spTransform(IFN3_SPT, CRS("+proj=longlat"))
IFN3_lat = IFN3_latlon@coords[,2]
date = spm@dates

plotIDs = rownames(IFN3_SPT@coords)
nplots = length(plotIDs)
swbres = vector("list", nplots)

pb = txtProgressBar(0, nplots, style = 3)
for(i in 1:nplots) {
  setTxtProgressBar(pb, i)
  plotID = plotIDs[i]
  
  #Load inputs
  load(file=paste0("Rdata/Plots/",plotID,".rda"))
  
  #Get meteo
  tmin = spm@data[[i]]$MinTemperature
  tmax = spm@data[[i]]$MaxTemperature
  rhmin = spm@data[[i]]$MinRelativeHumidity
  rhmax = spm@data[[i]]$MaxRelativeHumidity
  wind = spm@data[[i]]$WindSpeed
  rad = spm@data[[i]]$Radiation
  latitude = IFN3_lat[i]
  elevation = IFN3_SPT$elevation[i]
  slope = IFN3_SPT$slope[i]
  aspect = IFN3_SPT$aspect[i]
  rain = spm@data[[i]]$Precipitation
  doy = spm@data[[i]]$DOY
  er = ifelse((doy<=120) || (doy>=335),0.2,0.05)
  
  #Call SWB
  swbres[[i]]<-swb.day(x, soil, date, tmin, tmax, rhmin, rhmax, rad, wind, latitude , elevation, slope, aspect, rain, er, runon = 0)

  #Replace current plot state
  save(x,soil, file=paste0("Rdata/Plots/",plotID,".rda"))
}

save(swbres, file=paste0("Rdata/DailySWB/", as.character(date), ".rda"))
