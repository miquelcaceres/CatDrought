library(medfate)

updatePlotSWBYearTrends<-function(date = Sys.Date()) {
  load("Rdata/IFN3_SPT_cat.rdata")
  load(paste0("Rdata/DailySWB/", as.character(date), ".rda"))  
  plotIDs = rownames(IFN3_SPT@coords)
  npoints = length(plotIDs)
  y = as.numeric(format(date,"%Y"))
  m = as.numeric(format(date,"%m"))
  d = as.numeric(format(date,"%d"))
  date.first = as.Date(paste(y-1,m,d, sep="-"))
  dates = seq(date.first, date, by="day")
  dates = dates[(length(dates)-364):length(dates)]
  pb = txtProgressBar(0, npoints, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb,i)
    plotID = plotIDs[i]
    load(file=paste0("Rdata/PlotSWBYearTrends/", plotID, ".rda"))  
    resday = swbres[[i]]
    trends[1:364,] = trends[2:365,]
    trends[365,"PET"] = resday$PET
    if("Rain" %in% names(resday)) trends[365,"Rain"] = resday$Rain
    trends[365,"NetPrec"] = resday$NetPrec
    trends[365,"Runoff"] = resday$Runoff
    trends[365,"DeepDrainage"] = resday$DeepDrainage
    trends[365,"LAI"] = resday$LAIcell
    trends[365,"Esoil"] = sum(resday$EsoilVec, na.rm=TRUE)
    trends[365,"Eplant"] = sum(resday$EplantCoh, na.rm=TRUE)
    ## Use soil texture to calculate Theta from 
    load(file=paste0("Rdata/Plots/",plotID,".rda"))
    nlayers = length(soil$dVec)
    theta =numeric(nlayers)
    for(l in 1:nlayers) {
      theta[l] = soil.psi2theta(soil$clay[l], soil$sand[l], resday$psiVec[l])
    }
    trends[365,"Theta"] = sum((theta/soil$Theta_FC)*soil$Water_FC)/sum(soil$Water_FC)
    if(sum(is.na(theta))>0 || sum(is.na(soil$Water_FC))>0 || sum(soil$Water_FC)==0) trends[365,"Theta"] =NA
    
    row.names(trends) = as.character(dates)
    save(trends,file=paste0("Rdata/PlotSWBYearTrends/", plotID, ".rda"))  
  }
}