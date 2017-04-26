library(medfate)

updatePlotDroughtStressYearTrends<-function(date = Sys.Date()) {
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
    resday = swbres[[i]]
    load(file=paste0("Rdata/Plots/",plotID,".rda"))
    SP = x$above$SP
    LAI_expanded = x$above$LAI_expanded
    resday$DDS = pmax(pmin(resday$DDS,1),0) #Avoid out of range values
    load(file=paste0("Rdata/PlotDroughtStressYearTrends/", plotID, ".rda"))  
    trends[1:364,] = trends[2:365,]
    if(sum(SP==54)>0) {
      if(sum(LAI_expanded[SP==54])>0) {
        trends[365,"PinusHalepensis"] = sum(LAI_expanded[SP==54]*resday$DDS[SP==54])/sum(LAI_expanded[SP==54])
      } else {
        trends[365,"PinusHalepensis"] = 0
      }
    }
    if(sum(SP==55)>0) {
      if(sum(LAI_expanded[SP==55])>0) {
        trends[365,"PinusNigra"] = sum(LAI_expanded[SP==55]*resday$DDS[SP==55])/sum(LAI_expanded[SP==55])
      } else {
        trends[365,"PinusNigra"] = 0
      }
    }
    if(sum(SP==59)>0) {
      if(sum(LAI_expanded[SP==59])>0) {
        trends[365,"PinusSylvestris"] = sum(LAI_expanded[SP==59]*resday$DDS[SP==59])/sum(LAI_expanded[SP==59])
      } else {
        trends[365,"PinusSylvestris"] = 0
      }
    }
    if(sum(SP==60)>0) {
      if(sum(LAI_expanded[SP==60])>0) {
        trends[365,"PinusUncinata"] = sum(LAI_expanded[SP==60]*resday$DDS[SP==60])/sum(LAI_expanded[SP==60])
      } else {
        trends[365,"PinusUncinata"] = 0
      }
    }
    if(sum(SP==57)>0) {
      if(sum(LAI_expanded[SP==57])>0) {
        trends[365,"PinusPinea"] = sum(LAI_expanded[SP==57]*resday$DDS[SP==57])/sum(LAI_expanded[SP==57])
      } else {
        trends[365,"PinusPinea"] = 0
      }
    }
    if(sum(SP==56)>0) {
      if(sum(LAI_expanded[SP==56])>0) {
        trends[365,"PinusPinaster"] = sum(LAI_expanded[SP==56]*resday$DDS[SP==56])/sum(LAI_expanded[SP==56])
      } else {
        trends[365,"PinusPinaster"] = 0
      }
    }
    if(sum(SP==68)>0) {
      if(sum(LAI_expanded[SP==68])>0) {
        trends[365,"QuercusIlex"] = sum(LAI_expanded[SP==68]*resday$DDS[SP==68])/sum(LAI_expanded[SP==68])
      } else {
        trends[365,"QuercusIlex"] = 0
      }
    }
    if(sum(SP==72)>0) {
      if(sum(LAI_expanded[SP==72])>0) {
        trends[365,"QuercusSuber"] = sum(LAI_expanded[SP==72]*resday$DDS[SP==72])/sum(LAI_expanded[SP==72])
      } else {
        trends[365,"QuercusSuber"] = 0
      }
    }
    if(sum(SP==67)>0) {
      if(sum(LAI_expanded[SP==67])>0) {
        trends[365,"QuercusHumilis"] = sum(LAI_expanded[SP==67]*resday$DDS[SP==67])/sum(LAI_expanded[SP==67])
      } else {
        trends[365,"QuercusHumilis"] = 0
      }
    }
    if(sum(SP==66)>0) {
      if(sum(LAI_expanded[SP==66])>0) {
        trends[365,"QuercusFaginea"] = sum(LAI_expanded[SP==66]*resday$DDS[SP==66])/sum(LAI_expanded[SP==66])
      } else {
        trends[365,"QuercusFaginea"] = 0
      }
    }
    if(sum(SP==37)>0) {
      if(sum(LAI_expanded[SP==37])>0) {
        trends[365,"FagusSylvatica"] = sum(LAI_expanded[SP==37]*resday$DDS[SP==37])/sum(LAI_expanded[SP==37])
      } else {
        trends[365,"FagusSylvatica"] = 0
      }
    }
    trends[365,"Overall"] = sum(LAI_expanded*resday$DDS, na.rm=TRUE)/sum(LAI_expanded, na.rm=TRUE)
    
    row.names(trends) = as.character(dates)
    save(trends,file=paste0("Rdata/PlotDroughtStressYearTrends/", plotID, ".rda"))  
  }
}