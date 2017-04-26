library(medfate)
library(spatstat)
library(maptools)

droughtStressMapsCat<-function(date = Sys.Date(), radius = 3000) {
  load("Rdata/IFN3_SPT_cat.rdata")
  load(paste0("Rdata/DailySWB/", as.character(date), ".rda"))  
  plotIDs = rownames(IFN3_SPT@coords)
  npoints = length(plotIDs)
  
  df = data.frame(matrix(NA,nrow=npoints, ncol =12))
  names(df)<-c("PinusHalepensis","PinusNigra","PinusSylvestris","PinusUncinata", "PinusPinea", "PinusPinaster",
               "QuercusIlex","QuercusSuber",  "QuercusHumilis", "QuercusFaginea", "FagusSylvatica", "Overall")
  pb = txtProgressBar(0, npoints, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb,i)
    plotID = plotIDs[i]
    resday = swbres[[i]]
    load(file=paste0("Rdata/Plots/",plotID,".rda"))
    SP = x$above$SP
    LAI_expanded = x$above$LAI_expanded
    resday$DDS = pmax(pmin(resday$DDS,1),0) #Avoid out of range values
    if(sum(SP==54)>0) {
      if(sum(LAI_expanded[SP==54])>0) {
        df[i,"PinusHalepensis"] = sum(LAI_expanded[SP==54]*resday$DDS[SP==54])/sum(LAI_expanded[SP==54])
      } else {
        df[i,"PinusHalepensis"] = 0
      }
    }
    if(sum(SP==55)>0) {
      if(sum(LAI_expanded[SP==55])>0) {
        df[i,"PinusNigra"] = sum(LAI_expanded[SP==55]*resday$DDS[SP==55])/sum(LAI_expanded[SP==55])
      } else {
        df[i,"PinusNigra"] = 0
      }
    }
    if(sum(SP==59)>0) {
      if(sum(LAI_expanded[SP==59])>0) {
        df[i,"PinusSylvestris"] = sum(LAI_expanded[SP==59]*resday$DDS[SP==59])/sum(LAI_expanded[SP==59])
      } else {
        df[i,"PinusSylvestris"] = 0
      }
    }
    if(sum(SP==60)>0) {
      if(sum(LAI_expanded[SP==60])>0) {
        df[i,"PinusUncinata"] = sum(LAI_expanded[SP==60]*resday$DDS[SP==60])/sum(LAI_expanded[SP==60])
      } else {
        df[i,"PinusUncinata"] = 0
      }
    }
    if(sum(SP==57)>0) {
      if(sum(LAI_expanded[SP==57])>0) {
        df[i,"PinusPinea"] = sum(LAI_expanded[SP==57]*resday$DDS[SP==57])/sum(LAI_expanded[SP==57])
      } else {
        df[i,"PinusPinea"] = 0
      }
    }
    if(sum(SP==56)>0) {
      if(sum(LAI_expanded[SP==56])>0) {
        df[i,"PinusPinaster"] = sum(LAI_expanded[SP==56]*resday$DDS[SP==56])/sum(LAI_expanded[SP==56])
      } else {
        df[i,"PinusPinaster"] = 0
      }
    }
    if(sum(SP==68)>0) {
      if(sum(LAI_expanded[SP==68])>0) {
        df[i,"QuercusIlex"] = sum(LAI_expanded[SP==68]*resday$DDS[SP==68])/sum(LAI_expanded[SP==68])
      } else {
        df[i,"QuercusIlex"] = 0
      }
    }
    if(sum(SP==72)>0) {
      if(sum(LAI_expanded[SP==72])>0) {
        df[i,"QuercusSuber"] = sum(LAI_expanded[SP==72]*resday$DDS[SP==72])/sum(LAI_expanded[SP==72])
      } else {
        df[i,"QuercusSuber"] = 0
      }
    }
    if(sum(SP==67)>0) {
      if(sum(LAI_expanded[SP==67])>0) {
        df[i,"QuercusHumilis"] = sum(LAI_expanded[SP==67]*resday$DDS[SP==67])/sum(LAI_expanded[SP==67])
      } else {
        df[i,"QuercusHumilis"] = 0
      }
    }
    if(sum(SP==66)>0) {
      if(sum(LAI_expanded[SP==66])>0) {
        df[i,"QuercusFaginea"] = sum(LAI_expanded[SP==66]*resday$DDS[SP==66])/sum(LAI_expanded[SP==66])
      } else {
        df[i,"QuercusFaginea"] = 0
      }
    }
    if(sum(SP==37)>0) {
      if(sum(LAI_expanded[SP==37])>0) {
        df[i,"FagusSylvatica"] = sum(LAI_expanded[SP==37]*resday$DDS[SP==37])/sum(LAI_expanded[SP==37])
      } else {
        df[i,"FagusSylvatica"] = 0
      }
    }
    df[i,"Overall"] = sum(LAI_expanded*resday$DDS, na.rm=TRUE)/sum(LAI_expanded, na.rm=TRUE)
  }
  spdf = SpatialPointsDataFrame(coordinates(IFN3_SPT),df,proj4string = IFN3_SPT@proj4string)
  save(spdf,file=paste0("Rdata/SpatialPointDroughtStressMaps/", as.character(date), ".rda"))  
  
  #Mapping
  load("Rdata/Masks.rda")
  cc = IFN3_SPT@coords
  ow = as.owin(as.vector(t(IFN3_SPT@bbox)))
  ccm = coordinates(masks)
  ph.smooth = Smooth(ppp(x=cc[!is.na(df$PinusHalepensis),1], y=cc[!is.na(df$PinusHalepensis),2], window = ow, 
                          marks = df$PinusHalepensis[!is.na(df$PinusHalepensis)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  ph.sgdf = as.SpatialGridDataFrame.im(ph.smooth)
  pn.smooth = Smooth(ppp(x=cc[!is.na(df$PinusNigra),1], y=cc[!is.na(df$PinusNigra),2], window = ow, 
                         marks = df$PinusNigra[!is.na(df$PinusNigra)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  pn.sgdf = as.SpatialGridDataFrame.im(pn.smooth)
  ps.smooth = Smooth(ppp(x=cc[!is.na(df$PinusSylvestris),1], y=cc[!is.na(df$PinusSylvestris),2], window = ow, 
                         marks = df$PinusSylvestris[!is.na(df$PinusSylvestris)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  ps.sgdf = as.SpatialGridDataFrame.im(ps.smooth)
  pu.smooth = Smooth(ppp(x=cc[!is.na(df$PinusUncinata),1], y=cc[!is.na(df$PinusUncinata),2], window = ow, 
                         marks = df$PinusUncinata[!is.na(df$PinusUncinata)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  pu.sgdf = as.SpatialGridDataFrame.im(pu.smooth)
  ppi.smooth = Smooth(ppp(x=cc[!is.na(df$PinusPinea),1], y=cc[!is.na(df$PinusPinea),2], window = ow, 
                          marks = df$PinusPinea[!is.na(df$PinusPinea)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  ppi.sgdf = as.SpatialGridDataFrame.im(ppi.smooth)
  pps.smooth = Smooth(ppp(x=cc[!is.na(df$PinusPinaster),1], y=cc[!is.na(df$PinusPinaster),2], window = ow, 
                          marks = df$PinusPinaster[!is.na(df$PinusPinaster)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  pps.sgdf = as.SpatialGridDataFrame.im(pps.smooth)
  
  qi.smooth = Smooth(ppp(x=cc[!is.na(df$QuercusIlex),1], y=cc[!is.na(df$QuercusIlex),2], window = ow, 
                         marks = df$QuercusIlex[!is.na(df$QuercusIlex)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  qi.sgdf = as.SpatialGridDataFrame.im(qi.smooth)
  qs.smooth = Smooth(ppp(x=cc[!is.na(df$QuercusSuber),1], y=cc[!is.na(df$QuercusSuber),2], window = ow, 
                         marks = df$QuercusSuber[!is.na(df$QuercusSuber)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  qs.sgdf = as.SpatialGridDataFrame.im(qs.smooth)
  qh.smooth = Smooth(ppp(x=cc[!is.na(df$QuercusHumilis),1], y=cc[!is.na(df$QuercusHumilis),2], window = ow, 
                         marks = df$QuercusHumilis[!is.na(df$QuercusHumilis)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  qh.sgdf = as.SpatialGridDataFrame.im(qh.smooth)
  qf.smooth = Smooth(ppp(x=cc[!is.na(df$QuercusFaginea),1], y=cc[!is.na(df$QuercusFaginea),2], window = ow, 
                         marks = df$QuercusFaginea[!is.na(df$QuercusFaginea)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  qf.sgdf = as.SpatialGridDataFrame.im(qf.smooth)
  fs.smooth = Smooth(ppp(x=cc[!is.na(df$FagusSylvatica),1], y=cc[!is.na(df$FagusSylvatica),2], window = ow, 
                         marks = df$FagusSylvatica[!is.na(df$FagusSylvatica)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  fs.sgdf = as.SpatialGridDataFrame.im(fs.smooth)
  all.smooth = Smooth(ppp(x=cc[!is.na(df$Overall),1], y=cc[!is.na(df$Overall),2], window = ow, 
                          marks = df$Overall[!is.na(df$Overall)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  all.sgdf = as.SpatialGridDataFrame.im(all.smooth)
  
  spdf = ph.sgdf
  names(spdf@data) = "PinusHalepensis"
  spdf@data$PinusHalepensis[!masks$PinusHalepensis] = NA
  spdf@data$PinusNigra = pn.sgdf@data[,1]
  spdf@data$PinusNigra[!masks$PinusNigra] = NA
  spdf@data$PinusSylvestris = ps.sgdf@data[,1]
  spdf@data$PinusSylvestris[!masks$PinusSylvestris] = NA
  spdf@data$PinusUncinata = pu.sgdf@data[,1]
  spdf@data$PinusUncinata[!masks$PinusUncinata] = NA
  spdf@data$PinusPinea = ppi.sgdf@data[,1]
  spdf@data$PinusPinea[!masks$PinusPinea] = NA
  spdf@data$PinusPinaster = pps.sgdf@data[,1]
  spdf@data$PinusPinaster[!masks$PinusPinaster] = NA
  spdf@data$QuercusIlex = qi.sgdf@data[,1]
  spdf@data$QuercusIlex[!masks$QuercusIlex] = NA
  spdf@data$QuercusSuber = qs.sgdf@data[,1]
  spdf@data$QuercusSuber[!masks$QuercusSuber] = NA
  spdf@data$QuercusHumilis = qh.sgdf@data[,1]
  spdf@data$QuercusHumilis[!masks$QuercusHumilis] = NA
  spdf@data$QuercusFaginea = qf.sgdf@data[,1]
  spdf@data$QuercusFaginea[!masks$QuercusFaginea] = NA
  spdf@data$FagusSylvatica = fs.sgdf@data[,1]
  spdf@data$FagusSylvatica[!masks$FagusSylvatica] = NA
  spdf@data$Overall = all.sgdf@data[,1]
  spdf@data$Overall[!masks$Forest] = NA
  save(spdf,file=paste0("Rdata/SmoothedDroughtStressMaps/", as.character(date), ".rda"))
}
