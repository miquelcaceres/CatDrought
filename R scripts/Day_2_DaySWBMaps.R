library(medfate)
library(spatstat)
library(maptools)

swbPointMapsCat<-function(date = Sys.Date(), radius = 2000) {
  load("Rdata/IFN3_SPT_cat.rdata")
  load(paste0("Rdata/DailySWB/", as.character(date), ".rda"))  
  plotIDs = rownames(IFN3_SPT@coords)
  npoints = length(plotIDs)
  
  df = data.frame(matrix(NA,nrow=npoints, ncol =9))
  names(df)<-c("PET","Rain","NetPrec","Runoff", "DeepDrainage", "LAI", "Eplant","Esoil",  "Theta")
  pb = txtProgressBar(0, npoints, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb,i, title = "SWB maps")
    plotID = plotIDs[i]
    resday = swbres[[i]]
    df[i,"PET"] = resday$PET
    if("Rain" %in% names(resday)) df[i,"Rain"] = resday$Rain
    df[i,"NetPrec"] = resday$NetPrec
    df[i,"Runoff"] = resday$Runoff
    df[i,"DeepDrainage"] = resday$DeepDrainage
    df[i,"LAI"] = resday$LAIcell
    df[i,"Eplant"] = sum(resday$EplantCoh, na.rm=TRUE)
    df[i,"Esoil"] = sum(resday$EsoilVec, na.rm=TRUE)
    
    load(file=paste0("Rdata/Plots/",plotID,".rda"))
    ## Use soil texture to calculate REW from 
    nlayers = length(soil$dVec)
    theta =numeric(nlayers)
    for(l in 1:nlayers) {
      theta[l] = soil.psi2theta(soil$clay[l], soil$sand[l], resday$psiVec[l])
    }
    df[i,"Theta"] = max(0,min(1,sum((theta/soil$Theta_FC)*soil$Water_FC)/sum(soil$Water_FC)))
    if(sum(is.na(theta))>0 || sum(is.na(soil$Water_FC))>0 || sum(soil$Water_FC)==0) df[i,"Theta"] =NA
  }
  spdf = SpatialPointsDataFrame(coordinates(IFN3_SPT),df,proj4string = IFN3_SPT@proj4string)
  save(spdf,file=paste0("Rdata/SpatialPointSWBMaps/", as.character(date), ".rda"))  
  
  #Mapping
  load("Rdata/Masks.rda")
  cc = IFN3_SPT@coords
  ow = as.owin(as.vector(t(IFN3_SPT@bbox)))
  ccm = coordinates(masks)
  #PET
  pet.smooth = Smooth(ppp(x=cc[!is.na(df$PET),1], y=cc[!is.na(df$PET),2], window = ow, 
                 marks = df$PET[!is.na(df$PET)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  pet.sgdf = as.SpatialGridDataFrame.im(pet.smooth)
  pet.sgdf@data[,1] = pmax(pet.sgdf@data[,1],0)
  pet.sgdf@data[!masks$Forest,]=NA
  pet.sgdf@proj4string = masks@proj4string
  spdf = pet.sgdf
  save(spdf,file=paste0("Rdata/SmoothedSWBMaps/PET/", as.character(date), ".rda"))
  #Rain
  rain.smooth = Smooth(ppp(x=cc[!is.na(df$Rain),1], y=cc[!is.na(df$Rain),2], window = ow, 
                           marks = df$Rain[!is.na(df$Rain)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  rain.sgdf = as.SpatialGridDataFrame.im(rain.smooth)
  rain.sgdf@data[,1] = pmax(rain.sgdf@data[,1],0)
  rain.sgdf@data[!masks$Forest,]=NA
  rain.sgdf@proj4string = masks@proj4string
  spdf = rain.sgdf
  save(spdf,file=paste0("Rdata/SmoothedSWBMaps/Rain/", as.character(date), ".rda"))
  #NetPrec
  netprec.smooth = Smooth(ppp(x=cc[!is.na(df$NetPrec),1], y=cc[!is.na(df$NetPrec),2], window = ow, 
                          marks = df$NetPrec[!is.na(df$NetPrec)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  netprec.sgdf = as.SpatialGridDataFrame.im(netprec.smooth)
  netprec.sgdf@data[,1] = pmax(netprec.sgdf@data[,1],0)
  netprec.sgdf@data[!masks$Forest,]=NA
  netprec.sgdf@proj4string = masks@proj4string
  spdf = netprec.sgdf
  save(spdf,file=paste0("Rdata/SmoothedSWBMaps/NetPrec/", as.character(date), ".rda"))
  #Runoff
  runoff.smooth = Smooth(ppp(x=cc[!is.na(df$Runoff),1], y=cc[!is.na(df$Runoff),2], window = ow, 
                              marks = df$Runoff[!is.na(df$Runoff)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  runoff.sgdf = as.SpatialGridDataFrame.im(runoff.smooth)
  runoff.sgdf@data[,1] = pmax(runoff.sgdf@data[,1],0)
  runoff.sgdf@data[!masks$Forest,]=NA
  runoff.sgdf@proj4string = masks@proj4string
  spdf = runoff.sgdf
  save(spdf,file=paste0("Rdata/SmoothedSWBMaps/Runoff/", as.character(date), ".rda"))
  #Deep Drainage
  deep.smooth = Smooth(ppp(x=cc[!is.na(df$DeepDrainage),1], y=cc[!is.na(df$DeepDrainage),2], window = ow, 
                             marks = df$DeepDrainage[!is.na(df$DeepDrainage)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  deep.sgdf = as.SpatialGridDataFrame.im(deep.smooth)
  deep.sgdf@data[,1] = pmax(deep.sgdf@data[,1],0)
  deep.sgdf@data[!masks$Forest,]=NA
  deep.sgdf@proj4string = masks@proj4string
  spdf = deep.sgdf
  save(spdf,file=paste0("Rdata/SmoothedSWBMaps/DeepDrainage/", as.character(date), ".rda"))
  #LAI
  lai.smooth = Smooth(ppp(x=cc[!is.na(df$LAI),1], y=cc[!is.na(df$LAI),2], window = ow, 
                           marks = df$LAI[!is.na(df$LAI)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  lai.sgdf = as.SpatialGridDataFrame.im(lai.smooth)
  lai.sgdf@data[,1] = pmax(lai.sgdf@data[,1],0)
  lai.sgdf@data[!masks$Forest,]=NA
  lai.sgdf@proj4string = masks@proj4string
  spdf = lai.sgdf
  save(spdf,file=paste0("Rdata/SmoothedSWBMaps/LAI/", as.character(date), ".rda"))
  #Eplant
  eplant.smooth = Smooth(ppp(x=cc[!is.na(df$Eplant),1], y=cc[!is.na(df$Eplant),2], window = ow, 
                          marks = df$Eplant[!is.na(df$Eplant)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  eplant.sgdf = as.SpatialGridDataFrame.im(eplant.smooth)
  eplant.sgdf@data[,1] = pmax(eplant.sgdf@data[,1],0)
  eplant.sgdf@data[!masks$Forest,]=NA
  eplant.sgdf@proj4string = masks@proj4string
  spdf = eplant.sgdf
  save(spdf,file=paste0("Rdata/SmoothedSWBMaps/Eplant/", as.character(date), ".rda"))
  #Esoil
  esoil.smooth = Smooth(ppp(x=cc[!is.na(df$Esoil),1], y=cc[!is.na(df$Esoil),2], window = ow, 
                             marks = df$Esoil[!is.na(df$Esoil)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  esoil.sgdf = as.SpatialGridDataFrame.im(esoil.smooth)
  esoil.sgdf@data[,1] = pmax(esoil.sgdf@data[,1],0)
  esoil.sgdf@data[!masks$Forest,]=NA
  esoil.sgdf@proj4string = masks@proj4string
  spdf = esoil.sgdf
  save(spdf,file=paste0("Rdata/SmoothedSWBMaps/Esoil/", as.character(date), ".rda"))
  #Theta
  theta.smooth = Smooth(ppp(x=cc[!is.na(df$Theta),1], y=cc[!is.na(df$Theta),2], window = ow, 
                            marks = df$Theta[!is.na(df$Theta)]), sigma=radius, at="pixels", xy=list(x=ccm[,1], y=ccm[,2]))
  theta.sgdf = as.SpatialGridDataFrame.im(theta.smooth)
  theta.sgdf@data[,1] = pmin(pmax(theta.sgdf@data[,1],0),1)
  theta.sgdf@data[!masks$Forest,]=NA
  theta.sgdf@proj4string = masks@proj4string
  spdf = theta.sgdf
  save(spdf,file=paste0("Rdata/SmoothedSWBMaps/Theta/", as.character(date), ".rda"))

}
