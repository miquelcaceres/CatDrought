library(medfate)
library(spatstat)
library(maptools)

swbPointMapsCat<-function(date = Sys.Date(), radius = 3000, pixelres = 1000) {
  load("Rdata/IFN3_SPT_cat.rdata")
  load(paste0("Rdata/DailySWB/", as.character(date), ".rda"))  
  plotIDs = rownames(IFN3_SPT@coords)
  npoints = length(plotIDs)
  
  df = data.frame(matrix(NA,nrow=npoints, ncol =9))
  names(df)<-c("PET","Rain","NetPrec","Runoff", "DeepDrainage", "LAI", "Eplant","Esoil",  "Theta")
  pb = txtProgressBar(0, npoints, style = 3)
  for(i in 1:npoints) {
    setTxtProgressBar(pb,i)
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
    df[i,"Theta"] = sum((theta/soil$Theta_FC)*soil$Water_FC)/sum(soil$Water_FC)
    if(sum(is.na(theta))>0 || sum(is.na(soil$Water_FC))>0 || sum(soil$Water_FC)==0) df[i,"Theta"] =NA
  }
  spdf = SpatialPointsDataFrame(coordinates(IFN3_SPT),df,proj4string = IFN3_SPT@proj4string)
  save(spdf,file=paste0("Rdata/SpatialPointSWBMaps/", as.character(date), ".rda"))  
  
  #Smoothing
  cc = IFN3_SPT@coords
  ow = as.owin(as.vector(t(IFN3_SPT@bbox)))
  pet.smooth = Smooth(ppp(x=cc[!is.na(df$PET),1], y=cc[!is.na(df$PET),2], window = ow, 
                 marks = df$PET[!is.na(df$PET)]), sigma=radius, at="pixels", eps=c(pixelres,pixelres))
  pet.sgdf = as.SpatialGridDataFrame.im(pet.smooth)
  if("Rain" %in% names(resday)) {
    rain.smooth = Smooth(ppp(x=cc[!is.na(df$Rain),1], y=cc[!is.na(df$Rain),2], window = ow, 
                          marks = df$Rain[!is.na(df$Rain)]), sigma=radius, at="pixels", eps=c(pixelres,pixelres))
    rain.sgdf = as.SpatialGridDataFrame.im(rain.smooth)
  }
  netprec.smooth = Smooth(ppp(x=cc[!is.na(df$NetPrec),1], y=cc[!is.na(df$NetPrec),2], window = ow, 
                          marks = df$NetPrec[!is.na(df$NetPrec)]), sigma=radius, at="pixels", eps=c(pixelres,pixelres))
  netprec.sgdf = as.SpatialGridDataFrame.im(netprec.smooth)
  runoff.smooth = Smooth(ppp(x=cc[!is.na(df$Runoff),1], y=cc[!is.na(df$Runoff),2], window = ow, 
                              marks = df$Runoff[!is.na(df$Runoff)]), sigma=radius, at="pixels", eps=c(pixelres,pixelres))
  runoff.sgdf = as.SpatialGridDataFrame.im(runoff.smooth)
  deep.smooth = Smooth(ppp(x=cc[!is.na(df$DeepDrainage),1], y=cc[!is.na(df$DeepDrainage),2], window = ow, 
                             marks = df$DeepDrainage[!is.na(df$DeepDrainage)]), sigma=radius, at="pixels", eps=c(pixelres,pixelres))
  deep.sgdf = as.SpatialGridDataFrame.im(deep.smooth)
  lai.smooth = Smooth(ppp(x=cc[!is.na(df$LAI),1], y=cc[!is.na(df$LAI),2], window = ow, 
                           marks = df$LAI[!is.na(df$LAI)]), sigma=radius, at="pixels", eps=c(pixelres,pixelres))
  lai.sgdf = as.SpatialGridDataFrame.im(lai.smooth)
  eplant.smooth = Smooth(ppp(x=cc[!is.na(df$Eplant),1], y=cc[!is.na(df$Eplant),2], window = ow, 
                          marks = df$Eplant[!is.na(df$Eplant)]), sigma=radius, at="pixels", eps=c(pixelres,pixelres))
  eplant.sgdf = as.SpatialGridDataFrame.im(eplant.smooth)
  esoil.smooth = Smooth(ppp(x=cc[!is.na(df$Esoil),1], y=cc[!is.na(df$Esoil),2], window = ow, 
                             marks = df$Esoil[!is.na(df$Esoil)]), sigma=radius, at="pixels", eps=c(pixelres,pixelres))
  esoil.sgdf = as.SpatialGridDataFrame.im(esoil.smooth)
  theta.smooth = Smooth(ppp(x=cc[!is.na(df$Theta),1], y=cc[!is.na(df$Theta),2], window = ow, 
                            marks = df$Theta[!is.na(df$Theta)]), sigma=radius, at="pixels", eps=c(pixelres,pixelres))
  theta.sgdf = as.SpatialGridDataFrame.im(theta.smooth)
  
  spdf = pet.sgdf
  names(spdf@data) = "PET"
  spdf@data$NetPrec = netprec.sgdf@data[,1]
  spdf@data$Runoff = runoff.sgdf@data[,1]
  spdf@data$DeepDrainage = deep.sgdf@data[,1]
  spdf@data$LAI = lai.sgdf@data[,1]
  spdf@data$Eplant = eplant.sgdf@data[,1]
  spdf@data$Esoil = esoil.sgdf@data[,1]
  spdf@data$Theta = theta.sgdf@data[,1]
  save(spdf,file=paste0("Rdata/SmoothedSWBMaps/", as.character(date), ".rda"))  
}
