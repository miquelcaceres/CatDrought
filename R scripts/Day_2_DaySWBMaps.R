library(medfate)
library(spatstat)

swbPointMapsCat<-function(date = Sys.Date(), radius = 5000) {
  load("Rdata/IFN3_SPT_cat.rdata")
  load(paste0("Rdata/DailySWB/", as.character(date), ".rda"))  
  plotIDs = rownames(IFN3_SPT@coords)
  npoints = length(plotIDs)
  cc = IFN3_SPT@coords
  ow = as.owin(as.vector(t(IFN3_SPT@bbox)))
  
  df = data.frame(matrix(NA,nrow=npoints, ncol =9))
  names(df)<-c("PET","Rain","NetPrec","Runoff", "DeepDrainage", "LAI", "Eplant","Esoil",  "REW")
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
    df[i,"REW"] = sum((theta/soil$Theta_FC)*soil$Water_FC)/sum(soil$Water_FC)
    if(sum(is.na(theta))>0 || sum(is.na(soil$Water_FC))>0 || sum(soil$Water_FC)==0) df[i,"REW"] =NA
  }
  spdf = SpatialPointsDataFrame(coordinates(IFN3_SPT),df,proj4string = IFN3_SPT@proj4string)
  save(spdf,file=paste0("Rdata/SpatialPointSWBMaps/", as.character(date), ".rda"))  
}
