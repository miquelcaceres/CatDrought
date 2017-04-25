library(medfate)

#Init point year trends
load("Rdata/IFN3_SPT_cat.rdata")
plotIDs = rownames(IFN3_SPT@coords)
nplots = length(plotIDs)

pb = txtProgressBar(0, nplots, style = 3)
for(i in 1:nplots) {
  plotID = plotIDs[i]
  setTxtProgressBar(pb,i)
  trends = data.frame(matrix(NA, nrow=365, ncol=9))
  names(trends)<-c("PET","Rain","NetPrec","Runoff", "DeepDrainage", "LAI", "Eplant","Esoil",  "Theta")
  save(trends,file=paste0("Rdata/PlotSWBYearTrends/", plotID, ".rda"))  
  
  trends = data.frame(matrix(NA,nrow=365, ncol =11))
  names(trends)<-c("PinusHalepensis","PinusNigra","PinusSylvestris","PinusUncinata", "PinusPinea", "PinusPinaster",
               "QuercusIlex","QuercusSuber",  "QuercusHumilis", "QuercusFaginea", "FagusSylvatica")
  save(trends,file=paste0("Rdata/PlotDroughtStressYearTrends/", plotID, ".rda"))  
}