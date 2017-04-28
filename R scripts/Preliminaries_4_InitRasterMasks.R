library(maptools)
library(medfate)
#Load shape files and transform to UTM31
# tar_utm30 = readShapePoly("D:/Recerca/Datasets/MFE/MFE50_43_tcm7-261604/mfe50_43.shp", proj4string = CRS("+proj=utm +zone=30"))
# tar_utm31 = spTransform(tar_utm30, CRS("+proj=utm +zone=31"))
# rm(tar_utm30)
# 
# gir_utm30 = readShapePoly("D:/Recerca/Datasets/MFE/MFE50_17_tcm7-261557/mfe50_17.shp", proj4string = CRS("+proj=utm +zone=30"))
# gir_utm31 = spTransform(gir_utm30, CRS("+proj=utm +zone=31"))
# rm(gir_utm30)
# 
# lle_utm30 = readShapePoly("D:/Recerca/Datasets/MFE/MFE50_25_tcm7-261574/mfe50_25.shp", proj4string = CRS("+proj=utm +zone=30"))
# lle_utm31 = spTransform(lle_utm30, CRS("+proj=utm +zone=31"))
# rm(lle_utm30)
# 
# bcn_utm30 = readShapePoly("D:/Recerca/Datasets/MFE/MFE50_08_tcm7-261546/mfe50_08.shp", proj4string = CRS("+proj=utm +zone=30"))
# bcn_utm31 = spTransform(bcn_utm30, CRS("+proj=utm +zone=31"))
# rm(bcn_utm30)
# 


#Define grid topology
gt1km=GridTopology(c(260000,4498000), c(1000,1000), c(262,253))
mapcc=coordinates(gt1km)
sp=SpatialPoints(mapcc, CRS("+proj=utm +zone=31"))
load("D:/Recerca/Datasets/Limits/comarques.rdata")
comarques@proj4string =  CRS("+proj=utm +zone=31")
overcat=over(sp, comarques)
overcat = !is.na(overcat$COMARCA)

radius = 2000
load("Rdata/IFN3_SPF_cat.rdata")
# plot_ind = getGridIndex(IFN3_SPF@coords,gt1km, all.inside=TRUE)
ifncc = IFN3_SPF@coords
neighifn = vector("list", nrow(mapcc))
pb = txtProgressBar(0, nrow(mapcc), style = 3)
for(i in 1:nrow(mapcc)) {
  setTxtProgressBar(pb,i)
  d=sqrt(rowSums(sweep(ifncc,2,mapcc[i,])^2))
  neighifn[[i]] = which(d<radius)
}
formask = sapply(neighifn,FUN = length)>0
formask = formask & overcat

#Define mask rasters
maskdf = data.frame(matrix(FALSE, nrow=nrow(mapcc), ncol=12))
names(maskdf)<-c("PinusHalepensis","PinusNigra","PinusSylvestris","PinusUncinata", "PinusPinea", "PinusPinaster",
             "QuercusIlex","QuercusSuber",  "QuercusHumilis", "QuercusFaginea", "FagusSylvatica", "Forest")
maskdf$Forest = formask
# over_bcn=over(sp, bcn_utm31)
# over_tar=over(sp, tar_utm31)
# over_gir=over(sp, gir_utm31)
# over_lle=over(sp, lle_utm31)

#Forest mask
# forest = over_bcn$TFCCTOT>0
# forest[is.na(forest)] = FALSE
# maskdf$Forest = maskdf$Forest | forest
# forest = over_tar$TFCCTOT>0
# forest[is.na(forest)] = FALSE
# maskdf$Forest = maskdf$Forest | forest
# forest = over_lle$TFCCTOT>0
# forest[is.na(forest)] = FALSE
# maskdf$Forest = maskdf$Forest | forest
# forest = over_gir$TFCCTOT>0
# forest[is.na(forest)] = FALSE
# maskdf$Forest = maskdf$Forest | forest
# maskdf$Forest[plot_ind] = TRUE ## Plot coordinates

# coarserMask<-function(mask) {
#   out = mask
#   for(i in 1:n3km) {
#     if(sum(ind3km[mask]==i)<=2) {
#       out[ind3km==i] = FALSE
#     } else {
#       out[ind3km==i] = TRUE
#     }
#   } 
#   out[!overcat] = FALSE
#   return(out)
# }
# maskdf$Forest = coarserMask(maskdf$Forest)

maskSpecies<-function(codeMF=54, codeIFN=24) {
  # forest_sp_bcn = (over_bcn$SP1==codeIFN) | (over_bcn$SP2==codeIFN) | (over_bcn$SP3==codeIFN)
  # forest_sp_bcn[is.na(forest_sp_bcn)] = FALSE
  # forest_sp_lle = (over_lle$SP1==codeIFN) | (over_lle$SP2==codeIFN) | (over_lle$SP3==codeIFN)
  # forest_sp_lle[is.na(forest_sp_lle)] = FALSE
  # forest_sp_tar = (over_tar$SP1==codeIFN) | (over_tar$SP2==codeIFN) | (over_tar$SP3==codeIFN)
  # forest_sp_tar[is.na(forest_sp_tar)] = FALSE
  # forest_sp_gir = (over_gir$SP1==codeIFN) | (over_gir$SP2==codeIFN) | (over_gir$SP3==codeIFN)
  # forest_sp_gir[is.na(forest_sp_gir)] = FALSE
  
  #Add plots with cited species
  forest_sp = rep(FALSE, length(IFN3_SPF))
  for(i in 1:length(IFN3_SPF)){
      forest_sp[i] = codeMF %in% IFN3_SPF@forestlist[[i]]$treeData$Species 
  }
  inds =which(forest_sp)
  #Process points
  res = rep(FALSE, nrow(mapcc))
  pb = txtProgressBar(0, nrow(mapcc), style = 3)
  for(i in 1:nrow(mapcc)) {
    setTxtProgressBar(pb,i)
    res[i] = (sum(neighifn[[i]] %in% inds)>0) 
  }
  res = res & overcat
  return(res)
}
maskdf$PinusHalepensis = maskSpecies(54,24)
maskdf$PinusNigra = maskSpecies(55,25)
maskdf$PinusSylvestris = maskSpecies(59,21)
maskdf$PinusUncinata = maskSpecies(60,22)
maskdf$PinusPinea = maskSpecies(57,23)
maskdf$PinusPinaster = maskSpecies(56,26)
maskdf$QuercusIlex = maskSpecies(68,45)
maskdf$QuercusSuber = maskSpecies(72,46)
maskdf$QuercusHumilis = maskSpecies(67,243)
maskdf$QuercusFaginea = maskSpecies(66,44)
maskdf$FagusSylvatica = maskSpecies(37,71)


masks<-SpatialGridDataFrame(gt1km, maskdf, CRS("+proj=utm +zone=31"))
save(masks,file="Rdata/Masks.rda")
