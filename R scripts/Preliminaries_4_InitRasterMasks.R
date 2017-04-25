library(maptools)

#Load shape files and transform to UTM31
tar_utm30 = readShapePoly("D:/Recerca/Datasets/MFE/MFE50_43_tcm7-261604/mfe50_43.shp", proj4string = CRS("+proj=utm +zone=30"))
tar_utm31 = spTransform(tar_utm30, CRS("+proj=utm +zone=31"))
rm(tar_utm30)

gir_utm30 = readShapePoly("D:/Recerca/Datasets/MFE/MFE50_17_tcm7-261557/mfe50_17.shp", proj4string = CRS("+proj=utm +zone=30"))
gir_utm31 = spTransform(gir_utm30, CRS("+proj=utm +zone=31"))
rm(gir_utm30)

lle_utm30 = readShapePoly("D:/Recerca/Datasets/MFE/MFE50_25_tcm7-261574/mfe50_25.shp", proj4string = CRS("+proj=utm +zone=30"))
lle_utm31 = spTransform(lle_utm30, CRS("+proj=utm +zone=31"))
rm(lle_utm30)

bcn_utm30 = readShapePoly("D:/Recerca/Datasets/MFE/MFE50_08_tcm7-261546/mfe50_08.shp", proj4string = CRS("+proj=utm +zone=30"))
bcn_utm31 = spTransform(bcn_utm30, CRS("+proj=utm +zone=31"))
rm(bcn_utm30)

#Define grid topology
gt1km=GridTopology(c(260500,4498500), c(1000,1000), c(260,250))
mapcc=coordinates(gt1km)
max(mapcc[,1])
max(mapcc[,2])
sp=SpatialPoints(mapcc, CRS("+proj=utm +zone=31"))
gr = SpatialGrid(gt1km, CRS("+proj=utm +zone=31"))
#Define mask rasters
maskdf = data.frame(matrix(FALSE, nrow=nrow(mapcc), ncol=12))
names(maskdf)<-c("PinusHalepensis","PinusNigra","PinusSylvestris","PinusUncinata", "PinusPinea", "PinusPinaster",
             "QuercusIlex","QuercusSuber",  "QuercusHumilis", "QuercusFaginea", "FagusSylvatica", "Forest")
over_bcn=over(sp, bcn_utm31)
over_tar=over(sp, tar_utm31)
over_gir=over(sp, gir_utm31)
over_lle=over(sp, lle_utm31)

#Forest mask
forest = over_bcn$TFCCTOT>0
forest[is.na(forest)] = FALSE
maskdf$Forest = maskdf$Forest | forest
forest = over_tar$TFCCTOT>0
forest[is.na(forest)] = FALSE
maskdf$Forest = maskdf$Forest | forest
forest = over_lle$TFCCTOT>0
forest[is.na(forest)] = FALSE
maskdf$Forest = maskdf$Forest | forest
forest = over_gir$TFCCTOT>0
forest[is.na(forest)] = FALSE
maskdf$Forest = maskdf$Forest | forest


maskSpecies<-function(code=24) {
  forest_sp_bcn = (over_bcn$SP1==code) | (over_bcn$SP2==code) | (over_bcn$SP3==code)
  forest_sp_bcn[is.na(forest_sp_bcn)] = FALSE
  forest_sp_lle = (over_lle$SP1==code) | (over_lle$SP2==code) | (over_lle$SP3==code)
  forest_sp_lle[is.na(forest_sp_lle)] = FALSE
  forest_sp_tar = (over_tar$SP1==code) | (over_tar$SP2==code) | (over_tar$SP3==code)
  forest_sp_tar[is.na(forest_sp_tar)] = FALSE
  forest_sp_gir = (over_gir$SP1==code) | (over_gir$SP2==code) | (over_gir$SP3==code)
  forest_sp_gir[is.na(forest_sp_gir)] = FALSE
  return(forest_sp_bcn|forest_sp_lle|forest_sp_gir|forest_sp_tar)
}
maskdf$PinusHalepensis = maskSpecies(24)
maskdf$PinusNigra = maskSpecies(25)
maskdf$PinusSylvestris = maskSpecies(21)
maskdf$PinusUncinata = maskSpecies(22)
maskdf$PinusPinea = maskSpecies(23)
maskdf$PinusPinaster = maskSpecies(26)
maskdf$QuercusIlex = maskSpecies(45)
maskdf$QuercusSuber = maskSpecies(46)
maskdf$QuercusHumilis = maskSpecies(44)
maskdf$QuercusFaginea = maskSpecies(44)
maskdf$FagusSylvatica = maskSpecies(71)

masks<-SpatialGridDataFrame(gt1km, maskdf, CRS("+proj=utm +zone=31"))
save(masks,file="Rdata/Masks.rda")
