library(medfate)

#Species data
data("SpParamsMED")
SpParamsMED$IFNcodes = as.character(SpParamsMED$IFNcodes)


###########################
#   IFN3 (existencias)
###########################


#load IFN3 coordinates
ifn3_xy <- read.delim("InputData/ifn3_xy_cat_unique.txt", row.names=1, header=TRUE)
coords = ifn3_xy[,-1]
rownames(coords) = ifn3_xy[,1]
ifn3_sp = SpatialPoints(coords, CRS("+proj=utm +zone=31"))
#LOAD IFN3 topography
ifn3_topo =read.table(file="InputData/ifn3_topo.txt", header=TRUE, row.names=1, sep="\t")


#Create spatial points topography
IFN3_SPT<-SpatialPointsTopography(ifn3_sp, ifn3_topo$elevation, ifn3_topo$slope, ifn3_topo$aspect)

# TO SAVE RESULTS 
save(IFN3_SPT, file="Rdata/IFN3_SPT_cat.rdata")

#LOAD IFN3 data
load("InputData/shrubdata_exist_ifn3_catalunya.rdata")
load("InputData/treedata_exist_ifn3_catalunya.rdata")


#Replace regenerate as shrubs for shrub species
c =translateSpeciesCodes(treeDataIFN3, as.character(SpParamsMED$IFNcodes))
ids = which((SpParamsMED$GrowthForm[c+1]=="Shrub") & (treeDataIFN3$REG!=-1))
newShrubData = data.frame(ID = treeDataIFN3$ID[ids],
                          Especie = treeDataIFN3$Especie[ids],
                          FCC = (treeDataIFN3$N[ids]/127.3239546)*2.5,
                          Ht = treeDataIFN3$Ht[ids],
                          REG = treeDataIFN3$REG[ids],
                          Especie_MF = treeDataIFN3$Especie_MF[ids])
shrubDataIFN3 = rbind(shrubDataIFN3, newShrubData)
treeDataIFN3 = treeDataIFN3[-ids, ]

#remove records with missing data
treeDataIFN3 = treeDataIFN3[!is.na(treeDataIFN3$N), ]

#Load Herb data
# herbDataIFN3= read.delim("Data/ifn/ModelCombustible.txt", header=TRUE)
# herbDataIFN3 = data.frame(ID=herbDataIFN3$IdParcela, Cover = herbDataIFN3$HerbaciFcc, Height=herbDataIFN3$HerbaciHm)
# sel = herbDataIFN3$Height>=10
# sel[is.na(sel)] = FALSE
# herbDataIFN3$Height[sel] = herbDataIFN3$Height[sel]/10
# herbDataIFN3$Height = herbDataIFN3$Height*10


#LOAD soil data
SoilIFN3 <- read.delim("InputData/ifn3_soil.txt", stringsAsFactors=FALSE, dec=",", row.names=1)
SoilIFN3<-SoilIFN3[,!(names(SoilIFN3) %in% c("LT","PM","CRACKS","SD_ACCUA","ROCOSID", "TEXTURA", "ref_depth"))]
SoilIFN3 = SoilIFN3[rownames(coordinates(ifn3_sp)),]
STT = read.table("InputData/SoilTextureTable.txt",sep="\t",header=TRUE)[,-1]
SoilIFN3$TextType_TS = 2
SoilIFN3$TextType_TS[SoilIFN3$TS_clay > 35] = 1 
SoilIFN3$TextType_TS[SoilIFN3$TS_clay < 18 & SoilIFN3$TS_sand > 65] = 3 
SoilIFN3$TextType_SS = 2
SoilIFN3$TextType_SS[SoilIFN3$SS_clay > 35] = 1 
SoilIFN3$TextType_SS[SoilIFN3$SS_clay < 18 & SoilIFN3$SS_sand > 65] = 3 
SoilIFN3$Gsoil=STT$Gsoil[SoilIFN3$TextType_TS]
SoilIFN3$Ksoil=STT$Ksoil[SoilIFN3$TextType_TS]
#Set rock fragment content
SoilIFN3$TS_rfc = pmax(SoilIFN3$TS_gravel,SoilIFN3$ROCPERC, na.rm=TRUE)
SoilIFN3$SS_rfc = pmax(SoilIFN3$SS_gravel,SoilIFN3$ROCPERC, na.rm=TRUE)  
#Soil depth
SoilIFN3$SoilDepth = 1000
#Rock layer
SoilIFN3$RockLayerDepth = 0
SoilIFN3$RL_rfc = 95
SoilIFN3$RL_clay = SoilIFN3$SS_clay
SoilIFN3$RL_sand = SoilIFN3$SS_sand
SoilIFN3$RL_silt = SoilIFN3$SS_silt
SoilIFN3$RL_macro = SoilIFN3$SS_macro

#Turn to a numeric matrix
SoilIFN3 = apply(SoilIFN3,2,as.numeric) 
rownames(SoilIFN3) = rownames(coordinates(ifn3_sp))


control = defaultControl()
control$verbose=TRUE

#Create spatial points forest
IFN3_SPF<-SpatialPointsForest(treeDataIFN3,shrubDataIFN3, NULL, ifn3_sp, 
                                ifn3_topo$elevation, ifn3_topo$slope, ifn3_topo$aspect,
                                SpParams = SpParamsMED, 
                                SoilParamData = SoilIFN3, 
                                SFIcodes = SpParamsMED$IFNcodes, 
                                control=control)

# TO SAVE RESULTS 
save(IFN3_SPF, file="Rdata/IFN3_SPF_cat.rdata")
