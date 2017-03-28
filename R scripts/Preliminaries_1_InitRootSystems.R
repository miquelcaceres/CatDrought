library(medfate)

data("SpParamsMED")

load("Rdata/IFN3_SPF_cat.rdata")

load("InputData/Optimized RS DF - Miquel.RData")

oldcodemap = read.table("D:/Rpackages/medfate/speciescalibration/SpParamsOldCodeMap.txt", header=TRUE, sep="\t", row.names=1)

Z50.LDR <- function(V,Z,Z95){
  a <- log(V/(1-V))/2.94
  Z50 <- (Z/Z95^a)^(1/(1-a))
  return(Z50)
}

nplots = length(IFN3_SPF@forestlist)
pb = txtProgressBar(0, nplots, style = 3)
for(p in 1:nplots) {
  setTxtProgressBar(pb, p)
  # print(p)
  forest = IFN3_SPF@forestlist[[p]]
  soil = IFN3_SPF@soillist[[p]]
  
  m = opt.df[opt.df$ID==forest$ID,]
  ntree = nrow(forest$treeData)
  nshrub = nrow(forest$shrubData)
  
  if(sum(is.na(m$Zopt))==nrow(m)) {
    soil$SoilDepth = 500
    soil$dVec[2] = 500 - soil$dVec[1]
    if(ntree>0) {
      forest$treeData$Z95 = 500
      forest$treeData$Z50 = Z50.LDR(0.7,300,500)
    }
    if(nshrub>0) forest$shrubData$Z = 500
  } else {
    Zoptmax = round(max(max(m$Zopt, na.rm=TRUE),300)) #Find maximum rooting depth (at least 300 mm)
    soil$SoilDepth = Zoptmax
    soil$dVec[2] = Zoptmax - soil$dVec[1]
    if(ntree>0) {
      tree_spold = oldcodemap$OldSpIndex[forest$treeData$Species+1]
      for(i in 1:ntree) {
        indx = which(m$SP==tree_spold[i])
        if(length(indx)==1) {
          forest$treeData$Z95[i] = m$Zopt[indx]
          if(is.na(forest$treeData$Z95[i])) forest$treeData$Z95[i] = mean(m$Zopt, na.rm=TRUE)
          v1 = m$V1opt[indx]
          if(is.na(v1)) v1 =  mean(m$V1opt, na.rm=TRUE)
          v1 = min(v1, 0.95)
          forest$treeData$Z50[i] =  max(50,Z50.LDR(v1,300, forest$treeData$Z95[i]))
        } else {
          forest$treeData$Z95[i] = mean(m$Zopt, na.rm=TRUE)
          v1 = mean(m$V1opt, na.rm=TRUE)
          v1 = min(v1, 0.95)
          forest$treeData$Z50[i] =  max(50,Z50.LDR(v1,300, forest$treeData$Z95[i]))
        }
      }
    }
    if(nshrub>0) {
      shrub_spold = oldcodemap$OldSpIndex[forest$shrubData$Species+1]
      for(i in 1:nshrub) {
        indx = which(m$SP==shrub_spold[i])
        if(length(indx)==1) {
          forest$shrubData$Z[i] = m$Zopt[indx]
          if(is.na(forest$shrubData$Z[i])) forest$shrubData$Z[i] = mean(m$Zopt, na.rm=TRUE)
        } else {
          forest$shrubData$Z[i] = mean(m$Zopt, na.rm=TRUE)
        }
      }
    }
  }
  forest$treeData$Z95 = round(forest$treeData$Z95)
  forest$treeData$Z50 = round(forest$treeData$Z50)
  forest$shrubData$Z = round(forest$shrubData$Z)
  soil$Water_FC = soil$dVec*soil$Theta_FC*(1-(soil$rfc/100))
  if(sum(is.na(forest$treeData))>0) stop("kk")
  if(sum(is.na(forest$shrubData))>0) stop("kk")
  IFN3_SPF@forestlist[[p]] = forest
  IFN3_SPF@soillist[[p]] = soil 
}

save(IFN3_SPF, file="Rdata/IFN3_SPF_ROOT_cat.rdata")

