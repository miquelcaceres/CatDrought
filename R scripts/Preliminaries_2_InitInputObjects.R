library(medfate)

data("SpParamsMED")

load("Rdata/IFN3_SPF_ROOT_cat.rdata")

nplots = length(IFN3_SPF@forestlist)

pb = txtProgressBar(0, nplots, style = 3)
for(i in 1:nplots) {
  setTxtProgressBar(pb, i)
  soil = IFN3_SPF@soillist[[i]]
  forest = IFN3_SPF@forestlist[[i]]
  x = forest2swbInput(forest, soil, SpParamsMED, defaultControl())
  save(x,soil, file=paste0("Rdata/Plots/",forest$ID,".rda"))
}