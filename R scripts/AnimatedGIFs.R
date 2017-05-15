
dates = seq(as.Date("2017-01-01"), as.Date("2017-05-07"), by="day")
weeks = cut(dates,breaks="week")
nweeks = length(levels(weeks))

vars = c("PET","Rain","NetPrec","Runoff", "DeepDrainage", "LAI", "Eplant","Esoil",  "Theta")
funs = c("sum","sum","sum","sum","sum","mean","sum","sum","mean")
base = "//SERVERPROCESS/Miquel/CatDrought"
width = 700
height = 700
load("D:/Recerca/Datasets/Limits/catalonia.rdata")



## Define color scales for SWB rasters 
pal_WB <- as.data.frame(matrix(NA, nrow = length(vars), ncol = 5, dimnames = list(vars, c("min", "max", "color", "trans", "rev"))))
pal_WB$min <- 0
pal_WB$color <- "RdYlBu"
pal_WB$trans <- "log"
pal_WB$rev <- F
pal_WB[c("Rain", "NetPrec"), "max"] <- 100
pal_WB[c("PET"), "max"] <- 15
pal_WB[c("Eplant", "Esoil"), "max"] <- 5
pal_WB[c("Runoff", "Deep drainage"), "max"] <- 15
pal_WB["Theta", "max"] <- 1
pal_WB[c("Rain", "Net precipitation"), "rev"] <- F
pal_WB[c("Rain", "Net precipitation"), "color"] <- "Blues"
pal_WB[c("PET", "Eplant", "Esoil"), "color"] <- "Greens"
pal_WB[c("Runoff", "DeepDrainage"), "color"] <- "Reds"
pal_WB["Theta", "color"] <- "RdYlBu"
pal_WB["Theta", "trans"] <- "identity"
pal_WB["LAI", "max"] <- 9.5
pal_WB["LAI", "trans"] <- "identity"
pal_WB["LAI", "color"] <- "Greens"

log_trans <- function(dom, n = 10, digits = 1) {signif(exp(seq(log(dom[1]+1), log(dom[2]+1), length.out = n))-1, digits = digits)}
identity_trans <- function(dom, n = 10, digits = 1) {signif(seq(dom[1], dom[2], length.out = n), digits = digits)}


for(res in c("1km", "Smoothed")) {
  for(i in c(1,6,7,9)) {
  # for(i in c(9)) {
      var = vars[i]
    cat(paste("\n\n Processing", var, "at", res,"\n"))
    minv = pal_WB[var,"min"]
    maxv = pal_WB[var,"max"]
    b = seq(minv,maxv, length.out=20)
    if(pal_WB[var, "color"]=="Greens") cols = colorRampPalette(c("white","dark green"))(length(b)-1)
    else if(pal_WB[var, "color"]=="Reds") cols = colorRampPalette(c("white","dark red"))(length(b)-1)
    else if(pal_WB[var, "color"]=="Blues") cols = colorRampPalette(c("white","dark blue"))(length(b)-1)
    else if(pal_WB[var, "color"]=="RdYlBu") cols = colorRampPalette(c("dark red","yellow","dark blue"))(length(b)-1)
    pb = txtProgressBar(0, length(levels(weeks)), style=3)
    for(j in 1:length(levels(weeks))) {
      setTxtProgressBar(pb, j)
      w = levels(weeks)[j]
      # w = levels(weeks)[1] 
      dw = dates[weeks==w]
      nd = length(dw)
      load(paste0(base,"/Rdata/Maps/",res,"/SWB/",var,"/",dw[1],".rda"))
      spdftmp = spdf
      if(nd>1) {
        for(d in 2:nd) {
          load(paste0(base,"/Rdata/Maps/",res,"/SWB/",var,"/",dw[d],".rda"))
          spdftmp@data = spdftmp@data + spdf@data
        }
      }
      spdftmp@data =spdftmp@data /nd
      spdf = spdftmp
      js = as.character(j)
      if(nchar(js)==1) js = paste0("0",js)
      png(paste0("GIFs/", res,"/",var,"/",var,"_",js,".png"), width=width, height=height, bg="light gray")
      par(mar=c(0.5,0.5,3,0.5))
      image(spdf[var], breaks = b, main=paste0("Week ", j, ": ", dw[1], " to ", dw[length(dw)]), col=cols)
      plot(cat.contour, add=TRUE, lwd=1.5)
      dev.off()
    }
  }
}



medfate_sp <- c("Overall", "PinusHalepensis", "PinusNigra", "PinusSylvestris", "PinusUncinata", "PinusPinea", "PinusPinaster", 
                "QuercusIlex", "QuercusSuber", "QuercusHumilis", "QuercusFaginea", "FagusSylvatica")


for(res in c("1km", "Smoothed")) {
  for(i in 1:1) {
    # for(i in c(9)) {
    var = medfate_sp[i]
    cat(paste("\n\n Processing", var, "at", res,"\n"))
    minv = 0
    maxv = 1
    b = seq(minv,maxv, length.out=15)
    cols = colorRampPalette(c("dark blue","yellow","dark red"))(length(b)-1)
    pb = txtProgressBar(0, length(levels(weeks)), style=3)
    for(j in 1:length(levels(weeks))) {
      setTxtProgressBar(pb, j)
      w = levels(weeks)[j]
      # w = levels(weeks)[1] 
      dw = dates[weeks==w]
      nd = length(dw)
      load(paste0(base,"/Rdata/Maps/",res,"/DroughtStress/",var,"/",dw[1],".rda"))
      spdftmp = spdf
      if(nd>1) {
        for(d in 2:nd) {
          load(paste0(base,"/Rdata/Maps/",res,"/DroughtStress/",var,"/",dw[d],".rda"))
          spdftmp@data = spdftmp@data + spdf@data
        }
      }
      spdftmp@data =spdftmp@data /nd
      spdf = spdftmp
      js = as.character(j)
      if(nchar(js)==1) js = paste0("0",js)
      png(paste0("GIFs/", res,"/",var,"/",var,"_",js,".png"), width=width, height=height, bg="light gray")
      par(mar=c(0.5,0.5,3,0.5))
      image(spdf[var], breaks = b, main=paste0("Week ", j, ": ", dw[1], " to ", dw[length(dw)]), col=cols)
      plot(cat.contour, add=TRUE, lwd=1.5)
      dev.off()
    }
  }
}

# system2("convert -delay=80 *.png LAI.gif")
# for(i in 1:length(vars)) {
