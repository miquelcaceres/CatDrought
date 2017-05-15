# App lib requisites
library(sp)
library(rgdal)
library(raster)


# App data requisites
mapCRS  <- CRS("+init=epsg:4326")
catpolCRS <- CRS("+init=epsg:23031")
dataCRS <- CRS("+init=epsg:25831")
# Catalunya boundaries
cat.pol <- readOGR(dsn = path.expand("www/Comarques shapefile"), encoding = "UTF-8")
proj4string(cat.pol) <- catpolCRS
cat.pol <- spTransform(cat.pol, CRSobj = mapCRS)
# Municipalities boundaries
mun.pol <- readOGR(dsn = path.expand("www/Municipis shapefile"), encoding = "UTF-8")
proj4string(mun.pol) <- dataCRS
mun.pol <- spTransform(mun.pol, CRSobj = mapCRS)
# Watershed boundaries
con.pol <- readOGR(dsn = path.expand("www/Conques shapefile"), encoding = "UTF-8")
proj4string(con.pol) <- dataCRS
con.pol <- spTransform(con.pol, CRSobj = mapCRS)


# Rasters grid topology
gt1km=GridTopology(c(260000,4498000), c(1000,1000), c(262,253))
# IFN plots
load("//SERVERPROCESS/Miquel/CatDrought/Rdata/IFN3_SPT_cat.rdata")
IFN3.points <- SpatialPointsDataFrame(IFN3_SPT@coords, data.frame(ID = row.names(IFN3_SPT@coords)), proj4string = IFN3_SPT@proj4string)
IFN3.points <- spTransform(IFN3.points, CRSobj = mapCRS)
# Find county and municipality corresponding to each IFN plot
op <- over(x = IFN3.points, y = con.pol, returnList = F)
IFN3.points$ID_CONCA <- op$ID_USUARI
IFN3.points$NOM_CONCA <- op$CONCA
op <- over(x = IFN3.points, y = cat.pol, returnList = F)
IFN3.points$COMARCA <- op$COMARCA
IFN3.points$NOM_COMAR <- op$NOM_COMAR
op <- over(x = IFN3.points, y = mun.pol, returnList = F)
IFN3.points$MUNICIPI <- op$MUNICIPI
IFN3.points$NOM_MUNI <- op$NOM_MUNI
IFN3.points <- IFN3.points[!is.na(IFN3.points$COMARCA) & !is.na(IFN3.points$MUNICIPI),]
# Buffer around IFN points 
# load(file = "~/These/GIS/IFN3 points buffer - 500m radius.RData")
# IFN3.buffer <- spTransform(IFN3.buffer, CRSobj = mapCRS)

## Data input
folder_daily_trends <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/SWBTrends"
available_plots_trends <- list.files(folder_daily_trends)
available_plots_trends <- unlist(strsplit(available_plots_trends,split = ".rda"))
folder_projections <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/ProjectedSWB/CCLM4-8-17/rcp4.5/"
available_plots_projections <- list.files(folder_projections)
available_plots_projections <- unlist(strsplit(available_plots_projections,split = ".rda"))
folder_historic <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/HistoricSWB/"
available_plots_historic <- list.files(folder_historic)
available_plots_historic <- unlist(strsplit(available_plots_historic,split = ".rda"))




## Variable names correspondance between ui and medfate outputs
input_clim_var <- c("Precipitation (mm)", "Potential evapo-transpiration (mm)", "SPEI (k=3)", "SPEI (k=6)", "SPEI (k=12)")
medfate_clim_var <- c("Rain", "PET", "spei3","spei6","spei12")
clim_variables <- data.frame(input = input_clim_var, medfate = medfate_clim_var)

input_WB_var <- c("Net precipitation (mm)", "LAI (m2/m2)","Plants transpiration (mm)", "Soil evaporation (mm)", "Run-off (mm)", "Deep drainage (mm)", 
                  "Relative soil water content [0-1]")
medfate_WB_var <- c("NetPrec", "LAI", "Eplant", "Esoil", "Runoff", "DeepDrainage", "Theta")
WB_variables <- data.frame(input = input_WB_var, medfate = medfate_WB_var)

input_DS_var <-c("Daily stress", "Cumulative stress")
medfate_DS_var<-c("DDS","NDD")
DS_variables <- data.frame(input = input_DS_var, medfate = medfate_DS_var)

input_sp <- c("All woody species", "Pinus halepensis", "Pinus nigra", "Pinus sylvestris", "Pinus uncinata", "Pinus pinea", 
              "Pinus pinaster", "Quercus ilex", "Quercus suber", "Quercus humilis", "Quercus faginea", "Fagus sylvatica")
medfate_sp <- c("Overall", "PinusHalepensis", "PinusNigra", "PinusSylvestris", "PinusUncinata", "PinusPinea", "PinusPinaster", 
                "QuercusIlex", "QuercusSuber", "QuercusHumilis", "QuercusFaginea", "FagusSylvatica")
species <- data.frame(input = input_sp, medfate = medfate_sp)

climate_models<-c("CCLM4-8-17","RCA4")
names(climate_models)<-c("CNRM/CCLM4-8-17", "CNRM/RCA4")




## Define color scales for rasters 
pal_clim <- as.data.frame(matrix(NA, nrow = length(input_clim_var), ncol = 5, dimnames = list(input_clim_var, c("min", "max", "color", "trans", "rev"))))
pal_clim$min <- 0
pal_clim$color <- "Spectral"
pal_clim$trans <- "log"
pal_clim$rev <- F
pal_clim[c("Precipitation (mm)"), "max"] <- 100
pal_clim[c("Potential evapo-transpiration (mm)"), "max"] <- 15
pal_clim[c("Precipitation (mm)"), "rev"] <- F
pal_clim[c("Precipitation (mm)"), "color"] <- "Blues"
pal_clim[c("Potential evapo-transpiration (mm)"), "color"] <- "Greens"

## Define color scales for rasters 
pal_WB <- as.data.frame(matrix(NA, nrow = length(input_WB_var), ncol = 5, dimnames = list(input_WB_var, c("min", "max", "color", "trans", "rev"))))
pal_WB$min <- 0
pal_WB$color <- "Spectral"
pal_WB$trans <- "log"
pal_WB$rev <- F
pal_WB[c("Net precipitation (mm)"), "max"] <- 100
pal_WB[c("Plants transpiration (mm)", "Soil evaporation (mm)"), "max"] <- 5
pal_WB[c("Run-off (mm)", "Deep drainage (mm)"), "max"] <- 15
pal_WB["Relative soil water content [0-1]", "max"] <- 1
pal_WB[c("Net precipitation (mm)"), "rev"] <- F
pal_WB[c("Net precipitation (mm)"), "color"] <- "Blues"
pal_WB[c("Plants transpiration (mm)", "Soil evaporation (mm)"), "color"] <- "Greens"
pal_WB[c("Run-off (mm)", "Deep drainage (mm)"), "color"] <- "Reds"
pal_WB["Relative soil water content [0-1]", "color"] <- "RdYlBu"
pal_WB["Relative soil water content [0-1]", "trans"] <- "identity"
pal_WB["LAI (m2/m2)", "max"] <- 9.5
pal_WB["LAI (m2/m2)", "trans"] <- "identity"
pal_WB["LAI (m2/m2)", "color"] <- "Greens"

## Define color scales for rasters 
pal_DS <- as.data.frame(matrix(NA, nrow = length(input_DS_var), ncol = 5, dimnames = list(input_DS_var, c("min", "max", "color", "trans", "rev"))))
pal_DS$min <- 0
pal_DS$rev <- T
pal_DS$color <- "RdYlBu"
pal_DS["Daily stress","trans"] <- "identity"
pal_DS["Cumulative stress","trans"] <- "log"
pal_DS["Daily stress","max"]<-1
pal_DS["Cumulative stress","max"]<-365

log_trans <- function(dom, n = 10, digits = 1) {signif(exp(seq(log(dom[1]+1), log(dom[2]+1), length.out = n))-1, digits = digits)}
identity_trans <- function(dom, n = 10, digits = 1) {signif(seq(dom[1], dom[2], length.out = n), digits = digits)}
