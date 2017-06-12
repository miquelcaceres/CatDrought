library(shiny)
# App lib requisites
library(sp)
library(rgdal)
library(viridis)
library(raster)
library(scales)
library(chron)
library(leaflet)
library(dygraphs)
library(xts)
library(Kendall)
library(zyp)
library(meteoland)

if(Sys.info()["sysname"]=="Windows") { #Windows machine
  data_home <- "//SERVERPROCESS/Miquel/CatDrought/"
} else { #Linux server
  data_home <- "/home/miquel/serverprocess/"
}

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
# proj4string(mun.pol) <- dataCRS # the original file already has a (identical) crs
mun.pol <- spTransform(mun.pol, CRSobj = mapCRS)
# Watershed boundaries
con.pol <- readOGR(dsn = path.expand("www/Conques shapefile"), encoding = "UTF-8")
proj4string(con.pol) <- catpolCRS # the original file already has a crs but it seems wrong
con.pol <- spTransform(con.pol, CRSobj = mapCRS)



# Rasters grid topology
gt1km=GridTopology(c(260000,4498000), c(1000,1000), c(262,253))
# IFN plots
load(paste0(data_home,"Rdata/IFN3_SPT_cat.rdata"))
IFN3.points <- SpatialPointsDataFrame(IFN3_SPT@coords, data.frame(ID = row.names(IFN3_SPT@coords)), proj4string = IFN3_SPT@proj4string)
IFN3.points <- spTransform(IFN3.points, CRSobj = mapCRS)
IFN3.points$elevation = IFN3_SPT$elevation
IFN3.points$slope = IFN3_SPT$slope
IFN3.points$aspect = IFN3_SPT$aspect
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

## Data input
folder_daily_trends <- paste0(data_home,"Rdata/Plots/SWBTrends")
available_plots_trends <- list.files(folder_daily_trends)
available_plots_trends <- unlist(strsplit(available_plots_trends,split = ".rda"))
folder_projections <- paste0(data_home,"Rdata/Plots/ProjectedSWB/CCLM4-8-17/rcp4.5/")
available_plots_projections <- list.files(folder_projections)
available_plots_projections <- unlist(strsplit(available_plots_projections,split = ".rda"))
folder_historic <- paste0(data_home,"Rdata/Plots/HistoricSWB/IFN2-3/")
available_plots_historic <- list.files(folder_historic)
available_plots_historic <- unlist(strsplit(available_plots_historic,split = ".rda"))


##Weather station locations
# load(paste0(data_home,"Rdata/WeatherStations.Rdata"))
# station.prec <- spTransform(station.prec, CRSobj = mapCRS)

## Variable names correspondance between ui and medfate outputs
input_clim_var <- c("Precipitation (mm)", "Potential evapo-transpiration (mm)", "SPEI (k=3)", "SPEI (k=6)", "SPEI (k=12)")
medfate_clim_var <- c("Rain", "PET", "spei3","spei6","spei12")
clim_variables <- data.frame(input = input_clim_var, medfate = medfate_clim_var)

input_WB_var <- c("Net precipitation (mm)", "LAI (m2/m2)","Plants transpiration (mm)", "Soil evaporation (mm)", "Run-off (mm)", "Deep drainage (mm)",
                  "Relative soil water content [0-1]")
medfate_WB_var <- c("NetPrec", "LAI", "Eplant", "Esoil", "Runoff", "DeepDrainage", "Theta")
WB_variables <- data.frame(input = input_WB_var, medfate = medfate_WB_var)

input_soil_var<-c("Soil depth (cm)", "Water holding capacity (mm)", "Topsoil texture type", "Subsoil texture type","Topsoil rock fragment content (%)", "Subsoil rock fragment content (%)")
medfate_soil_var<-c("SoilDepth", "SWHC", "TopTT", "SubTT", "TopRFC", "SubRFC")
soil_variables <- data.frame(input = input_soil_var, medfate = medfate_soil_var)

input_ifn_var<-c("Leaf Area Index", "Density (ind/m2)", "Basal area (m2/ha)", "Average height (m)")
medfate_ifn_var<-c("LAI", "N", "BA", "H")
ifn_variables <- data.frame(input = input_ifn_var, medfate = medfate_ifn_var)

input_DS_var <-c("Stress intensity", "Stress duration")
medfate_DS_var<-c("DDS","NDD")
DS_variables <- data.frame(input = input_DS_var, medfate = medfate_DS_var)

input_sp <- c("All woody species", "Pinus halepensis", "Pinus nigra", "Pinus sylvestris", "Pinus uncinata", "Pinus pinea",
              "Pinus pinaster", "Quercus ilex", "Quercus suber", "Quercus humilis", "Quercus faginea", "Fagus sylvatica")
medfate_sp <- c("Overall", "PinusHalepensis", "PinusNigra", "PinusSylvestris", "PinusUncinata", "PinusPinea", "PinusPinaster",
                "QuercusIlex", "QuercusSuber", "QuercusHumilis", "QuercusFaginea", "FagusSylvatica")
species <- data.frame(input = input_sp, medfate = medfate_sp)

climate_models<-c("CCLM4-8-17","RCA4")
names(climate_models)<-c("CNRM/CCLM4-8-17", "CNRM/RCA4")

basemaps <- c("Esri.WorldGrayCanvas","Esri.WorldImagery","Esri.WorldTerrain","Esri.WorldShadedRelief","Stamen.TerrainBackground")



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
pal_clim[c("SPEI (k=3)","SPEI (k=6)","SPEI (k=12)"), "color"] <- "RdYlBu"
pal_clim[c("SPEI (k=3)","SPEI (k=6)","SPEI (k=12)"), "min"] <- -4
pal_clim[c("SPEI (k=3)","SPEI (k=6)","SPEI (k=12)"), "max"] <- 4
pal_clim[c("SPEI (k=3)","SPEI (k=6)","SPEI (k=12)"), "trans"] <- "identity"

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
pal_DS["Stress intensity","trans"] <- "identity"
pal_DS["Stress duration","trans"] <- "log"
pal_DS["Stress intensity","max"]<-1
pal_DS["Stress duration","max"]<-365

## Define color scales for rasters
pal_soil <- as.data.frame(matrix(NA, nrow = length(input_soil_var), ncol = 5, dimnames = list(input_soil_var, c("min", "max", "color", "trans", "rev"))))
pal_soil$min <- 0
pal_soil$trans <- "identity"
pal_soil$color <-"Reds"
pal_soil$rev <-F
pal_soil["Soil depth (cm)","min"] = 30
pal_soil["Soil depth (cm)","max"] = 1000
pal_soil["Soil depth (cm)","trans"] = "log"
pal_soil["Water holding capacity (mm)","color"]<-"Blues"
pal_soil["Water holding capacity (mm)","min"] = 10
pal_soil["Water holding capacity (mm)","max"] = 2000
pal_soil["Water holding capacity (mm)","trans"] = "log"
pal_soil["Topsoil rock fragment content (%)","max"] = 100
pal_soil["Subsoil rock fragment content (%)","max"] = 100
print(pal_soil)

log_trans <- function(dom, n = 10, digits = 1) {signif(exp(seq(log(dom[1]+1), log(dom[2]+1), length.out = n))-1, digits = digits)}
identity_trans <- function(dom, n = 10, digits = 1) {signif(seq(dom[1], dom[2], length.out = n), digits = digits)}


clim_scale_bins<-function(varName, bins) {
  dom = c(min(bins), max(bins))
  return(list(dom = dom,
              bins = bins,
              pal=colorBin(pal_clim[varName,"color"],
                      domain = dom, na.color = "transparent",
                      bins = bins, reverse = pal_clim[varName, "rev"])))
}
clim_scale<-function(varName){
  dom <- c(pal_clim[varName,"min"],pal_clim[varName,"max"])
  bins <- do.call(paste(pal_clim[varName, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 14, digits = 2))
  pal <- colorBin(pal_clim[varName,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_clim[varName, "rev"])
  return(list(dom = dom,bins = bins,pal = pal))
}
WB_scale<-function(varName, bins) {
  dom = c(min(bins), max(bins))
  return(list(dom = dom,
              bins = bins,
              pal=colorBin(pal_WB[varName,"color"],
                           domain = dom, na.color = "transparent",
                           bins = bins, reverse = pal_WB[varName, "rev"])))
}
DS_scale<-function(varName){
  dom <- c(pal_DS[varName,"min"],pal_DS[varName,"max"])
  bins <- do.call(paste(pal_DS[varName, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 14, digits = 2))
  pal <- colorBin(pal_DS[varName,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_DS[varName, "rev"])
  return(list(dom = dom,bins = bins,pal = pal))
}

abs_change_scale<-function(values, n=14, reverse = F) {
  abs_val = max(abs(values), na.rm=TRUE)
  dom = c(-abs_val -0.001, abs_val+0.001)
  bins = identity_trans(dom = dom, n = n, digits = 3)
  pal = colorBin(c("red","white","blue"), domain = dom, na.color = "transparent", bins = bins, reverse = reverse)
  return(list(dom=dom, bins=bins, pal=pal))
}
rel_change_scale<-function(reverse = F) {
  dom = c(-300,300)
  bins <- c(-400,-200,-100,-75,-50,-25,-10,-5,5,10,25,50,75,100,200,400)
  pal = colorBin(c("red","white","blue"), domain = dom, na.color = "transparent", bins = bins, reverse = reverse)
  return(list(dom=dom, bins=bins, pal=pal))
}

########################################################
### Define server logic required to draw a histogram ###
########################################################

shinyServer(function(input, output, session) {
  output$date_daily<-renderUI({
    dateInput("date_daily", "Date",value = Sys.Date()-1, min =as.Date("2017-01-01"), max = Sys.Date()-1, weekstart=1)
  })
  

  # Switch the mode of the daily output
  output$var_choice_daily <- renderUI({
    input_title <- "Choose variable"
    if(input$mode_daily == "Climate") {
      input_name <- "clim_daily"
      var_choice_daily <- input_clim_var[1:2]
      selected <- "Precipitation (mm)"
    } else if(input$mode_daily == "Forest water balance") {
      input_name <- "WB_daily"
      var_choice_daily <- input_WB_var
      selected <- "Relative soil water content [0-1]"
    } else {
      input_name <- "DS_daily"
      var_choice_daily <- input_DS_var
      selected <- "Stress intensity"
    }
    selectInput(input_name, input_title, choices = var_choice_daily, selected = selected)
  })
  # Switch the mode of the historic output
  output$var_choice_hist <- renderUI({
    input_title <- "Choose variable"
    if(input$mode_hist == "Climate") {
      input_name <- "clim_hist"
      if(input$agg_hist=="Month") var_choice_hist <- input_clim_var
      else var_choice_hist <- input_clim_var[1:2]
      selected <- "Precipitation (mm)"
    } else if(input$mode_hist == "Forest water balance") {
      input_name <- "WB_hist"
      var_choice_hist <- input_WB_var[-2]
      selected <- "Relative soil water content [0-1]"
    } else {
      input_name <- "DS_hist"
      var_choice_hist <- input_DS_var
      selected <- "Stress intensity"
    }
    selectInput(input_name, input_title, choices = var_choice_hist, selected = selected)
  })
  # Switch the mode of the projection output
  output$var_choice_proj <- renderUI({
    input_title <- "Choose variable"
    if(input$mode_proj == "Climate") {
      input_name <- "clim_proj"
      if(input$agg_proj=="Month") var_choice_proj <- input_clim_var
      else var_choice_proj <- input_clim_var[1:2]
      selected <- "Precipitation (mm)"
    } else if(input$mode_proj == "Forest water balance") {
      input_name <- "WB_proj"
      var_choice_proj <- input_WB_var[-2]
      selected <- "Relative soil water content [0-1]"
    } else {
      input_name <- "DS_proj"
      var_choice_proj <- input_DS_var
      selected <- "Stress intensity"
    }
    selectInput(input_name, input_title, choices = var_choice_proj, selected = selected)
  })
  # Switch the mode of the static info
  output$var_choice_stat <- renderUI({
    input_title <- "Choose variable"
    if(input$mode_stat == "Soil") {
      input_name <- "soil_stat"
      var_choice_stat <- input_soil_var
      selected <- "Soil depth (cm)"
    } else {
      input_name <- "ifn_stat"
      var_choice_stat <- input_ifn_var
      selected <- "Leaf Area Index"
    }
    selectInput(input_name, input_title, choices = var_choice_stat, selected = selected)
  })

  # Create a reactive value data for rasters
  map_daily_raster_data <- reactiveValues(x = list())
  map_hist_raster_data <- reactiveValues(x = list())
  map_proj_raster_data <- reactiveValues(x = list())
  map_stat_raster_data <- reactiveValues(x = list())

  # Sets raster layer for daily drought
  observe({
    if(input$mode_daily == "Climate"){
      if(!is.null(input$clim_daily)){
        folder <- paste0(data_home,"Rdata/Maps/Current")
        col <- as.character(clim_variables[clim_variables$input == input$clim_daily, "medfate"])
        dfin = input$date_daily
        dini  = max(as.Date("2017-01-01"),dfin-(as.numeric(input$agg_daily)-1))
        dw = seq(dini, dfin, by="day")
        nd = length(dw)
        load(paste(folder, "/", input$resolution_daily, "/SWB/",col, "/", dw[1], ".rda", sep = ""))
        spdftmp = spdf
        if(nd>1) {
          for(d in 2:nd) {
            load(paste(folder, "/", input$resolution_daily, "/SWB/",col, "/", dw[d], ".rda", sep = ""))
            spdftmp@data = spdftmp@data + spdf@data
          }
        }
        spdftmp@data =spdftmp@data /nd
        spdf = spdftmp


        dom <- c(pal_clim[input$clim_daily,"min"],pal_clim[input$clim_daily,"max"])
        bins <- do.call(paste(pal_clim[input$clim_daily, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))

        pal <- colorBin(pal_clim[input$clim_daily,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_clim[input$clim_daily, "rev"])

        map_daily_raster_data$x<-list(spdf = spdf, dom = dom, bins=bins, pal = pal)
      }
    }
    else if(input$mode_daily == "Forest water balance"){
      if(!is.null(input$WB_daily)){
        folder <- paste0(data_home,"Rdata/Maps/Current")
        col <- as.character(WB_variables[WB_variables$input == input$WB_daily, "medfate"])
        dfin = input$date_daily
        dini  = max(as.Date("2017-01-01"),dfin-(as.numeric(input$agg_daily)-1))
        dw = seq(dini, dfin, by="day")
        nd = length(dw)
        load(paste(folder, "/", input$resolution_daily, "/SWB/",col, "/", dw[1], ".rda", sep = ""))
        spdftmp = spdf
        if(nd>1) {
          for(d in 2:nd) {
            load(paste(folder, "/", input$resolution_daily, "/SWB/",col, "/", dw[d], ".rda", sep = ""))
            spdftmp@data = spdftmp@data + spdf@data
          }
        }
        spdftmp@data =spdftmp@data /nd
        spdf = spdftmp

        r <- raster(spdf)
        proj4string(r) <- dataCRS
        r <- projectRaster(r, crs = mapCRS)

        dom <- c(pal_WB[input$WB_daily,"min"],pal_WB[input$WB_daily,"max"])
        bins <- do.call(paste(pal_WB[input$WB_daily, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
        pal <- colorBin(pal_WB[input$WB_daily,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_WB[input$WB_daily, "rev"])
        map_daily_raster_data$x<-list(spdf = spdf, dom = dom, bins=bins, pal = pal)

      }
    }
    else {
      if(!is.null(input$DS_daily) && !is.null(input$sp_daily)){
        folder <- paste0(data_home,"Rdata/Maps/Current")
        ds_var <- as.character(DS_variables[DS_variables$input == input$DS_daily, "medfate"])
        col <- as.character(species[species$input == input$sp_daily, "medfate"])
        # print(ds_var)
        dfin = input$date_daily
        dini  = max(as.Date("2017-01-01"),dfin-(as.numeric(input$agg_daily)-1))
        dw = seq(dini, dfin, by="day")
        nd = length(dw)
        file = paste(folder, "/", input$resolution_daily, "/DroughtStress/",ds_var,"/",col, "/", dw[1], ".rda", sep = "")
        if(file.exists(file)) {
          load(file)
          spdftmp = spdf
          if(nd>1) {
            for(d in 2:nd) {
              load(paste(folder, "/", input$resolution_daily, "/DroughtStress/",ds_var,"/",col, "/", dw[d], ".rda", sep = ""))
              spdftmp@data = spdftmp@data + spdf@data
            }
          }
          spdftmp@data =spdftmp@data /nd
          spdf = spdftmp

          dom <- c(pal_DS[input$DS_daily,"min"],pal_DS[input$DS_daily,"max"])
          bins <- do.call(paste(pal_DS[input$DS_daily, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
          if(ds_var=="NDD") bins<-ceiling(bins)

          pal <- colorBin(pal_DS[input$DS_daily,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_DS[input$DS_daily, "rev"])

          map_daily_raster_data$x<-list(spdf = spdf, dom = dom, bins=bins, pal = pal)
        }
      }
    }
  })
  #Draws daily raster layer
  observe({
    if(!is.null(map_daily_raster_data$x$spdf)) {
      r <- raster(map_daily_raster_data$x$spdf)
      proj4string(r) <- dataCRS
      r <- projectRaster(r, crs = mapCRS)

      leafletProxy("map_daily") %>%
        clearImages() %>%
        clearControls() %>%
        addRasterImage(r, opacity = input$alpha_daily, colors = map_daily_raster_data$x$pal, layerId="raster", group="rasterGroup") %>%
        addLegend(pal = map_daily_raster_data$x$pal, values = values(r),opacity = input$alpha_daily, position = "bottomright", layerId="raster")

    }
  })
  #Downloads daily rasters
  dailyRaster<-reactive({
    if(!is.null(map_daily_raster_data$x)) {
      map_daily_raster_data$x$spdf
    }
  })
  output$downloadRasterDaily<-downloadHandler(
    filename = function() {
      paste("daily_raster.txt")
    },
    content = function(file) {
      write.asciigrid(dailyRaster(), fname=file)
    }
  )

  # Sets raster layers for historic drought
  observe({
    if(input$mode_hist == "Climate" && !is.null(input$clim_hist)){
      folder <- paste0(data_home,"Rdata/Maps/Historic")
      col <- as.character(clim_variables[clim_variables$input == input$clim_hist, "medfate"])

      #Set file to read
      if(input$climate_hist!='Year') {
        if(input$raster_trend_hist=="Average") {## Multiyear average
          if(input$agg_hist=="Year") file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016.rda", sep = "")
          else file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016-",input$month_hist, ".rda", sep = "")
          if(file.exists(file)) {
            load(file)
            if(input$agg_hist=="Year") {
              if(col %in% c("PET","Rain")) scale = clim_scale_bins(input$clim_hist, bins =c(seq(0,800, by=100),1000,1200,1400,2000,3000))
              else scale = clim_scale(input$clim_hist)
            } else {
              if(col %in% c("PET","Rain")) scale = clim_scale_bins(input$clim_hist, bins =c(seq(0,100, by=20),125,150,200,250))
              else scale = clim_scale(input$clim_hist)
            }
            map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
          } else {
            warning(paste0("File ", file, " not found!"))
          }
        }
        else if(input$raster_trend_hist %in% c("Absolute change", "Relative change")) { ## Multiyear absolute change
          if(input$agg_hist=="Year") file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016-trend.rda", sep = "")
          else file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016-",input$month_hist,"-trend.rda", sep = "")
          if(file.exists(file)) {
            load(file)
            if(input$raster_trend_hist=="Absolute change") { ## Multiyear absolute change
              spdf = spdf_slope
              spdf@data[,1] = spdf@data[,1]*26
              scale = abs_change_scale(spdf@data[,1], reverse=ifelse(col=="PET",T,F))
              spdf@data[spdf_pval@data[,1]>as.numeric(input$alpha_cut_hist),1] = NA
            } else if(input$raster_trend_hist=="Relative change") { ## Multiyear relative change
              if(input$agg_hist=="Year") load(paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016.rda", sep = ""))
              else load(paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016-",input$month_hist,".rda", sep = ""))
              spdf_h = spdf
              spdf = spdf_slope
              spdf@data[,1] = 100*(spdf@data[,1]*26/spdf_h@data[,1])
              scale = rel_change_scale(reverse=ifelse(col=="PET",T,F))
              spdf@data[spdf_pval@data[,1]>as.numeric(input$alpha_cut_hist),1] = NA
            }
            map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
          } else {
            warning(paste0("File ", file, " not found!"))
          }
        }
      }
      else {
        if(input$agg_hist=="Year") file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/", input$years_hist, ".rda", sep = "")
        else file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/", input$years_hist,"-",input$month_hist, ".rda", sep = "")
        #Load if exists
        if(file.exists(file)) {
          load(file)
          if(input$agg_hist=="Year") {
            if(col %in% c("PET","Rain")) scale = clim_scale_bins(input$clim_hist, bins =c(seq(0,800, by=100),1000,1200,1400,2000,3000))
            else  scale = clim_scale(input$clim_hist)
            map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
          } else {
            if(col %in% c("PET","Rain")) scale = clim_scale_bins(input$clim_hist, bins =c(seq(0,250, by=25),300,400,600))
            else  scale = clim_scale(input$clim_hist)
            map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
          }
        }
        else {
          warning(paste0("File ", file, " not found!"))
        }
      }
    }
    else if(input$mode_hist == "Forest water balance" && !is.null(input$WB_hist)){
      folder <- paste0(data_home,"Rdata/Maps/Historic")
      col <- as.character(WB_variables[WB_variables$input == input$WB_hist, "medfate"])

      #Set file to read
      if(input$climate_hist!='Year') {
        if(input$raster_trend_hist=="Average") {
          if(input$agg_hist=="Year") {
            file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016.rda", sep = "")
            if(file.exists(file)) {
              load(file)
              if(col=="NetPrec") scale = WB_scale(input$WB_hist, c(seq(0,1500, by=250),2000,2500,3000))
              else if(col=="Eplant") scale = WB_scale(input$WB_hist, c(seq(0,500, by=50),750,1000,1500))
              else if(col %in% c("Esoil", "Runoff","DeepDrainage")) {
                dom <- c(pal_WB[input$WB_hist,"min"],pal_WB[input$WB_hist,"max"]*100)
                bins <- do.call(paste(pal_WB[input$WB_hist, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
                pal <- colorBin(pal_WB[input$WB_hist,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_WB[input$WB_hist, "rev"])
                scale = list(dom =dom, bins=bins, pal=pal)
              }
              else {
                dom <- c(pal_WB[input$WB_hist,"min"],pal_WB[input$WB_hist,"max"])
                bins <- do.call(paste(pal_WB[input$WB_hist, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
                pal <- colorBin(pal_WB[input$WB_hist,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_WB[input$WB_hist, "rev"])
                scale = list(dom =dom, bins=bins, pal=pal)
              }
              map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
            } else {
              warning(paste0("File ", file, " not found!"))
            }
          } else {
            file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016-",input$month_hist, ".rda", sep = "")
            if(file.exists(file)) {
              load(file)
              if(col=="NetPrec") scale = WB_scale(input$WB_hist, c(seq(0,200, by=25),250,300,350,400,600))
              else if(col=="Eplant") scale = WB_scale(input$WB_hist, c(seq(0,100, by=10),150,200,250,300))
              else if(col %in% c("Esoil", "Runoff","DeepDrainage")) {
                dom <- c(pal_WB[input$WB_hist,"min"],pal_WB[input$WB_hist,"max"]*12)
                bins <- do.call(paste(pal_WB[input$WB_hist, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
                pal <- colorBin(pal_WB[input$WB_hist,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_WB[input$WB_hist, "rev"])
                scale = list(dom =dom, bins=bins, pal=pal)
              }
              else {
                dom <- c(pal_WB[input$WB_hist,"min"],pal_WB[input$WB_hist,"max"])
                bins <- do.call(paste(pal_WB[input$WB_hist, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
                pal <- colorBin(pal_WB[input$WB_hist,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_WB[input$WB_hist, "rev"])
                scale = list(dom =dom, bins=bins, pal=pal)
              }
              map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
            } else {
              warning(paste0("File ", file, " not found!"))
            }
          }
        }
        else if(input$raster_trend_hist %in% c("Absolute change", "Relative change")) {
          if(input$agg_hist=="Year") file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016-trend.rda", sep = "")
          else file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016-",input$month_hist,"-trend.rda", sep = "")
          if(file.exists(file)) {
            load(file)
            if(input$raster_trend_hist=="Absolute change") {
              spdf = spdf_slope
              spdf@data[,1] = spdf@data[,1]*26
              scale = abs_change_scale(spdf@data[,1])
              spdf@data[spdf_pval@data[,1]>as.numeric(input$alpha_cut_hist),1] = NA
            } else {
              if(input$agg_hist=="Year") load(paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016.rda", sep = ""))
              else load(paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/1991-2016-",input$month_hist,".rda", sep = ""))
              spdf_h = spdf
              spdf = spdf_slope
              base_data = spdf_h@data[,1]
              ind_base = spdf_h@grid.index %in% spdf@grid.index
              ind_change = spdf@grid.index %in% spdf_h@grid.index
              spdf@data[ind_change,1] = 100*(spdf@data[ind_change,1]*26/base_data[ind_base])
              scale = rel_change_scale()
              spdf@data[spdf_pval@data[,1]>as.numeric(input$alpha_cut_hist),1] = NA
            }
            map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
          } else {
            warning(paste0("File ", file, " not found!"))
          }
        }
      } else { #Specific year/month
        if(input$agg_hist=="Year") {
          file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/", input$years_hist, ".rda", sep = "")
          if(file.exists(file)) {
            load(file)
            if(col=="NetPrec") scale = WB_scale(input$WB_hist, c(seq(0,1500, by=250),2000,2500,3000))
            else if(col=="Eplant") scale = WB_scale(input$WB_hist, c(seq(0,500, by=50),750,1000,1500))
            else if(col %in% c("Esoil", "Runoff","DeepDrainage")){
              dom <- c(pal_WB[input$WB_hist,"min"],pal_WB[input$WB_hist,"max"]*100)
              bins <- do.call(paste(pal_WB[input$WB_hist, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
              pal <- colorBin(pal_WB[input$WB_hist,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_WB[input$WB_hist, "rev"])
              scale = list(dom =dom, bins=bins, pal=pal)
            } else {
              dom <- c(pal_WB[input$WB_hist,"min"],pal_WB[input$WB_hist,"max"])
              bins <- do.call(paste(pal_WB[input$WB_hist, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
              pal <- colorBin(pal_WB[input$WB_hist,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_WB[input$WB_hist, "rev"])
              scale = list(dom =dom, bins=bins, pal=pal)
            }
            map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
          } else {
            warning(paste0("File ", file, " not found!"))
          }
        }  else {
          file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/SWB/",col, "/", input$years_hist,"-",input$month_hist, ".rda", sep = "")
          load(file)
          if(file.exists(file)) {
            if(col=="NetPrec") scale = WB_scale(input$WB_hist, c(seq(0,200, by=25),250,300,350,400,600))
            else if(col=="Eplant") scale = WB_scale(input$WB_hist, c(seq(0,100, by=10),150,200,250,300))
            else if(col %in% c("Esoil", "Runoff","DeepDrainage")) {
              dom <- c(pal_WB[input$WB_hist,"min"],pal_WB[input$WB_hist,"max"]*12)
              bins <- do.call(paste(pal_WB[input$WB_hist, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
              pal <- colorBin(pal_WB[input$WB_hist,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_WB[input$WB_hist, "rev"])
              scale = list(dom =dom, bins=bins, pal=pal)
            }
            else {
              dom <- c(pal_WB[input$WB_hist,"min"],pal_WB[input$WB_hist,"max"])
              bins <- do.call(paste(pal_WB[input$WB_hist, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
              pal <- colorBin(pal_WB[input$WB_hist,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_WB[input$WB_hist, "rev"])
              scale = list(dom =dom, bins=bins, pal=pal)
            }
            map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
          }
        }
      }
    }
    else if(input$mode_hist == "Drought stress" && !is.null(input$sp_hist)){
      folder <- paste0(data_home,"Rdata/Maps/Historic")
      col <- as.character(species[species$input == input$sp_hist, "medfate"])
      ds_var <- as.character(DS_variables[DS_variables$input == input$DS_hist, "medfate"])
      if(length(ds_var)>0){
        if(input$climate_hist!='Year') {
          if(input$raster_trend_hist=="Average") {
            if(input$agg_hist=="Year") file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/DroughtStress/",ds_var,"/",col, "/1991-2016.rda", sep = "")
            else file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/DroughtStress/",ds_var,"/",col, "/1991-2016-",input$month_hist, ".rda", sep = "")
            if(file.exists(file)) {
              load(file)
              scale = DS_scale(input$DS_hist)
              map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
            } else {
              warning(paste0("File ", file, " not found!"))
            }
          }
          else if(input$raster_trend_hist %in% c("Absolute change", "Relative change")) {
            if(input$agg_hist=="Year") file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/DroughtStress/",ds_var,"/",col, "/1991-2016-trend.rda", sep = "")
            else file = paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/DroughtStress/",ds_var,"/",col, "/1991-2016-",input$month_hist, "-trend.rda", sep = "")
            if(file.exists(file)) {
              load(file)
              if(input$raster_trend_hist=="Absolute change") { ## Multiyear absolute change
                spdf = spdf_slope
                spdf@data[,1] = spdf@data[,1]*26
                scale = abs_change_scale(spdf@data[,1],reverse=TRUE)
                spdf@data[spdf_pval@data[,1]>as.numeric(input$alpha_cut_hist),1] = NA
              } else if(input$raster_trend_hist=="Relative change") { ## Multiyear relative change
                if(input$agg_hist=="Year") load(paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/DroughtStress/",ds_var,"/",col, "/1991-2016.rda", sep = ""))
                else  load(paste(folder, "/", input$resolution_hist,"/", input$agg_hist, "/DroughtStress/",ds_var,"/",col, "/1991-2016-",input$month_hist,".rda", sep = ""))
                spdf_h = spdf
                spdf = spdf_slope
                base_data = spdf_h@data[,1]
                ind_base = spdf_h@grid.index %in% spdf@grid.index
                ind_change = spdf@grid.index %in% spdf_h@grid.index
                spdf@data[ind_change,1] = 100*(spdf@data[ind_change,1]*26/base_data[ind_base])
                scale = rel_change_scale(reverse=TRUE)
                spdf@data[spdf_pval@data[,1]>as.numeric(input$alpha_cut_hist),1] = NA
              }
              map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
            } else {
              warning(paste0("File ", file, " not found!"))
            }
          }
        } else { # Year to year
          if(input$agg_hist=="Year") file = paste(folder, "/", input$resolution_hist,"/",input$agg_hist, "/DroughtStress/",ds_var,"/",col, "/", input$years_hist, ".rda", sep = "")
          else file = paste(folder, "/", input$resolution_hist,"/",input$agg_hist, "/DroughtStress/", ds_var,"/",col, "/", input$years_hist,"-",input$month_hist, ".rda", sep = "")
          if(file.exists(file)) {
            load(file)
            scale = DS_scale(input$DS_hist)
            map_hist_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
          } else {
            warning(paste0("File ", file, " not found!"))
          }
        }
      }
    }
  })
  #Draws historic raster layer
  observe({
    if(!is.null(map_hist_raster_data$x$spdf)) {
      r <- raster(map_hist_raster_data$x$spdf)
      proj4string(r) <- dataCRS
      r <- projectRaster(r, crs = mapCRS)

      leafletProxy("map_hist") %>%
        clearImages() %>%
        clearControls() %>%
        addRasterImage(r, opacity = input$alpha_hist, colors = map_hist_raster_data$x$pal, layerId="raster", group="rasterGroup") %>%
        addLegend(pal = map_hist_raster_data$x$pal, values = values(r),opacity = input$alpha_hist, position = "bottomright", layerId="raster")
    }
  })
  # observe({
  #   print(input$years_hist)
  #   sel = !is.na(station.prec@data[,as.character(input$years_hist)])
  #   station.prec$ID = paste(row.names(station.prec@data),as.character(station.prec@data[,as.character(input$years_hist)]), sep="/")
  #   a = station.prec[sel,]
  #   print(station.prec$ID[sel][a@data[,as.character(input$years_hist)]<150])
  #   leafletProxy("map_hist") %>%
  #     clearMarkers() %>%
  #     addCircleMarkers(data = station.prec[sel,], radius = 5,
  #                      color=ifelse(station.prec@data[sel,as.character(input$years_hist)]>150,"black","yellow"),
  #                      stroke = F, fillOpacity = 0.8,
  #                      label = ~as.character(ID))
  #
  # })

  #Downloads historic rasters
  historicRaster<-reactive({
    if(!is.null(map_hist_raster_data$x)) {
      map_hist_raster_data$x$spdf
    }
  })
  output$downloadRasterHist<-downloadHandler(
    filename = function() {
      paste("historic_raster.txt")
    },
    content = function(file) {
      write.asciigrid(historicRaster(), fname=file)
    }
  )

  # Sets raster layers for projection drought
  observe({
    folder <- paste0(data_home,"Rdata/Maps/Projected")
    folderhist<-paste0(data_home,"Rdata/Maps/Historic")
    if(input$mode_proj == "Climate" && !is.null(input$clim_proj)){
      col <- as.character(clim_variables[clim_variables$input == input$clim_proj, "medfate"])
      file = paste(folder, "/", climate_models[input$rcm_proj],"/", input$rcp_proj, "/", input$resolution_proj, "/", input$agg_proj, "/SWB/",col, ".rda", sep = "")

      if(file.exists(file)) {
        load(file)
        spdf = spdf_slope
        if(input$raster_trend_proj=="Absolute change") spdf@data[,1] = spdf@data[,1]*95
        else if(input$raster_trend_proj=="Relative change") {
          spdf@data[,1] = spdf@data[,1]*95
          spdf_p = spdf
          filehist = paste(folderhist, "/", input$resolution_proj, "/", input$agg_proj, "/SWB/",col, "/1991-2016.rda", sep = "")
          if(file.exists(filehist)) {
            load(filehist)
            spdf_h = spdf
            spdf = spdf_p
            spdf@data[,1] = 100*(spdf@data[,1]/spdf_h@data[,1])
          } else {
            warning(paste0("File historic ", filehist, " not found!"))
            spdf = spdf_p
          }
        }
        if(input$raster_trend_proj=="Relative change") {
          scale = rel_change_scale(reverse = ifelse(col=="PET",T,F))
        } else {
          scale = abs_change_scale(spdf@data[,1], reverse = ifelse(col=="PET",T,F))
        }
        spdf@data[spdf_pval@data[,1]>as.numeric(input$alpha_cut_proj),1] = NA
        map_proj_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
      }
      else {
        warning(paste0("File ", file, " not found!"))
      }
    }
    else if(input$mode_proj == "Forest water balance" && !is.null(input$WB_proj)){
      col <- as.character(WB_variables[WB_variables$input == input$WB_proj, "medfate"])
      file = paste(folder, "/", climate_models[input$rcm_proj],"/", input$rcp_proj, "/", input$resolution_proj, "/", input$agg_proj, "/SWB/",col, ".rda", sep = "")
      if(file.exists(file)) {
        load(file)
        spdf = spdf_slope
        if(input$raster_trend_proj=="Absolute change") spdf@data[,1] = spdf@data[,1]*95
        else if(input$raster_trend_proj=="Relative change") {
          spdf@data[,1] = spdf@data[,1]*95
          spdf_p = spdf
          filehist = paste(folderhist, "/", input$resolution_proj, "/", input$agg_proj, "/SWB/",col, "/1991-2016.rda", sep = "")
          if(file.exists(filehist)) {
            load(filehist)
            spdf_h = spdf
            spdf = spdf_p
            spdf@data[,1] = 100*(spdf@data[,1]/spdf_h@data[,1])
          } else {
            warning(paste0("File historic ", filehist, " not found!"))
            spdf = spdf_p
          }
        }
        if(input$raster_trend_proj=="Relative change") {
          scale = rel_change_scale(reverse = F)
        } else {
          scale = abs_change_scale(spdf@data[,1], reverse = F)
        }
        spdf@data[spdf_pval@data[,1]>as.numeric(input$alpha_cut_proj),1] = NA
        map_proj_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
      }
      else {
        warning(paste0("File ", file, " not found!"))
      }
    }
    else if(input$mode_proj == "Drought stress" && !is.null(input$sp_proj)){
      col <- as.character(species[species$input == input$sp_proj, "medfate"])
      ds_var <- as.character(DS_variables[DS_variables$input == input$DS_proj, "medfate"])
      if(length(ds_var)>0){
        file = paste(folder, "/", climate_models[input$rcm_proj],"/", input$rcp_proj, "/", input$resolution_proj, "/", input$agg_proj, "/DroughtStress/",ds_var,"/",col, ".rda", sep = "")
        if(file.exists(file)) {
          load(file)
          spdf = spdf_slope
          if(input$raster_trend_proj=="Absolute change") spdf@data[,1] = spdf@data[,1]*95
          else if(input$raster_trend_proj=="Relative change") {
            spdf@data[,1] = spdf@data[,1]*95
            spdf_p = spdf
            filehist = paste(folderhist, "/", input$resolution_proj, "/", input$agg_proj, "/DroughtStress/",ds_var,"/",col, "/1991-2016.rda", sep = "")
            if(file.exists(filehist)) {
              load(filehist)
              spdf_h = spdf
              spdf = spdf_p
              base_data = spdf_h@data[,1]
              ind_base = spdf_h@grid.index %in% spdf@grid.index
              ind_change = spdf@grid.index %in% spdf_h@grid.index
              spdf@data[ind_change,1] = 100*(spdf@data[ind_change,1]/base_data[ind_base])
            } else {
              warning(paste0("File historic ", filehist, " not found!"))
              spdf = spdf_p
            }
          }

          sel = spdf_pval@data[1,]>input$alpha_cut_proj
          sel[is.na(sel)] = FALSE
          spdf@data[sel,1] = 0
          if(input$raster_trend_proj=="Relative change") {
            scale = rel_change_scale(reverse = T)
          } else {
            scale = abs_change_scale(spdf@data[,1], reverse = T)
          }
          map_proj_raster_data$x<-list(spdf = spdf, dom = scale$dom, bins=scale$bins, pal = scale$pal)
        }
        else {
          warning(paste0("File ", file, " not found!"))
        }
      }
    }

  })
  #Draws projection raster layer
  observe({
    if(!is.null(map_proj_raster_data$x$spdf)) {
      r <- raster(map_proj_raster_data$x$spdf)
      proj4string(r) <- dataCRS
      r <- projectRaster(r, crs = mapCRS)

      leafletProxy("map_proj") %>%
        clearImages() %>%
        clearControls() %>%
        addRasterImage(r, opacity = input$alpha_proj, colors = map_proj_raster_data$x$pal, layerId="raster", group="rasterGroup") %>%
        addLegend(pal = map_proj_raster_data$x$pal, values = values(r),opacity = input$alpha_proj, position = "bottomright", layerId="raster")

    }
  })

  #Downloads projection rasters
  projectionRaster<-reactive({
    if(!is.null(map_proj_raster_data$x)) {
      map_proj_raster_data$x$spdf
    }
  })
  output$downloadRasterProj<-downloadHandler(
    filename = function() {
      paste("projection_raster.txt")
    },
    content = function(file) {
      write.asciigrid(projectionRaster(), fname=file)
    }
  )


  # Sets raster layer for static inputs
  observe({
    if(input$mode_stat == "Soil"){
      if(!is.null(input$soil_stat)){
        folder <- paste0(data_home,"Rdata/Maps/Static/Soil")
        col <- as.character(soil_variables[soil_variables$input == input$soil_stat, "medfate"])
        load(paste(folder, "/", input$resolution_stat, "/",col, ".rda", sep = ""))


        dom <- c(pal_soil[input$soil_stat,"min"],pal_soil[input$soil_stat,"max"])
        bins <- do.call(paste(pal_soil[input$soil_stat, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))

        pal <- colorBin(pal_soil[input$soil_stat,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_soil[input$soil_stat, "rev"])
        # print(pal)
        # print(head(spdf@data))
        map_stat_raster_data$x<-list(spdf = spdf, dom = dom, bins=bins, pal = pal)
      }
    }
  })
  #Draws static raster layer
  observe({
    if(!is.null(map_stat_raster_data$x$spdf)) {
      r <- raster(map_stat_raster_data$x$spdf)
      proj4string(r) <- dataCRS
      r <- projectRaster(r, crs = mapCRS)

      leafletProxy("map_stat") %>%
        clearImages() %>%
        clearControls() %>%
        addRasterImage(r, opacity = input$alpha_stat, colors = map_stat_raster_data$x$pal, layerId="raster", group="rasterGroup") %>%
        addLegend(pal = map_stat_raster_data$x$pal, values = values(r),opacity = input$alpha_stat, position = "bottomright", layerId="raster")

    }
  })


  # Create an interactive map centered on catalonia
  output$map_daily <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12)) %>%
      addProviderTiles(basemaps[1], layerId="basemap") %>%
      setView(lng = 1.74,lat = 41.69, zoom = 8)

  })
  observe({
    leafletProxy("map_daily") %>%
      removeImage(layerId="basemap") %>%
      addProviderTiles(input$basemap_daily,layerId="basemap") %>%
      showGroup("rasterGroup")
  })
  # Create an interactive map centered on catalonia
  output$map_hist <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12)) %>%
      addProviderTiles(basemaps[1]) %>%
      # addProviderTiles("Esri.WorldGrayCanvas") %>%
      setView(lng = 1.74,lat = 41.69, zoom = 8)
  })
  observe({
    leafletProxy("map_hist") %>%
      clearTiles() %>%
      addProviderTiles(input$basemap_hist)
  })
  # Create an interactive map centered on catalonia
  output$map_proj <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12)) %>%
      addProviderTiles(basemaps[1]) %>%
      setView(lng = 1.74,lat = 41.69, zoom = 8)
  })
  observe({
    leafletProxy("map_proj") %>%
      clearTiles() %>%
      addProviderTiles(input$basemap_proj)
  })
  # Create an interactive map centered on catalonia
  output$map_stat <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12)) %>%
      addProviderTiles(basemaps[1], layerId="basemap") %>%
      setView(lng = 1.74,lat = 41.69, zoom = 8)

  })
  observe({
    leafletProxy("map_stat") %>%
      removeImage(layerId="basemap") %>%
      addProviderTiles(input$basemap_stat,layerId="basemap") %>%
      showGroup("rasterGroup")
  })

  # Add shapes to daily SWB
  observe({
    # print(paste0("Daily ",input$display_daily))
    if(input$display_daily == "none"){
      leafletProxy("map_daily") %>%
        clearShapes() %>%
        clearMarkerClusters()
    } else if(input$display_daily == "Watersheds"){
      leafletProxy("map_daily") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = con.pol, color = "black", weight = 1, fillOpacity = 0, label = ~CONCA,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_daily == "Counties"){
      leafletProxy("map_daily") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = cat.pol, color = "black", weight = 1, fillOpacity = 0, label = ~NOM_COMAR,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_daily == "Municipalities"){
        leafletProxy("map_daily") %>%
          clearShapes() %>%
          clearMarkerClusters() %>%
          addPolygons(data = mun.pol, color = "black", weight = 1, fillOpacity = 0, label = ~NOM_MUNI,
                      highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_daily == "IFN plots"){
          leafletProxy("map_daily") %>%
            clearShapes() %>%
            addCircleMarkers(data = IFN3.points[IFN3.points$ID %in% available_plots_trends,], radius = 5, color="black", stroke = F, fillOpacity = 0.7,
                             label = ~as.character(ID), layerId = ~as.character(ID),
                             clusterOptions = markerClusterOptions(showCoverageOnHover = T, disableClusteringAtZoom = 12))

    }
  })
  # Add shapes to historic SWB
  observe({
    if(input$display_hist == "none"){
      leafletProxy("map_hist") %>%
        clearShapes() %>%
        clearMarkerClusters()
    } else if(input$display_hist == "Watersheds"){
      leafletProxy("map_hist") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = con.pol, color = "black", weight = 1, fillOpacity = 0, label = ~CONCA,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_hist == "Counties"){
      leafletProxy("map_hist") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = cat.pol, color = "black", weight = 1, fillOpacity = 0, label = ~NOM_COMAR,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_hist == "Municipalities"){
      leafletProxy("map_hist") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = mun.pol, color = "black", weight = 1, fillOpacity = 0, label = ~NOM_MUNI,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_hist == "IFN plots"){
      leafletProxy("map_hist") %>%
        clearShapes() %>%
        addCircleMarkers(data = IFN3.points[IFN3.points$ID %in% available_plots_historic,], radius = 5, stroke = F, color="black", fillOpacity = 0.7,
                         label = ~as.character(ID), layerId = ~as.character(ID),
                         clusterOptions = markerClusterOptions(showCoverageOnHover = T, disableClusteringAtZoom = 12))

    }
  })
  # Add shapes to projected SWB
  observe({
    # print(paste0("Proj ",input$display_proj))
    if(input$display_proj == "none"){
      leafletProxy("map_proj") %>%
        clearShapes() %>%
        clearMarkerClusters()
    } else if(input$display_proj == "Watersheds"){
      leafletProxy("map_proj") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = con.pol, color = "black", weight = 1, fillOpacity = 0, label = ~CONCA,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_proj == "Counties"){
      leafletProxy("map_proj") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = cat.pol, color = "black", weight = 1, fillOpacity = 0, label = ~NOM_COMAR,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_proj == "Municipalities"){
        leafletProxy("map_proj") %>%
          clearShapes() %>%
          clearMarkerClusters() %>%
          addPolygons(data = mun.pol, color = "black", weight = 1, fillOpacity = 0, label = ~NOM_MUNI,
                      highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_proj == "IFN plots"){
          leafletProxy("map_proj") %>%
            clearShapes() %>%
            addCircleMarkers(data = IFN3.points[IFN3.points$ID %in% available_plots_projections,], radius = 5, stroke = F, color="black", fillOpacity = 0.7, label = ~as.character(ID), layerId = ~as.character(ID),
                             clusterOptions = markerClusterOptions(showCoverageOnHover = T, disableClusteringAtZoom = 12))

    }
  })
  # Add shapes to static map
  observe({
    # print(paste0("Proj ",input$display_proj))
    if(input$display_stat == "none"){
      leafletProxy("map_stat") %>%
        clearShapes() %>%
        clearMarkerClusters()
    } else if(input$display_stat == "Watersheds"){
      leafletProxy("map_stat") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = con.pol, color = "black", weight = 1, fillOpacity = 0, label = ~CONCA,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_stat == "Counties"){
      leafletProxy("map_stat") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = cat.pol, color = "black", weight = 1, fillOpacity = 0, label = ~NOM_COMAR,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_stat == "Municipalities"){
      leafletProxy("map_stat") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = mun.pol, color = "black", weight = 1, fillOpacity = 0, label = ~NOM_MUNI,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))

    } else if(input$display_stat == "IFN plots"){
      leafletProxy("map_stat") %>%
        clearShapes() %>%
        addCircleMarkers(data = IFN3.points, radius = 5, stroke = F, color="black", fillOpacity = 0.7, label = ~as.character(ID), layerId = ~as.character(ID),
                         clusterOptions = markerClusterOptions(showCoverageOnHover = T, disableClusteringAtZoom = 12))

    }
  })

  # create a reactive values sensitive to clicks on shapes or markers
  map_daily_click <- reactiveValues(x = list())
  observe({
    if(input$display_daily %in% c("Watersheds","Counties","Municipalities")) {
      map_daily_click$x <- input$map_daily_shape_click
    } else if(input$display_daily=="IFN plots") {
      map_daily_click$x <- input$map_daily_marker_click
    } else {
      map_daily_click$x <-NULL
    }
  })
  map_hist_click <- reactiveValues(x = list())
  observe({
    if(input$display_hist %in% c("Watersheds", "Counties","Municipalities")) {
      map_hist_click$x <- input$map_hist_shape_click
    } else if(input$display_hist=="IFN plots") {
      map_hist_click$x <- input$map_hist_marker_click
    } else {
      map_hist_click$x <-NULL
    }
  })
  map_proj_click <- reactiveValues(x = list())
  observe({
    if(input$display_proj %in% c("Watersheds","Counties","Municipalities")) {
      map_proj_click$x <- input$map_proj_shape_click
    } else if(input$display_proj=="IFN plots") {
      map_proj_click$x <- input$map_proj_marker_click
    } else {
      map_proj_click$x <-NULL
    }
  })




  # Create a reactive value data for trends
  map_daily_data <- reactiveValues(x = list())
  map_hist_data <- reactiveValues(x = list())
  map_proj_data <- reactiveValues(x = list())



  #Changes tab panel
  observeEvent(map_daily_click$x, {
    if(!is.null(map_daily_click$x)){
      updateTabsetPanel(session, "DailyTabset",
                        selected = "Selected series"

      )
    }
  })
  observeEvent(map_hist_click$x, {
    if(!is.null(map_hist_click$x)){
      updateTabsetPanel(session, "HistTabset",
                        selected = "Selected series"

      )
    }
  })
  observeEvent(map_proj_click$x, {
    if(!is.null(map_proj_click$x)){
      updateTabsetPanel(session, "ProjTabset",
                        selected = "Selected series"

      )
    }
  })

  # React to clicks on the daily map
  observe({
    if(!is.null(map_daily_click$x)){

      # Convert coordinates of the click zone into a spatial point
      clicker <- SpatialPoints(coords = data.frame(x = map_daily_click$x$lng, y = map_daily_click$x$lat), proj4string = mapCRS)

      if(input$display_daily == "Watersheds"){
        info <- clicker %over% con.pol
        colnames(info) <- c("Id", "Name", "Fl")
        info$type <- "watershed"
        IFN3_sel <- IFN3.points@data[IFN3.points$ID_CONCA == info$Id,]
      } else if(input$display_daily == "Counties"){
        info <- clicker %over% cat.pol
        colnames(info) <- c("Id", "Name", "Capital", "Superficy")
        info$type <- "county"
        IFN3_sel <- IFN3.points@data[IFN3.points$COMARCA == info$Id,]
      } else if(input$display_daily == "Municipalities"){
        info <- clicker %over% mun.pol
        colnames(info) <- c("Id", "County", "Province", "Name", "Name2", "Name3", "Capital", "Capital2", "Capital3", "Superficy", "X")
        info$type <- "municipality"
        IFN3_sel <- IFN3.points@data[IFN3.points$MUNICIPI == info$Id,]
      } else if(input$display_daily == "IFN plots"){
        IFN3_sel <- IFN3.points@data[as.character(IFN3.points$ID) == map_daily_click$x$id,]
        info <- data.frame(Name = paste("plot '",IFN3_sel$ID,"'"), type = "IFN plot")
      } else {
        IFN3_sel <- IFN3.points@data[rep(FALSE,length(IFN3.points$ID)),]
      }
      # print(head(IFN3_sel))

      # Open relevant files and extract informations regarding the selected variable
      if(nrow(IFN3_sel)>0){
        if(input$mode_daily %in% c("Climate", "Forest water balance")){
          folder <- paste0(data_home,"Rdata/Plots/SWBTrends")
          plots_id <- IFN3_sel$ID
          plots_id <- plots_id[as.character(plots_id) %in% available_plots_trends]
          load(paste(folder, "/", plots_id[1], ".rda", sep = ""))

          # open all the files of the individual plots
          data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
          for(i in 1:length(plots_id)){
            load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
            data[,,i] <- as.matrix(trends)
          }
        } else if(input$mode_daily == "Drought stress"){
          folder <- paste0(data_home,"Rdata/Plots/DroughtStressTrends")
          plots_id <- IFN3_sel$ID
          plots_id <- plots_id[as.character(plots_id) %in% available_plots_trends]
          load(paste(folder, "/", plots_id[1], ".rda", sep = ""))

          # open all the files of the individual plots
          data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
          for(i in 1:length(plots_id)){
            load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
            data[,,i] <- as.matrix(trends)
          }

        }
        # calculate mean and condidence interval
        means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
        ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
        ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()

        dates <- as.Date(rownames(means))
        map_daily_data$x<-list("dates"=dates, "ci_inf"=ci_inf, "means"=means, "ci_sup"=ci_sup, "info"=info, "nplots" = nrow(IFN3_sel))
      } else {
        map_daily_data$x <- NULL
      }
    } else {
      map_daily_data$x <- NULL
    }
  })
  # React to clicks on the historic map
  observe({
    if(!is.null(map_hist_click$x)){

      # Convert coordinates of the click zone into a spatial point
      clicker <- SpatialPoints(coords = data.frame(x = map_hist_click$x$lng, y = map_hist_click$x$lat), proj4string = mapCRS)

      if(input$display_hist == "Watersheds"){
        info <- clicker %over% con.pol
        colnames(info) <- c("Id", "Name", "Fl")
        info$type <- "watershed"
        IFN3_sel <- IFN3.points@data[IFN3.points$ID_CONCA == info$Id,]
      } else if(input$display_hist == "Counties"){
        info <- clicker %over% cat.pol
        colnames(info) <- c("Id", "Name", "Capital", "Superficy")
        info$type <- "county"
        IFN3_sel <- IFN3.points@data[IFN3.points$COMARCA == info$Id,]
      } else if(input$display_hist == "Municipalities"){
        info <- clicker %over% mun.pol
        colnames(info) <- c("Id", "County", "Province", "Name", "Name2", "Name3", "Capital", "Capital2", "Capital3", "Superficy", "X")
        info$type <- "municipality"
        IFN3_sel <- IFN3.points@data[IFN3.points$MUNICIPI == info$Id,]
      } else if(input$display_hist == "IFN plots"){
        IFN3_sel <- IFN3.points@data[as.character(IFN3.points$ID) == map_hist_click$x$id,]
        info <- data.frame(Name = paste("plot '",IFN3_sel$ID,"'"), type = "IFN plot")
      } else {
        IFN3_sel <- IFN3.points@data[rep(FALSE,length(IFN3.points$ID)),]
      }

      # Open relevant files and extract informations regarding the selected variable
      if(nrow(IFN3_sel)>0){
        folder <- paste0(data_home,"Rdata/Plots/HistoricSWB/IFN2-3/")
        plots_id <- IFN3_sel$ID
        plots_id <- plots_id[as.character(plots_id) %in% available_plots_historic]
        if(length(plots_id)>0) {
          load(paste(folder, "/", plots_id[1], ".rda", sep = ""))
          if(input$mode_hist %in% c("Climate","Forest water balance")){
            if(input$agg_hist== "Month") trends = swb_month
            else trends = swb_year
            # open all the files of the individual plots
            datest<-row.names(trends)
            data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
            for(i in 1:length(plots_id)){
              load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
              if(input$agg_hist== "Month") trends = swb_month
              else trends = swb_year
              data[,,i] <- as.matrix(trends[datest,])
            }
            means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
            ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
            ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()

            dates <- as.Date(datest)
            map_hist_data$x<-list("dates"=dates, "ci_inf"=ci_inf, "means"=means, "ci_sup"=ci_sup, "info"=info, "nplots" = nrow(IFN3_sel))
          }
          else if(input$mode_hist == "Drought stress"){
            if(!is.null(input$DS_hist)){
              if(input$DS_hist=="Stress intensity"){
                if(input$agg_hist== "Month") trends = dds_month
                else trends = dds_year
              } else {
                if(input$agg_hist== "Month") trends = ndd_month
                else trends = ndd_year
              }
              datest<-row.names(trends)
              # open all the files of the individual plots
              data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
              for(i in 1:length(plots_id)){
                load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
                if(input$DS_hist=="Stress intensity"){
                  if(input$agg_hist== "Month") trends = dds_month
                  else trends = dds_year
                } else {
                  if(input$agg_hist== "Month") trends = ndd_month
                  else trends = ndd_year
                }
                data[,,i] <- as.matrix(trends[datest,])
              }
              means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
              ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
              ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()

              dates <- as.Date(datest)
              map_hist_data$x<-list("dates"=dates, "ci_inf"=ci_inf, "means"=means, "ci_sup"=ci_sup, "info"=info, "nplots" = nrow(IFN3_sel))

            }
          }
        }
      }
    }
  })
  # React to clicks on the projection map
  observe({
    if(!is.null(map_proj_click$x)){

      # Convert coordinates of the click zone into a spatial point
      clicker <- SpatialPoints(coords = data.frame(x = map_proj_click$x$lng, y = map_proj_click$x$lat), proj4string = mapCRS)

      if(input$display_proj == "Watersheds"){
        info <- clicker %over% con.pol
        colnames(info) <- c("Id", "Name", "Fl")
        info$type <- "watershed"
        IFN3_sel <- IFN3.points@data[IFN3.points$ID_CONCA == info$Id,]
      } else if(input$display_proj == "Counties"){
        info <- clicker %over% cat.pol
        colnames(info) <- c("Id", "Name", "Capital", "Superficy")
        info$type <- "county"
        IFN3_sel <- IFN3.points@data[IFN3.points$COMARCA == info$Id,]
      } else if(input$display_proj == "Municipalities"){
        info <- clicker %over% mun.pol
        colnames(info) <- c("Id", "County", "Province", "Name", "Name2", "Name3", "Capital", "Capital2", "Capital3", "Superficy", "X")
        info$type <- "municipality"
        IFN3_sel <- IFN3.points@data[IFN3.points$MUNICIPI == info$Id,]
      } else if(input$display_proj == "IFN plots"){
        IFN3_sel <- IFN3.points@data[as.character(IFN3.points$ID) == map_proj_click$x$id,]
        info <- data.frame(Name = paste("plot '",IFN3_sel$ID,"'"), type = "IFN plot")
      } else {
        IFN3_sel <- IFN3.points@data[rep(FALSE,length(IFN3.points$ID)),]
      }

      # Open relevant files and extract informations regarding the selected variable
      if(nrow(IFN3_sel)>0){
        folder <- paste0(data_home,"Rdata/Plots/ProjectedSWB/", climate_models[input$rcm_proj],"/",input$rcp_proj)
        plots_id <- IFN3_sel$ID
        plots_id <- plots_id[as.character(plots_id) %in% available_plots_projections]
        if(length(plots_id)>0) {
          if(input$mode_proj %in% c("Climate","Forest water balance")){
            load(paste(folder, "/", plots_id[1], ".rda", sep = ""))
            if(input$agg_proj== "Month") trends = swb_month
            else trends = swb_year
            # open all the files of the individual plots
            data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
            for(i in 1:length(plots_id)){
              load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
              if(input$agg_proj== "Month") trends = swb_month
              else trends = swb_year
              data[,,i] <- as.matrix(trends)
            }

            # calculate mean and condidence interval
            means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
            ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
            ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()

            dates <- as.Date(rownames(means))
            map_proj_data$x<-list("dates"=dates, "ci_inf"=ci_inf, "means"=means, "ci_sup"=ci_sup, "info"=info, "nplots" = nrow(IFN3_sel))

          }
          else if(input$mode_proj == "Drought stress"){
            if(!is.null(input$DS_proj)){
              load(paste(folder, "/", plots_id[1], ".rda", sep = ""))
              if(input$DS_proj=="Stress intensity"){
                if(input$agg_proj== "Month") trends = dds_month
                else trends = dds_year
              } else {
                if(input$agg_proj== "Month") trends = ndd_month
                else trends = ndd_year
              }
              # open all the files of the individual plots
              data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
              for(i in 1:length(plots_id)){
                load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
                if(input$DS_proj=="Stress intensity"){
                  if(input$agg_proj== "Month") trends = dds_month
                  else trends = dds_year
                } else {
                  if(input$agg_proj== "Month") trends = ndd_month
                  else trends = ndd_year
                }
                data[,,i] <- as.matrix(trends)
              }
              # calculate mean and condidence interval
              means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
              ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
              ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()

              dates <- as.Date(rownames(means))
              map_proj_data$x<-list("dates"=dates, "ci_inf"=ci_inf, "means"=means, "ci_sup"=ci_sup, "info"=info, "nplots" = nrow(IFN3_sel))
            }
          }
        }
      }
    }
  })


  #Reacts to changes in variable selected and map_daily_data changes
  output$trends_daily<-renderDygraph({
    if(!is.null(map_daily_data$x)) {
      if(input$mode_daily == "Climate") {
        col <- as.character(clim_variables[clim_variables$input == input$clim_daily, "medfate"])
        title <- paste(input$clim_daily," at ",as.character(map_daily_data$x$info$Name))
        label=input$clim_daily
      } else if(input$mode_daily == "Forest water balance") {
        col <- as.character(WB_variables[WB_variables$input == input$WB_daily, "medfate"])
        title <- paste(input$WB_daily," at ",as.character(map_daily_data$x$info$Name))
        label=input$WB_daily
      } else {
        col <- as.character(species[species$input == input$sp_daily, "medfate"])
        title <- paste("Stress intensity for ", input$sp_daily," at ",as.character(map_daily_data$x$info$Name))
        label="Drought stress"
      }
      first=which(!is.na(map_daily_data$x$means[,col]))[1]
      end = length(map_daily_data$x$means[,col])
      m<-cbind( map_daily_data$x$ci_sup[first:end,col], map_daily_data$x$means[first:end,col],map_daily_data$x$ci_inf[first:end,col])
      colnames(m)<-c("lower", "mean","upper")
      x<-xts(m,map_daily_data$x$dates[first:end])
      if(map_daily_data$x$nplots>1) title<-title<-paste0(title, " (",map_daily_data$x$nplots," plots)")
      if(input$mode_daily=="Drought stress") {
        dygraph(x, main= title) %>%
          dySeries(c("lower", "mean","upper"), label=label) %>%
          dyRangeSelector() %>%
          dyAxis("y", label = "Stress intensity", valueRange = c(0, 1)) %>%
          dyLimit(0.5, color="red")
      } else {
        dygraph(x, main= title) %>%
          dySeries(c("lower", "mean","upper"), label=label) %>%
          dyRangeSelector()
      }
    }
  })

  # Reacts to changes in variable selected and map_hist_data changes
  output$trends_hist<-renderDygraph({
    if(!is.null(map_hist_data$x)) {
      if(input$mode_hist == "Climate") {
        col <- as.character(clim_variables[isolate(clim_variables$input == input$clim_hist), "medfate"])
        title <- paste(input$clim_hist," at ",as.character(map_hist_data$x$info$Name))
        label=input$clim_hist
      } else if(input$mode_hist == "Forest water balance") {
        col <- as.character(WB_variables[isolate(WB_variables$input == input$WB_hist), "medfate"])
        title <- paste(input$WB_hist," at ",as.character(map_hist_data$x$info$Name))
        label=input$WB_hist
      } else {
        col <- as.character(species[species$input == input$sp_hist, "medfate"])
        if(isolate(input$DS_hist=="Stress intensity")) title <- paste("Drought stress index for ", input$sp_hist," at ",as.character(map_hist_data$x$info$Name))
        else title <- paste("Stress duration for ", input$sp_hist," at ",as.character(map_hist_data$x$info$Name))
        label="Drought stress"
      }
      if(col %in% names(map_hist_data$x$means)){
        m<-cbind( map_hist_data$x$ci_sup[,col], map_hist_data$x$means[,col],map_hist_data$x$ci_inf[,col])
        colnames(m)<-c("lower", "mean","upper")
        rownames(m)<-as.character(map_hist_data$x$dates)
        if(input$agg_hist=="Month" && !input$allmonths_hist) {
          mth = as.numeric(format(as.Date(rownames(m)),"%m"))
          m = m[mth==as.numeric(input$trend_month_hist),]
        }
        x<-xts(m,as.Date(rownames(m)))
        if(map_hist_data$x$nplots>1) title<-title<-paste0(title, " (",map_hist_data$x$nplots," plots)")
        if(input$mode_hist=="Drought stress") {
          if(input$DS_hist=="Stress intensity"){
            dygraph(x, main= title) %>%
              dySeries(c("lower", "mean","upper"), label=label) %>%
              dyRangeSelector() %>%
              dyAxis("y", label = "Average stress intensity", valueRange = c(0, 1)) %>%
              dyLimit(0.5, color="red")
          } else {
            dygraph(x, main= title) %>%
              dySeries(c("lower", "mean","upper"), label=label) %>%
              dyRangeSelector() %>%
              dyAxis("y", label = "Maximum stress duration")
          }
        } else {
          dygraph(x, main= title) %>%
            dySeries(c("lower", "mean","upper"), label=label) %>%
            dyRangeSelector()
        }
      }
    }
  })
  #Reacts to changes in variable selected and map_proj_data changes
  output$trends_proj<-renderDygraph({
    if(!is.null(map_proj_data$x)) {
      if(input$mode_proj == "Climate") {
        col <- as.character(clim_variables[clim_variables$input == input$clim_proj, "medfate"])
        title <- paste(input$clim_proj," at ",as.character(map_proj_data$x$info$Name))
        label=input$clim_proj
      } else if(input$mode_proj == "Forest water balance") {
        col <- as.character(WB_variables[WB_variables$input == input$WB_proj, "medfate"])
        title <- paste(input$WB_proj," at ",as.character(map_proj_data$x$info$Name))
        label=input$WB_proj
      } else {
        col <- as.character(species[species$input == input$sp_proj, "medfate"])
        if(isolate(input$DS_proj=="Stress intensity")) title <- paste("Stress intensity index for ", input$sp_proj," at ",as.character(map_proj_data$x$info$Name))
        else title <- paste("Stress duration for ", input$sp_proj," at ",as.character(map_proj_data$x$info$Name))
        label="Drought stress"
      }
      if(col %in% names(map_proj_data$x$means)){
        title<- paste0(title," - ", input$rcm_proj," - ", input$rcp_proj)
        m<-cbind( map_proj_data$x$ci_sup[,col], map_proj_data$x$means[,col],map_proj_data$x$ci_inf[,col])
        colnames(m)<-c("lower", "mean","upper")
        rownames(m)<-as.character(map_proj_data$x$dates)
        if(input$agg_proj=="Month" && !input$allmonths_proj) {
          mth = as.numeric(format(as.Date(rownames(m)),"%m"))
          m = m[mth==as.numeric(input$trend_month_proj),]
        }
        x<-xts(m,as.Date(rownames(m)))
        if(map_proj_data$x$nplots>1) title<-title<-paste0(title, " (",map_proj_data$x$nplots," plots)")
        if(input$mode_proj=="Drought stress") {
          if(input$DS_proj=="Stress intensity"){
            dygraph(x, main= title) %>%
              dySeries(c("lower", "mean","upper"), label=label) %>%
              dyRangeSelector() %>%
              dyAxis("y", label = "Average stress intensity", valueRange = c(0, 1)) %>%
              dyLimit(0.5, color="red")
          } else {
            dygraph(x, main= title) %>%
              dySeries(c("lower", "mean","upper"), label=label) %>%
              dyRangeSelector() %>%
              dyAxis("y", label = "Maximum stress duration")
          }
        } else {
          dygraph(x, main= title) %>%
            dySeries(c("lower", "mean","upper"), label=label) %>%
            dyRangeSelector()
        }
      }
    }
  })

  #Downloads daily trends
  dailyTrends<-reactive({
    if(input$mode_daily == "Climate") {
      col <- as.character(clim_variables[clim_variables$input == input$clim_daily, "medfate"])
    } else if(input$mode_daily == "Forest water balance") {
      col <- as.character(WB_variables[WB_variables$input == input$WB_daily, "medfate"])
    } else {
      col <- as.character(species[species$input == input$sp_daily, "medfate"])
    }
    first=which(!is.na(map_daily_data$x$means[,col]))[1]
    end = length(map_daily_data$x$means[,col])
    m<-cbind( map_daily_data$x$ci_inf[first:end,col], map_daily_data$x$means[first:end,col],map_daily_data$x$ci_sup[first:end,col])
    colnames(m)<-c("lower", "mean","upper")
    rownames(m)<-as.character(map_daily_data$x$dates[first:end])
    # as.data.frame(m)
    m
  })
  output$downloadTrendDaily<-downloadHandler(
    filename = function() {
      paste("daily_trend.txt")
    },
    content = function(file) {
      write.table(dailyTrends(), file=file,sep="\t", quote=FALSE)
    }
  )

  #Downloads historic trends
  historicTrends<-reactive({
    if(input$mode_hist == "Climate") {
      col <- as.character(clim_variables[clim_variables$input == input$clim_hist, "medfate"])
    } else if(input$mode_hist == "Forest water balance") {
      col <- as.character(WB_variables[WB_variables$input == input$WB_hist, "medfate"])
    } else {
      col <- as.character(species[species$input == input$sp_hist, "medfate"])
    }
    first=which(!is.na(map_hist_data$x$means[,col]))[1]
    end = length(map_hist_data$x$means[,col])
    m<-cbind( map_hist_data$x$ci_inf[first:end,col], map_hist_data$x$means[first:end,col],map_hist_data$x$ci_sup[first:end,col])
    colnames(m)<-c("lower", "mean","upper")
    rownames(m)<-as.character(map_hist_data$x$dates[first:end])
    # as.data.frame(m)
    m
  })
  output$downloadTrendHist<-downloadHandler(
    filename = function() {
      paste("historic_trend.txt")
    },
    content = function(file) {
      write.table(historicTrends(), file=file,sep="\t", quote=FALSE)
    }
  )
  output$MK_hist<-renderPrint({
    if(!is.null(map_hist_data$x)) {
      if(isolate(input$mode_hist == "Climate")) {
        col <- as.character(clim_variables[clim_variables$input == input$clim_hist, "medfate"])
      } else if(input$mode_hist == "Forest water balance") {
        col <- as.character(WB_variables[WB_variables$input == input$WB_hist, "medfate"])
      } else {
        col <- as.character(species[species$input == input$sp_hist, "medfate"])
      }
      if((length(col)>0) && (!is.null(map_hist_data$x$means))){
        if(col %in% names(map_hist_data$x$means)){
          first=which(!is.na(map_hist_data$x$means[,col]))[1]
          end = length(map_hist_data$x$means[,col])
          m = map_hist_data$x$means[first:end,col]
          names(m)<-as.character(map_hist_data$x$dates[first:end])
          if(input$agg_hist=="Month" && !input$allmonths_hist) {
            mth = as.numeric(format(as.Date(names(m)),"%m"))
            m = m[mth==as.numeric(input$trend_month_hist)]
          }
          MannKendall(m)
        }
      }
    }
  })
  output$TS_slope_hist<-renderText({
    if(!is.null(map_hist_data$x)) {
      if(input$mode_hist == "Climate") {
        col <- as.character(clim_variables[clim_variables$input == input$clim_hist, "medfate"])
      } else if(input$mode_hist == "Forest water balance") {
        col <- as.character(WB_variables[WB_variables$input == input$WB_hist, "medfate"])
      } else {
        col <- as.character(species[species$input == input$sp_hist, "medfate"])
      }
      if(length(col)>0 && !is.null(map_hist_data$x$means)){
        if(col %in% names(map_hist_data$x$means)) {
          first=which(!is.na(map_hist_data$x$means[,col]))[1]
          end = length(map_hist_data$x$means[,col])
          m = map_hist_data$x$means[first:end,col]
          names(m)<-as.character(map_hist_data$x$dates[first:end])
          timeunit = "year"
          if(input$agg_hist=="Month") {
            timeunit = "month"
            if(!input$allmonths_hist) {
              mth = as.numeric(format(as.Date(names(m)),"%m"))
              m = m[mth==as.numeric(input$trend_month_hist)]
              timeunit="year"
            }
          }
          t = 1:length(m)
          z<-zyp.sen(m~t)
          paste0(signif(as.numeric(coefficients(z)[2])), " units per ", timeunit)
        }
      }
    }
  })


  #Downloads projected trends
  projectedTrends<-reactive({
    if(input$mode_proj == "Climate") {
      col <- as.character(clim_variables[clim_variables$input == input$clim_proj, "medfate"])
    } else if(input$mode_proj == "Forest water balance") {
      col <- as.character(WB_variables[WB_variables$input == input$WB_proj, "medfate"])
    } else {
      col <- as.character(species[species$input == input$sp_proj, "medfate"])
    }
    first=which(!is.na(map_proj_data$x$means[,col]))[1]
    end = length(map_proj_data$x$means[,col])
    m<-cbind( map_proj_data$x$ci_inf[first:end,col], map_proj_data$x$means[first:end,col],map_proj_data$x$ci_sup[first:end,col])
    colnames(m)<-c("lower", "mean","upper")
    rownames(m)<-as.character(map_proj_data$x$dates[first:end])
    # as.data.frame(m)
    m
  })
  output$downloadTrendProj<-downloadHandler(
    filename = function() {
      paste("projected_trend.txt")
    },
    content = function(file) {
      write.table(projectedTrends(), file=file,sep="\t", quote=FALSE)
    }
  )
  output$MK_proj<-renderPrint({
    if(!is.null(map_proj_data$x)) {
      if(input$mode_proj == "Climate") {
        col <- as.character(clim_variables[clim_variables$input == input$clim_proj, "medfate"])
      } else if(input$mode_proj == "Forest water balance") {
        col <- as.character(WB_variables[WB_variables$input == input$WB_proj, "medfate"])
      } else {
        col <- as.character(species[species$input == input$sp_proj, "medfate"])
      }
      if((length(col)>0) && (!is.null(map_proj_data$x$means))){
        first=which(!is.na(map_proj_data$x$means[,col]))[1]
        end = length(map_proj_data$x$means[,col])
        m = map_proj_data$x$means[first:end,col]
        names(m)<-as.character(map_proj_data$x$dates[first:end])
        if(input$agg_proj=="Month" && !input$allmonths_proj) {
          mth = as.numeric(format(as.Date(names(m)),"%m"))
          m = m[mth==as.numeric(input$trend_month_proj)]
        }
        MannKendall(m)
      }
    }
  })
  output$TS_slope_proj<-renderText({
    if(!is.null(map_proj_data$x)) {
      if(input$mode_proj == "Climate") {
        col <- as.character(clim_variables[clim_variables$input == input$clim_proj, "medfate"])
      } else if(input$mode_proj == "Forest water balance") {
        col <- as.character(WB_variables[WB_variables$input == input$WB_proj, "medfate"])
      } else {
        col <- as.character(species[species$input == input$sp_proj, "medfate"])
      }
      if((length(col)>0) && (!is.null(map_proj_data$x$means))){
        first=which(!is.na(map_proj_data$x$means[,col]))[1]
        end = length(map_proj_data$x$means[,col])
        m = map_proj_data$x$means[first:end,col]
        names(m)<-as.character(map_proj_data$x$dates[first:end])
        timeunit = "year"
        if(input$agg_proj=="Month") {
          timeunit = "month"
          if(!input$allmonths_proj) {
            mth = as.numeric(format(as.Date(names(m)),"%m"))
            m = m[mth==as.numeric(input$trend_month_proj)]
            timeunit="year"
          }
        }
        t = 1:length(m)
        z<-zyp.sen(m~t)
        paste0(signif(as.numeric(coefficients(z)[2])), " units per ", timeunit)
      }
    }
  })

  
  # What are the different inputs?
  # output$inputList_daily <- renderPrint({
  #   str(reactiveValuesToList(input))
  #   str(map_daily_click$x)
  #   str(map_daily_data$x)
  # })
  # output$inputList_hist <- renderPrint({
  #   str(reactiveValuesToList(input))
  # })
  # output$inputList_proj <- renderPrint({
  #   str(reactiveValuesToList(input))
  # })
})