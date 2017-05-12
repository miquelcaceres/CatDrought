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


# Rasters grid topology
gt1km=GridTopology(c(260000,4498000), c(1000,1000), c(262,253))
# IFN plots
load("//SERVERPROCESS/Miquel/CatDrought/Rdata/IFN3_SPT_cat.rdata")
IFN3.points <- SpatialPointsDataFrame(IFN3_SPT@coords, data.frame(ID = row.names(IFN3_SPT@coords)), proj4string = IFN3_SPT@proj4string)
IFN3.points <- spTransform(IFN3.points, CRSobj = mapCRS)
# Find county and municipality corresponding to each IFN plot
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

input_sp <- c("All woordy species", "Pinus halepensis", "Pinus nigra", "Pinus sylvestris", "Pinus uncinata", "Pinus pinea", 
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

log_trans <- function(dom, n = 10, digits = 1) {signif(exp(seq(log(dom[1]+1), log(dom[2]+1), length.out = n))-1, digits = digits)}
identity_trans <- function(dom, n = 10, digits = 1) {signif(seq(dom[1], dom[2], length.out = n), digits = digits)}


########################################################
### Define server logic required to draw a histogram ###
########################################################

shinyServer(function(input, output) {
  # Switch the mode of the daily output 
  observe({
    if(input$mode_daily == "Climate") {
      input_name <- "clim_daily"
      input_title <- "Choose variable"
      var_choice_daily <- input_clim_var[1:2]
      selected <- "Precipitation (mm)"
    } else if(input$mode_daily == "Water balance") {
      input_name <- "WB_daily"
      input_title <- "Choose variable"
      var_choice_daily <- input_WB_var
      selected <- "Relative soil water content [0-1]"
    } else {
      input_name <- "sp_daily"
      input_title <- "Choose species"
      var_choice_daily <- input_sp
      selected <- "All woody species"
    }
    
    output$var_choice_daily <- renderUI({
      selectInput(input_name, input_title, choices = var_choice_daily, selected = selected)
    })
  })
    
  # Switch the mode of the historic output 
  observe({
    if(input$mode_hist == "Climate") {
      input_name <- "clim_hist"
      input_title <- "Choose variable"
      if(input$agg_hist=="Month") var_choice_hist <- input_clim_var
      else var_choice_hist <- input_clim_var[1:2]
      selected <- "Precipitation (mm)"
    } else if(input$mode_hist == "Water balance") {
      input_name <- "WB_hist"
      input_title <- "Choose variable"
      var_choice_hist <- input_WB_var[-2]
      selected <- "Relative soil water content [0-1]"
    } else {
      input_name <- "sp_hist"
      input_title <- "Choose species"
      var_choice_hist <- input_sp
      selected <- "All woody species"
    }
    
    output$var_choice_hist <- renderUI({
      selectInput(input_name, input_title, choices = var_choice_hist, selected = selected)
    })
  })
  
  # Switch the mode of the projection output 
  observe({
    if(input$mode_proj == "Climate") {
      input_name <- "clim_proj"
      input_title <- "Choose variable"
      if(input$agg_proj=="Month") var_choice_proj <- input_clim_var
      else var_choice_proj <- input_clim_var[1:2]
      selected <- "Precipitation (mm)"
    } else if(input$mode_proj == "Water balance") {
      input_name <- "WB_proj"
      input_title <- "Choose variable"
      var_choice_proj <- input_WB_var[-2]
      selected <- "Relative soil water content [0-1]"
    } else {
      input_name <- "sp_proj"
      input_title <- "Choose species"
      var_choice_proj <- input_sp
      selected <- "All woody species"
    }
    
    output$var_choice_proj <- renderUI({
      selectInput(input_name, input_title, choices = var_choice_proj, selected = selected)
    })
  })
  
  # Create an interactive map centered on catalonia
  output$map_daily <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12)) %>%
      addProviderTiles("Stamen.TerrainBackground") %>%
      setView(lng = 1.74,lat = 41.69, zoom = 8)
    
  })
  observe({
    leafletProxy("map_daily") %>%
      clearTiles() %>%
      addProviderTiles(input$basemap_daily)
  }) 
  # Add raster layers for daily drought
  observe({
    # print(input$var_daily)
    if(input$mode_daily == "Climate"){
      if(!is.null(input$clim_daily)){
        folder <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Maps"
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
        
        r <- raster(spdf)
        proj4string(r) <- dataCRS
        r <- projectRaster(r, crs = mapCRS)
        
        dom <- c(pal_clim[input$clim_daily,"min"],pal_clim[input$clim_daily,"max"])
        bins <- do.call(paste(pal_clim[input$clim_daily, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
        
        pal <- colorBin(pal_clim[input$clim_daily,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_clim[input$clim_daily, "rev"])
        
        leafletProxy("map_daily") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(r, opacity = input$alpha_daily, colors = pal) %>% 
          addLegend(pal = pal, values = values(r),opacity = input$alpha_daily, position = "bottomright")
      } 
    } else if(input$mode_daily == "Water balance"){
      if(!is.null(input$WB_daily)){
        folder <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Maps"
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
        
        leafletProxy("map_daily") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(r, opacity = input$alpha_daily, colors = pal) %>% 
          addLegend(pal = pal, values = values(r),opacity = input$alpha_daily, position = "bottomright")
      } 
    } else {
      if(!is.null(input$sp_daily)){
        folder <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Maps"
        col <- as.character(species[species$input == input$sp_daily, "medfate"])
        
        dfin = input$date_daily
        dini  = max(as.Date("2017-01-01"),dfin-(as.numeric(input$agg_daily)-1))
        dw = seq(dini, dfin, by="day")
        nd = length(dw)
        load(paste(folder, "/", input$resolution_daily, "/DroughtStress/",col, "/", dw[1], ".rda", sep = ""))
        spdftmp = spdf
        if(nd>1) {
          for(d in 2:nd) {
            load(paste(folder, "/", input$resolution_daily, "/DroughtStress/",col, "/", dw[d], ".rda", sep = ""))
            spdftmp@data = spdftmp@data + spdf@data
          }
        }
        spdftmp@data =spdftmp@data /nd
        spdf = spdftmp
        
        
        r <- raster(spdf)
        proj4string(r) <- dataCRS
        r <- projectRaster(r, crs = mapCRS)
        
        dom <- c(0,1)
        bins <- seq(0,1,length.out = 15)
        
        pal <- colorBin("RdYlBu", domain = dom, na.color = "transparent", bins = bins, reverse = T)
        # leaflet() %>% addRasterImage(r, opacity = input$alpha_daily, colors = pal) %>% addLegend(pal = pal, values = values(r))
        
        leafletProxy("map_daily") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(r, opacity = input$alpha_daily, colors = pal) %>% 
          addLegend(pal = pal, values = values(r),opacity = input$alpha_daily, position = "bottomright")
      }
    }
  })
  
  # Create an interactive map centered on catalonia
  output$map_hist <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12)) %>%
      addProviderTiles("Stamen.TerrainBackground") %>%
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
      addProviderTiles("Stamen.TerrainBackground") %>%
      setView(lng = 1.74,lat = 41.69, zoom = 8)
  })
  observe({
    leafletProxy("map_proj") %>%
      clearTiles() %>%
      addProviderTiles(input$basemap_proj)
  })  

  # Add shapes to daily SWB
  observe({
    # print(paste0("Daily ",input$display_daily))
    if(input$display_daily == "none"){
      leafletProxy("map_daily") %>%
        clearShapes() %>%
        clearMarkerClusters()
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
            addCircleMarkers(data = IFN3.points[IFN3.points$ID %in% available_plots_trends,], radius = 5, color="black", stroke = F, fillOpacity = 0.7, label = ~as.character(ID), layerId = ~as.character(ID),  
                             clusterOptions = markerClusterOptions(showCoverageOnHover = T, disableClusteringAtZoom = 12))
          
    }
  })
  # Add shapes to historic SWB
  observe({
    if(input$display_hist == "none"){
      leafletProxy("map_hist") %>%
        clearShapes() %>%
        clearMarkerClusters()
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
        addCircleMarkers(data = IFN3.points[IFN3.points$ID %in% available_plots_historic,], radius = 5, stroke = F, color="black", fillOpacity = 0.7, label = ~as.character(ID), layerId = ~as.character(ID),  
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
  
  # create a reactive value sensitive to clicks on shapes or markers
  map_daily_click <- reactiveValues(x = list())
  observe({
    if(input$display_daily %in% c("Counties","Municipalities")) {
      map_daily_click$x <- input$map_daily_shape_click
    } else if(input$display_daily=="IFN plots") {
      map_daily_click$x <- input$map_daily_marker_click
    } else {
      map_daily_click$x <-NULL
    }
  })
  
  map_hist_click <- reactiveValues(x = list())
  observe({
    if(input$display_hist %in% c("Counties","Municipalities")) {
      map_hist_click$x <- input$map_hist_shape_click
    } else if(input$display_hist=="IFN plots") {
      map_hist_click$x <- input$map_hist_marker_click
    } else {
      map_hist_click$x <-NULL
    }
  })
  map_proj_click <- reactiveValues(x = list())
  observe({
    if(input$display_proj %in% c("Counties","Municipalities")) {
      map_proj_click$x <- input$map_proj_shape_click
    } else if(input$display_proj=="IFN plots") {
      map_proj_click$x <- input$map_proj_marker_click
    } else {
      map_proj_click$x <-NULL
    }
  })
  

  # Create a reactive value data
  map_daily_data <- reactiveValues(x = list())
  map_hist_data <- reactiveValues(x = list())
  map_proj_data <- reactiveValues(x = list())
  
  # React to clicks on the daily map 
  observe({
    if(!is.null(map_daily_click$x)){
      
      # Convert coordinates of the click zone into a spatial point
      clicker <- SpatialPoints(coords = data.frame(x = map_daily_click$x$lng, y = map_daily_click$x$lat), proj4string = mapCRS) 
      
      if(input$display_daily == "Counties"){
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
        if(input$mode_daily %in% c("Climate", "Water balance")){
          folder <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/SWBTrends"
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
            folder <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/DroughtStressTrends"
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
  
  #Reacts to changes in variable selected and map_daily_data changes
  output$trends_daily<-renderDygraph({
    if(!is.null(map_daily_data$x)) {
      if(input$mode_daily == "Climate") {
        col <- as.character(clim_variables[clim_variables$input == input$clim_daily, "medfate"])
        title <- paste(input$clim_daily," at ",as.character(map_daily_data$x$info$Name))
        label=input$clim_daily
      } else if(input$mode_daily == "Water balance") {
        col <- as.character(WB_variables[WB_variables$input == input$WB_daily, "medfate"])
        title <- paste(input$WB_daily," at ",as.character(map_daily_data$x$info$Name))
        label=input$WB_daily
      } else {
        col <- as.character(species[species$input == input$sp_daily, "medfate"])
        title <- paste("Drought stress index for ", input$sp_daily," at ",as.character(map_daily_data$x$info$Name))
        label="Drought stress"
      }
      first=which(!is.na(map_daily_data$x$means[,col]))[1]
      end = length(map_daily_data$x$means[,col])
      m<-cbind( map_daily_data$x$ci_sup[first:end,col], map_daily_data$x$means[first:end,col],map_daily_data$x$ci_inf[first:end,col])
      colnames(m)<-c("lower", "mean","upper")
      x<-xts(m,map_daily_data$x$dates[first:end])
      if(map_daily_data$x$nplots>1) title<-title<-paste0(title, " (",map_daily_data$x$nplots," plots)")
      dygraph(x, main= title) %>% dySeries(c("lower", "mean","upper"), label=label) %>% dyRangeSelector()
    }
  })

  # React to clicks on the historic map 
  observe({

    if(!is.null(map_hist_click$x)){
      
      # Convert coordinates of the click zone into a spatial point
      clicker <- SpatialPoints(coords = data.frame(x = map_hist_click$x$lng, y = map_hist_click$x$lat), proj4string = mapCRS) 
      
      if(input$display_hist == "Counties"){
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
        folder <- paste0("//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/HistoricSWB/")
        plots_id <- IFN3_sel$ID
        plots_id <- plots_id[as.character(plots_id) %in% available_plots_historic]
        if(length(plots_id)>0) {
          load(paste(folder, "/", plots_id[1], ".rda", sep = ""))
          if(input$mode_hist %in% c("Climate","Water balance")){
            if(input$agg_hist== "Month") trends = swb_month
            else trends = swb_year
            # open all the files of the individual plots
            data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
            for(i in 1:length(plots_id)){
              load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
              if(input$agg_hist== "Month") trends = swb_month
              else trends = swb_year
              data[,,i] <- as.matrix(trends)
            }
          } else if(input$mode_hist == "Drought stress"){ 
            
            if(input$agg_hist== "Month") trends = dds_month
            else trends = dds_year
            # open all the files of the individual plots
            data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
            for(i in 1:length(plots_id)){
              load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
              if(input$agg_hist== "Month") trends = dds_month
              else trends = dds_year
              data[,,i] <- as.matrix(trends)
            }
          }
          # calculate mean and condidence interval
          means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
          ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
          ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()
          
          dates <- as.Date(rownames(means))
          map_hist_data$x<-list("dates"=dates, "ci_inf"=ci_inf, "means"=means, "ci_sup"=ci_sup, "info"=info, "nplots" = nrow(IFN3_sel))
        } else {
          map_hist_data$x <- NULL
        }
      } else {
        map_hist_data$x <- NULL
      }
    } else {
      map_hist_data$x <- NULL
    }
  })
  
  #Reacts to changes in variable selected and map_hist_data changes
  output$trends_hist<-renderDygraph({
    if(!is.null(map_hist_data$x)) {
      if(input$mode_hist == "Climate") {
        col <- as.character(clim_variables[clim_variables$input == input$clim_hist, "medfate"])
        title <- paste(input$clim_hist," at ",as.character(map_hist_data$x$info$Name))
        label=input$clim_hist
      } else if(input$mode_hist == "Water balance") {
        col <- as.character(WB_variables[WB_variables$input == input$WB_hist, "medfate"])
        title <- paste(input$WB_hist," at ",as.character(map_hist_data$x$info$Name))
        label=input$WB_hist
      } else {
        col <- as.character(species[species$input == input$sp_hist, "medfate"])
        title <- paste("Drought stress index for ", input$sp_hist," at ",as.character(map_hist_data$x$info$Name))
        label="Drought stress"
      } 
      m<-cbind( map_hist_data$x$ci_sup[,col], map_hist_data$x$means[,col],map_hist_data$x$ci_inf[,col])
      colnames(m)<-c("lower", "mean","upper")
      x<-xts(m,map_hist_data$x$dates)
      if(map_hist_data$x$nplots>1) title<-title<-paste0(title, " (",map_hist_data$x$nplots," plots)")
      dygraph(x, main= title) %>% dySeries(c("lower", "mean","upper"), label=label) %>% dyRangeSelector()
    }
  })
  

  # React to clicks on the projection map 
  observe({
    if(!is.null(map_proj_click$x)){
      
      # Convert coordinates of the click zone into a spatial point
      clicker <- SpatialPoints(coords = data.frame(x = map_proj_click$x$lng, y = map_proj_click$x$lat), proj4string = mapCRS) 
      
      if(input$display_proj == "Counties"){
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
        folder <- paste0("//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/ProjectedSWB/", climate_models[input$rcm_proj],"/",input$rcp_proj)
        plots_id <- IFN3_sel$ID
        plots_id <- plots_id[as.character(plots_id) %in% available_plots_projections]
        if(length(plots_id)>0) {
          load(paste(folder, "/", plots_id[1], ".rda", sep = ""))
          if(input$mode_proj %in% c("Climate","Water balance")){
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
          }
          else if(input$mode_proj == "Drought stress"){ 
            
            if(input$agg_proj== "Month") trends = dds_month
            else trends = dds_year
            # open all the files of the individual plots
            data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
            for(i in 1:length(plots_id)){
              load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
              if(input$agg_proj== "Month") trends = dds_month
              else trends = dds_year
              data[,,i] <- as.matrix(trends)
            }
          }
          # calculate mean and condidence interval
          means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
          ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
          ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()
          
          dates <- as.Date(rownames(means))
          map_proj_data$x<-list("dates"=dates, "ci_inf"=ci_inf, "means"=means, "ci_sup"=ci_sup, "info"=info, "nplots" = nrow(IFN3_sel))
        } else {
          map_proj_data$x <- NULL
        }
      } else {
        map_proj_data$x <- NULL
      }
    } else {
      map_proj_data$x <- NULL
    }
  })
  
  #Reacts to changes in variable selected and map_proj_data changes
  output$trends_proj<-renderDygraph({
    if(!is.null(map_proj_data$x)) {
      if(input$mode_proj == "Climate") {
        col <- as.character(clim_variables[clim_variables$input == input$clim_proj, "medfate"])
        title <- paste(input$clim_proj," at ",as.character(map_proj_data$x$info$Name))
        label=input$clim_proj
      } else if(input$mode_proj == "Water balance") {
        col <- as.character(WB_variables[WB_variables$input == input$WB_proj, "medfate"])
        title <- paste(input$WB_proj," at ",as.character(map_proj_data$x$info$Name))
        label=input$WB_proj
      } else {
        col <- as.character(species[species$input == input$sp_proj, "medfate"])
        title <- paste("Drought stress index for ", input$sp_proj," at ",as.character(map_proj_data$x$info$Name))
        label="Drought stress"
      }
      title<- paste0(title," - ", input$rcm_proj," - ", input$rcp_proj)
      m<-cbind( map_proj_data$x$ci_sup[,col], map_proj_data$x$means[,col],map_proj_data$x$ci_inf[,col])
      colnames(m)<-c("lower", "mean","upper")
      x<-xts(m,map_proj_data$x$dates)
      if(map_proj_data$x$nplots>1) title<-title<-paste0(title, " (",map_proj_data$x$nplots," plots)")
      dygraph(x, main= title) %>% dySeries(c("lower", "mean","upper"), label=label) %>% dyRangeSelector()
    }
  })

  # What are the different inputs?
  output$inputList_daily <- renderPrint({
    str(reactiveValuesToList(input))
    str(map_daily_click$x)
    str(map_daily_data$x)
  })
  output$inputList_hist <- renderPrint({
    str(reactiveValuesToList(input))
  })
  output$inputList_proj <- renderPrint({
    str(reactiveValuesToList(input))
  })
})