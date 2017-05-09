library(shiny)
# App lib requisites
library(sp)
library(rgdal)
library(viridis)
library(raster)
library(scales)
library(chron)
library(leaflet)


# App data requisites
mapCRS  <- CRS("+init=epsg:4326")
catpolCRS <- CRS("+init=epsg:23031")
dataCRS <- CRS("+init=epsg:25831")
# Catalunya boundaries
cat.pol <- readOGR(dsn = path.expand("../www/Comarques shapefile"), encoding = "UTF-8")
proj4string(cat.pol) <- catpolCRS
cat.pol <- spTransform(cat.pol, CRSobj = mapCRS)
# Municipalities boundaries
mun.pol <- readOGR(dsn = path.expand("../www/Municipis shapefile"), encoding = "UTF-8")
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


## Variable names correspondance between ui and medfate outputs
input_var <- c("Precipitation (mm)", "Net precipitation (mm)", "Potential evapo-transpiration (mm)", "LAI (m2/m2)","Plants transpiration (mm)", "Soil evaporation (mm)", "Run-off (mm)", "Deep drainage (mm)", 
               "Relative soil water content (0-1)")
medfate_var <- c("Rain", "NetPrec", "PET","LAI", "Eplant", "Esoil", "Runoff", "DeepDrainage", "Theta")
variables <- data.frame(input = input_var, medfate = medfate_var)

input_sp <- c("All", "Pinus halepensis", "Pinus nigra", "Pinus sylvestris", "Pinus uncinata", "Pinus pinea", 
              "Pinus pinaster", "Quercus ilex", "Quercus suber", "Quercus humilis", "Quercus faginea", "Fagus sylvatica")
medfate_sp <- c("Overall", "PinusHalepensis", "PinusNigra", "PinusSylvestris", "PinusUncinata", "PinusPinea", "PinusPinaster", 
                "QuercusIlex", "QuercusSuber", "QuercusHumilis", "QuercusFaginea", "FagusSylvatica")
species <- data.frame(input = input_sp, medfate = medfate_sp)

# temporal<-data.frame(row.names = c("Day","Week", "Month"), medfate=c(""," weekly", " monthly"))

## Define color scales for rasters 
pal_WB <- as.data.frame(matrix(NA, nrow = length(input_var), ncol = 5, dimnames = list(input_var, c("min", "max", "color", "trans", "rev"))))
pal_WB$min <- 0
pal_WB$color <- "Spectral"
pal_WB$trans <- "log"
pal_WB$rev <- F
pal_WB[c("Precipitation (mm)", "Net precipitation (mm)"), "max"] <- 100
pal_WB[c("Potential evapo-transpiration (mm)", "Plants transpiration (mm)", "Soil evaporation (mm)"), "max"] <- 15
pal_WB[c("Plants transpiration (mm)", "Soil evaporation (mm)"), "max"] <- 5
pal_WB[c("Run-off (mm)", "Deep drainage (mm)"), "max"] <- 15
pal_WB["Relative soil water content (0-1)", "max"] <- 1
pal_WB[c("Precipitation (mm)", "Net precipitation (mm)"), "rev"] <- F
pal_WB[c("Precipitation (mm)", "Net precipitation (mm)"), "color"] <- "Blues"
pal_WB[c("Potential evapo-transpiration (mm)", "Plants transpiration (mm)", "Soil evaporation (mm)"), "color"] <- "Greens"
pal_WB[c("Run-off (mm)", "Deep drainage (mm)"), "color"] <- "Reds"
pal_WB["Relative soil water content (0-1)", "color"] <- "RdYlBu"
pal_WB["Relative soil water content (0-1)", "trans"] <- "identity"
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
    if(input$mode_daily == "Water balance") {
      input_name <- "var_daily"
      input_title <- "Choose variable"
      var_choice_daily <- input_var
      selected <- "Precipitation"
    } else {
      input_name <- "sp_daily"
      input_title <- "Choose species"
      var_choice_daily <- input_sp
      selected <- "All"
    }
    
    output$var_choice_daily <- renderUI({
      selectInput(input_name, input_title, choices = var_choice_daily, selected = selected)
    })
  })
    
  # Switch the mode of the projection output 
  observe({
    if(input$mode_proj == "Water balance") {
      input_name <- "var_proj"
      input_title <- "Choose variable"
      var_choice_proj <- input_var
      selected <- "Precipitation"
    } else {
      input_name <- "sp_proj"
      input_title <- "Choose species"
      var_choice_proj <- input_sp
      selected <- "All"
    }
    
    output$var_choice_proj <- renderUI({
      selectInput(input_name, input_title, choices = var_choice_proj, selected = selected)
    })
  })
  
  # Create an interactive map centered on catalonia
  output$map_daily <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12)) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      setView(lng = 1.74,lat = 41.69, zoom = 8)
  })
  # Create an interactive map centered on catalonia
  output$map_proj <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12)) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      setView(lng = 1.74,lat = 41.69, zoom = 8)
  })
  
  # Add raster layers
  observe({
    # print(input$var_daily)
    if(input$mode_daily == "Water balance"){
      if(!is.null(input$var_daily)){
        folder <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Maps"
        col <- as.character(variables[variables$input == input$var_daily, "medfate"])
        d = input$date_daily
        if(input$agg_daily=="none") {
          load(paste(folder, "/", input$resolution_daily, "/SWB/",col, "/", d, ".rda", sep = ""))
        } else if(input$agg_daily=="1 week") {
          dfin = input$date_daily
          dini  = dfin-7
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
        } else if(input$agg_daily=="2 weeks") {
          dfin = input$date_daily
          dini  = dfin-14
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
        }
        
        r <- raster(spdf)
        proj4string(r) <- dataCRS
        r <- projectRaster(r, crs = mapCRS)
        
        dom <- c(pal_WB[input$var_daily,"min"],pal_WB[input$var_daily,"max"])
        # print(pal_WB[input$var_daily, "trans"])
        # print(dom)
        bins <- do.call(paste(pal_WB[input$var_daily, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
        
        pal <- colorBin(pal_WB[input$var_daily,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_WB[input$var_daily, "rev"])
        # leaflet() %>% addRasterImage(r, opacity = input$alpha_daily, colors = pal) %>% addLegend(pal = pal, values = values(r))
        
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
        load(paste(folder, "/", input$resolution_daily,"/DroughtStress/",col, "/", input$date_daily, ".rda", sep = ""))
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
  
  # Add shapes to daily SWB
  observe({
    print(paste0("Daily ",input$display_daily))
    if(input$display_daily == "Counties"){
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
            addCircleMarkers(data = IFN3.points, radius = 5, color="black", stroke = F, fillOpacity = 0.7, label = ~as.character(ID), layerId = ~as.character(ID),  
                             clusterOptions = markerClusterOptions(showCoverageOnHover = T, disableClusteringAtZoom = 12))
          
    }
  })
  
  # Add shapes to projected SWB
  observe({
    print(paste0("Proj ",input$display_proj))
    if(input$display_proj == "Counties"){
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
            addCircleMarkers(data = IFN3.points, radius = 5, stroke = F, color="black", fillOpacity = 0.7, label = ~as.character(ID), layerId = ~as.character(ID),  
                             clusterOptions = markerClusterOptions(showCoverageOnHover = T, disableClusteringAtZoom = 12))
          
    }
  })
  
  # create a reactive value sensitive to clicks on shapes or markers
  map_daily_click <- reactiveValues(x = list())
  observe({
    map_daily_click$x <- input$map_daily_shape_click
  })
  observe({
    map_daily_click$x <- input$map_daily_marker_click
  })
  map_proj_click <- reactiveValues(x = list())
  observe({
    map_proj_click$x <- input$map_proj_shape_click
  })
  observe({
    map_proj_click$x <- input$map_proj_marker_click
  })
  
  # React to clicks on the map (using observeEvent() instead of observe() allows to trigger code only when the value of input$map_shape_click changes)
  observeEvent(map_daily_click$x,{
    print("hola")
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
          info <- data.frame(Name = IFN3_sel$ID, type = "IFN plot")
      }
      print(head(IFN3_sel))
      
      output$pol_info_daily <- renderPrint({
        cat("Shape type: ", info$type, "; name: ", as.character(info$Name), sep = "")
        cat("\nContains", nrow(IFN3_sel), "plots from the 3rd Spanish Forest Inventory")
      })
      
      # Open relevant files and extract informations regarding the selected variable 
      if(nrow(IFN3_sel)>0){
        if(input$mode_daily == "Water balance"){
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
          
          # calculate mean and condidence interval
          means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
          ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
          ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()
          
          col <- as.character(variables[variables$input == input$var_daily, "medfate"])
          dates <- as.Date(rownames(means))
          
          output$trends_daily <- renderPlot({
            plot(dates, ci_sup[,col], type = "l", xlab = "Date", ylab = paste(input$var_daily), ylim = c(0, max(ci_sup[,col], na.rm = T)), col = "red", lty = 3)
            lines(dates, ci_inf[,col], col = "red", lty = 3)
            lines(dates, means[,col])
          })
          
        } else {
          if(input$mode_daily == "Drought stress"){ 
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
            
            # calculate mean and condidence interval
            means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
            ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
            ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()
            
            col <- as.character(species[species$input == input$sp, "medfate"])
            dates <- as.Date(rownames(means))
            filled <- !is.na(means[,col]) 
            output$trends <- renderPlot({
              plot(dates, ci_sup[,col], type = "l", xlab = "Date", ylab = paste("Drought stress index of", input$species), ylim = c(0,1), col = "red", lty = 3)
              lines(dates, ci_inf[,col], col = "red", lty = 3)
              lines(dates, means[,col])
            })
            
          } else {}
        }
      } else {}
    } else {}
  })
  
  # React to clicks on the map (using observeEvent() instead of observe() allows to trigger code only when the value of input$map_shape_click changes)
  observeEvent(map_proj_click$x,{
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
        info <- data.frame(Name = IFN3_sel$ID, type = "IFN plot")
      }
      output$pol_info_proj <- renderPrint({
        cat("Shape type: ", info$type, "; name: ", as.character(info$Name), sep = "")
        cat("\nContains", nrow(IFN3_sel), "plots from the 3rd Spanish Forest Inventory")
      })
      
      # Open relevant files and extract informations regarding the selected variable 
      if(nrow(IFN3_sel)>0){
        folder <- paste0("//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/ProjectedSWB/", input$rcm_proj,"/",input$rcp_proj)
        plots_id <- IFN3_sel$ID
        plots_id <- plots_id[as.character(plots_id) %in% available_plots_projections]
        if(length(plots_id)>0) {
          load(paste(folder, "/", plots_id[1], ".rda", sep = ""))
          if(input$mode_proj == "Water balance"){
            if(input$agg_proj== "Month") trends = swb_month
            else trends = swb_year
            
            # open all the files of the individual plots
            data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
            for(i in 1:length(plots_id)){
              load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
              data[,,i] <- as.matrix(trends)
            }
            
            # calculate mean and condidence interval
            means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
            ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
            ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()
            
            col <- as.character(variables[variables$input == input$var_proj, "medfate"])
            dates <- as.Date(rownames(means))
            
            output$trends_proj <- renderPlot({
              print(ci_sup[,col])
              plot(dates, ci_sup[,col], type = "l", xlab = "", ylab = paste(input$var_proj), ylim = c(min(ci_inf[,col], na.rm = T), max(ci_sup[,col], na.rm = T)), col = "red", lty = 3)
              lines(dates, ci_inf[,col], col = "red", lty = 3)
              lines(dates, means[,col])
            })
            
          }
          else if(input$mode_proj == "Drought stress"){ 
            if(input$agg_proj== "Month") trends = dds_month
            else trends = dds_year
            
            # open all the files of the individual plots
            data <- array(NA, dim = c(nrow(trends),ncol(trends),length(plots_id)), dimnames = list(rownames(trends), colnames(trends), plots_id))
            for(i in 1:length(plots_id)){
              load(paste(folder, "/", plots_id[i], ".rda", sep = ""))
              data[,,i] <- as.matrix(trends)
            }
            
            # calculate mean and condidence interval
            means <- apply(data, MARGIN = c(1,2), FUN = mean, na.rm = T) %>% as.data.frame()
            ci_sup <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.975, na.rm = T)) %>% as.data.frame()
            ci_inf <- apply(data, MARGIN = c(1,2), FUN = function(x) quantile(x, p = 0.025, na.rm = T)) %>% as.data.frame()
            
            col <- as.character(species[species$input == input$sp_proj, "medfate"])
            dates <- as.Date(rownames(means))
            filled <- !is.na(means[,col]) 
            output$trends_proj <- renderPlot({
              plot(dates, ci_sup[,col], type = "l", xlab = "Date", ylab = paste("Drought stress index of", input$species), ylim = c(0,1), col = "red", lty = 3)
              lines(dates, ci_inf[,col], col = "red", lty = 3)
              lines(dates, means[,col])
            })
            
          }
        }
      } 
    } 
  })
  
  # What are the different inputs?
  output$inputList_daily <- renderPrint({
    str(reactiveValuesToList(input))
    str(map_daily_click$x)
  })
  # What are the different inputs?
  output$inputList_proj <- renderPrint({
    str(reactiveValuesToList(input))
    str(map_proj_click$x)
  })
})