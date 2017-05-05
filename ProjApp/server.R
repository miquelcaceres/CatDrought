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
folder <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/ProjectedSWB/CCLM4-8-17/rcp4.5/"
available_plots <- list.files(folder)
available_plots <- unlist(strsplit(available_plots,split = ".rda"))


## Variable names correspondance between ui and medfate outputs
input_var <- c("Precipitation", "Net precipitation", "Potential evapo-transpiration","Plants transpiration", "Soil evaporation", "Run-off", "Deep drainage", 
               "Relative soil water content")
medfate_var <- c("Rain", "NetPrec", "PET","Eplant", "Esoil", "Runoff", "DeepDrainage", "Theta")
variables <- data.frame(input = input_var, medfate = medfate_var)

input_sp <- c("All", "Pinus halepensis", "Pinus nigra", "Pinus sylvestris", "Pinus uncinata", "Pinus pinea", 
              "Pinus pinaster", "Quercus ilex", "Quercus suber", "Quercus humilis", "Quercus faginea", "Fagus sylvatica")
medfate_sp <- c("Overall", "PinusHalepensis", "PinusNigra", "PinusSylvestris", "PinusUncinata", "PinusPinea", "PinusPinaster", 
                "QuercusIlex", "QuercusSuber", "QuercusHumilis", "QuercusFaginea", "FagusSylvatica")
species <- data.frame(input = input_sp, medfate = medfate_sp)


########################################################
### Define server logic required to draw a histogram ###
########################################################

shinyServer(function(input, output) {
  # Switch the mode of the output 
  observe({
    if(input$mode == "Water balance") {
      input_name <- "var"
      input_title <- "Choose variable"
      var_choice <- input_var
      selected <- "Precipitation"
    } else {
      input_name <- "sp"
      input_title <- "Choose species"
      var_choice <- input_sp
      selected <- "All"
    }
    
    output$var_choice <- renderUI({
      selectInput(input_name, input_title, choices = var_choice, selected = selected)
    })
  })
  
  # Create an interactive map centered on catalonia
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12)) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      setView(lng = 1.74,lat = 41.69, zoom = 8)
  })
  
  # Add shapes
  observe({
    if(input$display == "Counties"){
      leafletProxy("map") %>%
        clearShapes() %>%
        clearMarkerClusters() %>%
        addPolygons(data = cat.pol, color = "black", weight = 1, fillOpacity = 0, label = ~NOM_COMAR,
                    highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))
      
    } else {
      if(input$display == "Municipalities"){
        leafletProxy("map") %>%
          clearShapes() %>%
          clearMarkerClusters() %>%
          addPolygons(data = mun.pol, color = "black", weight = 1, fillOpacity = 0, label = ~NOM_MUNI,
                      highlightOptions = highlightOptions(color = "white", weight = 3, opacity = 1, bringToFront = T))
        
      } else {
        if(input$display == "IFN plots"){
          leafletProxy("map") %>%
            clearShapes() %>%
            addCircleMarkers(data = IFN3.points, radius = 5, stroke = F, fillOpacity = 0.5, label = ~as.character(ID), layerId = ~as.character(ID),  
                             clusterOptions = markerClusterOptions(showCoverageOnHover = T, disableClusteringAtZoom = 12))
          
        } else {}
      }
    }
  })
  
  
  # create a reactive value sensitive to clicks on shapes or markers
  map_click <- reactiveValues(x = list())
  observe({
    map_click$x <- input$map_shape_click
  })
  observe({
    map_click$x <- input$map_marker_click
  })
  
  # React to clicks on the map (using observeEvent() instead of observe() allows to trigger code only when the value of input$map_shape_click changes)
  observeEvent(map_click$x,{
    if(!is.null(map_click$x)){
      
      # Convert coordinates of the click zone into a spatial point
      clicker <- SpatialPoints(coords = data.frame(x = map_click$x$lng, y = map_click$x$lat), proj4string = mapCRS) 
      
      if(input$display == "Counties"){
        info <- clicker %over% cat.pol
        colnames(info) <- c("Id", "Name", "Capital", "Superficy")
        info$type <- "county"
        IFN3_sel <- IFN3.points@data[IFN3.points$COMARCA == info$Id,]
      } else {
        if(input$display == "Municipalities"){
          info <- clicker %over% mun.pol
          colnames(info) <- c("Id", "County", "Province", "Name", "Name2", "Name3", "Capital", "Capital2", "Capital3", "Superficy", "X")
          info$type <- "municipality"
          IFN3_sel <- IFN3.points@data[IFN3.points$MUNICIPI == info$Id,]
        } else {
          if(input$display == "IFN plots"){
            IFN3_sel <- IFN3.points@data[as.character(IFN3.points$ID) == map_click$x$id,]
            info <- data.frame(Name = IFN3_sel$ID, type = "IFN plot")
          } else {}
        } 
      }
      output$info <- renderPrint({
        cat("Shape type: ", info$type, "; name: ", as.character(info$Name), sep = "")
        cat("\nContains", nrow(IFN3_sel), "plots from the 3rd Spanish Forest Inventory")
      })
      
      # Open relevant files and extract informations regarding the selected variable 
      if(nrow(IFN3_sel)>0){
        folder <- paste0("//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/ProjectedSWB/", input$rcm,"/",input$rcp)
        plots_id <- IFN3_sel$ID
        plots_id <- plots_id[as.character(plots_id) %in% available_plots]
        load(paste(folder, "/", plots_id[1], ".rda", sep = ""))
        
        if(input$mode == "Water balance"){
          if(input$agg== "Month") trends = swb_month
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
          
          col <- as.character(variables[variables$input == input$var, "medfate"])
          dates <- as.Date(rownames(means))
          
          output$trends <- renderPlot({
            plot(dates, ci_sup[,col], type = "l", xlab = "Date", ylab = paste(input$var), ylim = c(min(ci_inf[,col], na.rm = T), max(ci_sup[,col], na.rm = T)), col = "red", lty = 3)
            lines(dates, ci_inf[,col], col = "red", lty = 3)
            lines(dates, means[,col])
          })
          
        } else {
          if(input$mode == "Drought stress"){ 
            if(input$agg== "Month") trends = dds_month
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
  
  # What are the different inputs?
  output$inputList <- renderPrint({
    str(reactiveValuesToList(input))
    str(map_click$x)
  })
})