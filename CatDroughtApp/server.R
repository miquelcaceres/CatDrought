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



########################################################
### Define server logic required to draw a histogram ###
########################################################

shinyServer(function(input, output, session) {
  # Switch the mode of the daily output 
  observe({
    input_title <- "Choose variable"
    if(input$mode_daily == "Climate") {
      input_name <- "clim_daily"
      var_choice_daily <- input_clim_var[1:2]
      selected <- "Precipitation (mm)"
    } else if(input$mode_daily == "Soil water balance") {
      input_name <- "WB_daily"
      var_choice_daily <- input_WB_var
      selected <- "Relative soil water content [0-1]"
    } else {
      input_name <- "DS_daily"
      var_choice_daily <- input_DS_var
      selected <- "Daily stress"
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
    } else if(input$mode_hist == "Soil water balance") {
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
    } else if(input$mode_proj == "Soil water balance") {
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
      addProviderTiles("Esri.WorldGrayCanvas", layerId="basemap") %>%
      setView(lng = 1.74,lat = 41.69, zoom = 8)
    
  })
  observe({
    leafletProxy("map_daily") %>%
      removeImage(layerId="basemap") %>%
      addProviderTiles(input$basemap_daily,layerId="basemap") %>% 
      showGroup("rasterGroup")
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
          addRasterImage(r, opacity = input$alpha_daily, colors = pal, layerId="raster", group="rasterGroup") %>% 
          addLegend(pal = pal, values = values(r),opacity = input$alpha_daily, position = "bottomright", layerId="raster")
      } 
    } else if(input$mode_daily == "Soil water balance"){
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
          addRasterImage(r, opacity = input$alpha_daily, colors = pal, layerId="raster", group="rasterGroup") %>% 
          addLegend(pal = pal, values = values(r),opacity = input$alpha_daily, position = "bottomright", layerId="raster")
      } 
    } else {
      if(!is.null(input$DS_daily) && !is.null(input$sp_daily)){
        folder <- "//SERVERPROCESS/Miquel/CatDrought/Rdata/Maps"
        ds_var <- as.character(DS_variables[DS_variables$input == input$DS_daily, "medfate"])
        col <- as.character(species[species$input == input$sp_daily, "medfate"])
        # print(ds_var)
        dfin = input$date_daily
        dini  = max(as.Date("2017-01-01"),dfin-(as.numeric(input$agg_daily)-1))
        dw = seq(dini, dfin, by="day")
        nd = length(dw)
        load(paste(folder, "/", input$resolution_daily, "/DroughtStress/",ds_var,"/",col, "/", dw[1], ".rda", sep = ""))
        spdftmp = spdf
        if(nd>1) {
          for(d in 2:nd) {
            load(paste(folder, "/", input$resolution_daily, "/DroughtStress/",ds_var,"/",col, "/", dw[d], ".rda", sep = ""))
            spdftmp@data = spdftmp@data + spdf@data
          }
        }
        spdftmp@data =spdftmp@data /nd
        spdf = spdftmp
        
        
        r <- raster(spdf)
        proj4string(r) <- dataCRS
        r <- projectRaster(r, crs = mapCRS)
        
        dom <- c(pal_DS[input$DS_daily,"min"],pal_DS[input$DS_daily,"max"])
        bins <- do.call(paste(pal_DS[input$DS_daily, "trans"], "trans", sep = "_"), args = list(dom = dom, n = 15, digits = 2))
        if(ds_var=="NDD") bins<-ceiling(bins)
        
        pal <- colorBin(pal_DS[input$DS_daily,"color"], domain = dom, na.color = "transparent", bins = bins, reverse = pal_DS[input$DS_daily, "rev"])

        leafletProxy("map_daily") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(r, opacity = input$alpha_daily, colors = pal, layerId="raster", group="rasterGroup") %>%
          addLegend(pal = pal, values = values(r),opacity = input$alpha_daily, position = "bottomright", layerId="raster")
      }
    }
  })
  
  # Create an interactive map centered on catalonia
  output$map_hist <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 12)) %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
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
      addProviderTiles("Esri.WorldGrayCanvas") %>%
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
  

  # Create a reactive value data
  map_daily_data <- reactiveValues(x = list())
  map_hist_data <- reactiveValues(x = list())
  map_proj_data <- reactiveValues(x = list())
  

  #Reacts to changes in variable selected and map_daily_data changes
  output$trends_daily<-renderDygraph({
    if(!is.null(map_daily_data$x)) {
      if(input$mode_daily == "Climate") {
        col <- as.character(clim_variables[clim_variables$input == input$clim_daily, "medfate"])
        title <- paste(input$clim_daily," at ",as.character(map_daily_data$x$info$Name))
        label=input$clim_daily
      } else if(input$mode_daily == "Soil water balance") {
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
      if(input$mode_daily=="Drought stress") {
        dygraph(x, main= title) %>% 
          dySeries(c("lower", "mean","upper"), label=label) %>% 
          dyRangeSelector() %>%
          dyAxis("y", label = "Daily drought stress", valueRange = c(0, 1)) %>%
          dyLimit(0.5, color="red")
      } else {
        dygraph(x, main= title) %>% 
          dySeries(c("lower", "mean","upper"), label=label) %>% 
          dyRangeSelector()
      }
    }
  })
  #Reacts to changes in variable selected and map_hist_data changes
  output$trends_hist<-renderDygraph({
    if(!is.null(map_hist_data$x)) {
      if(input$mode_hist == "Climate") {
        col <- as.character(clim_variables[clim_variables$input == input$clim_hist, "medfate"])
        title <- paste(input$clim_hist," at ",as.character(map_hist_data$x$info$Name))
        label=input$clim_hist
      } else if(input$mode_hist == "Soil water balance") {
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
      if(input$mode_hist=="Drought stress") {
        dygraph(x, main= title) %>% 
          dySeries(c("lower", "mean","upper"), label=label) %>% 
          dyRangeSelector() %>%
          dyAxis("y", label = "Average daily drought stress", valueRange = c(0, 1)) %>%
          dyLimit(0.5, color="red")
      } else {
        dygraph(x, main= title) %>% 
          dySeries(c("lower", "mean","upper"), label=label) %>% 
          dyRangeSelector()
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
      } else if(input$mode_proj == "Soil water balance") {
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
      if(input$mode_proj=="Drought stress") {
        dygraph(x, main= title) %>% 
          dySeries(c("lower", "mean","upper"), label=label) %>% 
          dyRangeSelector() %>%
          dyAxis("y", label = "Average daily drought stress", valueRange = c(0, 1)) %>%
          dyLimit(0.5, color="red")
      } else {
        dygraph(x, main= title) %>% 
          dySeries(c("lower", "mean","upper"), label=label) %>% 
          dyRangeSelector()
      }
    }
  })
  
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
        if(input$mode_daily %in% c("Climate", "Soil water balance")){
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
        folder <- paste0("//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/HistoricSWB/")
        plots_id <- IFN3_sel$ID
        plots_id <- plots_id[as.character(plots_id) %in% available_plots_historic]
        if(length(plots_id)>0) {
          load(paste(folder, "/", plots_id[1], ".rda", sep = ""))
          if(input$mode_hist %in% c("Climate","Soil water balance")){
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
        folder <- paste0("//SERVERPROCESS/Miquel/CatDrought/Rdata/Plots/ProjectedSWB/", climate_models[input$rcm_proj],"/",input$rcp_proj)
        plots_id <- IFN3_sel$ID
        plots_id <- plots_id[as.character(plots_id) %in% available_plots_projections]
        if(length(plots_id)>0) {
          load(paste(folder, "/", plots_id[1], ".rda", sep = ""))
          if(input$mode_proj %in% c("Climate","Soil water balance")){
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