library(shiny)
# Libraries for ui
library(leaflet)
library(dygraphs)

input_sp <- c("All woody species", "Pinus halepensis", "Pinus nigra", "Pinus sylvestris", "Pinus uncinata", "Pinus pinea", 
              "Pinus pinaster", "Quercus ilex", "Quercus suber", "Quercus humilis", "Quercus faginea", "Fagus sylvatica")
medfate_sp <- c("Overall", "PinusHalepensis", "PinusNigra", "PinusSylvestris", "PinusUncinata", "PinusPinea", "PinusPinaster", 
                "QuercusIlex", "QuercusSuber", "QuercusHumilis", "QuercusFaginea", "FagusSylvatica")
species <- data.frame(input = input_sp, medfate = medfate_sp)

shinyUI(
  navbarPage("Catalan Forest Drought Prediction Tool",
     tabPanel("Current",
          wellPanel(
                sidebarLayout(sidebarPanel(
                           radioButtons("mode_daily", "Variable type", choices = c("Climate","Soil water balance", "Drought stress")),
                           uiOutput("var_choice_daily"),
                           conditionalPanel(
                             condition = "input.mode_daily=='Drought stress'",
                             selectInput("sp_daily", "Choose species", choices = input_sp, selected = "Overall")
                           ),
                           hr(),
                           dateInput("date_daily", "Date",value = Sys.Date()-1, min =as.Date("2017-01-01"), max = Sys.Date()-1, weekstart=1),
                           selectInput("agg_daily", "Temporal aggregation (days)", choices=1:30, selected=1),
                           width=3),
                         mainPanel(
                             leafletOutput("map_daily", width = "100%", height = "600px"),
                             fluidRow(
                               column(width =3,
                                selectInput("display_daily", "Selection type", choices = c("none", "Counties", "Municipalities", "IFN plots"), selected = "none")
                               ),
                               column(width=3,
                                 selectInput("basemap_daily","Base map", choices = c("Esri.WorldGrayCanvas","Stamen.TerrainBackground"))
                               ),
                               column(width=3,
                                 radioButtons("resolution_daily", "Spatial resolution", choices = c("Smoothed","1km", "200m"), selected = "Smoothed", inline=TRUE)
                                ),
                               column(width=3,
                                 sliderInput("alpha_daily", "Raster opacity", min = 0, max = 1, value = 1, ticks = FALSE)
                               )
                             ),
                         width=9)
                        
                    )
          ),
          wellPanel(
            h4("Selected area/plot series:"),
            # verbatimTextOutput("pol_info_daily"),
            dygraphOutput("trends_daily"),                  
            hr(),
            p(strong("List of available inputs")),
            verbatimTextOutput("inputList_daily")
          )
    ),
    tabPanel("Historic (1990-2015)",
             wellPanel(
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("mode_hist", "Variable type", choices = c("Climate","Soil water balance", "Drought stress")),
                   uiOutput("var_choice_hist"),
                   hr(),
                   radioButtons("agg_hist", "Temporal scale", choices = c("Year", "Month"), selected="Month", inline=TRUE)
                 ,width=3),
                 mainPanel(
                   leafletOutput("map_hist", width = "100%", height = "600px"),
                   fluidRow(
                     column(3,
                            selectInput("display_hist", "Selection type", choices = c("none", "Counties", "Municipalities", "IFN plots"), selected = "none")
                     ),
                     column(3,
                            selectInput("basemap_hist","Base map", choices = c("Esri.WorldGrayCanvas","Stamen.TerrainBackground"))
                     )
                   ),
                 width=9)
               )
             ),
             wellPanel(
               h4("Selected area/plot series:"),
               # verbatimTextOutput("pol_info_hist"),
               dygraphOutput("trends_hist"),
               hr(),
               p(strong("List of available inputs")),
               verbatimTextOutput("inputList_hist")
             )
    ), 
    tabPanel("Climate change scenarios",
             wellPanel(
               sidebarLayout(
                    sidebarPanel(
                           radioButtons("mode_proj", "Variable type", choices = c("Climate","Soil water balance", "Drought stress")),
                           uiOutput("var_choice_proj"),
                           hr(),
                           selectInput("rcm_proj", "Climate model (GCM/RCM)", choices = c("CNRM/CCLM4-8-17", "CNRM/RCA4")),
                           selectInput("rcp_proj", "Climate scenario (RCP)", choices = c("rcp4.5", "rcp8.5")),
                           hr(),
                           radioButtons("agg_proj", "Temporal scale", choices = c("Year", "Month"), inline=TRUE),
                           width=3),
                    mainPanel(
                           leafletOutput("map_proj", width = "100%", height = "600px"),
                           fluidRow(
                             column(3,
                                    selectInput("display_proj", "Selection type", choices = c("none", "Counties", "Municipalities", "IFN plots"), selected = "none")
                             ),
                             column(3,
                                    selectInput("basemap_proj","Base map", choices = c("Esri.WorldGrayCanvas","Stamen.TerrainBackground"))
                             )
                           ),
                       width=9)
                  )
             ),
             wellPanel(
               h4("Selected area/plot series:"),
               # verbatimTextOutput("pol_info_proj"),
               dygraphOutput("trends_proj"),
               hr(),
               p(strong("List of available inputs")),
               verbatimTextOutput("inputList_proj")
             )
  
    ), 
    tabPanel("Technical specifications",
             wellPanel(
               includeMarkdown("../Docs/TechnicalSpecifications.Rmd")
             )
    ),
    tabPanel("Acknowledgements",
             wellPanel(
               includeMarkdown("../Docs/Credits.Rmd"),
               hr(),
               fluidRow(
                 a(href = "http://www.ctfc.cat/", img(src="logo_ctfc.png")), 
                 a(href = "http://www.creaf.cat/", img(src="logo_creaf.png"))
               )
             )
    ),
    id="navbar",
    fluid=TRUE
  )
)
