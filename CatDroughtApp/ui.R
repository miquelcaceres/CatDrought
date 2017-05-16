library(shiny)
library(shinythemes)
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
     theme = shinythemes::shinytheme("sandstone"),
     #### CURRENT FOREST DROUGHT  ####
     tabPanel("Current",
          wellPanel(
                fluidRow(
                  column(width=3,
                         selectInput("mode_daily", "Variable type", choices = c("Climate","Soil water balance", "Drought stress"))
                  ),
                  column(width=3,
                         uiOutput("var_choice_daily")
                  ),
                  column(width=3,
                         conditionalPanel(
                           condition = "input.mode_daily=='Drought stress'",
                           selectInput("sp_daily", "Choose species", choices = input_sp, selected = "Overall")
                         )
                  ),
                  column(width=3)
                ),
                tabsetPanel(
                  tabPanel("Map",
                      wellPanel(
                        sidebarLayout(
                         sidebarPanel(
                           fluidRow(
                             column(width=8,
                                dateInput("date_daily", "Date",value = Sys.Date()-1, min =as.Date("2017-01-01"), max = Sys.Date()-1, weekstart=1)
                             ),
                             column(width=4,
                                    selectInput("agg_daily", "Aggr.", choices=1:30, selected=1)
                             )
                           ),
                           hr(),
                           selectInput("display_daily", "Selection type", choices = c("none", "Watersheds", "Counties", "Municipalities", "IFN plots"), selected = "none"),
                           hr(),
                           selectInput("basemap_daily","Base map", choices = c("Esri.WorldGrayCanvas","Stamen.TerrainBackground")),
                           radioButtons("resolution_daily", "Raster resolution", choices = c("Smoothed","1km", "200m"), selected = "Smoothed", inline=TRUE),
                           sliderInput("alpha_daily", "Raster opacity", min = 0, max = 1, value = 1, ticks = FALSE),
                           hr(),
                           downloadButton('downloadRasterDaily', 'Download raster'),
                         width=3),
                         mainPanel(
                               leafletOutput("map_daily", width = "100%", height = "600px")
                         ,width=9)
                       )
                      )
                  ),
                  tabPanel("Selected series",
                        wellPanel(
                            dygraphOutput("trends_daily"), 
                            hr(),
                            downloadButton('downloadTrendDaily', 'Download trend')
                        )
                  ),
                   id="DailyTabset"
                )
          )
          # wellPanel(
          #   # verbatimTextOutput("pol_info_daily"),
          #   p(strong("List of available inputs")),
          #   verbatimTextOutput("inputList_daily")
          # )
    ),
    #### HISTORIC FOREST DROUGHT  ####
    tabPanel("Historic (1990-2015)",
        wellPanel(
              fluidRow(
                 column(width=3,
                        selectInput("mode_hist", "Variable type", choices = c("Climate","Soil water balance", "Drought stress"))
                 ),
                 column(width=3,
                        uiOutput("var_choice_hist")
                 ),
                 column(width = 3,
                        radioButtons("agg_hist", "Temporal resolution", choices = c("Year", "Month"), selected="Year", inline=TRUE)
                 ),
                 column(width=3)
              ),
              tabsetPanel(
                tabPanel("Map",
                      wellPanel(
                           sidebarLayout(
                             sidebarPanel(
                                 fluidRow(
                                   column(width=6,
                                     selectInput("years_hist","Year", choices=as.character(1990:2015), selected="2015")
                                   ),
                                   column(width=6,
                                          conditionalPanel(
                                            condition = "input.agg_hist=='Month'",
                                            selectInput("month_hist", "Month", choices = as.character(1:12), selected="12")
                                          )
                                   )
                                 ),
                                 hr(),
                                 selectInput("display_hist", "Selection type", choices = c("none","Watersheds",  "Counties", "Municipalities", "IFN plots"), selected = "none"),
                                 hr(),
                                 selectInput("basemap_hist","Base map", choices = c("Esri.WorldGrayCanvas","Stamen.TerrainBackground")),
                                 sliderInput("alpha_hist", "Raster opacity", min = 0, max = 1, value = 1, ticks = FALSE),
                                 hr(),
                                 downloadButton('downloadRasterHist', 'Download raster')
                               ,width=3),
                             mainPanel(
                                 leafletOutput("map_hist", width = "100%", height = "600px"),
                              width=9)
                         )
                      )
                ),
                tabPanel("Selected series",
                      wellPanel(
                        dygraphOutput("trends_hist") ,
                        hr(),
                        downloadButton('downloadTrendHist', 'Download trend')
                      )
                ),
                id = "HistTabset")
        )
        # wellPanel(
        #    p(strong("List of available inputs")),
        #    verbatimTextOutput("inputList_hist")
        # )
    ), 
    #### CC FOREST DROUGHT  ####
    tabPanel("Climate change scenarios",
        wellPanel(
           fluidRow(
             column(width=3,
                    selectInput("mode_proj", "Variable type", choices = c("Climate","Soil water balance", "Drought stress"))
             ),
             column(width=3,
                    uiOutput("var_choice_proj")
             ),
             column(width=2,
                    selectInput("rcm_proj", "Climate model", choices = c("CNRM/CCLM4-8-17", "CNRM/RCA4"))
             ),
             column(width=2,
                    selectInput("rcp_proj", "Climate scenario", choices = c("rcp4.5", "rcp8.5"))
             ),
             column(width=2,
                    radioButtons("agg_proj", "Temporal resolution", choices = c("Year", "Month"), inline=TRUE)
             )
           ),
           tabsetPanel(
             tabPanel("Map",
               wellPanel(
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("display_proj", "Selection type", choices = c("none", "Watersheds", "Counties", "Municipalities", "IFN plots"), selected = "none"),
                     hr(),
                     selectInput("basemap_proj","Base map", choices = c("Esri.WorldGrayCanvas","Stamen.TerrainBackground")),
                     hr(),
                     downloadButton('downloadRasterProj', 'Download raster')
                     ,
                     width=3),
                   mainPanel(
                     leafletOutput("map_proj", width = "100%", height = "600px")
                     ,
                     width=9)
                 )
               )
             ),
             tabPanel("Selected series",
                  wellPanel(
                          dygraphOutput("trends_proj"),
                          hr(),
                          downloadButton('downloadTrendProj', 'Download trend')
                  )
             ),
             id = "ProjTabset")
          )
          # wellPanel(
          #    p(strong("List of available inputs")),
          #    verbatimTextOutput("inputList_proj")
          # )
  
    ),
    navbarMenu("Documentation",
               tabPanel("Technical specifications",
                        wellPanel(
                          includeMarkdown("Docs/TechnicalSpecifications.Rmd")
                        )
               ),
               tabPanel("Acknowledgements",
                        wellPanel(
                          includeMarkdown("Docs/Credits.Rmd"),
                          hr(),
                          hr(),
                          fluidRow(
                            column(3),
                            column(2,
                              a(href = "http://www.ctfc.cat/", img(src="logo_ctfc.png"))
                            ),
                            column(2),
                            column(2,
                                   a(href = "http://www.creaf.cat/", img(src="logo_creaf.png"))
                            ),
                            column(3)
                          )
                        )
               )
    ),
    id="navbar",
    fluid=TRUE
  )
)
