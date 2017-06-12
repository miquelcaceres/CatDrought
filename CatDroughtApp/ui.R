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

basemaps <- c("Esri.WorldGrayCanvas","Esri.WorldImagery","Esri.WorldShadedRelief","Stamen.TerrainBackground")

shinyUI(
  navbarPage("Catalan Forest Drought Observatory",
     theme = shinythemes::shinytheme("sandstone"),
    ### CURRENT FOREST DROUGHT  ####
     tabPanel("Current",
                fluidRow(
                  column(width=3,
                         selectInput("mode_daily", "Variable type", choices = c("Climate","Forest water balance", "Drought stress"), selected = "Forest water balance")
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
                         tags$head(
                             includeCSS("styles.css")
                         ),
                         h5(""),
                        # wellPanel(
                        sidebarLayout(
                         sidebarPanel(
                           uiOutput("date_daily"),
                           selectInput("agg_daily", "Aggr. (days)", choices=1:30, selected=1),
                           hr(),
                           selectInput("display_daily", "Selection type", choices = c("none", "Watersheds", "Counties", "Municipalities", "IFN plots"), selected = "none"),
                           hr(),
                           radioButtons("resolution_daily", "Raster resolution", choices = c("Smoothed","1km", "200m"), selected = "Smoothed"),
                           hr(),
                           downloadButton('downloadRasterDaily', 'Download raster'),
                         width=3),
                         mainPanel(
                               leafletOutput("map_daily", width = "100%", height = "600px")
                         ,width=9)
                       # )
                      )
                  ),
                  tabPanel("Selected series",
                        # wellPanel(
                            h5(""),
                            dygraphOutput("trends_daily"),
                            hr(),
                            downloadButton('downloadTrendDaily', 'Download trend')
                        # )
                  ),

                  id="DailyTabset"
                ),
                conditionalPanel(
                  condition = "input.DailyTabset=='Map'",
                  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                draggable = TRUE, top = 223, left = "auto", right = 30, bottom = "auto",
                                width = 250, height = 180,
                                h4(""),
                                selectInput("basemap_daily","Base map", choices = basemaps),
                                sliderInput("alpha_daily", "Raster opacity", min = 0, max = 1, value = 1, ticks = FALSE)
                  )

          )
          # wellPanel(
          #   # verbatimTextOutput("pol_info_daily"),
          #   p(strong("List of available inputs")),
          #   verbatimTextOutput("inputList_daily")
          # )
    ),
    #### HISTORIC FOREST DROUGHT  ####
    tabPanel("Historic (1986-2016)",
              fluidRow(
                 column(width=3,
                        selectInput("mode_hist", "Variable type", choices = c("Climate","Forest water balance", "Drought stress"), selected = "Forest water balance")
                 ),
                 column(width=3,
                        uiOutput("var_choice_hist")
                 ),
                 column(width=3,
                        conditionalPanel(
                          condition = "input.mode_hist=='Drought stress'",
                          selectInput("sp_hist", "Choose species", choices = input_sp, selected = "Overall")
                        )
                 ),
                 column(width = 3,
                        radioButtons("agg_hist", "Temporal resolution", choices = c("Year", "Month"), selected="Year", inline=TRUE)
                 )
              ),
              tabsetPanel(
                tabPanel("Map",
                         tags$head(
                           includeCSS("styles.css")
                         ),
                         h5(""),
                      # wellPanel(
                           sidebarLayout(
                             sidebarPanel(
                               radioButtons("climate_hist", label="Mode", choices=c("1991-2016 period", "Year")),
                               conditionalPanel(
                                 condition = "input.climate_hist=='Year'",
                                 selectInput("years_hist",label=NULL, choices=as.character(1986:2016), selected = 2016)
                               ),
                               conditionalPanel(
                                 condition = "input.agg_hist=='Month'",
                                 selectInput("month_hist", "Month", choices = as.character(1:12), selected="1")
                               ),
                               conditionalPanel(
                                 condition = "input.climate_hist=='1991-2016 period'",
                                 radioButtons("raster_trend_hist", "Raster type", choices = c("Average","Absolute change", "Relative change")),
                                 conditionalPanel(
                                   condition= "input.raster_trend_hist!='Average'",
                                   selectInput("alpha_cut_hist", "Sign. level", choices=c(1.0,0.5,0.1,0.05,0.01,0.001,0.0001), selected=1.0)
                                 )
                               ),
                               hr(),
                               radioButtons("resolution_hist", "Raster resolution", choices = c("Smoothed","1km"), selected = "Smoothed"),
                               hr(),
                               selectInput("display_hist", "Selection type", choices = c("none","Watersheds",  "Counties", "Municipalities", "IFN plots"), selected = "none"),
                               hr(),
                               downloadButton('downloadRasterHist', 'Download raster')
                               ,width=3),
                             mainPanel(
                                 leafletOutput("map_hist", width = "100%", height = "600px"),
                              width=9)
                         )
                      # )
                ),
                tabPanel("Selected series",
                      # wellPanel(
                        h5(""),
                        dygraphOutput("trends_hist") ,
                        hr(),
                        wellPanel(
                          fluidRow(
                            column(width=3,
                                   conditionalPanel(
                                     condition="input.agg_hist=='Month'",
                                     column(width=4,
                                            h4(" "),
                                            checkboxInput("allmonths_hist","All months", value=TRUE)
                                     ),
                                     column(width=8,
                                            h4(" "),
                                            conditionalPanel(
                                              condition="!input.allmonths_hist",
                                              selectInput("trend_month_hist", "Month", choices = as.character(1:12), selected="1")
                                            )

                                     )
                                   )
                            ),
                            column(1),
                            column(3,
                                   h4("Mann-Kendall test"),
                                   verbatimTextOutput("MK_hist")
                            ),
                            column(3,
                                   h4("Ten-sheil slope"),
                                   verbatimTextOutput("TS_slope_hist")
                            ),
                            column(2,
                                   h4(" "),
                                   downloadButton('downloadTrendHist', 'Download trend')
                            )

                          )

                        )


                      # )
                ),
                id = "HistTabset"
              ),
              conditionalPanel(
                condition = "input.HistTabset=='Map'",
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 223, left = "auto", right = 30, bottom = "auto",
                              width = 250, height = 180,
                              h4(""),
                              selectInput("basemap_hist","Base map", choices = basemaps),
                              sliderInput("alpha_hist", "Raster opacity", min = 0, max = 1, value = 1, ticks = FALSE)
              )
        )
        # wellPanel(
        #    p(strong("List of available inputs")),
        #    verbatimTextOutput("inputList_hist")
        # )
    ),
    #### CC FOREST DROUGHT  ####
    tabPanel("Climate change scenarios",
           fluidRow(
             column(width=2,
                    selectInput("mode_proj", "Variable type", choices = c("Climate","Forest water balance", "Drought stress"), selected = "Forest water balance")
             ),
             column(width=2,
                    uiOutput("var_choice_proj")
             ),
             column(width=2,
                    conditionalPanel(
                      condition = "input.mode_proj=='Drought stress'",
                      selectInput("sp_proj", "Choose species", choices = input_sp, selected = "Overall")
                    )
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
              tags$head(
                  includeCSS("styles.css")
              ),
              h5(""),
               # wellPanel(
                 sidebarLayout(
                   sidebarPanel(
                     radioButtons("raster_trend_proj", "Raster type", choices=c("Slope","Absolute change", "Relative change")),
                     selectInput("alpha_cut_proj", "Sign. level", choices=c(1.0,0.5,0.1,0.05,0.01,0.001,0.0001), selected=1.0),
                     hr(),
                     radioButtons("resolution_proj", "Raster resolution", choices = c("Smoothed","1km"), selected = "Smoothed"),
                     hr(),
                     selectInput("display_proj", "Selection type", choices = c("none", "Watersheds", "Counties", "Municipalities", "IFN plots"), selected = "none"),
                     hr(),
                     downloadButton('downloadRasterProj', 'Download raster')
                     ,
                     width=3),
                   mainPanel(
                     leafletOutput("map_proj", width = "100%", height = "600px")
                     ,
                     width=9)
                 )
               # )
             ),
             tabPanel("Selected series",
                      h5(""),
                      dygraphOutput("trends_proj") ,
                      hr(),
                      wellPanel(
                        fluidRow(
                          column(width=3,
                                 conditionalPanel(
                                   condition="input.agg_proj=='Month'",
                                   column(width=4,
                                          h4(" "),
                                          checkboxInput("allmonths_proj","All months", value=TRUE)
                                   ),
                                   column(width=8,
                                          h4(" "),
                                          conditionalPanel(
                                            condition="!input.allmonths_proj",
                                            selectInput("trend_month_proj", "Month", choices = as.character(1:12), selected="1")
                                          )

                                   )
                                 )
                          ),
                          column(1),
                          column(3,
                                 h4("Mann-Kendall test"),
                                 verbatimTextOutput("MK_proj")
                          ),
                          column(3,
                                 h4("Ten-sheil slope"),
                                 verbatimTextOutput("TS_slope_proj")
                          ),
                          column(2,
                                 h4(" "),
                                 downloadButton('downloadTrendProj', 'Download trend')
                          )

                        )

                      )

             ),
             id = "ProjTabset"
             ),
             conditionalPanel(
               condition = "input.ProjTabset=='Map'",
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 223, left = "auto", right = 30, bottom = "auto",
                           width = 250, height = 180,
                           h4(""),
                           selectInput("basemap_proj","Base map", choices = basemaps),
                           sliderInput("alpha_proj", "Raster opacity", min = 0, max = 1, value = 1, ticks = FALSE)
               )

          )
          # wellPanel(
          #    p(strong("List of available inputs")),
          #    verbatimTextOutput("inputList_proj")
          # )

    ),
    tabPanel("Static inputs",
             fluidRow(
               column(width=3,
                      selectInput("mode_stat", "Variable type", choices = c("Soil","IFN2", "IFN3"), selected = "Soil")
               ),
               column(width=3,
                      uiOutput("var_choice_stat")
               ),
               column(width=3,
                      conditionalPanel(
                        condition = "input.mode_stat!='Soil'",
                        selectInput("sp_stat", "Choose species", choices = input_sp, selected = "Overall")
                      )
               )
             ),
             tabsetPanel(
               tabPanel("Map",
                        tags$head(
                          includeCSS("styles.css")
                        ),
                        h5(""),
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons("resolution_stat", "Raster resolution", choices = c("Smoothed","1km","200m"), selected = "Smoothed"),
                            hr(),
                            selectInput("display_stat", "Selection type", choices = c("none","Watersheds",  "Counties", "Municipalities", "IFN plots"), selected = "none"),
                            width=3),
                          mainPanel(
                            leafletOutput("map_stat", width = "100%", height = "600px"),
                            width=9)
                        )
               ),
               tabPanel("Selected area"
               ),
               id = "StatTabset"
             ),
             conditionalPanel(
               condition = "input.StatTabset=='Map'",
               absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 223, left = "auto", right = 30, bottom = "auto",
                             width = 250, height = 180,
                             h4(""),
                             selectInput("basemap_stat","Base map", choices = basemaps),
                             sliderInput("alpha_stat", "Raster opacity", min = 0, max = 1, value = 1, ticks = FALSE)
               )
             )

    ),
    navbarMenu("Documentation",
               tabPanel("User's guide",
                          includeMarkdown("Docs/UserGuide.Rmd")
               ),
               tabPanel("Technical specifications",
                        # wellPanel(
                          includeMarkdown("Docs/TechnicalSpecifications.Rmd")
                          # style = "overflow-y:scroll; max-height: 700px"
                        # )
               ),
               tabPanel("Acknowledgements",
                          includeMarkdown("Docs/Credits.Rmd"),
                          hr(),
                          hr(),
                          fluidRow(
                            column(3),
                            column(2,
                              a(href = "http://www.ctfc.cat/", img(src="logo_ctfc.png"))
                            ),
                            column(2,
                              a(href = "http://vegmod.ctfc.cat/", img(src="LOGO_Group.png"))
                            ),
                            column(2,
                                   a(href = "http://www.creaf.cat/", img(src="logo_creaf.png"))
                            ),
                            column(3)
                          )
               )
    ),
    id="navbar",
    fluid=TRUE
  )
)
