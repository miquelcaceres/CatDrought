library(shiny)
# Libraries for ui
library(leaflet)
library(dygraphs)

shinyUI(
  navbarPage("Catalan Forest Drought Prediction Tool",
     tabPanel("Current forest drought",
          wellPanel(
                sidebarLayout(sidebarPanel(
                           radioButtons("mode_daily", "Variable type", choices = c("Water balance", "Drought stress"), inline=TRUE),
                           uiOutput("var_choice_daily"),
                           hr(),
                           dateInput("date_daily", "Choose date",value = Sys.Date()-1, min =as.Date("2017-01-01"), max = Sys.Date()-1, weekstart=1),
                           selectInput("agg_daily", "Temporal aggregation", choices = c("none", "1 week", "2 weeks", "3 weeks", "4 weeks")),
                           radioButtons("resolution_daily", "Spatial resolution", choices = c("Smoothed","1km", "200m"), selected = "Smoothed", inline=TRUE),
                           sliderInput("alpha_daily", "Raster opacity", min = 0, max = 1, value = 1, ticks = FALSE),
                           hr(),
                           hr(),
                           radioButtons("display_daily", "Selection area", choices = c("Counties", "Municipalities", "IFN plots"), selected = "Counties", inline = TRUE)
                           # hr(),
                           # p(strong("List of available inputs")),
                           # verbatimTextOutput("inputList_daily")
                         ),
                         mainPanel(
                           leafletOutput("map_daily", width = "100%", height = "600px")
                         )
                )
             
          ),
          wellPanel(
            h4("Selected area/plot series:"),
            # verbatimTextOutput("pol_info_daily"),
            dygraphOutput("trends_daily")                  
          )
    ),
    tabPanel("Forest drought under climate change",
             wellPanel(
               sidebarLayout(
                    sidebarPanel(
                           radioButtons("mode_proj", "Variable type", choices = c("Water balance", "Drought stress"), inline=TRUE),
                           uiOutput("var_choice_proj"),
                           radioButtons("agg_proj", "Temporal scale", choices = c("Year", "Month"), inline=TRUE),
                           hr(),
                           selectInput("rcm_proj", "Climate model (GCM/RCM)", choices = c("CNRM/CCLM4-8-17", "CNRM/RCA4")),
                           selectInput("rcp_proj", "Climate scenario (RCP)", choices = c("rcp4.5", "rcp8.5")),
                           hr(),
                           radioButtons("display_proj", "Selection area", choices = c("Counties", "Municipalities", "IFN plots"), selected = "Counties", inline=TRUE)
                           # hr(),
                           # p(strong("List of available inputs")),
                           # verbatimTextOutput("inputList_proj")
                    ),
                    mainPanel(
                           leafletOutput("map_proj", width = "100%", height = "500px")
                    )
               )
             ),
             wellPanel(
               h4("Selected area/plot series:"),
               # verbatimTextOutput("pol_info_proj"),
               dygraphOutput("trends_proj")
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
