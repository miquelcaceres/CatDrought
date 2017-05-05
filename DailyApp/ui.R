library(shiny)
# Libraries for ui
library(leaflet)

shinyUI(
  fluidPage(
    
    # Application title
    titlePanel("Medfate"),
    splitLayout(cellWidths = c("15%", "50%", "35%"),
                wellPanel(
                  radioButtons("mode", "Variable type", choices = c("Water balance", "Drought stress")),
                  uiOutput("var_choice"),
                  hr(),
                  dateInput("date", "Select a date", value = Sys.Date()-1, min =as.Date("2017-01-01"), max = Sys.Date()-1),
                  radioButtons("agg", "Temporal scale (not yet implemented)", choices = c("Day", "Week", "Month", "Year")),
                  hr(),
                  p(strong("List of available inputs")),
                  verbatimTextOutput("inputList")
                ),
                column(width = 12,
                       leafletOutput("map", width = "100%", height = "600px"),
                       sliderInput("alpha", "Raster opacity", min = 0, max = 1, value = 10),
                       radioButtons("display", "Display", choices = c("Counties", "Municipalities", "IFN plots"), selected = "Counties"),
                       radioButtons("resolution", "Resolution", choices = c("Smoothed","1km", "200m"), selected = "Smoothed")
                       ),
                wellPanel(
                  h4("Output summary"),
                  verbatimTextOutput("pol_info"),
                  plotOutput("trends")
                )
    )
  )
)
