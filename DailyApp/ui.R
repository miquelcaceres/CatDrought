library(shiny)
# Libraries for ui
library(leaflet)

shinyUI(
  fluidPage(
    
    # Application title
    titlePanel("Daily SWB simulations"),
    splitLayout(cellWidths = c("20%", "80%"),
                wellPanel(
                  radioButtons("mode", "Variable type", choices = c("Water balance", "Drought stress"), inline=TRUE),
                  uiOutput("var_choice"),
                  hr(),
                  dateInput("date", "Raster date",value = Sys.Date()-1, min =as.Date("2017-01-01"), max = Sys.Date()-1, weekstart=1),
                  radioButtons("agg", "Raster temporal aggregation", choices = c("none", "1 week", "2 weeks"), inline = TRUE),
                  radioButtons("resolution", "Raster resolution", choices = c("Smoothed","1km", "200m"), selected = "Smoothed", inline=TRUE),
                  sliderInput("alpha", "Raster opacity", min = 0, max = 1, value = 10),
                  hr(),
                  radioButtons("display", "Summary level", choices = c("Counties", "Municipalities", "IFN plots"), selected = "Counties", inline = TRUE)
                  # hr(),
                  # p(strong("List of available inputs")),
                  # verbatimTextOutput("inputList")
                ),
                column(width = 12,
                       wellPanel(
                         leafletOutput("map", width = "100%", height = "800px")
                       ),
                       h4("Summary trends"),
                       wellPanel(
                         plotOutput("trends"),
                         verbatimTextOutput("pol_info")
                       )
                )
    )
  )
)
