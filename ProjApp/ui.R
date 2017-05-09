library(shiny)
# Libraries for ui
library(leaflet)

shinyUI(
  fluidPage(
    
    # Application title
    titlePanel("SWB Projections"),
    splitLayout(cellWidths = c("20%", "80%"),
                wellPanel(
                  radioButtons("mode", "Variable type", choices = c("Water balance", "Drought stress")),
                  uiOutput("var_choice"),
                  radioButtons("agg", "Temporal scale", choices = c("Year", "Month"), inline=TRUE),
                  hr(),
                  radioButtons("rcm", "RCM", choices = c("CCLM4-8-17", "RCA4"), inline=TRUE),
                  radioButtons("rcp", "RCP scenario", choices = c("rcp4.5", "rcp8.5"), inline=TRUE),
                  radioButtons("display", "Selection", choices = c("Counties", "Municipalities", "IFN plots"), selected = "Counties", inline=TRUE),
                  hr(),
                  p(strong("List of available inputs")),
                  verbatimTextOutput("inputList")
                ),
                column(width = 12,
                       wellPanel(
                         leafletOutput("map", width = "100%", height = "500px")
                       ),
                       h4("Output summary"),
                       wellPanel(
                         plotOutput("trends"),
                         verbatimTextOutput("pol_info")
                       )
                )
    )
  )
)
