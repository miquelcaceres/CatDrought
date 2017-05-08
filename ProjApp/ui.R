library(shiny)
# Libraries for ui
library(leaflet)

shinyUI(
  fluidPage(
    
    # Application title
    titlePanel("SWB Projections"),
    splitLayout(cellWidths = c("15%", "25%", "60%"),
                wellPanel(
                  radioButtons("mode", "Variable type", choices = c("Water balance", "Drought stress")),
                  uiOutput("var_choice"),
                  radioButtons("agg", "Temporal scale", choices = c("Month", "Year"), inline=TRUE),
                  hr(),
                  radioButtons("rcm", "RCM", choices = c("CCLM4-8-17", "RCA4"), inline=TRUE),
                  radioButtons("rcp", "RCP scenario", choices = c("rcp4.5", "rcp8.5"), inline=TRUE),
                  p(strong("List of available inputs")),
                  verbatimTextOutput("inputList")
                ),
                column(width = 12,
                       leafletOutput("map", width = "100%", height = "600px"),
                       radioButtons("display", "Display", choices = c("Counties", "Municipalities", "IFN plots"), selected = "Counties", inline=TRUE)
                       ),
                wellPanel(
                  h4("Output summary"),
                  verbatimTextOutput("pol_info"),
                  plotOutput("trends")
                )
    )
  )
)
