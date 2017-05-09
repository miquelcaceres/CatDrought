library(shiny)
# Libraries for ui
library(leaflet)

shinyUI(
  navbarPage("Catalan Forest Drought Assessment Tool",
    tabPanel("Daily SWB",
             splitLayout(cellWidths = c("20%", "80%"),
                         wellPanel(
                           radioButtons("mode_daily", "Variable type", choices = c("Water balance", "Drought stress"), inline=TRUE),
                           uiOutput("var_choice_daily"),
                           hr(),
                           dateInput("date_daily", "Raster date",value = Sys.Date()-1, min =as.Date("2017-01-01"), max = Sys.Date()-1, weekstart=1),
                           radioButtons("agg_daily", "Raster temporal aggregation", choices = c("none", "1 week", "2 weeks"), inline = TRUE),
                           radioButtons("resolution_daily", "Raster resolution", choices = c("Smoothed","1km", "200m"), selected = "Smoothed", inline=TRUE),
                           sliderInput("alpha_daily", "Raster opacity", min = 0, max = 1, value = 10),
                           hr(),
                           radioButtons("display_daily", "Summary level", choices = c("Counties", "Municipalities", "IFN plots"), selected = "Counties", inline = TRUE),
                           hr(),
                           p(strong("List of available inputs")),
                           verbatimTextOutput("inputList_daily")
                         ),
                         column(width = 12,
                                wellPanel(
                                  leafletOutput("map_daily", width = "100%", height = "800px")
                                ),
                                h4("Summary trends"),
                                wellPanel(
                                  verbatimTextOutput("pol_info_daily"),
                                  plotOutput("trends_daily")
                                )
                         )
             )
    ),
    tabPanel("SWB under climate change",
             splitLayout(cellWidths = c("20%", "80%"),
                         wellPanel(
                           radioButtons("mode_proj", "Variable type", choices = c("Water balance", "Drought stress"), inline=TRUE),
                           uiOutput("var_choice_proj"),
                           radioButtons("agg_proj", "Temporal scale", choices = c("Year", "Month"), inline=TRUE),
                           hr(),
                           radioButtons("rcm_proj", "RCM", choices = c("CCLM4-8-17", "RCA4"), inline=TRUE),
                           radioButtons("rcp_proj", "RCP scenario", choices = c("rcp4.5", "rcp8.5"), inline=TRUE),
                           radioButtons("display_proj", "Selection", choices = c("Counties", "Municipalities", "IFN plots"), selected = "Counties", inline=TRUE),
                           hr(),
                           p(strong("List of available inputs")),
                           verbatimTextOutput("inputList_proj")
                         ),
                         column(width = 12,
                                wellPanel(
                                  leafletOutput("map_proj", width = "100%", height = "500px")
                                ),
                                h4("Output summary"),
                                wellPanel(
                                  verbatimTextOutput("pol_info_proj"),
                                  plotOutput("trends_proj")
                                )
                         )
             )
      ), id="navbar",
    fluid=TRUE
  )
)
