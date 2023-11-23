library(shiny)
library(shinythemes) 

source("ViolationsMap.r")
source("CategoryByHour.R")
source("HeatMapYear.R")


ui <- navbarPage("Dashboard for Parking Violations in NY", 
                 theme = shinytheme("flatly"), 
                 tabPanel("Violations Map", violationsMapUI("violationsMap")),
                 tabPanel("Violations Over Time",
                          verticalLayout(
                            categoryHourUI("categoryHour"),
                            heatmapUI("heatmap")
                          )
                 )
)
server <- function(input, output, session) {
  violationsMapServer("violationsMap")
  categoryHourServer("categoryHour")
  heatmapServer("heatmap")
}

shinyApp(ui, server)

