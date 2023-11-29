library(shiny)
library(shinythemes) 

source("ViolationsMap.r")
source("CategoryByHour.R")
source("StateNType.R")
source("HeatMapYear.R")


ui <- navbarPage("Dashboard for Parking Violations in NY", 
                 theme = shinytheme("flatly"), 
                 tabPanel("Heat Map",heatmapUI("heatmap")),
                 tabPanel("Violations Map", violationsMapUI("violationsMap")),
                 tabPanel("Vehicle Type by State", plateTypeModuleUI("regState")),
                 tabPanel("Hour Map", categoryHourUI("categoryHour"))
              
)
server <- function(input, output, session) {
  violationsMapServer("violationsMap")
  categoryHourServer("categoryHour")
  plateTypeModuleServer("regState",data)
  heatmapServer("heatmap")
}

shinyApp(ui, server)

