library(shiny)
library(shinythemes) 

source("ViolationsMap.r")
source("CategoryByHour.R")
source("StateNType.R")
source("HeatMapYear.R")


ui <- navbarPage("Dashboard for Parking Violations in NY", 
                 theme = shinytheme("flatly"), 
                 tabPanel("Violations Map", violationsMapUI("violationsMap")),
                 tabPanel("Violations Over Time",
                          verticalLayout(
                            categoryHourUI("categoryHour"),
                            heatmapUI("heatmap")
                          )
                 ),
                 tabPanel("Vehicle Type by State", plateTypeModuleUI("regState"))
)
server <- function(input, output, session) {
  violationsMapServer("violationsMap")
  categoryHourServer("categoryHour")
  plateTypeModuleServer("regState",data)

  heatmapServer("heatmap")
}

shinyApp(ui, server)

