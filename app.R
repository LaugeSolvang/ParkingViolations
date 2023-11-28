library(shiny)
library(shinythemes) 

source("ViolationsMap.r")
source("CategoryByHour.R")
source("StateNType.R")


# Define the unified UI
ui <- navbarPage("Dashboard for Parking Violations in NY", 
                 theme = shinytheme("flatly"), 
                 tabPanel("Violations Map", violationsMapUI("violationsMap")),
                 tabPanel("Category by Hour", categoryHourUI("categoryHour")),
                 tabPanel("Vehicle Type by State", plateTypeModuleUI("regState"))
)
server <- function(input, output, session) {
  violationsMapServer("violationsMap")
  categoryHourServer("categoryHour")
  plateTypeModuleServer("regState",data)

  
}

shinyApp(ui, server)

