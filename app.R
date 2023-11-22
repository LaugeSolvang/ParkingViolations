library(shiny)
library(shinythemes) 

source("ViolationsMap.r")
source("CategoryByHour.R")


# Define the unified UI
ui <- navbarPage("Dashboard for Parking Violations in NY", 
                 theme = shinytheme("flatly"), 
                 tabPanel("Violations Map", violationsMapUI("violationsMap")),
                 tabPanel("Category by Hour", categoryHourUI("categoryHour"))
)
server <- function(input, output, session) {
  violationsMapServer("violationsMap")
  categoryHourServer("categoryHour")
  
}

shinyApp(ui, server)

