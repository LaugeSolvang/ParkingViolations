library(shiny)

source("ViolationsMap.r")
source("CategoryByHour.R")


# Define the unified UI
ui <- fluidPage(
  titlePanel("Combined Dashboard"),
  tabsetPanel(
    tabPanel("Violations Map", violationsMapUI("violationsMap")),
    tabPanel("Category by Hour", categoryHourUI("categoryHour"))
    
  )
)
server <- function(input, output, session) {
  violationsMapServer("violationsMap")
  categoryHourServer("categoryHour")
  
}

shinyApp(ui, server)

