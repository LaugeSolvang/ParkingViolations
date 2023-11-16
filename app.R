library(shiny)

source("ViolationsMap.r")
source("CategoryByHour.R")

ui <- fluidPage(
  navbarPage("My Shiny App",
             tabPanel("Map", uitest),        # First tab with content from ViolationsMap.r
             tabPanel("Category by Hour", categoryHourUI)  # Second tab from CategoryByHour.R
  )
)

shinyApp(ui, server)

