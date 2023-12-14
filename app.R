library(shiny)
library(shinythemes) 

source("ViolationsMap.r")
source("CategoryByHour.R")
source("PlateType.R")
source("HeatMapYear.R")
source("vehicles.R")
source("MostLikelyTicket.R")

ui <- navbarPage("Dashboard for Parking Violations in NY", 
                 theme = shinytheme("flatly"),
                 tabPanel("Violations Map", violationsMapUI("violationsMap")),
                 tabPanel("Violations Over Time",
                          verticalLayout(
                            categoryHourUI("categoryHour"),
                            heatmapUI("heatmap")
                          )
                 ),
                 tabPanel("Vehicle Type by County", plateTypeModuleUI("regState")),
                 tabPanel('Vehicles', vehiclesUI('vehicles')),
                 tabPanel("Most Likely Ticket", mostLikelyTicketModuleUI("likelyTicket"))
)

server <- function(input, output, session) {
  violationsMapServer("violationsMap")
  categoryHourServer("categoryHour")
  plateTypeModuleServer("regState",data)
  heatmapServer("heatmap")
  vehiclesServer('vehicles')
  mostLikelyTicketModuleServer("likelyTicket")
}

shinyApp(ui, server)

