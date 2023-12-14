library(shiny)
library(shinythemes) 

source("ViolationsMap.r")
source("CategoryByHour.R")
<<<<<<< HEAD
source("PlateType.R")
source("HeatMapYear.R")
=======
source("StateNType.R")
source("HeatMapYear.R", encoding = "UTF-8")
>>>>>>> a6c5f8999f493dd3fc77333673b1bf564d2e2d61
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

