library(shiny)
library(shinythemes) 

source("ViolationsMap.r")
source("CategoryByHour.R")

source("PlateType.R")

source("HeatMapYear.R", encoding = "UTF-8")

source("vehicles.R")
source("MostLikelyTicket.R")
source("ViolationTrendsAnimate.r")

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
                 tabPanel('Violations by Vehicle Make', vehiclesUI('vehicles')),
                 tabPanel("Violation Trends over time", violationTrendsModuleUI("violationTrend")),
                 tabPanel("Download Report",
                          downloadButton("downloadReport", "Download Data Visualization Report")
                 )
)

server <- function(input, output, session) {
  violationsMapServer("violationsMap")
  categoryHourServer("categoryHour")
  plateTypeModuleServer("regState",data)
  heatmapServer("heatmap")
  vehiclesServer('vehicles')
  mostLikelyTicketModuleServer("likelyTicket")
  
  output$animated_plot <- renderImage({
    list(src = "www/animated_violation_code.gif",
         contentType = "image/gif",
         width = "100%",
         height = "auto")
  }, deleteFile = FALSE)
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      "DataVisualization_report.pdf"
    },
    content = function(file) {
      # Ensure the file path is correct relative to your Shiny app directory
      file.copy("DataVisualization_report.pdf", file)
    }
  )
  
}

shinyApp(ui, server)

