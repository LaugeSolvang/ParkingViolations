library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

# Define UI
vehiclesUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Parking Violations by Vehicle Make"),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("orderSelector"))  # Add the selectInput dynamically
      ),
      mainPanel(
        plotOutput(ns("vehiclePlot"))
      )
    )
  )
}

# Define server logic
vehiclesServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Read and process the data
    violations <- read.csv("sampled_200k_rows.csv")
    
    vehicle_df <- data.frame(
      VehicleMake = violations$Vehicle.Make,
      ViolationCode = violations$Violation.Code
    )
    
    df_percentage <- transform(vehicle_df, Percentage = (ViolationCode / sum(ViolationCode)) * 100)
    
    # Filter to include only VehicleMakes with at least 100 entries
    included_vehicle_makes <- df_percentage %>%
      group_by(VehicleMake) %>%
      summarise(Count = n()) %>%
      filter(Count >= 1000) %>%
      pull(VehicleMake)
    
    df_filtered <- df_percentage %>%
      filter(VehicleMake %in% included_vehicle_makes)
    
    df_ordered <- df_filtered %>%
      group_by(VehicleMake) %>%
      summarise(TotalPercentage = sum(Percentage)) %>%
      arrange(desc(TotalPercentage))
    
    print(df_ordered)
    
    # Create the bar chart
    output$vehiclePlot <- renderPlot({
      ggplot(df_ordered, aes(x = reorder(VehicleMake, TotalPercentage, FUN = sum), y = TotalPercentage, fill = VehicleMake)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Percentage of Parking Violations by Vehicle Make", x = "Vehicle Make", y = "Percentage of Violations") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    })
  })
}

ui <- fluidPage(
  vehiclesUI("vehicles")
)

# Define the server
server <- function(input, output, session) {
  vehiclesServer("vehicles")
}

# Run the app
shinyApp(ui, server)
