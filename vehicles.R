library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Define UI
vehiclesUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Parking Violations by Vehicle Make"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("orderSelector"), "Order by:", c("Ascending", "Descending"))
      ),
      mainPanel(
        plotlyOutput(ns("vehiclePlot"))
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
    
    # Filter to include only VehicleMakes with at least 1000 entries
    included_vehicle_makes <- df_percentage %>%
      group_by(VehicleMake) %>%
      summarise(Count = n()) %>%
      filter(Count >= 1000) %>%
      pull(VehicleMake)
    
    df_filtered <- df_percentage %>%
      filter(VehicleMake %in% included_vehicle_makes)
    
    df_ordered <- df_filtered %>%
      group_by(VehicleMake) %>%
      summarise(TotalPercentage = sum(Percentage))
    
    # Update the plot based on user input
    observeEvent(input$orderSelector, {
      # Define x based on user input
      x_var <- if (input$orderSelector == "Descending") {
        reorder(df_ordered$VehicleMake, desc(df_ordered$TotalPercentage))
      } else {
        reorder(df_ordered$VehicleMake, df_ordered$TotalPercentage)
      }
      
      # Update the plot
      output$vehiclePlot <- renderPlotly({
        p <- ggplot(df_ordered, aes(x = x_var, y = TotalPercentage, fill = VehicleMake, text = paste(VehicleMake, ": Total Percentage: ", round(TotalPercentage, 2), "%"))) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(title = "Percentage of Parking Violations by Vehicle Make", x = "Vehicle Make", y = "Percentage of Violations") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        
        # Convert ggplot to plotly
        ggplotly(p, tooltip = c("text"))
      })
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
