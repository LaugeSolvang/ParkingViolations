library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

violations <- read.csv("sampled_200k_rows.csv")

# Define UI
vehiclesUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Parking Violations by Vehicle Make"),
    selectInput(ns("orderSelector"), "Order by:", c("Ascending", "Descending")),
    fluidRow(
      column(
        width = 6,
        plotlyOutput(ns("vehiclePlot"))
      ),
      column(
        width = 6,
        plotlyOutput(ns("bodyTypePieChart"))
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
      ViolationCode = violations$Violation.Code,
      VehicleBodyType = violations$Vehicle.Body.Type
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
    
    x_var_reactive <- reactiveVal()
    
    # Update the plot based on user input
    observeEvent(input$orderSelector, {
      # Define x based on user input
      x_var <- if (input$orderSelector == "Descending") {
        reorder(df_ordered$VehicleMake, desc(df_ordered$TotalPercentage))
      } else {
        reorder(df_ordered$VehicleMake, df_ordered$TotalPercentage)
      }
      x_var_reactive(x_var)
      
      # Update the bar chart
      output$vehiclePlot <- renderPlotly({
        p <- ggplot(df_ordered, aes(x = x_var, y = TotalPercentage, fill = VehicleMake, text = paste(VehicleMake, ": Total Percentage: ", round(TotalPercentage, 2), "%"))) +
          geom_bar(stat = "identity") +
          theme_minimal() +
          labs(title = "Percentage of Parking Violations by Vehicle Make", x = "Vehicle Make", y = "Percentage of Violations") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        
        # Convert ggplot to plotly
        p <- ggplotly(p, tooltip = c("text"))
      })
    })
    
    clicked_make <- "TOYOT"
    output$bodyTypePieChart <- renderPlotly({
        selected_make <- clicked_make
        filtered_data <- df_filtered %>% filter(VehicleMake == selected_make)
        
        body_type_distribution <- filtered_data %>%
          group_by(VehicleBodyType) %>%
          summarise(Count = n()) %>%
          mutate(Percentage = (Count / sum(Count)) * 100) %>%
          arrange(desc(Percentage)) %>%
          slice_head(n = 5)  # Select only the top 5 body types
        
        plot_ly(
          data = body_type_distribution,
          labels = ~VehicleBodyType,
          values = ~Percentage,
          type = "pie",
          textinfo = "percent+label",
          insidetextorientation = "radial"
        ) %>%
          layout(title = paste("Vehicle Body Type Distribution for", selected_make))
      })
    
    # Capture click events and update server with selected VehicleMake
    observeEvent(event_data("plotly_click"), {
      clicked_index <- as.numeric(event_data("plotly_click")$x)
      clicked_make <- levels(x_var_reactive())[clicked_index]
      
      # Show pie chart for vehicle body type distribution
      output$bodyTypePieChart <- renderPlotly({
        selected_make <- clicked_make
        filtered_data <- df_filtered %>% filter(VehicleMake == selected_make)
        
        body_type_distribution <- filtered_data %>%
          group_by(VehicleBodyType) %>%
          summarise(Count = n()) %>%
          mutate(Percentage = (Count / sum(Count)) * 100) %>%
          arrange(desc(Percentage)) %>%
          slice_head(n = 5)  # Select only the top 5 body types
        
        plot_ly(
          data = body_type_distribution,
          labels = ~VehicleBodyType,
          values = ~Percentage,
          type = "pie",
          textinfo = "percent+label",
          insidetextorientation = "radial"
        ) %>%
          layout(title = paste("Vehicle Body Type Distribution for", selected_make))
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
