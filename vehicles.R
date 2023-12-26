library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

violations <- read.csv("sampled_200k_rows.csv")

# Define UI
vehiclesUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Distribution of Parking Violations by Vehicle Make"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("orderSelector"), "Order by:", c("Ascending", "Descending")),
        p("Click on one of the bars, to see their internal distribution of parking tickets and vehicle types below"),
        width = 3
      ),
      mainPanel(
        plotlyOutput(ns("vehiclePlot")), 
        width = 9
      )
    ),
    titlePanel("Parking Violations by Vehicle Make"),
    fluidRow(
      column(
        width = 6,
        plotlyOutput(ns("bodyTypePieChart")),
      ),
      column(
        width = 6,
        plotOutput(ns("ticketCountPlot"))
      )
    ),
  )
}


mostLikelyTicketModuleServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
  })
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
    clicked_make_reactive <- reactiveVal("TOYOT")  # Initialize with default value
    
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
          labs(x = "Vehicle Make", y = "Percentage of Violations") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                legend.position = "none") +
          scale_fill_manual(values = ifelse(df_ordered$VehicleMake == clicked_make_reactive(), "blue", "grey"))
        
        # Convert ggplot to plotly
        p <- ggplotly(p, tooltip = c("text"))
      })
    })
    
    # Function to generate pie chart
    generatePieChart <- function(filtered_data) {
      body_type_distribution <- filtered_data %>%
        group_by(VehicleBodyType) %>%
        summarise(Count = n()) %>%
        mutate(Percentage = (Count / sum(Count)) * 100) %>%
        arrange(desc(Percentage)) %>%
        slice_head(n = 5)  # Select only the top 5 body types
      
      # Mutate the VehicleBodyType to include descriptions
      body_type_distribution <- body_type_distribution %>%
        mutate(BodyTypeDescription = case_when(
          VehicleBodyType %in% c("4DSD", "4DS") ~ "4 Dr. Sedan",
          VehicleBodyType %in% c("2DSD", "2DS") ~ "2 Dr. Sedan",
          VehicleBodyType %in% c("4D", "4DS", "4 DR") ~ "4 Dr.",
          VehicleBodyType %in% c("SDN", "S4", "S2") ~ "Sedan",
          VehicleBodyType %in% c("SUBN", "SUB", "SBR", "SB") ~ "Suburban",
          VehicleBodyType %in% c("WAGO", "WAG") ~ "Wagon",
          VehicleBodyType %in% c("DELV", "DEL") ~ "Delivery",
          VehicleBodyType %in% c("PICK", "PIC", "PK") ~ "Pickup",
          VehicleBodyType %in% c("VAN", "V") ~ "Van",
          VehicleBodyType %in% c("TRAC", "TR", "TRA") ~ "Trac",
          VehicleBodyType %in% c("REFG", "REF", "RE") ~ "Refrigerator Truck",
          VehicleBodyType %in% c("UTIL", "UT", "UTE") ~ "Utility",
          VehicleBodyType %in% c("CONV", "CON", "CVT") ~ "Convertible",
          VehicleBodyType %in% c("SW", "SWA") ~ "Station Wagon",
          VehicleBodyType %in% c("UT", "UTL") ~ "Utility",
          VehicleBodyType %in% c("TK", "TRK", "TRK", "T") ~ "Truck",
          VehicleBodyType %in% c("TRLR", "TR") ~ "Trailer",
          # Add more conditions as needed
          TRUE ~ as.character(VehicleBodyType)  # Default if no match
        ))
      
      plot_ly(
        data = body_type_distribution,
        labels = ~BodyTypeDescription,  # Use the new variable
        values = ~Percentage,
        type = "pie",
        textinfo = "percent+label",
        insidetextorientation = "radial"
      ) %>%
        layout(title = paste("Vehicle Body Type Distribution for", unique(filtered_data$VehicleMake)))
    }
    
    # UI and Server logic ...
    
    # Output for pie chart
    output$bodyTypePieChart <- renderPlotly({
      selected_make <- clicked_make_reactive()
      filtered_data <- df_filtered %>% filter(VehicleMake == selected_make)
      generatePieChart(filtered_data)
    })
    
    ns <- session$ns
    data <- select(violations, Violation.Description, Vehicle.Make)
    
    topTicketTypes <- reactive({
      data %>%
        group_by(Vehicle.Make) %>%
        summarise(Count = n(), .groups = "drop") %>%
        arrange(desc(Count)) %>%
        slice_head(n=10) %>%
        pull(Vehicle.Make)
    })
    
    filteredData <- reactive({
      data %>%
        filter(Vehicle.Make == clicked_make_reactive()) %>%
        group_by(Violation.Description) %>%
        summarise(Count = n(), .groups = "drop") %>%
        arrange(desc(Count)) %>%
        top_n(10) %>%
        mutate(Percentage = Count / sum(Count) * 100)
    })
    
    generateTicketCountPlot <- function(filtered_data) {
      color_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f")
      
      fd <- filtered_data
      fd$Violation.Description <- factor(fd$Violation.Description)
      
      dc <- ggplot(fd, aes(x = "", y = Count, fill = Violation.Description)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        geom_polygon(data = data.frame(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1)), aes(x = x, y = y), fill = "white", color = "white", show.legend = FALSE) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(
          fill = "Top 10 most likely violations",
          title = paste("Ticket Distribution for", clicked_make_reactive())
        ) +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = setNames(color_palette, levels(fd$Violation.Description))) +
        theme(
          plot.title = element_text(hjust = 0.5)
        )
      
      return(dc)
    }
    
    # Output for most likely tickets
    output$ticketCountPlot <- renderPlot({
      fd <- filteredData()
      generateTicketCountPlot(fd)
    })
    
    # Capture click events and update server with selected VehicleMake
    observeEvent(event_data("plotly_click"), {
      clicked_index <- as.numeric(event_data("plotly_click")$x)
      clicked_make <- levels(x_var_reactive())[clicked_index]
      clicked_make_reactive(clicked_make)
      
      # Show pie chart for vehicle body type distribution
      output$bodyTypePieChart <- renderPlotly({
        selected_make <- clicked_make_reactive()
        filtered_data <- df_filtered %>% filter(VehicleMake == selected_make)
        generatePieChart(filtered_data)
      })
      
      # Show Donut chart for most likely tickes
      output$ticketCountPlot <- renderPlot({
        fd <- filteredData()
        generateTicketCountPlot(fd)
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