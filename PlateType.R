library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)

plateTypeModuleUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Top Five Violation Counties and Vehicle Plate Types"),
    mainPanel(
      plotlyOutput(ns("groupedBarChart")) 
    )
  )
}

# Module Server function
plateTypeModuleServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Read the data
    Raw_data <- read.csv("sampled_200k_rows.csv", stringsAsFactors = TRUE)
    
    # Prepare the data
    data <- data.frame(
      Type = Raw_data$Plate.Type,
      ViolationCode = Raw_data$Violation.Code,
      County = Raw_data$Violation.County
    )
    df_percentage <- transform(data, Percentage = (ViolationCode / sum(ViolationCode)) * 100)
    
    # Determine the top five plate types and counties
    top_plate_types <- df_percentage %>%
      count(Type) %>%
      top_n(5, n) %>%
      pull(Type)
    
    top_violation_counties <- df_percentage %>%
      count(County) %>%
      top_n(5, n) %>%
      pull(County)
    
    
    # Filter data for only top plate types and violation counties
    filtered_data <- df_percentage %>%
      filter(Type %in% top_plate_types & County %in% top_violation_counties) %>%
      mutate(
        County = case_when(
          County == "BK" ~ "Brooklyn",
          County == "BX" ~ "Bronx",
          County == "K" ~ "Kings",
          County == "NY" ~ "New York",
          County == "Q" ~ "Queens",
          TRUE ~ County
        ), # Apply the new names for County
        
        Type = case_when(
          Type == "PAS" ~ "Passenger",
          Type == "COM" ~ "Commercial",
          Type == "OMS" ~ "Rental",
          Type == "OMT" ~ "Taxi",
          Type == "SRF" ~ "Special Passenger",
          TRUE ~ Type
        ) # Correctly apply the new names for Type
      )
    

    summarized_data <- filtered_data %>%
      count(Type, County) %>%
      pivot_wider(names_from = County, values_from = n, values_fill = list(n = 0)) %>%
      pivot_longer(cols = -Type, names_to = 'County', values_to = 'Count')
    
    # Render the grouped bar chart using plotly
    output$groupedBarChart <- renderPlotly({
      summarized_data <- summarized_data %>%
        group_by(County) %>%
        mutate(Total = sum(Count), Percentage = Count / Total * 100) %>%
        ungroup()
      
      # Create the plot with renamed counties
      p <- ggplot(summarized_data, aes(x = County, y = Percentage, fill = Type,
                                       text = paste(Type, "Vehicle: ", round(Percentage, 2), "%"))) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        scale_fill_brewer(palette = "Paired") +
        labs(x = "Violation County", y = "Percentage", title = "Comparison among Plate Types") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 65, hjust = 1))
      
      # Convert the ggplot object to a plotly object with tooltips
      ggplotly(p, tooltip = c("text"))
    })
  })
}

ui <- fluidPage(
  plateTypeModuleUI("regState")
)

server <- function(input, output, session) {
  plateTypeModuleServer("regState")
}

shinyApp(ui = ui, server = server)
