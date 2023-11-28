library(shiny)
library(dplyr)
library(ggplot2)


Raw_data <- read.csv("sampled_200k_rows.csv",stringsAsFactors = TRUE)
data <- select(Raw_data, Plate.Type, Registration.State)


plateTypeModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Top Five Vehicle Types for Each Registration State"),
    uiOutput(ns("dropdownOptions")),
    plotOutput(ns("stateCountPlot"))
  )
}

# Module Server function
plateTypeModuleServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    

    top_plate_types <- reactive({
      data %>%
        group_by(Registration.State) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        arrange(desc(Count)) %>%
        slice_head(n = 5) %>%  # Select top 5 most frequent Registration.State values
        pull(Registration.State)
    })
    
    # Update dropdown options for top Plate.Type values
    output$dropdownOptions <- renderUI({
      selectInput(ns("selectedPlateType"), "Choose a Registration State:", choices = top_plate_types())
    })
    
    # Reactive expression to filter data based on selected Plate.Type
    filtered_data <- reactive({
      req(input$selectedPlateType) # Require input to be non-null
      data %>% 
        filter(Registration.State == input$selectedPlateType) %>%
        group_by(Plate.Type) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        arrange(desc(Count)) %>%
        top_n(5) %>%
        mutate(Percentage = Count / sum(Count) * 100)
    })
    
    # Render the plot based on the filtered data
    output$stateCountPlot <- renderPlot({
      # Get the filtered data
      df <- filtered_data()
      
      # Create the pie chart
      ggplot(df, aes(x = "", y = Count, fill = Plate.Type)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(fill = "Top Five Plate Type") +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5))
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
