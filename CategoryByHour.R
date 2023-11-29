library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)

categoryHourUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Violations Over the Course of a Day by Category"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(ns("valueType"), "Choose Value Type:",
                     choices = list("Absolute" = "absolute", "Proportional" = "proportional"),
                     selected = "absolute"),
        selectInput(ns("chartType"), "Choose Chart Type:",
                    choices = list("Bar Chart" = "bar", "Stacked Bar Chart" = "stacked"),
                    selected = "bar"),
        selectInput(ns("category"), "Choose Category:", choices = c("All" = "All"))
        
      ),
      mainPanel(
        plotlyOutput(ns("violationPlot")) 
      )
    )
  )
}

# Define server logic
categoryHourServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Read and process the data
    violations <- read.csv("sampled_200k_rows.csv")
    
    convert_and_round_time <- function(time_str) {
      time_str <- sub("([0-1][0-9])([0-5][0-9])([AP])", "\\1:\\2 \\3M", time_str)
      time <- parse_date_time(time_str, orders = "HMp")
      time_rounded <- round_date(time, unit = "30 minutes")
      format(time_rounded, "%H:%M")
    }
    
    violations_processed <- violations %>%
      mutate(ViolationTimeRounded = convert_and_round_time(Violation.Time)) %>%
      filter(!is.na(ViolationTimeRounded))
    
    violations_summary <- violations_processed %>%
      group_by(Violation.Code) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(ViolationCategory = ifelse(Count > 8000, as.character(Violation.Code), "Other"))
    
    
    violations_processed <- violations_processed %>%
      left_join(violations_summary, by = "Violation.Code") %>%
      mutate(ViolationCategory = ifelse(is.na(ViolationCategory), "Other", ViolationCategory))
    
    violations_processed <- violations_processed %>%
      mutate(ViolationDescription = case_when(
        Violation.Code == 7 ~ "Red Light",
        Violation.Code == 14 ~ "No Standing",
        Violation.Code == 20 ~ "No Parking",
        Violation.Code == 21 ~ "Street Cleaning",
        Violation.Code == 36 ~ "School Zone Speeding",
        Violation.Code == 37 ~ "Parking Meter Excess",
        Violation.Code == 38 ~ "No Meter Receipt",
        Violation.Code == 40 ~ "Close to Fire Hydrant",
        Violation.Code == 46 ~ "Double Parking",
        Violation.Code == 71 ~ "No Inspection Sticker",
        TRUE ~ "Unknown"  # Default case
      ))
    
    create_violation_plot <- function(violation_data, y_value, plot_title, chart_type) {
      violation_data$LegendLabel <- paste(violation_data$ViolationCategory, "-", violation_data$ViolationDescription)
      
      # Prepare hover text
      hover_text <- paste("Time: ", violation_data$ViolationTimeRounded, 
                          "<br>Category: ", violation_data$ViolationCategory, 
                          "<br>Description: ", violation_data$ViolationDescription, 
                          "<br>", y_value, ": ", violation_data[[y_value]])
      
      # Create a basic plotly object
      p <- plot_ly(violation_data, x = ~ViolationTimeRounded, y = as.formula(paste0("~", y_value)), 
                   type = 'bar', color = ~LegendLabel, colors = "Set3",
                   text = hover_text, hoverinfo = "text")
      
      # Modify the plot based on the chart type
      if (chart_type == "bar") {
        p <- p %>% layout(barmode = 'group')
      } else if (chart_type == "stacked") {
        p <- p %>% layout(barmode = 'stack')
      }
      
      # Add labels and adjust layout
      p <- p %>% layout(title = plot_title,
                        xaxis = list(title = "Time of Day (24-hour format)"),
                        yaxis = list(title = y_value),
                        legend = list(title = list(text = "Violation Category")),
                        margin = list(b = 150)) # Adjust bottom margin if labels are cut off
      
      return(p)    }
    observe({
      # First, create a summary that maps categories to descriptions
      category_descriptions <- violations_processed %>%
        group_by(ViolationCategory) %>%
        summarise(Description = first(ViolationDescription), .groups = 'drop') %>%
        arrange(ViolationCategory)
      
      # Create a named vector for the selectInput choices
      categories_with_descriptions <- setNames(
        category_descriptions$ViolationCategory,
        paste(category_descriptions$ViolationCategory, "-", category_descriptions$Description)
      )
      
      # Add "All" option
      categories_with_descriptions <- c("All" = "All", categories_with_descriptions)
      
      # Update the selectInput with these new choices
      updateSelectInput(session, "category", choices = categories_with_descriptions)
    })
    
    category_to_description <- violations_processed %>%
      group_by(ViolationCategory) %>%
      summarise(Description = first(ViolationDescription), .groups = 'drop') %>%
      pull(Description, name = ViolationCategory)
    
    output$violationPlot <- renderPlotly({
      # Data filtering logic based on user input
      filtered_data <- violations_processed
      if (input$category != "All") {
        filtered_data <- filtered_data %>% filter(ViolationCategory == input$category)
      }
      # Decide whether to use absolute or proportional values
      data_for_plot <- if (input$valueType == "proportional") {
        calculate_proportional_data(violations_processed, filtered_data, input$category)
      } else {
        calculate_absolute_data(filtered_data)
      }
      
      plot_title <- if (input$category == "All") {
        "Violations Over the Course of a Day by Category"
      } else {
        category_desc <- category_to_description[input$category]
        paste("Violations Over the Course of a Day for", input$category, category_desc)
      }
      
      y_value <- if (input$valueType == "proportional") "Percentage" else "Count"
      
      create_violation_plot(data_for_plot, y_value, plot_title, input$chartType)
    })
  })
}
calculate_proportional_data <- function(all_data, filtered_data, category) {
  total_violations_by_time <- all_data %>%
    group_by(ViolationTimeRounded) %>%
    summarise(TotalCount = n(), .groups = 'drop')
  
  data_to_use <- if (category == "All") all_data else filtered_data
  
  data_to_use %>%
    group_by(ViolationTimeRounded, ViolationCategory, ViolationDescription) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    left_join(total_violations_by_time, by = "ViolationTimeRounded") %>%
    mutate(Percentage = (Count / TotalCount) * 100)
}


calculate_absolute_data <- function(data) {
  data %>%
    group_by(ViolationTimeRounded, ViolationCategory, ViolationDescription) %>%
    summarise(Count = n(), .groups = 'drop')
}

ui <- fluidPage(
  categoryHourUI("violationsMap")
)

# Define the server
server <- function(input, output, session) {
  categoryHourServer("violationsMap")
}

# Run the app
shinyApp(ui, server)

