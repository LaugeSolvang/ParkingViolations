library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

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
        plotlyOutput(ns("violationPlot")) # Changed to plotlyOutput
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
    
    create_violation_plot <- function(data, y_value, plot_title, chart_type) {
      p <- ggplot(data, aes(x = ViolationTimeRounded, y = !!sym(y_value), fill = ViolationCategory))
      
      if (chart_type == "bar") {
        p <- p + geom_bar(stat = "identity", position = "dodge")
      } else if (chart_type == "stacked") {
        p <- p + geom_bar(stat = "identity", position = "stack")
      }
      
      p + theme_minimal() +
        labs(x = "Time of Day (24-hour format)", 
             y = y_value, 
             fill = "Violation Category",
             title = plot_title) +
        scale_fill_brewer(palette = "Set3") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      ggplotly(p)
    }
    observe({
      categories <- c("All", sort(unique(violations_processed$ViolationCategory)))
      updateSelectInput(session, "category", choices = categories)
    })
    
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
        paste("Violations Over the Course of a Day for", input$category)
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
    group_by(ViolationTimeRounded, ViolationCategory) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    left_join(total_violations_by_time, by = "ViolationTimeRounded") %>%
    mutate(Percentage = (Count / TotalCount) * 100)
}


calculate_absolute_data <- function(data) {
  data %>%
    group_by(ViolationTimeRounded, ViolationCategory) %>%
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
