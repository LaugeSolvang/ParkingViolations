library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)

categoryHourUI <- fluidPage(
  titlePanel("Violations Over the Course of a Day by Category"),
  sidebarLayout(
    sidebarPanel(
      # Radio buttons to switch between absolute and proportional values
      radioButtons("valueType", "Choose Value Type:",
                   choices = list("Absolute" = "absolute", "Proportional" = "proportional"),
                   selected = "absolute"),
      # Dropdown to choose between bar chart and stacked bar chart
      selectInput("chartType", "Choose Chart Type:",
                  choices = list("Bar Chart" = "bar", "Stacked Bar Chart" = "stacked"),
                  selected = "bar")
    ),
    
    
    mainPanel(
      plotOutput("violationPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read and process the data
  violations <- read.csv("C:\\Users\\lauge\\Downloads\\sampled_100k_rows.csv")
  
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
    mutate(ViolationCategory = ifelse(Count > 4000, as.character(Violation.Code), "Other"))
  
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
  }
  
  
  # Render the plot based on the selected value type
  output$violationPlot <- renderPlot({
    
    if (input$valueType == "absolute") {
      violations_by_time_and_category <- violations_processed %>%
        group_by(ViolationTimeRounded, ViolationCategory) %>%
        summarise(Count = n(), .groups = 'drop')
      
      create_violation_plot(violations_by_time_and_category, "Count", "Violations Over the Course of a Day by Category", input$chartType)
      
    } else {
      total_violations_by_time <- violations_processed %>%
        group_by(ViolationTimeRounded) %>%
        summarise(TotalCount = n(), .groups = 'drop')
      
      violations_by_time_and_category <- violations_processed %>%
        group_by(ViolationTimeRounded, ViolationCategory) %>%
        summarise(Count = n(), .groups = 'drop') %>%
        left_join(total_violations_by_time, by = "ViolationTimeRounded") %>%
        mutate(Percentage = (Count / TotalCount) * 100)
      
      create_violation_plot(violations_by_time_and_category, "Percentage", "Percentage of Violations Over the Course of a Day by Category", input$chartType)
      
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

