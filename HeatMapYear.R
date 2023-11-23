library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(viridis)
library(tools)
library(shiny)

# UI
heatmapUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Heatmap of Parking Violations"),
    mainPanel(
      plotOutput(ns("violationsHeatmap"), width = "100%") # Adjusted to viewport height
    )
  )
}


heatmapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Read the violations data
    violations <- read.csv("sampled_200k_rows.csv")
    
    # Convert 'Issue.Date' to Date type
    violations$Issue.Date <- as.Date(violations$Issue.Date, format = "%m/%d/%Y")
    
    # Define date range
    start_date <- as.Date("2016-06-27")
    end_date <- as.Date("2017-06-26")
    
    # Filter data within the specified date range
    violations_filtered <- violations %>%
      filter(Issue.Date >= start_date & Issue.Date <= end_date)
    
    # Aggregate data by day and add week and weekday information
    violations_by_day <- violations_filtered %>%
      mutate(Day = floor_date(Issue.Date, "day"),
             Week = isoweek(Issue.Date),
             Weekday = wday(Issue.Date, label = TRUE)) %>%
      group_by(Day, Week, Weekday) %>%
      summarize(Count = n(), .groups = 'drop')
    
    date_seq <- seq.Date(min(violations_by_day$Day), max(violations_by_day$Day), by = "day")
    
    # Determine the first week of each month within that date range
    month_weeks <- data.frame(
      Date = date_seq,
      Week = isoweek(date_seq),
      Month = month(date_seq),
      YearMonth = format(date_seq, "%Y-%m")
    ) %>%
      group_by(YearMonth) %>%
      summarize(FirstWeek = min(Week)) %>%
      ungroup()
    
    # Ensure that the YearMonth column is formatted as a date before converting to MonthLabel
    month_weeks$MonthLabel <- format(as.Date(paste0(month_weeks$YearMonth, "-01")), "%b")
    
    # Create a mapping of weeks to month labels
    week_to_month <- month_weeks %>%
      select(Week = FirstWeek, MonthLabel) %>%
      distinct()
    
    #Remove duplicate month
    week_to_month <- week_to_month[-1, ]
    
    week_to_month$Week <- week_to_month$Week + 0.5
    week_to_month$MonthLabel <- toTitleCase(week_to_month$MonthLabel)
    
    # Heatmap generation logic
    output$violationsHeatmap <- renderPlot({
      # Plot with ggplot2 using the correct week to month mapping
      ggplot(violations_by_day, aes(x = Week, y = Weekday, fill = Count)) +
        geom_tile(color = "white") +
        scale_fill_viridis(option = "D") +
        labs(
          title = "Heatmap of Parking Violations",
          subtitle = "Each square represents a day",
          x = "Month",
          y = "Day of the Week",
          fill = "Number of Violations"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt") # Adjusted margins
        ) +
        scale_x_continuous(breaks = week_to_month$Week, labels = week_to_month$MonthLabel) +
        coord_equal() # Changed from coord_fixed to coord_equal
    }, res = 100) # Increased resolution for better quality
  })
}

ui <- fluidPage(
  
  heatmapUI("heatmap")
)

server <- function(input, output, session) {
  heatmapServer("heatmap")
}

shinyApp(ui, server)
