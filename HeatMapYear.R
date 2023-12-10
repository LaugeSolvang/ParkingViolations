library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(viridis)
library(tools)
library(shiny)
library(plotly)

# UI
heatmapUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Heatmap of Parking Violations"),
    mainPanel(
      plotlyOutput(ns("violationsHeatmap"), width = "100%") # Adjusted to viewport height
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
    end_date <- as.Date("2017-06-25")
    
    
    # Filter data within the specified date range
    violations_filtered <- violations %>%
      filter(Issue.Date >= start_date & Issue.Date <= end_date)
    
    violations_by_day <- violations_filtered %>%
      mutate(Day = floor_date(Issue.Date, "day"),
             Week = isoweek(Issue.Date),
             Weekday = wday(Issue.Date, label = TRUE, week_start = 1)) %>%
      group_by(Day, Week, Weekday) %>%
      summarize(Count = n(), .groups = 'drop')
    
    date_seq <- seq.Date(min(violations_by_day$Day), max(violations_by_day$Day), by = "day")
    

    special_dates_info <- data.frame(
      Day = as.Date(c("2016-11-24", "2017-03-14", "2017-02-09", "2016-07-04", "2016-09-05", "2017-05-29", "2017-06-25")),
      Description = c(
        "Thanksgiving Day",
        "Snowstorm",
        "Snowstorms",
        "Independence Day",
        "Labor Day",
        "Memorial Day",
        "Pride March"
      )
    )
    violations_by_day <- violations_by_day %>%
      left_join(special_dates_info, by = "Day")
    
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
    
  
    aspect_ratio <- 52 / 7
    
    # Calculate plot height based on aspect ratio and browser width
    plot_width <- 1800
    plot_height <- plot_width / aspect_ratio
    
    print(plot_width)
    
    output$violationsHeatmap <- renderPlotly({
      
      # Custom text for tooltip
      custom_text <- ifelse(is.na(violations_by_day$Description), 
                            paste("Date:", violations_by_day$Day, "<br>Count:", violations_by_day$Count), 
                            paste("Date:", violations_by_day$Day, "<br>Count:", violations_by_day$Count,"<br>Description:", violations_by_day$Description))
      
      p <- plot_ly(violations_by_day, x = ~Week, y = ~Weekday, z = ~Count, type = 'heatmap', text = ~custom_text, hoverinfo = 'text',
                   colors = viridis::viridis(256, direction = -1),  # Reversed Viridis color scale
                   width = plot_width, height = plot_height) %>% # Set width and height here
        layout(
          xaxis = list(title = 'Month', tickvals = week_to_month$Week, ticktext = week_to_month$MonthLabel),
          yaxis = list(title = 'Day of the Week', autorange = "reversed", scaleanchor = "x", scaleratio = 1),
          margin = list(t = 5, r = 5, b = 5, l = 5) # Adjust margins as needed
          
        ) %>%
        colorbar(title = "Violations",
                 len = 1,      # Length of the color bar (70% of the plot height)
                 y = 1,        # Position
                 thickness = 20  # Thickness of the color bar
        )      
      p
    })
  })
}

ui <- fluidPage(
  
  heatmapUI("heatmap")
)

server <- function(input, output, session) {
  heatmapServer("heatmap")
}

shinyApp(ui, server)

