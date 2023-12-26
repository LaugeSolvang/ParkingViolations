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
    titlePanel("Seasonal Rhythms and Weekly Patterns in Parking Violations"),
    sidebarLayout(
      # Subtitle in a sidebar panel
      sidebarPanel(
        tags$h4("Throughout the year, parking violations display a consistent pattern, with a notable decrease during summer months. Weekends, in particularly Sundays, see fewer violations. Holidays and snow days almost bring these violations to a complete stop."),
        width = 3
      ),
      # Heatmap in the main panel
      mainPanel(
        plotlyOutput(ns("violationsHeatmap"), width = "auto"),
        width = 9
      )
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
    
    # Create a named vector for Danish-to-English weekday translation
    danish_to_english <- c(mandag = "Monday", tirsdag = "Tuesday", onsdag = "Wednesday", 
                           torsdag = "Thursday", fredag = "Friday", lørdag = "Saturday", søndag = "Sunday")
    
    # Process your data
    violations_by_day <- violations_filtered %>%
      mutate(
        Day = floor_date(Issue.Date, "day"),
        Week = isoweek(Issue.Date),
        Weekday = wday(Issue.Date, label = TRUE, week_start = 1)
      ) %>%
      group_by(Day, Week, Weekday) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      mutate(Weekday = danish_to_english[Weekday]) # Translate to English
    custom_weekday_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    violations_by_day$Weekday <- factor(violations_by_day$Weekday, levels = custom_weekday_order)
    
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
    
    Sys.setlocale("LC_TIME", "C")
    
    # Ensure that the YearMonth column is formatted as a date before converting to MonthLabel
    month_weeks$MonthLabel <- format(as.Date(paste0(month_weeks$YearMonth, "-01")), "%B")
    

    # Create a mapping of weeks to month labels
    week_to_month <- month_weeks %>%
      select(Week = FirstWeek, MonthLabel) %>%
      distinct()
    
    #Remove duplicate month
    week_to_month <- week_to_month[-1, ]
    
    week_to_month$Week <- week_to_month$Week + 1.5

  
    aspect_ratio <- 52 / 7
    
    # Calculate plot height based on aspect ratio and browser width
    plot_width <- 1400
    plot_height <- plot_width / aspect_ratio
    
    weekday_to_int <- c(Sunday = 7, Saturday = 6, Friday = 5, Thursday = 4, Wednesday = 3, Tuesday = 2, Monday = 1)
    
    first_days_of_months <- seq.Date(from = as.Date("2016-06-01"), to = as.Date("2017-06-27"), by = "month")
    first_days_of_months <- floor_date(first_days_of_months, unit = "month")
    
    month_boundaries <- violations_by_day %>%
      filter(Day %in% first_days_of_months) %>%
      select(Week, Weekday)
    
    # Add Weekday Integer column
    month_boundaries <- month_boundaries %>%
      mutate(WeekdayInt = weekday_to_int[Weekday])
    

    output$violationsHeatmap <- renderPlotly({
      # Define shapes for each month's start
      shapes <- lapply(1:nrow(month_boundaries), function(i) {
        list(
          type = "line",
          x0 = month_boundaries$Week[i] - 0.5, y0 = 0,
          x1 = month_boundaries$Week[i] - 0.5, y1 = month_boundaries$WeekdayInt[i] / 7,  # Adjust y1 based on WeekdayInt
          xref = "x", yref = "paper",
          line = list(color = "black", width = 2)
        )
      })
      # Define the second set of shapes
      shapes2 <- lapply(1:nrow(month_boundaries), function(i) {
        if (month_boundaries$WeekdayInt[i] / 7 != 1) {  # Check if y value is not equal to 1 (7/7)
          list(
            type = "line",
            x0 = month_boundaries$Week[i] - 0.5, y0 = month_boundaries$WeekdayInt[i] / 7,
            x1 = month_boundaries$Week[i] + 0.5, y1 = month_boundaries$WeekdayInt[i] / 7,  # Adjust y1 based on WeekdayInt
            xref = "x", yref = "paper",
            line = list(color = "black", width = 2)
          )
        } else {
          NULL  # Return NULL if the condition is not met
        }      })
      # Define the second set of shapes
      shapes3 <- lapply(1:nrow(month_boundaries), function(i) {
        list(
          type = "line",
          x0 = month_boundaries$Week[i] + 0.5, y0 = month_boundaries$WeekdayInt[i] / 7,
          x1 = month_boundaries$Week[i] + 0.5, y1 = 1,  # Adjust y1 based on WeekdayInt
          xref = "x", yref = "paper",
          line = list(color = "black", width = 2)
        )
      })
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
          margin = list(t = 5, r = 5, b = 5, l = 5), # Adjust margins as needed
          shapes = c(shapes, shapes2, shapes3)  # Add the shapes here
          
          
        ) %>%
        colorbar(title = "Number of Violations",
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

