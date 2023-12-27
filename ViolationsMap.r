library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)


# Read the data outside the server function to do it only once
precincts <- st_read("policePrecincts/geo_export_c2050280-78f2-4481-be68-d54c4e876c56.shp")
violations <- read.csv("sampled_200k_rows.csv")

violations_summary <- violations %>%
  group_by(Violation.Precinct) %>%
  summarize(Count = n())

merged_data <- merge(precincts, violations_summary, by.x = "precinct", by.y = "Violation.Precinct")

# Calculation of breaks using a square root scale, then squared for more linearity
third_root_min <- (min(merged_data$Count) - 1)^(1/2.5)
third_root_max <- (max(merged_data$Count) + 1)^(1/2.5) # Adding 1 to avoid zero in case of min value is 0
breaks_third_root <- seq(third_root_min, third_root_max, length.out = 10) ^ 2.5

# Print breaks for verification
print(breaks_third_root)
# Use the 'YlOrRd' color palette from RColorBrewer
palette_function <- colorBin(palette = "YlOrRd", bins = breaks_third_root, domain = merged_data$Count)

# UI function for the violations map
violationsMapUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("The Color of Compliance: Painting New York's Parking Landscape"),
    fluidRow(
      column(
        width = 12,
        tags$h3("Dense Parking Violations in Lower and Midtown Manhattan Spare Central Park")
      )
    ),
    
    fluidRow(
      column(
        width = 12, # Full width
        leafletOutput(ns("map"), height = 800) # You can adjust the height as needed
      )
    ),
    verbatimTextOutput(ns("info"))
  )
  
}


# Define the server logic
violationsMapServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    output$map <- renderLeaflet({
      leaflet(merged_data) %>% 
        addTiles() %>% 
        addPolygons(
          fillColor = ~palette_function(Count),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            color = "blue", weight = 2,
            bringToFront = TRUE
          ),
          label = ~paste("Precinct:", precinct, "Violations:", Count),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        )
    })
    
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      info <- merged_data[merged_data$precinct == click$id, ]
      output$info <- renderText({
        paste0("Precinct: ", info$precinct, "<br>",
               "Parking Violations: ", info$Count)
      })
    })
  })
}

ui <- fluidPage(
  violationsMapUI("categoryHour")
)

# Define the server
server <- function(input, output, session) {
  violationsMapServer("categoryHour")
}

# Run the app
shinyApp(ui, server)

