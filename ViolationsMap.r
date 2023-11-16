library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RColorBrewer)


# Read the data outside the server function to do it only once
precincts <- st_read("Police Precincts\\geo_export_c2050280-78f2-4481-be68-d54c4e876c56.shp")
violations <- read.csv("sampled_200k_rows.csv")

violations_summary <- violations %>%
  group_by(Violation.Precinct) %>%
  summarize(Count = n())

merged_data <- merge(precincts, violations_summary, by.x = "precinct", by.y = "Violation.Precinct")

breaks_sqrt <- seq(sqrt(min(merged_data$Count)), sqrt(max(merged_data$Count)), length.out = 10) ^ 2
palette_function <- colorBin(palette = brewer.pal(9, "Blues"), bins = breaks_sqrt, domain = merged_data$Count)

# Define the UI
ui <- fluidPage(
  titlePanel("Parking Violations in New York Precincts"),
  leafletOutput("map"),
  verbatimTextOutput("info"),
  
)

# Define the server logic
server <- function(input, output, session) {
  
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
}

# Run the app
shinyApp(ui, server)

