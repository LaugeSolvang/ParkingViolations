library(shiny)

source("ViolationsMap.r")
source("CategoryByHour.R")

<<<<<<< HEAD
ui <- fluidPage(
  navbarPage("My Shiny App",
             tabPanel("Map", uitest),        # First tab with content from ViolationsMap.r
             tabPanel("Category by Hour", categoryHourUI)  # Second tab from CategoryByHour.R
  )
=======
# Read the data outside the server function to do it only once
precincts <- st_read("policePrecincts/geo_export_c2050280-78f2-4481-be68-d54c4e876c56.shp")
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
  verbatimTextOutput("info")
>>>>>>> 3a4a5feb5016a03a1069bdb3b26864ac17ad6b32
)

shinyApp(ui, server)

