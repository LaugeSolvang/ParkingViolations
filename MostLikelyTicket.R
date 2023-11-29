library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)


mostLikelyTicketModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Most likely ticket to get for each vehicle make"),
    uiOutput(ns("dropDownOptions")),
    plotOutput(ns("ticketCountPlot"))
  )
}


mostLikelyTicketModuleServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rawData <- read.csv("sampled_200k_rows.csv", stringsAsFactors = TRUE)
    data <- select(rawData, Violation.Description, Vehicle.Make)

    topTicketTypes <- reactive({
      data %>%
        group_by(Vehicle.Make) %>%
        summarise(Count = n(), .groups = "drop") %>%
        arrange(desc(Count)) %>%
        slice_head(n=10) %>%
        pull(Vehicle.Make)
    })

    output$dropDownOptions <- renderUI({
      selectInput(ns("selectedVehicleMake"), "Choose Vehicle Make", choices = topTicketTypes())
    })

    filteredData <- reactive({
      req(input$selectedVehicleMake)
      data %>%
        filter(Vehicle.Make == input$selectedVehicleMake) %>%
        group_by(Violation.Description) %>%
        summarise(Count = n(), .groups = "drop") %>%
        arrange(desc(Count)) %>%
        top_n(10) %>%
        mutate(Percentage = Count / sum(Count) * 100)
    })

    output$ticketCountPlot <- renderPlot({
      fd <- filteredData()
      color_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f")
      fd$Violation.Description <- factor(fd$Violation.Description)

      dc <- ggplot(fd, aes(x="", y=Count, fill = Violation.Description)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        geom_polygon(data = data.frame(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1)), aes(x = x, y = y), fill = "white", color = "white", show.legend = FALSE) +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(fill = "Top 10 most likely violations") +
        geom_text(aes(label = paste0(round(Percentage, 1),"%")), position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = setNames(color_palette, levels(fd$Violation.Description))) +
        theme(legend.position = "bottom")
      dc
    })
  })
}

ui <- fluidPage(
  mostLikelyTicketModuleUI("likelyTicket")
)
server <- function(input, output, session) {
  mostLikelyTicketModuleServer("likelyTicket")
}

shinyApp(ui = ui, server = server)