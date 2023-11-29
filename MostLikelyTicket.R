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
    data <- select(rawData, Violation.Code, Vehicle.Make)

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
        group_by(Violation.Code) %>%
        summarise(Count = n(), .groups = "drop") %>%
        arrange(desc(Count)) %>%
        top_n(10) %>%
        mutate(Percentage = Count / sum(Count) * 100)
    })

    output$ticketCountPlot <- renderPlot({
      fd <- filteredData()
      ggplot(fd, aes(x="", y=Count, fill = Violation.Code)) +
        geom_bar(stat = "identity", width = 1) + 
        coord_polar("y", start = 0) +
        theme_void() + 
        labs(fill = "Top 10 most likely violations") +
        geom_text(aes(label = paste0(round(Percentage, 1),"%")), position = position_stack(vjust = 0.5))
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