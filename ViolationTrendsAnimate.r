library(ggplot2)
library(gganimate)
library(gifski)
library(shiny)

violationTrendsModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel("Animated Violation Descriptions"),
    mainPanel(
      imageOutput("animated_plot")
    )
  )
} 

violations <- read.csv("sampled_200k_rows.csv", stringsAsFactors = TRUE)

violations$Issue.Date <- as.Date(violations$Issue.Date, format = "%m/%d/%Y")

before_date <- as.Date("2016-06-01", format = "%Y-%m-%d")
after_date <- as.Date("2017-06-30", format = "%Y-%m-%d")

violations <- violations[violations$Issue.Date >= before_date, ]
violations <- violations[violations$Issue.Date <= after_date, ]

base_plot <- ggplot(violations, aes(x = Violation.Code)) +
  geom_bar(aes(fill = factor(Violation.Code)), width = 0.9) +
  labs(title = "Distribution of Violation Codes", subtitle = "Date: {frame_time}", x = "Violation Code", y = "Count") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100)) +
  theme(legend.position = "none")

animated_plot <- base_plot + transition_time(Issue.Date)
anim_save("www/animated_violation_code.gif", animation = animated_plot, renderer = gifski_renderer(), fps = 5)

ui <- fluidPage(
  violationTrendsModuleUI("violationTrend")
)

server <- function(input, output) {
  output$animated_plot <- renderImage({
    list(src = "www/animated_violation_code.gif",
         contentType = "image/gif",
         width = "100%",
         height = "auto")
  }, deleteFile = FALSE)
}

shinyApp(ui = ui, server = server)
