library(dplyr)
library(ggimage)
library(ggplot2)
library(readr)
library(shiny)

marriage_amount <- c(282, 267, 244, 280, 307, 255, 211, 228, 193)
years <- c(1946, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2017)

marriage <- data.frame(
  years = years,
  marriage_amount = marriage_amount
)

plot_marriage <- function(selected_marriage, year_period) {
  marriage_subset <- marriage %>% filter(years %in% selected_marriage$years &
                                           marriage_amount %in% selected_marriage$marriage_amount)
  ggplot(marriage, aes(x = years, y = marriage_amount, label = marriage_amount)) +
    geom_line(color = 'blue', alpha = 0.5) + 
    geom_point(color = 'red', size = 4) + 
    geom_text(data = marriage_subset, size = 4, hjust = 1.5) +
    scale_x_continuous(breaks = years, limits = c(year_period[1], year_period[2]), name = 'Lata') + 
    scale_y_continuous(limits = c(0,310), name = 'Liczba zawartych małżeństw (w tyś.)') + 
    theme(plot.title = element_text(hjust = 0.5))
}

ui <- fluidPage(
  titlePanel("Liczba zawartych małżeństw na przestrzeni lat"),
  sidebarPanel(
    sliderInput(inputId = "chosen_time_interval",
                label = "Select time interval",
                sep = "",
                min = min(marriage$years),
                max = max(marriage$years),
                value = c(1943, 2018))
  ),
  mainPanel(
    plotOutput(outputId = "marriage_plot",
               hover = "marriage_hover")
  )
)

server <- function(input, output) {
  selected_marriage <- reactiveValues(
    selected = list(
      years = NULL,
      marriage_amount = NULL
    )
  )
  
  observeEvent(input$marriage_hover, {
    selected_marriage$selected <- nearPoints(marriage, input$marriage_hover, maxpoints = 1)
  })
  
  output$marriage_plot <- renderPlot({
    plot_marriage(selected_marriage$selected,
                  input$chosen_time_interval)
  })
  
}

shinyApp(ui = ui, server = server)