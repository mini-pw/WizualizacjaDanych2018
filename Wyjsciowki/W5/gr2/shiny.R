library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Chicks"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_diet",
                         label = "Select diet:",
                         choices = unique(ChickWeight$Diet)),
      sliderInput(inputId = "range_start", label = "Start przedziału czasu", 
                  min = min(ChickWeight$Time), max = max(ChickWeight$Time), 
                  value = min(ChickWeight$Time)),
      sliderInput(inputId = "range_end", label = "Koniec przedziału czasu", 
                  min = min(ChickWeight$Time), max = max(ChickWeight$Time), 
                  value = max(ChickWeight$Time))
    ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("plot", height = 600)
    )
  )
)

server <- function(input, output) {
  chicks <- reactive({
    r <- filter(ChickWeight, between(Time, input[["range_start"]], input[["range_end"]])) 
    r$Diat <- factor(r$Diet)
    r
  })
  
  output[["plot"]] <- renderPlot({
    chicks() %>%
      filter(Diet %in% input[["chosen_diet"]]) %>%
        ggplot(aes(group = Diet, x = Time, y = weight)) + 
          geom_point(aes(col = Diet)) + 
          stat_smooth(aes(col = Diet), se = FALSE) + 
          theme(legend.position = 'none')
  })
}

shinyApp(ui = ui, server = server)
