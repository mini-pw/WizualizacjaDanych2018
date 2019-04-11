library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sliderInput("slider1", label = h3("Birth Rate Range"), min = 0,
                max = 50, value = c(0, 50)),
  sliderInput("slider2", label = h3("Death Rate Range"), min = 0,
              max = 20, value = c(0, 20)),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_continent",
                         label = "Select continent names:",
                         choices = unique(countries[["continent"]]),
                         selected = unique(countries[["continent"]]))
    ),
   
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("countries_plot", height = 600, brush = "country_brush"),
      h2("Table"),
      tableOutput("countries_table")
    )
  )
)

server <- function(input, output) {
  
  continent_colors <- c(Asia = "red", Europe = "green", Africa = "orange", Americas = "black", 
                        Oceania = "blue")
  
  countries_r <- reactive({
    filter(countries, continent %in% input[["chosen_continent"]])
  })
  
  countries_b <- reactive({
    filter(countries_r(),
           birth.rate > input$slider1[1],
           birth.rate < input$slider1[2],
           death.rate > input$slider2[1],
           death.rate < input$slider2[2])
  })
  
  output[["countries_plot"]] <- renderPlot({
    p <- ggplot(countries_b(), aes(x = birth.rate, y = death.rate, color = continent)) +
      geom_point() +
      scale_color_manual(values = continent_colors[input[["chosen_continent"]]]) +
      theme_bw()
    
    p
  })
  
  output[["countries_table"]] <- renderTable({
    validate(
      need(input[["country_brush"]], "Select at least one country")
    )
  })
  
}

shinyApp(ui = ui, server = server)