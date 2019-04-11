library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simpler Shiny App"),
  
  h2("Scatterplot"),
  plotOutput("countries_plot", height = 600, click = "country_click"),
  verbatimTextOutput("plot_value")
)

server <- function(input, output) {

    continent_colors <- c(Asia = "red", Europe = "green", Africa = "orange", Americas = "black", 
                          Oceania = "blue")
    
    output[["countries_plot"]] <- renderPlot({
      ggplot(countries, aes(x = birth.rate, y = death.rate, color = continent)) +
        geom_point() +
        scale_color_manual(values = continent_colors) +
        theme_bw()
    })
    
}

shinyApp(ui = ui, server = server)
