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
  
  selected_countires <- reactiveValues(
    selected = character()
  )
  
  observeEvent(input[["country_click"]], {
    selected_countires[["selected"]] <- c(selected_countires[["selected"]], 
                                          nearPoints(countries, input[["country_click"]], 
                                                     maxpoints = 1)[["country"]])
  })
    
    continent_colors <- c(Asia = "red", Europe = "green", Africa = "orange", Americas = "black", 
                          Oceania = "blue")
    
    output[["countries_plot"]] <- renderPlot({
      mutate(countries, selected = country %in% selected_countires[["selected"]]) %>% 
        ggplot(aes(x = birth.rate, y = death.rate, color = continent, size = selected)) +
        geom_point() +
        scale_color_manual(values = continent_colors) +
        scale_size_manual(values = c(2, 10)) +
        theme_bw()
    })
    
    output[["plot_value"]] <- renderPrint({
      selected_countires[["selected"]]
    })
    
}

shinyApp(ui = ui, server = server)
