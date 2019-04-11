library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Wyjsciowka 5"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_feature",
                         label = "Select feature:",
                         choices = c("Ozone", "Solar.R", "Wind", "Temp"),
                         selected = "Ozone")
    ),
    
    mainPanel(
      h2("Plot"),
      plotOutput("air_plot", height = 600, brush = "air_brush")
    )
  )
)

server <- function(input, output) {
  air_r <- reactive({
    na.omit(airquality) %>% 
    select(Month, input[["chosen_feature"]]) %>% 
      group_by(Month) %>% 
      summarise(mean = mean(get(input[['chosen_feature']])))
  })
  
  output[["air_plot"]] <- renderPlot({
    p <- ggplot(air_r(), aes(x = Month, y = mean)) +
      geom_bar(stat='identity') +
      ylab(input[['chosen_feature']]) +
      # scale_color_manual(values = continent_colors[input[["chosen_continent"]]]) +
      theme_bw()
    
    p
  })

}

shinyApp(ui = ui, server = server)