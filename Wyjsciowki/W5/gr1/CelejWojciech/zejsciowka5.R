library(shiny)
library(SmarterPoland)
library(dplyr)

airquality_agg <- airquality %>% 
  select(-Day) %>% 
  group_by(Month) %>% 
  summarise(Ozone = mean(Ozone, na.rm = TRUE),
            Solar.R = mean(Solar.R, na.rm = TRUE),
            Wind = mean(Wind, na.rm = TRUE),
            Temp = mean(Temp, na.rm = TRUE))
  
ui <- fluidPage(
  
  titlePanel("Zejsciowka nr 5"),
  
  selectInput(inputId = "variable", label = "Variables:", 
              choices = c("Ozone", "Solar.R", "Wind", "Temp")),
    
  h2("Plot"),
  plotOutput("ozone_plot", height = 600)
)

server <- function(input, output) {
  
  airquality_agg_r <- reactive({
    select(airquality_agg, input[["variable"]], Month)
  })
  
  output[["ozone_plot"]] <- renderPlot({
    p <- ggplot(airquality_agg_r(), aes_string(x = "Month", y = input[["variable"]])) +
    geom_bar(stat = "identity") +
    theme_gray()
    p
  })
}

shinyApp(ui = ui, server = server)