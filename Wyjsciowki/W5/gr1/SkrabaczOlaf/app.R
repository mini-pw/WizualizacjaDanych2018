library(shiny)
library(SmarterPoland)
library(dplyr)
#na osi y pokazujemy wartość ozone, wind i temperature na osi x miesiac dane zagregowane po miesiacu
ui <- fluidPage(
  
  titlePanel("W5 - OlafSkrabacz"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_column",
                         label = "Select column:",
                         choices = colnames(airquality)[1:4],
                         selected = "Ozone")
    ),
    
    mainPanel(
      h2("Boxplot"),
      plotOutput("airquality_plot", height = 600)
      
  ))
)

server <- function(input, output) {
  

  airquality_grouped <- reactive({
    airquality %>% group_by(Month) %>% summarise_all(mean, na.rm=TRUE)
  })
  
  
  output[["airquality_plot"]] <- renderPlot({
    p <- ggplot(airquality_grouped(), aes_string(x = "Month", y = input[['chosen_column']])) +
      geom_bar(stat="identity") +
      theme_bw()
    
    p
  })
  
}


shinyApp(ui = ui, server = server)
