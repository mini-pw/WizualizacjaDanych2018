
library(shiny)
library(dplyr)
?selectInput
ui <- fluidPage(
  
  titlePanel("Airquality App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "chosen_param",
                         label = "Select one parameter:",
                         choices = c("Ozone", "Solar.R", "Wind", "Temp"))
    ),
    
    mainPanel(
      h2("Bar plot"),
      plotOutput("param_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  param_colors <- c(Ozone = "red", Solar.R = "green", Wind = "orange", Temp = "black")
  
  param_r <- reactive({
    airquality%>%group_by(Month)%>%summarise(Mean = mean(eval(parse(text = input[["chosen_param"]])),na.rm=TRUE))
    })
  
  output[["param_plot"]] <- renderPlot({

    p <- ggplot(param_r(), aes_string(x = "Month", y ="Mean")) +
      geom_bar(stat="identity", fill = param_colors[input[["chosen_param"]]]) +
      scale_color_manual(values = param_colors[input[["chosen_param"]]]) +
      theme_bw()
    
    p
  })
  
}

shinyApp(ui = ui, server = server)

