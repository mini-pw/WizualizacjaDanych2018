library(shiny)
library(SmarterPoland)
library(dplyr)
head(airquality)

ui <-  pageWithSidebar(
  
  # App title ----
  headerPanel("Ozone data"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    selectInput("variable", "Variable:", 
                c("ozon" = "Ozone",
                  "solar rate" = "Solar.R",
                  "wiatr" = "Wind",
                  "temperatura" = "Temp")),
    checkboxInput("outliers", "x", FALSE)
    
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    h3(textOutput("caption")),
    plotOutput("monthPlot")
  )
)




server <- function(input, output) {
  formulaText <- reactive({
    paste("Month ~", input$variable)
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  
  output$monthPlot <- renderPlot({
    
    p <- ggplot(airquality, aes_string(x = "Month", y = input$variable)) +
      geom_bar(stat="identity")+
      theme_bw()
    
    p

  })
  
}

shinyApp(ui, server)