#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)

station <- as.factor(c('TVP 1','Polsat','TVN','TVP 2','TVN24','TV4','TVP INFO','TV PULS','TVN7','TTV'))

df <- data.frame(station, jan2018=c(791502,759871,669120,621696,267821,286673,262265,228494,242612,149526)
                 , jan2019=c(740636,702797,633988,617677,349907,256386,255191,24826,214997,155856))

ui <- fluidPage(
   titlePanel("Viewership in Poland"),
   sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "stations",
                           label = "Select stations:",
                           choices = unique(df[["station"]]),
                           selected = unique(df[["station"]])),
        checkboxInput(inputId="flip", label="Flip?", value=FALSE)
      ),
      mainPanel(
         plotOutput("viewership",hover="onhover"),
         verbatimTextOutput("detailsTooltip"),
         tableOutput("detailsTable")
      )
   )
)

server <- function(input, output) {

  data <- reactive({
    df %>% 
      filter(station %in% input$stations) %>% 
      gather(DATA, AMR, jan2018:jan2019) %>% 
      mutate(DATA = factor(DATA, levels=c('jan2018', 'jan2019'), labels=c('Styczen 2018r.', 'Styczen 2019r.')))
  })
  
  ordering <- reactive({
    df %>% filter(station %in% as.factor(input$stations)) %>% arrange(desc(jan2019)) %>% pull(station)    
  })
  
  output$detailsTooltip <- renderText({
    if (is.null(input$onhover)) {
      return('Hover on bar to see details (Work in progress)')
    }
  })
  
  output$detailsTable <- renderTable({
    if (!is.null(input$onhover)) {
      point <- nearPoints(data(), input$onhover, threshold = Inf, maxpoints = 1)
      df %>% filter(station == point$station)
    }
  })
  
  output$viewership <- renderPlot({
    gg <- ggplot(data = data(), aes(x = station, y = AMR, fill = DATA), labels('Rok 2019', 'Rok 2018')) +
      geom_col(position = "dodge") +
      scale_x_discrete(limits = ordering()) +
      scale_fill_discrete(name = 'Okres badania') +
      scale_y_continuous(labels = function(x) sprintf("%s tys.", x / 1000)) + 
      xlab('Stacja') + 
      ylab('Srednia ogladalnosc minutowa')
    if (input$flip) {
      gg <- gg + coord_flip()
    }
    return(gg)
  })
}

shinyApp(ui = ui, server = server)
