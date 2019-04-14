library(shiny)
library(SmarterPoland)
library(dplyr)


## CREATE DATAFRAME
library(dplyr)
library(ggplot2)

console <- c("Nintendo Switch", "PlayStation 4", "Nintendo 3DS", "PS Vita", "Xbox One")
manufacturer <- c("Nintendo", "Sony Interactive Entertainment", "Nintendo", 
                  "Sony Interactive Entertainment", "Microsoft")
consoles_sold <- c(3482388, 1695227, 566420, 181728, 15339)
consoles <- data.frame(console, manufacturer, consoles_sold) %>% 
  mutate(consoles_sold_mln = consoles_sold / 1000000)  

## APP

ui <- fluidPage(
  
  titlePanel("Consoles sold in Japan"),
  h2("Author: Muszyński Rafał"),
  h3("Click on a bar or above it to get exact number of sold consoles"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_consoles",
                         label = "Select console names:",
                         choices = unique(consoles[["console"]]),
                         selected = unique(consoles[["console"]]))
    ),
    
    mainPanel(
      plotOutput("consoles_plot", height = 600, click = "consoles_click")
    )
  )
)

server <- function(input, output) {
  
  consoles_r <- reactive({
    filter(consoles, console %in% input[["chosen_consoles"]]) 
  })
  
  clicked <- reactiveValues(
    x = -1
  )
  
  observeEvent(input[["consoles_click"]], {
    new <- round(input[["consoles_click"]]$x)
    clicked[['x']] <- ifelse(new == clicked[['x']], -1, new)
  })
  
  getLabels <- function(){
    n <- length(consoles_r()$consoles_sold_mln)
    sold <- rep('', n)
    if(0 < clicked[['x']] && clicked[['x']] <= n){
      sold[clicked[['x']]] <- consoles_r()$consoles_sold_mln[clicked[['x']]]
    }
    sold
  }
  
  output[["consoles_plot"]] <- renderPlot({
    colorMap <- c("Nintendo" = "red", "Sony Interactive Entertainment" = "blue", "Microsoft" = "green")
    ggplot(consoles_r(), aes(x=reorder(console, -consoles_sold), y=consoles_sold_mln, fill=manufacturer)) + 
      geom_bar(stat="identity") + 
      geom_text(aes(label = getLabels()), size = 6, vjust = -0.3) +
      labs(y="Number of sold consoles (mln)", x = "Console") + 
      guides(fill=guide_legend(title="Producent")) +
      scale_fill_manual(values = colorMap) +
      theme_bw(base_size = 18)
  })
  
}

shinyApp(ui = ui, server = server)