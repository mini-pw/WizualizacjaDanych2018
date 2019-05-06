library(shiny)
library(reshape2)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_stations",
                         label = "Select stations:",
                         choices = c("TVP 1", "Polsat", "TVN", "TVP2", "TVN24", "TV4", "TVP INFO", "TVN7", "TTV", "TV PULS"),
                         selected = c("TVP 1", "Polsat", "TVN", "TVP2", "TVN24", "TV4", "TVP INFO", "TVN7", "TTV", "TV PULS")),
      hr(),
      checkboxGroupInput(inputId = "chosen_year",
                  label = "Select year:",
                  choices = c(2018,2019),
                  selected = c(2018,2019))
    ),
    
    mainPanel(
      h2("Ogladalnosc telewizji w styczniu"),
      plotOutput("main_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  output[["main_plot"]] <- renderPlot({
    options(scipen=999)
    csv <- read.csv(file='dane.csv')
    x <- melt(csv, id.vars='station')
    years <- input[["chosen_year"]] == c(2018,2019)
    if(years[1] == TRUE && years[2] == TRUE){
      x$variable <- factor(x$variable, levels=c('jan2018', 'jan2019'), labels=c('2018', '2019'))
    } else if(years[1] == TRUE && years[2] == FALSE){
      x$variable <- factor(x$variable, levels=c('jan2018'), labels=c('2018'))
    } else if(years[1] == FALSE && years[2] == TRUE){
      x$variable <- factor(x$variable, levels=c('jan2019'), labels=c('2019'))
    } 
    
    
    ordering <- csv %>% arrange(desc(jan2019)) %>% filter(station %in% input[["chosen_stations"]]) %>% pull(station)
    p <-  filter(x, station %in% input[["chosen_stations"]]) %>% 
      na.omit %>%
      ggplot(aes(x = station, y = value, fill = variable)) +
        geom_bar(stat = 'identity',  position = 'dodge') +
        scale_x_discrete(limits = ordering) +
        scale_fill_discrete(name = 'Rok') +
        xlab('Stacja') + 
        ylab('Ogladalnosc')
    
    p
  })
  
}

shinyApp(ui = ui, server = server)