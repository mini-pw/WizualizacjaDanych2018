library(ggplot2)
library(shiny)
library(dplyr)


data <- read.csv('all.csv')

months <- c(
  `1` = 'styczeń',
  `2` = 'luty',
  `3` = 'marzec',
  `4` = 'kwiecień',
  `5` = 'maj',
  `6` = 'czerwiec',
  `7` = 'lipiec',
  `8` = 'sierpień',
  `9` = 'wrzesień',
  `10` = 'październik',
  `11` = 'listopad',
  `12` = 'grudzień'
)

data <- 
  data %>% 
  mutate(dummy = 0)

ui <- fluidPage(
  titlePanel('Oglądalność stacji telewizyjnych w 2018 roku'),
    checkboxGroupInput(inputId = 'selected_stations',
                       label = 'Wybierz stację',
                       inline = T,
                       choices = unique(data$Station),
                       selected = unique(data$Station)),
  plotOutput("stations_plot", height = 600, hover = hoverOpts(id = 'hover', delay = 10)),
  actionButton("button", 'Wyczyść'),
  verbatimTextOutput("month_details")
)

server <- function(input, output, session) {
  stations <- reactive({
    data %>% filter(Station %in% input[['selected_stations']])
  })
  
  rv = reactiveValues(selected_month = NULL);
  observeEvent(input[['hover']], {
    hover <- input[['hover']]
    if(is.null(hover)) {
      hover$y = 0
    }
    
    rv[['selected_month']] <- nearPoints(stations(), input[["hover"]], maxpoints = 1, threshold = Inf, yvar = 'dummy') %>%
      select(Month) %>% head() %>% as.integer()
  })
  
  observeEvent(input[['button']],{
    rv[['selected_month']] <- NULL
  })
  
  
  output[['stations_plot']] <- renderPlot({
    plot <- stations() %>%
      ggplot(aes(x = Month, y = Share, color = Station)) +
      guides(color = guide_legend(title = 'Stacja')) +
      scale_x_discrete(limits = months) +
      xlab('') +
      ylab('Procentowy udział w rynku') +
      geom_line()
    
    month <- rv[['selected_month']]
    if(!is.null(month)) {
      plot <- plot +
        geom_vline(xintercept = as.integer(month), linetype = 'dashed')
    }
    plot
  })
  
  output[['month_details']] <- renderPrint({
    if(is.null(rv[['selected_month']])) {
      return (stations() %>% filter(Month == -1))
    }
    
    stations() %>%
        filter(Month == rv[['selected_month']]) %>%
        arrange(desc(Share)) %>% 
        select(Stacja = Station, Udział = Share)
  })
}

shinyApp(ui = ui, server = server)



