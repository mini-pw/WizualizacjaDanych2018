library(ggplot2)
library(shiny)



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
  # sidebarPanel(
    checkboxGroupInput(inputId = 'selected_stations',
                       label = 'Wybierz stację',
                       inline = T,
                       choices = unique(data$Station),
                       selected = unique(data$Station)),
  # ),
  plotOutput("stations_plot", height = 600, hover = hoverOpts(id = 'hover', delay = 10)),
  verbatimTextOutput("month_details")
)

server <- function(input, output, session) {
  stations <- reactive({
    data %>% filter(Station %in% input[['selected_stations']])
  })
  
  selected_month <- reactive({
    hover <- input[['hover']]
    if(is.null(hover)) {
      hover$y = 0
    }
    
    nearPoints(stations(), input[["hover"]], maxpoints = 1, threshold = Inf, yvar = 'dummy') %>% select(Month) %>% head()
  })
  
  output[['stations_plot']] <- renderPlot({
    plot <- stations() %>%
      ggplot(aes(x = Month, y = Share, color = Station)) +
      guides(color = guide_legend(title = 'Stacja')) +
      scale_x_discrete(limits = months) +
      xlab('') +
      ylab('Procentowy udział w rynku') +
      geom_line()
    
    # month <- selected_month()
    # if(!is.null(month)) {
    #   # plot <- plot +
    #   #   geom_vline(xintercept = month)
    # }
    plot
  })
  
  output[['month_details']] <- renderPrint({
    # input[['hover']]
    # if(!is.na(selected_month())) {
    #   return (
    #   )
    # }
    # selected_month()
    # if(!is.na(selected_month())){
    #   return (
    # if(!is.null(selected_month())){
      month <- selected_month()
      stations() %>% filter(Month == as.integer(month)) %>% arrange(desc(Share)) %>% select(Stacja = Station, Udział = Share)
    # }
    #     # %>% arrange(desc(Share))
    #   )
    # return (NULL)
    # }
  })
}

shinyApp(ui = ui, server = server)



