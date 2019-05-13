# Source
# https://github.com/mini-pw/WizualizacjaDanych2018/pull/179/files?file-filters%5B%5D=.html#diff-457ed911f033e454c21423e68c848469

library(ggplot2)
library(dplyr)
library(shiny)


Dzielnice <- c('Bemowo', 'Białołęka', 'Bielany', 'Mokotów', 'Ochota', 'Praga-Południe', 'Praga-Północ', 'Rembertów', 'Śródmieście',  'Targówek', 'Ursus', 'Ursynów', 'Wawer', 'Wesoła', 'Wilanów', 'Włochy', 'Wola', 'Żoliborz')

LiczbaInterwencji <- c(177, 295, 623, 578, 532, 838, 564, 88, 1446, 431, 162, 195, 294, 55, 157, 185, 900, 400)

dane <- data.frame(Dzielnice, LiczbaInterwencji)
dane <- dane %>% mutate(Dzielnica = factor(Dzielnice,levels=rev(unique(Dzielnice))))



ui <- fluidPage(
  titlePanel("Liczba interwencji Straży Mijeskiej m.st. Warszawy w styczniu 2019 \r\nw podziale na dzielnice"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "dzielniceChb",
        label = "Select districts names:",
        choices = unique(dane[["Dzielnice"]]),
        selected = unique(dane[["Dzielnice"]])
      ),
      sliderInput(
        inputId = "slider",
        label = "Zakres liczby interwencji",
        min = min(dane$LiczbaInterwencji),
        max = max(dane$LiczbaInterwencji),
        step = 200,
        round = TRUE,
        value = c(min(dane$LiczbaInterwencji), max(dane$LiczbaInterwencji))
      )
    ),
    
    mainPanel(
      plotOutput("plot", height = 600, click = "click"),
      verbatimTextOutput("plot_value"),
      textOutput("selected_var")
    )
  )
)

server <- function(input, output) {
  Data_r <- reactive({
    filter(
      dane,
      Dzielnice %in% input[["dzielniceChb"]],
      LiczbaInterwencji >= input$slider[1],
      LiczbaInterwencji <= input$slider[2]
    )
  })
  
  output[["plot"]] <- renderPlot({
    ggplot(data=Data_r(), aes(x=Dzielnica, y=LiczbaInterwencji)) + 
      scale_x_discrete() +
      scale_y_continuous() + 
      geom_bar(stat='identity') + 
      ggtitle('Liczba interwencji Straży Mijeskiej m.st. Warszawy w styczniu 2019 \r\nw podziale na dzielnice') + 
      xlab('Dzielnice') +
      ylab('Liczba interwencji')  +
      coord_flip()
  })
}

shinyApp(ui = ui, server = server)