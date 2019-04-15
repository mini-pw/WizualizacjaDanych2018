library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

ui <- fluidPage(
  
  titlePanel('Liczba interwencji Strazy Miejskiej m.st. Warszawy  w styczniu 2019r. w podziale na dzielnice'),
  sidebarLayout(
    sidebarPanel( h4('Podsumowanie'), tableOutput('statistics')),
    mainPanel(
      plotOutput('plot'),
      
      hr(),
      
      fluidRow(
        
        column(6,
               wellPanel(
                 h4('Uporzadkowanie'),
                 checkboxInput('descending', 'Malejaco', FALSE),
                 br(),
                 selectInput('color', 'Kolor', c('Czerwony', 'Zielony', 'Niebieski', 'Kolorowy'))
               )
        ),
        column(6,
               wellPanel(
                 sliderInput("slider", "Ilosc interwencji",  
                             min = 1, max = 1446, value = c(1, 1446))
               )       
        )
      )
    ), "right")
)

server <- function(input, output) {
  
  
  #colorsValues() <- reactive({ c("Czerwony" = "red",  "Zielony" = "green", "Niebieski" = "blue") })
  output$statistics <- renderTable({
    
    Dzielnice <- c('Bemowo', 'Bialoleka', 'Bielany', 'Mokotow', 'Ochota', 'Praga-Poludnie', 'Praga-Polnoc', 'Rembertow', 'Srodmiescie',  'Targowek', 'Ursus', 'Ursynow', 'Wawer', 'Wesola', 'Wilanow', 'Wlochy', 'Wola', 'Zoliborz')
    LiczbaInterwencji <- c(177, 295, 623, 578, 532, 838, 564, 88, 1446, 431, 162, 195, 294, 55, 157, 185, 900, 400)
    dane <- data.frame(Dzielnice, LiczbaInterwencji)
    dane <- dane %>% mutate(Dzielnica = factor(Dzielnice,levels=rev(unique(Dzielnice))))
    dane <- dane %>% mutate(isPloted = (LiczbaInterwencji >= input$slider[1] & LiczbaInterwencji <= input$slider[2]))
    #rowser()
    if (input$descending == FALSE)
    {
      permut <- order(dane$LiczbaInterwencji, decreasing = FALSE)
    }
    else
    {
      permut <- order(dane$LiczbaInterwencji, decreasing = TRUE)
    }
    dane$Dzielnica <- factor(dane$Dzielnica, levels = dane$Dzielnica[permut])
    d <- dane[dane$isPloted == TRUE, ]
    #browser()
    sr <- mean(d$LiczbaInterwencji)
    med <- median(d$LiczbaInterwencji)
    max <- max(d$LiczbaInterwencji)
    min <- min(d$LiczbaInterwencji)
    Statystyka <- c('Srednia', 'Mediana', 
                    paste0('Maksimum (', d$Dzielnica[which(d$LiczbaInterwencji == max)], ')'),
                    paste0('Minimum (', d$Dzielnica[which(d$LiczbaInterwencji == min)], ')'))
    IloscInterwencji <- c(sr, med, max, min)
    result <- data.frame(Statystyka, IloscInterwencji)
    colnames(result)[2] <- 'Ilosc Interwencji'
    result
  })
  
  output$plot <- renderPlot({
    
    Dzielnice <- c('Bemowo', 'Bialoleka', 'Bielany', 'Mokotow', 'Ochota', 'Praga-Poludnie', 'Praga-Polnoc', 'Rembertow', 'Srodmiescie',  'Targowek', 'Ursus', 'Ursynow', 'Wawer', 'Wesola', 'Wilanow', 'Wlochy', 'Wola', 'Zoliborz')
    LiczbaInterwencji <- c(177, 295, 623, 578, 532, 838, 564, 88, 1446, 431, 162, 195, 294, 55, 157, 185, 900, 400)
    dane <- data.frame(Dzielnice, LiczbaInterwencji)
    dane <- dane %>% mutate(Dzielnica = factor(Dzielnice,levels=rev(unique(Dzielnice))))
    dane <- dane %>% mutate(isPloted = (LiczbaInterwencji >= input$slider[1] & LiczbaInterwencji <= input$slider[2]))
    #rowser()
    if (input$descending == FALSE)
    {
      permut <- order(dane$LiczbaInterwencji, decreasing = FALSE)
    }
    else
    {
      permut <- order(dane$LiczbaInterwencji, decreasing = TRUE)
    }
    dane$Dzielnica <- factor(dane$Dzielnica, levels = dane$Dzielnica[permut])
    n <- length(dane$Dzielnice[dane$isPloted == TRUE])
    colors <- list("Czerwony" = rep("#de2d26", n),
                   "Zielony" = rep("#2ca25f", n),
                   "Niebieski" = rep("#2b8cbe", n), 
                   "Kolorowy" = colorRampPalette(brewer.pal(9, "Set1"))(n))
    #browser()
    ggplot(data=dane[which(dane$isPloted == TRUE),], aes(x=Dzielnica, y=LiczbaInterwencji, fill = Dzielnice)) + 
      scale_x_discrete() +
      scale_y_continuous() +
      geom_bar(stat='identity') +
      scale_fill_manual(values = colors[[input$color]]) +
      theme_minimal()+
      xlab('Dzielnice') +
      ylab('Liczba interwencji')+
      ylim(c(0, 1600))+
      theme(legend.position = 'None')+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13, lineheight = 1),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14)) +
      geom_text(aes(label = LiczbaInterwencji, y = LiczbaInterwencji), size = 5, vjust = -1)
    
  })
}

shinyApp(ui = ui, server = server)