library(shiny)
library(SmarterPoland)
library(dplyr)
library(patchwork)
library(formattable)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Praca domowa 4"),
  h2("Prognoza wynagrodzen na rok 2040"),
  radioButtons("plotPick", "rodzaj wykresu",
               c("Przyrost wynagrodzeń", "Średnie wynagrodzenia", "oba"), selected = "Średnie wynagrodzenia"),
  plotOutput("countries_plot", height = 600, click = "clickbar"),
  verbatimTextOutput("value")
  
)

server <- function(input, output) {

  countries1 <- c("Indie", "Malezja", "Indonezja", "Chiny", "Polska", "Niemcy", "Wielka Brytania", "USA", "Korea Południowa", "Francja", "Hiszpania", "Włochy", "RPA")
  values1 <- c(2.22, 1.84, 1.76, 1.45, 1.41, 0.41, 0.29, 0.21, 0, 0, 0, 0, 0)
  values1 <- percent(values1)
  dat1 <- data.frame(kraj = countries1, wzrost_pensji = values1) %>% 
    arrange(wzrost_pensji) %>% 
    mutate(is_poland = kraj=='Polska')
  kraj_order <- dat1 %>% pull(kraj)
  
    countries2 <- c("Indie", "Malezja", "Indonezja", "Chiny", "Korea Południowa", "USA", "Niemcy", "Francja", "Wielka Brytania", "Hiszpania", "Włochy", "RPA", "Polska")
  values2 <- c(0, 0, 0, 0, 5500, 5000, 4600, 4000, 4000, 3700, 3500, 3500, 3000)
  dat2 <- data.frame(kraj = countries2, pensja = values2) %>% 
    mutate(is_poland = kraj=='Polska')
  
  p1 <- ggplot(data = dat1, aes(x = kraj, y = wzrost_pensji, fill = is_poland)) +
    scale_x_discrete(limits = kraj_order) +
    geom_bar(stat = "identity") +
    geom_text(aes(y=wzrost_pensji, label=ifelse(wzrost_pensji>0, '', 'brak danych'), hjust=0)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 2.5)) +
    coord_flip( )+
    theme(legend.position = "none") +
    xlab("") + ylab("") +
    ggtitle("Przyrost średnich wynagrodzeń w 2040 roku\nw stosunku do obecnego")
  
  p1 <- p1 + theme(plot.title = element_text(size = 14, hjust = 0.5))
  
  p1g <- ggplot_build(p1)
  
  p2 <- ggplot(data = dat2, aes(x = kraj, y = pensja, fill=is_poland)) +
    scale_x_discrete(limits = kraj_order) +
    scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 6000)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y=pensja, label=ifelse(pensja>0, '', 'brak danych'), hjust=0)) +
    coord_flip() +
    theme(legend.position = "none") +
    xlab("") + ylab("") +
    ggtitle("Średnie wynagrodzenia w 2040 roku")
  
  
  p2 <- p2 + theme(plot.title = element_text(size = 14, hjust = 0.5))
  
  p2g <- ggplot_build(p2)
  dane2fix <- tibble(kraj = p1g$plot$data$kraj)
  dane2 <- tibble(kraj = p2g$plot$data$kraj, pensja = p2g$plot$data$pensja)
  dane2fix <- left_join(dane2fix, dane2, by = "kraj")
  
  colors <- rep("red", 13)
  
   picked <- reactiveValues(
    selected = numeric()
  )
   
   clicked <- reactiveValues(
     selected = numeric()
   )
  
  observeEvent(input$plotPick, {
    if (input$plotPick == "Średnie wynagrodzenia"){
      picked[["selected"]] <- 2
    }
    else if (input$plotPick == "Przyrost wynagrodzeń"){
      picked[["selected"]] <- 1
    }
    else{
      picked[["selected"]] <- 3
    }
  })
  
  observeEvent(input$clickbar, {
    clicked[["selected"]] <- round(input$clickbar$y)
  })
  
  output[["countries_plot"]] <- renderPlot({
    if(picked[["selected"]] == 1){
      p1
      }
    
    else if(picked[["selected"]] == 2){
      p2
    }
    
    else{
      p1 + p2
    }
    
    })
    
  output[["value"]] <- renderPrint({
    validate(
      need(clicked[["selected"]], "Click at one of the bars for exact reading")
    )
    if (picked[["selected"]] == 2){
      if(dane2fix$pensja[clicked[["selected"]]] == 0) 
        paste("brak danych")
      else{
      paste(dane2fix$kraj[clicked[["selected"]]], "będą miały średnie wynagrodzenia na poziomie",  dane2fix$pensja[clicked[["selected"]]], "$")
      }
    }
    else if (picked[["selected"]] == 1){
      if(p1g$plot$data$wzrost_pensji[clicked[["selected"]]] == 0){
        paste("brak danych")
      }
      else{
      paste(p1g$plot$data$kraj[clicked[["selected"]]], "odnotują przyrost w wysokości", p1g$plot$data$wzrost_pensji[clicked[["selected"]]], "%")
      }
    }
  })
    
}

shinyApp(ui = ui, server = server)


#do poprawy wyświetlane wartości, i wykres jest przetasowany. wyciagnac wartosci i kraje z wykresu ggbuilda
