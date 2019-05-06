library(shiny)
library(SmarterPoland)
library(dplyr)
library(patchwork)
library(formattable)
library(ggplot2)
library(dplyr)



countries1 <- c("Indie", "Malezja", "Indonezja", "Chiny", "Polska", "Niemcy", "Wielka Brytania", "USA", "Korea Poludniowa", "Francja", "Hiszpania", "Wlochy", "RPA")
values1 <- c(2.22, 1.84, 1.76, 1.45, 1.41, 0.41, 0.29, 0.21, 0, 0, 0, 0, 0)
values1 <- percent(values1)
dat1 <- data.frame(kraj = countries1, wzrost_pensji = values1) %>% 
  mutate(is_poland = kraj=='Polska')

countries2 <- c("Indie", "Malezja", "Indonezja", "Chiny", "Korea Poludniowa", "USA", "Niemcy", "Francja", "Wielka Brytania", "Hiszpania", "Wlochy", "RPA", "Polska")
values2 <- c(0, 0, 0, 0, 5500, 5000, 4600, 4000, 4000, 3700, 3500, 3500, 3000)
dat2 <- data.frame(kraj = countries2, pensja = values2) %>% 
  mutate(is_poland = kraj=='Polska')




ui <- fluidPage(
  
  titlePanel("Prognoza wynagrodzen na rok 2040"),
  p(
    "autor: Szymon Kowalik",
    br(),
    "autor oryginalnego wykresu: Piotr Olesiejuk"
  ),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput(inputId = "showEmpty",
                    label = "Pokaz brakujace dane na wykresie",
                    value = TRUE
      ),
      conditionalPanel(
        condition = "input.showEmpty == true",
        radioButtons(inputId = "sort",
                     label = "Sortowanie:",
                     choices = c("Lewy wykres", "Prawy wykres")
        )
      )
    ),
    
    mainPanel(
      plotOutput("salary_plot", height = 600)
    )
  )
)

server <- function(input, output) {
  
  dat1_r <- reactive({
    if(input[["showEmpty"]]) {
        dat1
      } else {
        dat1 %>% 
          filter(wzrost_pensji > 0)
      }
  })
  
  dat2_r <- reactive({
    if(input[["showEmpty"]]) {
      dat2
    } else {
      dat2 %>% 
        filter(pensja > 0)
    }
  })
  
  kraj_order1_r <- reactive({
    if(!input[["showEmpty"]] || input[["sort"]] == 'Lewy wykres'){
      dat1_r() %>% 
        arrange(wzrost_pensji) %>% 
        pull(kraj)  
    } else {
      dat2_r() %>% 
        arrange(pensja) %>% 
        pull(kraj)  
    } 
  })
  
  kraj_order2_r <- reactive({
    if(!input[["showEmpty"]] || input[["sort"]] == 'Prawy wykres'){
      dat2_r() %>% 
        arrange(pensja) %>% 
        pull(kraj)  
    } else {
      dat1_r() %>% 
        arrange(wzrost_pensji) %>% 
        pull(kraj)  
    } 
  })
  
  output[["salary_plot"]] <- renderPlot({
    
    p1 <- ggplot(data = dat1_r(), aes(x = kraj, y = wzrost_pensji, fill=is_poland)) +
      scale_x_discrete(limits = kraj_order1_r()) +
      geom_bar(stat = "identity") +
      geom_text(aes(y=wzrost_pensji, label=ifelse(wzrost_pensji>0, '', 'brak danych'), hjust=0)) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 2.5)) +
      coord_flip( )+
      theme(legend.position = "none") +
      xlab("") + ylab("") +
      ggtitle("Przyrost srednich wynagrodzen w 2040 roku\nw stosunku do obecnego")
    
    p1 <- p1 + theme(plot.title = element_text(size = 14, hjust = 0.5))
    
    
    p2 <- ggplot(data = dat2_r(), aes(x = kraj, y = pensja, fill=is_poland)) +
      scale_x_discrete(limits = kraj_order2_r()) +
      scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 6000)) +
      geom_bar(stat = "identity") +
      geom_text(aes(y=pensja, label=ifelse(pensja>0, '', 'brak danych'), hjust=0)) +
      coord_flip() +
      theme(legend.position = "none") +
      xlab("") + ylab("") +
      ggtitle("Srednie wynagrodzenia w 2040 roku")
    
    p2 <- p2 + theme(plot.title = element_text(size = 14, hjust = 0.5))
    
    p_final <- p1 + p2 
    p_final
  })
}

shinyApp(ui = ui, server = server)
