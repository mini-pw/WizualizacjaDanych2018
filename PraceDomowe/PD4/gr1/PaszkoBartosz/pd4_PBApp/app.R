library(shiny)
library(SmarterPoland)
library(dplyr)

procenty_ <- c(29,20,25,11,0,15, 37,13,6,3,0,41,
              55,26,5,9,1,4, 39, 31, 6, 1, 5, 18)
zrodla_ <- c("Podatki które ja płacę", "Podatki które płacą inni", "Pieniądze rządowe","Podatki płacone przez firmy", "Inne", "Nie wiem")
partie_ <- factor(c("PiS", "Kukiz15", "PO", "Nowoczesna"))
colors_ <- hcl(h = seq(15, 375, length = 6 + 1), l = 65, c = 100)[1:6]

df <- tibble(procenty=procenty_, zrodla=factor(rep(zrodla_, 4), levels=zrodla_),
             partie=rep(partie_, each=6))



ui <- fluidPage(
  titlePanel("Praca Domowa 4"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "party_input",
                  label = "Wybierz partię:",
                  choices = unique(df$partie),
                  selected = unique(df$partie)
      ),
      selectInput(inputId = 'source_input',
               label = 'Wybierz źródło:',
               choices = c(levels(df$zrodla), 'Wszystkie'),
               selected = 'Wszystkie'
      )
    ),
    
    mainPanel(
      h2("Plot"),
      plotOutput("party_plot", height = 600, click = 'p')
    )
  )
)

server <- function(input, output) {
  selected_party <- reactive({
    data <- df %>% filter(partie %in% input[['party_input']])
    if (input[['source_input']] != 'Wszystkie'){
      data <- data %>% filter(zrodla == input[['source_input']])
      data$zrodla <- data$zrodla %>% droplevels()
    }
    data$partie <- data$partie %>% droplevels()
    data
  })
  
  
  output[["party_plot"]] <- renderPlot({
    data <- selected_party()
    n_parties <- length(unique(data$partie))
    bars_colors <- colors_
    
    p <- data %>% ggplot(aes(x=partie, y=procenty, fill=zrodla)) +
      xlab('Partie polityczne') + ylab('Procent odpowiedzi')
    
    if (input[['source_input']] != 'Wszystkie') {
      color_idx <- match(input[['source_input']], levels(df$zrodla))
      bars_colors <- colors_[color_idx]
    }
    
    if (nrow(data) > 0) {
      p <- p + geom_bar(stat='identity', position='dodge', width=0.8) + 
      ggtitle("Jakie Twoim zdaniem jest główne źródło pieniędzy na\nwypłaty w ramach programu 500 plus?") +
      scale_x_discrete(limits = partie_[partie_ %in% data$partie]) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      scale_fill_manual(values = bars_colors) +
      labs(fill = 'Źródła') +
      theme(plot.title = element_text(hjust = 0.5))
    }
    
    if (n_parties > 1){
      p <- p + geom_vline(xintercept = seq(from=1.5, to=n_parties-0.5, 1), linetype='dashed')
    }
    
    p
  })
  
}

shinyApp(ui = ui, server = server)
