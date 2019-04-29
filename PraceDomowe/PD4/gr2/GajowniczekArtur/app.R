library(shiny)
library(SmarterPoland)
library(dplyr)
library(ggplot2)

district <- c('Żoliborz', 'Wola', 'Włochy', 'Wilanów', 'Wesoła', 'Wawer', 'Ursynów', 'Ursus', 'Targówek', 'Śródmieście', 'Rembertów', 'Praga-Północ', 'Praga-Południe', 'Ochota', 'Mokotów', 'Bielany', 'Białołęka', 'Bemowo')
n_interventions <- c(400, 900, 185, 157, 55, 294, 195, 162, 431, 1446, 88, 564, 838, 532, 578, 623, 295, 177)
df <- data.frame(district, n_interventions)
df <- df %>% mutate(district = factor(district, levels=unique(district)))

ui <- fluidPage(
  
  titlePanel("Artur Gajowniczek PD4"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "selected_districts",
        label = "Dzielnice:",
        choices = rev(unique(df[["district"]])),
        selected = rev(unique(df[["district"]]))
      ),
      
      actionButton("reset", label="Reset"),

      selectInput(
        inputId = "sort_order",
        label = "Sortuj według",
        choices = c("Liczba interwencji" = "n_interventions", "Alfabetycznie" = "district"),
      )
    ),
    mainPanel(
      plotOutput("interventions_plot", height = 600),
      verbatimTextOutput("plot_value") 
    )
  )
)

server <- function(input, output, session) {
  observe({
    if (input$reset > 0){
      updateCheckboxGroupInput(session=session, inputId="selected_districts", selected=unique(df[["district"]]))
    }
  })
  
  output[["interventions_plot"]] <- renderPlot({
    df %>%
      filter(district %in% input[["selected_districts"]]) %>%
      arrange(get(input[["sort_order"]])) %>%
      mutate(district = factor(district, levels=unique(district))) %>%
    
      ggplot(aes(x=district, y=n_interventions)) + 
        geom_bar(stat="identity") + 
        geom_text(aes(label=n_interventions), hjust = -0.5, size = 3, position = position_dodge(width = 1)) + 
        scale_x_discrete() +
        scale_y_continuous(expand = c(.1, .1)) + 
        ggtitle('Liczba interwencji Straży Miejskiej m.st. Warszawy w styczniu 2019 r.') + 
        xlab('Dzielnice') +
        ylab('Liczba interwencji')  +
        coord_flip() +
        theme_bw()
  })
}

shinyApp(ui = ui, server = server)

