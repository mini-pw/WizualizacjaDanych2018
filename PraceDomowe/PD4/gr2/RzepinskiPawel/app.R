source("data.R")

library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

ui <- fluidPage(
  
  titlePanel("PŚ Inssbruck, Seefeld 2019"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_nationality",
                         label = "Wybierz narodowość:",
                         choices = unique(df[["Nationality"]]),
                         selected = unique(df[["Nationality"]])),
      radioButtons(inputId = "chosen_series",
                         label = "Wybierz serię:",
                         choiceNames = c("Seria 1", "Seria 2", "Sumarycznie"),
                         choiceValues = c("Dist1", "Dist2", "Total"))
    ),
    mainPanel(
      h2("Dystans"),
      plotOutput("countries_plot", height = 700)
    )
  )
)

server <- function(input, output) {
  
  df_filter <- reactive({
    filter(na.omit(df), Nationality %in% input[["chosen_nationality"]]) %>%
      mutate_(Total = input[["chosen_series"]])
  })
  
  output[["countries_plot"]] <- renderPlot({
    ggplot(df_filter(), aes(x = reorder(Name, Total), y = Total, fill = Nationality)) +
      geom_bar(stat = "identity") + coord_flip() + ylim(0, 300) +
      scale_fill_brewer(name = "Narodowość", type = "div") +
      xlab("") + 
      ylab("Dystans") +
      theme_bw() +
      theme(axis.text = element_text(size = 12, face = "bold"))
  })
  
}

shinyApp(ui = ui, server = server)