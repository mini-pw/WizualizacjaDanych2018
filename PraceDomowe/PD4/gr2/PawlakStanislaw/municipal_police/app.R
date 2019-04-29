# link: https://stapaw.shinyapps.io/municipal_police/

library(shiny)
library(dplyr)
library(SmarterPoland)
library(ggplot2)


districts <- c(
  'Bemowo',
  'Bialoleka',
  'Bielany',
  'Mokotów',
  'Ochota',
  'Praga-Poludnie',
  'Praga-Pólnoc',
  'Rembertów',
  'Sródmiescie',
  'Targówek',
  'Ursus',
  'Ursynów',
  'Wawer',
  'Wesola',
  'Wilanów',
  'Wlochy',
  'Wola',
  'Żoliborz'
)

numberOfInterventions <- c(177,
                           295,
                           623,
                           578,
                           532,
                           838,
                           564,
                           88,
                           1446,
                           431,
                           162,
                           195,
                           294,
                           55,
                           157,
                           185,
                           900,
                           400)

districtData <- data.frame(districts, numberOfInterventions)

order_districts <- group_by(districtData, districts) %>%
  arrange(desc(numberOfInterventions)) %>%
  pull((districts))

order_values <- group_by(districtData, districts) %>%
  arrange(desc(numberOfInterventions)) %>%
  pull((numberOfInterventions))

districtData <- districtData %>%
  mutate(districts = factor(order_districts, levels = rev(order_districts))) %>%
  mutate(numberOfInterventions = order_values)



ui <- fluidPage(titlePanel("Zadanie Domowe 4"),
                
                sidebarLayout(
                  mainPanel(
                    h2(
                      "Liczba interwencji Straży Miejskiej m.st. Warszawy w styczniu 2019 \r\nw podziale na dzielnice"
                    ),
                    plotOutput("municipal_plot", height = 600)
                  ),
                  
                  sidebarPanel(
                    h2("Podsumowanie"),
                    tableOutput("summing_table"),
                    sliderInput(
                      inputId = "intervention_number",
                      label = h3("Wybierz przedzial interwencji na dzielnice"),
                      min = min(numberOfInterventions),
                      max = max(numberOfInterventions),
                      value = c(min(numberOfInterventions), max(numberOfInterventions))
                    ),
                    checkboxGroupInput(
                      inputId = "chosen_districts",
                      label = "Wybierz uwzgledniane dzielnice",
                      choices = unique(districts),
                      selected = unique(districts)
                    )
                  )
                  
                  
                ))


server <- function(input, output) {
  districtData_r <- reactive({
    filter(
      districtData,
      districts %in% input[["chosen_districts"]] &
        numberOfInterventions %in% seq(input[["intervention_number"]][1],
                                       input[["intervention_number"]][2])
    )
  })
  
  districts_sum_up <- reactive({
    nazwa <- c(
      "Liczba dzielnic",
      "Sumaryczna liczba interwencji",
      "Średnia liczba interwencji na dzielnice"
    )
    wartosc <- c(
      length(districtData_r()$numberOfInterventions),
      sum(districtData_r()$numberOfInterventions),
      floor(sum(districtData_r()$numberOfInterventions)/length(districtData_r()$numberOfInterventions)))
    data.frame(nazwa, wartosc)
  })
  
  output[["municipal_plot"]] <- renderPlot({
    validate(need(input[["intervention_number"]][1] != input[["intervention_number"]][2], "Select wider range."))
    ggplot(data = districtData_r(),
           aes(x = districts, y = numberOfInterventions, fill = "FF####")) +
      scale_x_discrete() +
      scale_y_continuous() +
      geom_bar(stat = 'identity') +
      xlab('Dzielnice') +
      ylab('Liczba interwencji')  +
      coord_flip() +
      geom_text(label = districtData_r()$numberOfInterventions,
                hjust = 1.2) +
      theme(legend.position = "none")
  })
  
  output[["summing_table"]] <- renderTable({
    validate(need(input[["chosen_districts"]], "Select at least one district"))
    districts_sum_up()
    
  })
  
}

shinyApp(ui = ui, server = server)
