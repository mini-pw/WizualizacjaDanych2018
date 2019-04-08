# Na podstawie danych ChickWeight stwórz aplikację shiny, która wyświetla zależność wagi kurczaka (oś Y) od czasu (oś X).
# Wykorzystaj sliderInput do wyboru przedziału czasu i checkboxGroupInput do wyboru diety.

library("shiny")
# library("dplyr")
library("data.table")

ui <- fluidPage(
    
    titlePanel("Simple Shiny Chicken App"),
    
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "chosen_diet",
                               label = "Select diet type:",
                               choices = unique(ChickWeight[["Diet"]]),
                               selected = unique(ChickWeight[["Diet"]]))
        ),
        
        mainPanel(
            h2("Chick-o-plot"),
            plotOutput("chicken_plot", height = 600),
            sliderInput(
                "chicken_brush", "Time:",
                min = min(ChickWeight[["Time"]]),
                max = max(ChickWeight[["Time"]]),
                value = c(
                    min(ChickWeight[["Time"]]),
                    max(ChickWeight[["Time"]]))
                )
        )
    )
)

server <- function(input, output) {
    
    chicks_c <- reactive({
        data.table(ChickWeight)[Diet %in% input[["chosen_diet"]]]
    })
    
    chicks_cs <- reactive({
        chicks_c()[
            between(
                Time,
                min(input[["chicken_brush"]]),
                max(input[["chicken_brush"]])
            )
        ]
    })
    
    output[["chicken_plot"]] <- renderPlot({
        chick_data <- chicks_cs()
        (
            ggplot(chick_data, aes(x = Time, y = weight, fill = Chick, color = Diet)) +
                
                # Punkty poszczególnych kurczaków
                geom_point() + 
                
                # Linie poszczególnych kurczaków
                geom_line(size = 0.15) + 
                
                # Średnia waga kurczaków w czasie i odchylenie
                geom_smooth(
                    inherit.aes = FALSE, se = TRUE,
                    data = chick_data[, .(avg = mean(weight)), by = .(Diet, Time)],
                    aes(x = Time, y = avg, color = Diet)
                ) +
                theme_classic() +
                
                # Zakresy osi i przesunięcie początku wykresu do (0, 0)
                scale_x_continuous(expand = c(0, 0), limits = c(0, 1.01*max(ChickWeight[["Time"]]))) +
                scale_y_continuous(expand = c(0, 0), limits = c(0, 1.01*max(ChickWeight[["weight"]]))) +
                
                # Schowanie legendy kolorów
                scale_fill_discrete(guide = FALSE) +
                
                # Opis wykresu
                labs(x = "Time", y = "Weight", color = "Diet type") +
                
                #
                theme(text = element_text(size = 15))
        )
    })
}

shinyApp(ui = ui, server = server)
