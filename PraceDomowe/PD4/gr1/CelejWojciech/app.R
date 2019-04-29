library(ggplot2)
library(ggthemes)
library(ggimage)
library(extrafont)
library(RColorBrewer)
library(shiny)
library(dplyr)

Sys.setlocale("LC_ALL", "Polish")
options(encoding = "UTF-8")

Name <- c("Eisenbichler M.", "Geiger K.", "Peier K.", "Kobayashi R.", "Stoch K.", 
          "Kraft S.", "Forfang J.", "Johansson R.", "Freitag R.", "Zajc T.", 
          "Huber D.", "Kubacki D.", "Aschenwald P.", "Hayboeck M.", "Ammann S.", 
          "Prevc P.", "Kobayashi J.", "Klimov E.", "Zyla P.", "Ito D.", "Sato Y.", 
          "Koudelka R.", "Jelar Z.", "Fettner M.", "Polasek V.", "Schuler A.", 
          "Boyd-Clowes M.", "Learoyd J.", "Stjernen A.", "Zografski V.")

Nationality <- as.factor(c("GER", "GER", "SUI", "JPN", "POL", "AUT", "NOR", "NOR", "GER", 
                           "SLO", "AUT", "POL", "AUT", "AUT", "SUI", "SLO", "JPN", "RUS", 
                           "POL", "JPN", "JPN", "CZE", "SLO", "AUT", "CZE", "SUI", "CAN", 
                           "FRA", "NOR", "BUL"))

Dist1 <- c(131.5, 131.0, 131.0, 133.5, 128.5, 130.0, 132.5, 128.0, 125.5, 127.0, 126.0,
           128.5, 120.0, 122.0, 122.5, 123.5, 116.0, 126.5, 128.5, 119.0, 120.0, 120.5, 
           118.5, 117.5, 120.0, 117.5, 117.0, 116.5, 124.5, 117.0)

Dist2 <- c(135.5, 130.5, 129.5, 126.5, 129.5, 126.5, 125.5, 129.0, 129.5, 124.0, 125.5, 
           125.5, 128.0, 125.5, 126.0, 122.0, 132.0, 121.0, 121.0, 126.0, 124.0, 120.5, 
           121.0, 122.5, 117.5, 119.0, 118.5, 116.5, 102.0, 0)

Total <- c(279.4, 267.3, 266.1, 262.0, 259.4, 256.1, 250.9, 248.9, 248.7, 245.5, 242.0, 
           240.2, 239.9, 233.7, 230.6, 230.5, 230.0, 229.1, 228.7, 225.7, 221.4, 220.1, 
           219.8, 219.0, 218.2, 212.6, 212.1, 205.9, 185.5, 106.1)

MS2019_scores <- data.frame(Name, Nationality, Dist1, Dist2, Total)

Nationalities <- as.factor(c("GER", "SUI", "JPN", "POL", "AUT", "NOR", "SLO", "RUS", "CZE", "CAN", "FRA", "BUL"))

Images <- c("http://icons.iconarchive.com/icons/wikipedia/flags/128/DE-Germany-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/CH-Switzerland-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/JP-Japan-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/PL-Poland-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/AT-Austria-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/NO-Norway-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/SI-Slovenia-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/RU-Russia-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/CZ-Czech-Republic-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/CA-Canada-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/RE-Reunion-Flag-icon.png",
            "http://icons.iconarchive.com/icons/wikipedia/flags/128/BG-Bulgaria-Flag-icon.png"
)
Flags <- data.frame(Nationalities, Images)

data = merge(MS2019_scores, Flags, by.x = "Nationality", by.y = "Nationalities")

x_labels = list()
iter = 1
for (value in reorder(MS2019_scores$Name, Total)) {
  prefix = ifelse(iter <10, ".   ", ". ")
  x_labels[value] = strwrap(paste(iter, prefix ,value, sep = ""), 50)
  iter = iter + 1
}

# SHINY APP
ui <- fluidPage(
  
  titlePanel("Wyniki mistrzostw świata w skokach narciarskich 2019 \nkonkurs indywidualny na skoczni dużej"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_nationalities",
                         label = "Wybierz narodowość:",
                         choices = unique(data[["Nationality"]]),
                         selected = unique(data[["Nationality"]])),
      width = 2
    ),
    
    mainPanel(
      selectInput(inputId = "variable", 
                  label = "Wyświetl:", 
                  choices = c("Suma punktów", "Odległość po 1. serii", "Odległość po 2. serii")),
      plotOutput("plot", height = 700),
      width = 10
    ),
    position = "right"
  )

)

server <- function(input, output) {
  
  values <- reactiveValues(y_column = "Dist1", color = "dodgerblue4")
  
  observeEvent(input[["variable"]], {
    if(input[["variable"]] == "Suma punktów") {
      values[["y_column"]] <- "Total"
      values[["color"]] <- "dodgerblue4"
    }
    else if (input[["variable"]] == "Odległość po 1. serii") {
      values[["y_column"]]  <- "Dist1"
      values[["color"]] <- "orangered4"
    }
    else if (input[["variable"]] == "Odległość po 2. serii") {
      values[["y_column"]]  <- "Dist2"
      values[["color"]] <- "darkgoldenrod4"
    }
  })
  
  nationalities_r <- reactive({
    validate(
      need(input[["chosen_nationalities"]], "Wybierz przynajmniej jedną narodowość")
    )
    filter(data, Nationality %in% input[["chosen_nationalities"]]) 
  })
  
  output[["plot"]] <- renderPlot({
    p <- ggplot(data = nationalities_r(), aes(x = reorder(Name, Total))) +
      geom_bar(aes_string(y = values[["y_column"]]), stat = "identity", fill = values[["color"]]) +
      geom_text(aes_string(y = values[["y_column"]], label = values[["y_column"]]), 
                position=position_dodge(width=1), hjust = -0.3) +
      scale_x_discrete(labels = x_labels) +
      coord_flip() +
      scale_y_continuous(expand = expand_scale(add = c(0, 0)), limits = c(0, 320)) +
      labs(x="", y=input[["variable"]]) +
      theme_classic() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 12),
            axis.title.x = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 11),
            legend.background = element_rect(colour = "grey"),
            axis.text.y = element_text(hjust = 0)) + 
      geom_image(aes(y = 15, image = Images), size = 0.04)
    p
  })
}

shinyApp(ui = ui, server = server)
