library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggrepel)
library(DT)

jornada <- seq(10, 24)
barcelona <- c(21, 24, 24, 25, 28, 31, 34, 37, 40, 43, 46, 49, 50, 51, 54)
atletico <- c(19, 20, 23, 24, 25, 28, 31, 34, 35, 38, 41, 44, 44, 44, 47)
real <- c(14, 17, 20, 20, 23, 26, 29, 30, 30, 33, 36, 39, 42, 45, 45)

puntos.data <- data.frame(jornada, barcelona, atletico, real)
loss <- apply(puntos.data, 1, max)
puntos.data <- cbind(puntos.data, loss)

teams <- c("barcelona", "atletico", "real")
full_teams <- c("FC Barcelona", "Atletico Madrid", "Real Madrid")

puntos.long <- melt(puntos.data, id = "jornada", measure = teams) %>% 
  mutate(variable = case_when(variable == teams[1] ~ full_teams[1],
                              variable == teams[2] ~ full_teams[2],
                              variable == teams[3] ~ full_teams[3],
                              TRUE ~ ''),
         loss = loss - value)

ui <- fluidPage(
  
  titlePanel("La Liga top teams score"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_team",
                         label = "Select team:",
                         choices = unique(full_teams),
                         selected = unique(full_teams))
    ),
    
    mainPanel(
      h2("Points for top teams in La Liga"),
      plotOutput("laliga_plot", height = 600, click = "team_click"),
      h2(textOutput("team_title")),
      dataTableOutput("team_table")
      )
    )
)

server <- function(input, output) {
  
  selected <- reactiveValues(
    match_day = '',
    team = ''
  )
  
  observeEvent(input[["team_click"]], {
    near_points <- nearPoints(puntos.long, input[["team_click"]], 
               maxpoints = 1)
    selected[["match_day"]] <- ifelse(nrow(near_points) > 0, near_points[["jornada"]], "")
    selected[["team"]] <- ifelse(nrow(near_points) > 0, near_points[["variable"]], "")
  })
  
  output[["laliga_plot"]] <- renderPlot({
    teams <- puntos.long %>% filter(variable %in% input[["chosen_team"]])
    score <- teams %>%
      mutate(selected = case_when(selected[["team"]] == variable & selected[["match_day"]] == jornada ~ 1,
                                  TRUE ~ 0))
    p <- ggplot(data = teams, aes(jornada, value, colour = variable, label = value)) +
      geom_line(size=0.3) +
      guides(size = FALSE) +
      geom_point(data=score, aes(jornada, value, colour = variable, label = value, size = as.factor(selected))) +
      scale_x_continuous(breaks = seq(10, 24, by=1), limits=c(10,24)) +
      scale_y_continuous(breaks = seq(0, 60, by=10), limits=c(0,60)) +
      labs(x="Match day", y="Points", color="Team") +
      scale_size_manual(values = c(2, 7)) +
      theme_bw()
    
    p
  })
  
  output[["team_title"]] <- renderText({
    validate(
      need(selected[["team"]], "")
    )
    paste(c("Score statistics for", selected[["team"]]), " ") 
  })
  
  output[["team_table"]] <- renderDataTable({
    validate(
      need(selected[["team"]], "Select team")
    )
    team <- puntos.long %>% filter(variable == selected[["team"]]) %>% select(-c("variable"))
    datatable(
      team, 
      rownames = FALSE,
      colnames = c("Match day", "Points", "Loss to first place"),
      options = list(pageLength = nrow(team), autoWidth = TRUE)
    ) %>% formatStyle("jornada",backgroundColor=styleEqual(selected[["match_day"]], "gray"), target = "row")
      
  })
  
}

shinyApp(ui = ui, server = server)
