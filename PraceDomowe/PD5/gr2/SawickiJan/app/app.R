library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

source("./process.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App"),
  dashboardSidebar(sidebarMenu(
    collapsed = TRUE,
    id = "tabs",
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    )
  )),
  dashboardBody(tabItems(
    tabItem(
      "dashboard",
      infoBox(
        title = "Don't gamble.",
        subtitle = "Make milions becoming a data scientist instead.
        Click this and begin the journey!",
        value = NULL,
        icon = shiny::icon("exclamation-triangle"),
        color = "red",
        width = NULL,
        href = "https://github.com/mini-pw/WizualizacjaDanych2018",
        fill = FALSE
      ),
      div("Choose the season"),
      selectInput(
        inputId = "chosen_season",
        label = "Choose a season",
        choices = levels(as.factor(team_results$season))
      ),
      selectInput(
        inputId = "chosen_team",
        label = "Highlight a team",
        choices = levels(as.factor(team_results$team))
      ),
      actionButton(
        style = "font-size: 10px;",
        inputId = "sort_wins_button",
        label = "Sort-A-Z",
        icon = icon("sort-alpha-asc")
      ),
      fluidRow(
        column(5, plotOutput("wins_plot")),
        column(5, plotOutput("points_plot")),
        column(5, plotOutput("goals_plot")),
        column(5, plotOutput("max_goals_in_match_plot")),
        column(5, plotOutput("ties_plot"))
      )
    )
  ))
)

server <- function(session, input, output) {
  data_r <- reactive({
    team_results$selected = 0.5
    team_results[team_results$team == input[["chosen_team"]], ]$selected = 1
    
    team_results %>% arrange(wins) %>% filter(season == input[["chosen_season"]])
  })
  
  output[["wins_plot"]] <- renderPlot({
    ggplot(data = data_r(),
           aes(
             x = reorder(data_r()$team, data_r()$wins),
             y = data_r()$wins
           ),
           alpha = 0.5) +
      geom_text(
        aes(label = data_r()$wins),
        hjust = -0.1,
        color = "black",
        size = 4
      ) +
      geom_bar(stat = "identity",
               fill = "dodgerblue",
               alpha = data_r()$selected) + coord_flip() +
      xlab("Team") + ylab("wins") + ylim(c(0, max(data_r()$wins)+2)) +
      ggtitle("Wins in the season") + theme_minimal()
  })
  
  output[["points_plot"]] <- renderPlot({
    ggplot(data = data_r(), aes(x = reorder(data_r()$team, data_r()$points), y = data_r()$points)) +
      geom_text(
        aes(label = data_r()$points),
        hjust = -0.1,
        color = "black",
        size = 4
      ) +
      geom_bar(stat = "identity",
               fill = "dodgerblue",
               alpha = data_r()$selected) + coord_flip() +
      xlab("Team") + ylab("points") +
      ggtitle("Points acquired in the season") + theme_minimal()
  })
  
  output[["goals_plot"]] <- renderPlot({
    ggplot(data = data_r(), aes(x = reorder(data_r()$team, data_r()$goals), y = data_r()$goals)) +
      geom_text(
        aes(label = data_r()$goals),
        hjust = -0.1,
        color = "black",
        size = 4
      ) +
      geom_bar(stat = "identity",
               fill = "dodgerblue",
               alpha = data_r()$selected) + coord_flip() +
      xlab("Team") + ylab("goals") +
      ggtitle("Goals scroed in the season") + theme_minimal()
  })
  
  output[["max_goals_in_match_plot"]] <- renderPlot({
    ggplot(data = data_r(),
           aes(x = reorder(data_r()$team, data_r()$max_goals_in_match), y = data_r()$max_goals_in_match)) +
      geom_text(
        aes(label = data_r()$max_goals_in_match),
        hjust = -0.1,
        color = "black",
        size = 4
      ) +
      geom_bar(stat = "identity",
               fill = "dodgerblue",
               alpha = data_r()$selected) + coord_flip() +
      xlab("Team") + ylab("Max goals in one match") +
      ggtitle("Max goals in one match in the season") + theme_minimal()
  })
  
  output[["ties_plot"]] <- renderPlot({
    ggplot(data = data_r(),
           aes(x = reorder(data_r()$team, data_r()$ties), y = data_r()$ties)) +
      geom_text(
        aes(label = data_r()$wins),
        hjust = -0.1,
        color = "black",
        size = 4
      ) +
      geom_bar(stat = "identity",
               fill = "dodgerblue",
               alpha = data_r()$selected) + coord_flip() +
      xlab("Team") + ylab("Ties") + ylim(c(0, max(data_r()$ties)+2)) +
      ggtitle("Ties in the season") + theme_minimal()
  })
}

shinyApp(ui, server)
