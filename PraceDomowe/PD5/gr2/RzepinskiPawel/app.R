library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(DT)
library(data.table)
library(bbplot)

source("data-processing.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput(
        inputId = "chosen_team",
        "Select team", 
        choices = sort(unique(dat$home_team))
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(box(title = "Overall performance", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           valueBoxOutput("current_wins", width = 4),
                           valueBoxOutput("current_draws", width = 4),
                           valueBoxOutput("current_loses", width = 4),
                           valueBoxOutput("performance_change", width = 12),
                           width = 3),
                       box(title = "Average points for selected team against", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("points_against_plot"), width = 9)),
              fluidRow(box(title = "Average goals for selected team against", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("goals_against_plot"), width = 12)),
              fluidRow(box(title = "Win rate for all teams", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           plotOutput("win_rate_plot"), width = 12)),
              fluidRow(box(title = "All results", status = "primary",
                           solidHeader = TRUE, collapsible = TRUE, 
                           dataTableOutput("results_listing"), width = 12)
              )
      )
    )
  )
)

server <- function(input, output) {
  goals_against_dat <- reactive({
    rbind(filter(dat, home_team == input[["chosen_team"]]) %>% 
            select(opponent = away_team, goals = home_team_goal),
          filter(dat, away_team == input[["chosen_team"]]) %>% 
            select(opponent = home_team, goals = away_team_goal)) %>% 
      group_by(opponent) %>% 
      summarise(mean_goals = mean(goals))
  })
  
  output[["goals_against_plot"]] <- renderPlot({
    team_order <- goals_against_dat() %>% 
      arrange(desc(mean_goals)) %>% 
      pull(opponent) %>% 
      as.character()
    
    mutate(goals_against_dat(), opponent = factor(opponent, levels = team_order)) %>% 
      ggplot(aes(x = opponent, y = mean_goals)) +
      geom_col() +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
      ylab("Average goals scored") + 
      xlab("Opponent") + 
      bbc_style()
  })
  
  points_against_dat <- reactive({
    rbind(filter(point_dat, home_team == input[["chosen_team"]]) %>% 
            select(opponent = away_team, points = home_team_points),
          filter(point_dat, away_team == input[["chosen_team"]]) %>% 
            select(opponent = home_team, points = away_team_points)) %>% 
      group_by(opponent) %>% 
      summarise(mean_points = mean(points))
  })
  
  output[["points_against_plot"]] <- renderPlot({
    team_order <- points_against_dat() %>% 
      arrange(desc(mean_points)) %>% 
      pull(opponent) %>% 
      as.character()
    
    mutate(points_against_dat(), opponent = factor(opponent, levels = team_order)) %>% 
      ggplot(aes(x = opponent, y = mean_points)) +
      geom_col() +
      scale_y_continuous(limits = c(0, 3)) +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
      ylab("Average points scored") + 
      xlab("Opponent") +
      bbc_style()
  })
  
  output[["win_rate_plot"]] <- renderPlot({
    team_order2015 <- filter(win_perc_dat, season == "2015/2016") %>% 
      arrange(desc(win_perc)) %>% 
      pull(team) %>% 
      as.character()
    
    
    mutate(win_perc_dat, team = factor(team, levels = team_order2015)) %>% 
      ggplot(aes(x = team, y = win_perc, fill = season)) +
      geom_col(position = "dodge") +
      theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
      ylab("Average win percentage") + 
      xlab("Opponent") +
      bbc_style()
  })
  
  output[["results_listing"]] = DT::renderDataTable({
    dat %>% 
      mutate(result = paste(home_team_goal, away_team_goal, sep = "-"), 
             date = as.character(as.Date(date, format = "%Y-%m-%d %H:%M:%S"))) %>% 
      select(c("season", "date", "home_team", "away_team", "result"))
  })
  
  output[["performance_change"]] <- renderValueBox({
    prev <- win_perc_dat %>% filter(team == input[["chosen_team"]] & season == "2014/2015") %>% pull(win_perc)
    curr <- win_perc_dat %>% filter(team == input[["chosen_team"]] & season == "2015/2016") %>% pull(win_perc)
    if (curr > prev) {
      valueBox("On the rise!", "Seasonal form", color = "green")
    }
    else
    {
      valueBox("Falling down!", "Seasonal form", color = "red")
    }
  })
  
  output[["current_wins"]] <- renderValueBox({
    data <- point_dat %>% 
      filter((home_team == input[["chosen_team"]] & home_team_points == 3) | 
               (away_team == input[["chosen_team"]] & away_team_points == 3)) %>% 
      count()
    valueBox(data, "Wins", color = "green")
  })
  
  output[["current_draws"]] <- renderValueBox({
    data <- point_dat %>% 
      filter((home_team == input[["chosen_team"]] & home_team_points == 1) | 
               (away_team == input[["chosen_team"]] & away_team_points == 1)) %>% 
      count()
    valueBox(data, "Draw", color = "yellow")
  })
  
  output[["current_loses"]] <- renderValueBox({
    data <- point_dat %>% 
      filter((home_team == input[["chosen_team"]] & home_team_points == 0) | 
               (away_team == input[["chosen_team"]] & away_team_points == 0)) %>% 
      count()
    valueBox(data, "Loses", color = "red")
  })
}

shinyApp(ui, server)
