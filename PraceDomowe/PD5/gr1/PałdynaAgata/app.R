library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(DT)
library(data.table)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new", badgeColor = "green"),
      menuItem("See also", icon = icon("send", lib = 'glyphicon'), href = "https://github.com/mini-pw/WizualizacjaDanych2018/"),
      radioButtons(inputId = "selectedSeason", label = "Select season", choices = unique(dat$season)),
      selectInput(inputId = "selectedTeam", label = "Select team", choices = sort(unique(dat$home_team)))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              fluidRow(box(title = "Points scored by all teams in selected season", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("points_plot"), width = 6),
                       box(title = "Goals scored/lost by all teams in selected season", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("goals_plot"), width = 6)),
              fluidRow(box(title = "Team's points during selected season", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("team_points_plot"), width = 6),
                       box(title = "Team's goals during selected season", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("team_goals_plot"), width = 6)),
              fluidRow(box(title = "Team's matches during selected season - plot", status = "primary", solidHeader = TRUE, collapsible = TRUE, plotOutput("team_matches_plot"), width = 4),
                       box(title = "Team's matches during selected season - table", status = "primary", solidHeader = TRUE, collapsible = TRUE, dataTableOutput("team_matches_table", height = "100%"), width = 8))
      ),
      tabItem("about",
              "Shiny dashboard application for WD course"
      )
    )
  )
)

server <- function(input, output) {
  
  dat_season <- reactive({
    validate(need(input[["selectedSeason"]], "Select season"))
    dat %>% filter(season == input[["selectedSeason"]])
  })
  
  dat_teams <- reactive({
    dat_home_team <- dat_season() %>% 
      select(stage, goals_scored = home_team_goal, goals_lost = away_team_goal, team = home_team)
    
    dat_away_team <- dat_season() %>% 
      select(stage, goals_scored = away_team_goal, goals_lost = home_team_goal, team = away_team)
    
    rbind(dat_home_team, dat_away_team) 
  })
  
  teams_points <- reactive(
    dat_teams() %>% 
      mutate(points = ifelse(goals_scored > goals_lost, 3, ifelse(goals_scored == goals_lost, 1, 0)))
  )
  
  total_points <- reactive({
    teams_points() %>%
      group_by(team) %>%
      summarise(points = sum(points))
  })
  
  total_goals <- reactive({
    dat_teams() %>% 
      group_by(team) %>% 
      summarise(scored = sum(goals_scored), lost = sum(goals_lost))
  })
  
  team_points <- reactive({
    validate(need(input[["selectedTeam"]] %in% dat_teams()$team, "Team doesn't play this season"))
    
    teams_points() %>%
      filter(team == input[["selectedTeam"]]) %>%
      arrange(stage) %>%
      mutate(points = cumsum(points))
  })
  
  team_goals <- reactive({
    validate(need(input[["selectedTeam"]] %in% dat_teams()$team, "Team doesn't play this season"))

    dat_team <- dat_teams() %>%
      filter(team == input[["selectedTeam"]])

    scored_goals <- dat_team %>%
      select(stage, goals_scored) %>%
      arrange(stage) %>%
      mutate(goals = cumsum(goals_scored)) %>%
      select(stage, goals)

    lost_goals <- dat_team %>%
      select(stage, goals_lost) %>%
      arrange(stage) %>%
      mutate(goals = cumsum(goals_lost)) %>%
      select(stage, goals)

    scored_goals$Type <- "scored"
    lost_goals$Type <- "lost"

    rbind(scored_goals, lost_goals)
  })
  
  team_matches <- reactive({
    validate(need(input[["selectedTeam"]] %in% dat_teams()$team, "Team doesn't play this season"))
    
    dat_teams() %>% 
      mutate(result = ifelse(goals_scored > goals_lost, "win", ifelse(goals_scored == goals_lost, "draw", "loss"))) %>% 
      filter(team == input[["selectedTeam"]]) %>% 
      select(team, result) %>% 
      group_by(result) %>% 
      count()
  })
  
  team_matches_table <- reactive({
    validate(need(input[["selectedTeam"]] %in% dat_teams()$team, "Team doesn't play this season"))
    
    df1 <- dat_season() %>%
      filter(home_team == input[["selectedTeam"]]) %>%
      mutate(result = ifelse(home_team_goal > away_team_goal, "win", ifelse(home_team_goal == away_team_goal, "draw", "loss"))) %>%
      select(oponent = away_team, team_goals = home_team_goal, oponent_goals = away_team_goal, result, stage)
    
    df2 <- dat_season() %>%
      filter(away_team == input[["selectedTeam"]]) %>%
      mutate(result = ifelse(away_team_goal > home_team_goal, "win", ifelse(away_team_goal == home_team_goal, "draw", "loss"))) %>%
      select(oponent = home_team, team_goals = away_team_goal, oponent_goals = home_team_goal, result, stage)
    
    rbind(df1, df2) %>%
      setnames(old = c("stage", "team_goals", "oponent_goals", "oponent", "result"), new = c("Stage", "Team's goals", "Oponent's goals", "Oponent", "Result"))
  })
  
  output[["points_plot"]] <- renderPlot(
    total_points() %>% 
      ggplot(aes(x = team, y = points)) +
      geom_bar(stat = "identity", fill = "#CC6666") +
      labs(title = "", x = "", y = "Number of points") +
      theme(axis.text.x = element_text(size = 15, angle = 45, vjust = 1, hjust = 1),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 17, face = "bold", vjust = 2))
  )
  
  output[["goals_plot"]] <- renderPlot(
    total_goals() %>% 
      melt(id.vars = "team") %>%
      ggplot(aes(x = team, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("green3", "red3")) +
      labs(title = "", x = "", y = "Number of goals", fill = "Goals' type") +
      theme(axis.text.x = element_text(size = 15, angle = 45, vjust = 1, hjust = 1),
            axis.text.y = element_text(size = 15),
            axis.title.y = element_text(size = 17, face = "bold", vjust = 2),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 17, face = "bold"),
            legend.position = "right")
  )
  
  output[["team_points_plot"]] <- renderPlot(
    team_points() %>% 
      ggplot(aes(x = stage, y = points)) +
      geom_point(size = 6, colour = "#CC6666") +
      geom_line(size = 1, colour = "#CC6666") +
      labs(title = "", x = "Stage", y = "Number of points") +
      theme(axis.text = element_text(size = 15),
            axis.title.x = element_text(size = 17, face = "bold"),
            axis.title.y = element_text(size = 17, face = "bold", vjust = 2))
  )
  
  output[["team_goals_plot"]] <- renderPlot(
    team_goals() %>%
      ggplot(aes(x = stage, y = goals, group = Type, col = Type, fill = Type)) +
      geom_line(size = 1) +
      geom_point(size = 5) +
      labs(title = "", x = "Stage", y = "Number of goals") +
      scale_color_manual(values = c("red3", "green3")) +
      theme(axis.text = element_text(size = 15),
            axis.title.x = element_text(size = 17, face = "bold"),
            axis.title.y = element_text(size = 17, face = "bold", vjust = 2),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 17, face = "bold"),
            legend.position = "right")
  )
  
  output[["team_matches_plot"]] <- renderPlot(
    team_matches() %>%
      ggplot(aes(x = result, y = n)) +
      geom_bar(stat = "identity", fill = c("salmon", "chocolate", "gold1")) +
      geom_text(aes(label = n), position = position_stack(0.5), size = 8) +
      labs(title = "", x = "", y = "Number of matches") +
      theme(axis.text = element_text(size = 15),
            axis.title.y = element_text(size = 17, face = "bold", vjust = 2))
  )
  
  output[["team_matches_table"]] = renderDataTable({
    team_matches_table()
  },
  rownames = FALSE, options = list(pageLength = 8, lengthMenu = c(8, 16, 32), order = list(0, "asc"))
  )
}

shinyApp(ui, server)
