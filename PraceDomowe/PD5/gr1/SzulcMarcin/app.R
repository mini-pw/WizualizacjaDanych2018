library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)
library(lubridate)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

total_goal <- rbind(select(dat, season, team_goal = home_team_goal, 
                           team_name = home_team), 
                    select(dat, season, team_goal = away_team_goal, 
                           team_name = away_team)) %>% 
  group_by(season, team_name) %>% 
  summarise(total_goals = sum(team_goal)) 

total_wins <- dat %>% 
  mutate(winner=if_else(home_team_goal > away_team_goal, home_team, if_else(home_team_goal < away_team_goal, away_team, factor(NA)))) %>% 
  na.omit() %>% 
  group_by(season, winner) %>% 
  count()

wins_quater <- dat %>% 
  mutate(winner=if_else(home_team_goal > away_team_goal, home_team, if_else(home_team_goal < away_team_goal, away_team, factor(NA))),
         q=quarter(date,  with_year = TRUE)) %>% 
  group_by(q, winner) %>% 
  na.omit() %>% 
  count()

goals_quarter <- rbind(select(dat, season, date, team_goal= home_team_goal, 
             team_name = home_team), 
      select(dat, season, date, team_goal = away_team_goal, 
             team_name = away_team)) %>% 
  mutate(q=quarter(date, with_year = TRUE)) %>% 
  group_by(q, team_name) %>% 
  count()


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App",
                  dropdownMenu(type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("exclamation-triangle"), status = "info",
                                                "This app is underdeveloped"
                               )
                  )),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "seasons", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItem("seasons",
        tabBox(width="12",
          tabPanel(
          title = "Goals per season",
          selectInput(inputId = "selected_season",
                      label = "Select season",
                      choices = levels(dat[["season"]])),
          plotOutput("total_goal_plot")
          ),
          tabPanel(
            title="Wins per season",
            selectInput(inputId = "selected_season_wins", label = "Selected season",
                        choices = levels(dat[["season"]])),
            plotOutput("total_wins_plot")
          )
        )
      ),
    box(
      title="Wins over time (quaters)",
      selectInput(inputId = "selected_team_wins", label = "Selected team",
                  choices = levels(wins_quater[["winner"]])),
      plotOutput("wins_quater_plot")
    ),
    box(title="Goals over time (quarters)",
        selectInput(inputId = "selected_team_goals", label = "Selected team",
                    choices = levels(goals_quarter[["team_name"]])),
        plotOutput("goals_quarter_plot")),
    tabItem("matches_vs",
      fluidRow(
        box("Two teams matches history", width=12,
          fluidRow(
            box(
              selectInput(inputId = "selected_team_1", label = "Select first team", selected = "Arsenal",
                          choices = levels(dat[["home_team"]]))
            ),
            box(
              selectInput(inputId = "selected_team_2", label = "Select second team", selected = "Chelsea",
                          choices = levels(dat[["away_team"]]))
            )
          ),
          fluidRow(
            column(4, 
              plotOutput("matches_vs_plot")
            ),
            column(8, 
              div(DT::dataTableOutput("matches_table"), style = "overflow-x: auto; width:100%;")
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  output[["total_goal_plot"]] <- renderPlot({
    team_order <- filter(total_goal, season == input[["selected_season"]]) %>% 
      arrange(desc(total_goals)) %>% 
      pull(team_name) %>% 
      as.character()
    
    full_team_order <- c(team_order,
                         setdiff(total_goal[["team_name"]], team_order))
    
    mutate(total_goal, 
           team_name = factor(team_name, levels = full_team_order)) %>%
      complete(season, nesting(team_name), fill = list(total_goals = 0)) %>% 
      ggplot(aes(x = team_name, y = total_goals, 
                 fill = season)) +
      geom_col(position = "dodge") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      xlab("Team") +
      ylab("Goals")
  })
  
  output[["total_wins_plot"]] <- renderPlot({
    
    team_order <- filter(total_wins, season == input[["selected_season_wins"]]) %>% 
      arrange(desc(n)) %>% 
      pull(winner) %>% 
      as.character()
    
    full_team_order <- c(team_order,
                         setdiff(total_wins[["winner"]], team_order))
    
    mutate(total_wins, 
           team = factor(winner, levels = full_team_order)) %>%
      complete(season, nesting(team), fill = list(n = 0)) %>% 
      ggplot(aes(x = team, y = n, 
                 fill = season)) +
      geom_col(position = "dodge") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      xlab("Team") +
      ylab("Number of wins")
  })
  
  output[["wins_quater_plot"]] <- renderPlot({
    wins_quater %>% 
      filter(winner==input[["selected_team_wins"]]) %>% 
      ggplot(aes(x=as.factor(q), y=n, group=1)) + 
      scale_y_continuous(limits=c(0,10)) + 
      xlab("Year/quarter") +
      ylab("Number of won matches") +
      theme_bw() + 
      geom_line()
  })
  
  output[["goals_quarter_plot"]] <- renderPlot({
    goals_quarter %>% 
      filter(team_name==input[["selected_team_goals"]]) %>% 
      ggplot(aes(x=as.factor(q), y=n, group=1)) + 
      scale_y_continuous(limits=c(0,20)) + 
      xlab("Year/quarter") +
      ylab("Number of goals") +
      theme_bw() + 
      geom_line()
  })
  
  output[["matches_vs_plot"]] <- renderPlot({
    team_1 <- input[["selected_team_1"]]
    team_2 <- input[["selected_team_2"]] 
    
    dat %>% 
      filter((away_team==team_1 & home_team==team_2) | (away_team==team_2 & home_team==team_1)) %>% 
      mutate(winner=ifelse(home_team_goal > away_team_goal, as.character(home_team),
                           ifelse(home_team_goal < away_team_goal, as.character(away_team), "draw"))) %>% 
      group_by(winner) %>% 
      count() %>% 
      ggplot(aes(x=winner, y=n)) + 
      geom_bar(stat="identity", fill="green") +
      scale_y_continuous(limits=c(0, 6)) + 
      xlab("Winner team") + 
      ylab("Number of matches")+ 
      theme_bw()
    
    
  })
  
  output[["matches_table"]] <- DT::renderDataTable({
    team_1 <- input[["selected_team_1"]]
    team_2 <- input[["selected_team_2"]] 
    dat %>% 
      rename(AwayTeam=away_team, HomeTeam=home_team, Date=date, Season=season, Stage=stage,
             HomeTeamGoals = home_team_goal, AwayTeamGoals=away_team_goal) %>% 
      filter((AwayTeam==team_1 & HomeTeam==team_2) | (AwayTeam==team_2 & HomeTeam==team_1)) 
    
  })
  
}


shinyApp(ui, server)
