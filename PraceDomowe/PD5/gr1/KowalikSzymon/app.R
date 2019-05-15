library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

statuses <- c("Win" , "Loss" , "Draw")
measures <- c("Total score (win = 3, draw = 1, loss = 0)", "total goals")

dat_with_scores <- mutate(dat,
                        home_team_status = ifelse(home_team_goal > away_team_goal, "Win", ifelse(home_team_goal == away_team_goal, "Draw", "Loss")),
                        away_team_status = ifelse(away_team_goal > home_team_goal, "Win", ifelse(away_team_goal == home_team_goal, "Draw", "Loss")),
                        home_team_points = ifelse(home_team_goal > away_team_goal, 3, ifelse(home_team_goal == away_team_goal, 1, 0)),
                        away_team_points = ifelse(away_team_goal > home_team_goal, 3, ifelse(away_team_goal == home_team_goal, 1, 0))
                        )

dat_for_team <- rbind(select(dat_with_scores, season, date, team_status = home_team_status, team_points = home_team_points, team_goal = home_team_goal,
                             team_name = home_team), 
                      select(dat_with_scores, season, date, team_status = away_team_status, team_points = away_team_points, team_goal = away_team_goal,
                             team_name = away_team))
dat_for_team$date <- as.Date(dat_for_team$date)

total_score <- dat_for_team %>% 
  group_by(season, team_name, team_status) %>% 
  tally()
total_score$team_status = factor(total_score$team_status, levels = statuses)

total_points <- dat_for_team %>% 
  group_by(season, team_name) %>% 
  summarise(total_points = sum(team_points))

total_goal <- dat_for_team %>% 
  group_by(season, team_name) %>% 
  summarise(total_goals = sum(team_goal))


team_scores_plot <- function(team, sseason) {
  data <- total_score %>% 
    filter(team_name == team & season == sseason)
  
  res <- data %>%
    ggplot(aes(x = team_name, y = n, fill = team_status)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) +
    labs(y = "Number of results", fill="") +
    theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5, size = 11)) +
    scale_fill_manual(values=c('#4D9078', '#D95D39', '#F2C14E')) + 
    theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid=element_blank(),
      axis.text.x = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  return(res)
}


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("General statistics", tabName = "generalDashboard", icon = icon("dashboard")),
      menuItem("Teams comparison", tabName = "teamDashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("generalDashboard",
              titlePanel("Teams quality"),
              selectInput(inputId = "sort_season", label = "Sort by season:", 
                          choices = levels(dat[["season"]])),
              selectInput(inputId = "measure", label = "Measure:", 
                          choices = measures),
              plotOutput("total_goal_plot", height = 500),
              
              titlePanel("Wins, losses, draws"),
              selectInput(inputId = "selected_season", label = "Select season:", 
                          choices = levels(dat[["season"]])),
              selectInput(inputId = "sort_score", label = "Sort by:", 
                          choices = c('As on first plot', statuses)),
              plotOutput("total_score_plot", height = 500)
      ),
      tabItem("teamDashboard",
              selectInput(inputId = "teams_season", label = "Select season:", 
                          choices = levels(dat[["season"]])
                          ),
              fluidRow(
                column(6,
                       selectInput(inputId = "team1", label = "Team 1:", 
                                   choices = levels(dat_for_team[["team_name"]]))
                ),
                column(6,
                       selectInput(inputId = "team2", label = "Team 2:", 
                                   choices = levels(dat_for_team[["team_name"]]))
                )
              ),
              titlePanel("Wins, losses, draws"),
              fluidRow(
                column(6,
                       plotOutput("team1_score_plot", height = 350)
                ),
                column(6,
                       plotOutput("team2_score_plot", height = 350)
                )
              ),
              titlePanel("Teams points comparision"),
              plotOutput("teams_points_plot", height = 500)
      )
    )
  )
)

server <- function(input, output) {
  team_quality <- reactive({
    if(input[["measure"]] != "total goals") {
      total_points %>% rename(quality = total_points)
    } else {
      total_goal %>% rename(quality = total_goals)
    }
  })
  
  goal_team_order_r <- reactive({
    team_order <- filter(team_quality(), season == input[["sort_season"]]) %>% 
      arrange(desc(quality)) %>% 
      pull(team_name) %>% 
      as.character()
    
    team_order
  })
  
  output[["total_goal_plot"]] <- renderPlot({
    full_team_order <- c(goal_team_order_r(),
                        setdiff(team_quality()[["team_name"]], goal_team_order_r()))

    mutate(team_quality(), 
           team_name = factor(team_name, levels = full_team_order)) %>%
      complete(season, nesting(team_name), fill = list(quality = 0)) %>% 
      ggplot(aes(x = team_name, y = quality, fill = season)) +
      geom_col(position = "dodge") +
      labs(x = "Team name", y = input[["measure"]], fill="Season") +
      theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5, size = 11)) +
      scale_fill_manual(values=c('#4F6D7A', '#F2C14E'))
  })
  
  
  output[["total_score_plot"]] <- renderPlot({
    total_score_season <- filter(total_score, season == input[["selected_season"]])
    
    if(input[["sort_score"]] == "as on first plot") {
      team_order <- goal_team_order_r()
    } else {
      team_order <- filter(total_score_season, team_status == input[["sort_score"]]) %>% 
        arrange(desc(n)) %>% 
        pull(team_name) %>% 
        as.character()
    }
      
    full_team_order <- c(team_order,
                        setdiff(total_score_season[["team_name"]], team_order))
    
    mutate(ungroup(total_score_season),
           team_name = factor(team_name, levels = full_team_order)) %>%
      ggplot(aes(x = team_name, y = n, fill = team_status)) +
      geom_col(position = "dodge") +
      labs(x = "Team name", y = "Number of results", fill="") +
      theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5, size = 11)) +
      scale_fill_manual(values=c('#4D9078', '#D95D39', '#F2C14E'))
  })
  
  
  output[["team1_score_plot"]] <- renderPlot({
    team_scores_plot(input[["team1"]], input[["teams_season"]])
  })
  
  output[["team2_score_plot"]] <- renderPlot({
    team_scores_plot(input[["team2"]], input[["teams_season"]])
  })
  
  output[["teams_points_plot"]] <- renderPlot({
    dat_for_team %>%
      filter((team_name == input[["team1"]] | team_name == input[["team2"]]) & season == input[["teams_season"]]) %>% 
      arrange(date) %>% 
      plyr::ddply(plyr::.(team_name), transform, c_team_points = cumsum(team_points)) %>% 
      ggplot(aes(x=date, y=c_team_points, group=team_name)) +
        geom_line(aes(color=team_name)) +
        geom_point(aes(color=team_name)) +
        scale_x_date(date_breaks = "1 month", labels = date_format("%m-%Y")) +
        labs(x = "Time", y = "Points", color="") +
        scale_color_manual(values=c('#4F6D7A', '#F2C14E'))
  })
}

shinyApp(ui, server)
