library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyr)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

total_goal <- rbind(select(dat, season, team_goal = home_team_goal, 
                           team_name = home_team), 
                    select(dat, season, team_goal = away_team_goal, 
                           team_name = away_team)) %>% 
  group_by(season, team_name) %>% 
  summarise(total_goals = sum(team_goal)) 

statuses <- c("win" , "loss" , "draw")

dat_mutated <- mutate(dat,
                        home_team_status = ifelse(home_team_goal > away_team_goal, "win", ifelse(home_team_goal == away_team_goal, "draw", "loss")),
                        away_team_status = ifelse(away_team_goal > home_team_goal, "win", ifelse(away_team_goal == home_team_goal, "draw", "loss")),
                        home_team_points = ifelse(home_team_goal > away_team_goal, 3, ifelse(home_team_goal == away_team_goal, 1, 0)),
                        away_team_points = ifelse(away_team_goal > home_team_goal, 3, ifelse(away_team_goal == home_team_goal, 1, 0))
                        )

total_score <- rbind(select(dat_mutated, season, team_status = home_team_status, team_points = home_team_points,
                           team_name = home_team), 
                    select(dat_mutated, season, team_status = away_team_status, team_points = away_team_points,
                           team_name = away_team)) %>% 
  group_by(season, team_name, team_status) %>% 
  tally()

total_score$team_status = factor(total_score$team_status, levels = statuses)


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Football App",
                  
                  dropdownMenu(type = "notifications", badgeStatus = "warning",
                               notificationItem(icon = icon("exclamation-triangle"), status = "info",
                                                "This app is underdeveloped"
                               )
                  )),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("General statistics", tabName = "generalDashboard", icon = icon("dashboard")),
      menuItem("Team statistics", tabName = "teamDashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about",
               badgeColor = "green"),
      menuItem("See also", icon = icon("send",lib='glyphicon'), 
               href = "https://github.com/mini-pw/WizualizacjaDanych2018/")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("generalDashboard",
              titlePanel("Total goals"),
              selectInput(inputId = "sort_season", label = "Sort by season:", 
                          choices = levels(dat[["season"]])),
              plotOutput("total_goal_plot", height = 500),
              
              titlePanel("Wins, losses, draws"),
              selectInput(inputId = "selected_season", label = "Select season:", 
                          choices = levels(dat[["season"]])),
              selectInput(inputId = "sort_score", label = "Sort by:", 
                          choices = c('as on first plot', statuses)),
              plotOutput("total_score_plot", height = 500)
      ),
      tabItem("teamDashboard",
              titlePanel("Under construction")
      ),
      tabItem("about",
              "About the app",
              includeMarkdown("example.md")
      )
    )
  )
)

server <- function(input, output) {
  
  output[["total_goal_plot"]] <- renderPlot({
    team_order <- filter(total_goal, season == input[["sort_season"]]) %>% 
      arrange(desc(total_goals)) %>% 
      pull(team_name) %>% 
      as.character()
    
    full_team_order <- c(team_order,
                         setdiff(total_goal[["team_name"]], team_order))

    mutate(total_goal, 
           team_name = factor(team_name, levels = full_team_order)) %>%
      complete(season, nesting(team_name), fill = list(total_goals = 0)) %>% 
      ggplot(aes(x = team_name, y = total_goals, fill = season)) +
      geom_col(position = "dodge") +
      labs(x = "Team name", y = "Total goals", fill="Season") +
      theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5, size = 11)) +
      scale_fill_manual(values=c('#4F6D7A', '#F2C14E'))
  })
  
  
  output[["total_score_plot"]] <- renderPlot({
    total_score_season <- filter(total_score, season == input[["selected_season"]])
    
    team_order <- filter(total_score_season, team_status == input[["sort_score"]]) %>% 
      arrange(desc(n)) %>% 
      pull(team_name) %>% 
      as.character()
    
    full_team_order <- c(team_order,
                        setdiff(total_score_season[["team_name"]], team_order))
    
    mutate(ungroup(total_score_season),
           team_name = factor(team_name, levels = full_team_order)) %>%
    #  complete(season, nesting(team_name), fill = list(total_goals = 0)) %>% 
    #total_score_season %>% 
      ggplot(aes(x = team_name, y = n, fill = team_status)) +
      geom_col(position = "dodge") +
      labs(x = "Team name", y = "Number of results", fill="") +
      theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5, size = 11)) +
      scale_fill_manual(values=c('#4D9078', '#D95D39', '#F2C14E'))
  })
  
}

shinyApp(ui, server)
