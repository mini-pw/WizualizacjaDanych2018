library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(markdown)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

dat1 <- filter(dat, season == "2014/2015")
dat2 <- filter(dat, season == "2015/2016")

dat$home_winner <- " "
dat$away_winner <- " "

for(i in 1:760){
  if(dat[i,4] > dat[i,5]){
    dat[i,8] <- as.character(dat[i,6])
    dat[i,9] <- "NotWin"
  }
  else if(dat[i,5] > dat[i,4]){
    dat[i,8] <- "NotWin"
    dat[i,9] <- as.character(dat[i,7])
  }
  else{
    dat[i,8] <- "NotWin"
    dat[i,9] <- "NotWin"
  }
}

total_home_wins <- select(dat, season, team_name = home_winner) %>% 
  group_by(season, team_name) %>% 
  count(team_name) %>%
  filter(team_name != "NotWin")

total_away_wins <- select(dat, season, team_name = away_winner) %>% 
  group_by(season, team_name) %>% 
  count(team_name) %>%
  filter(team_name != "NotWin")

total_goal <- rbind(select(dat, season, team_goal = home_team_goal, 
                           team_name = home_team), 
                    select(dat, season, team_goal = away_team_goal, 
                           team_name = away_team)) %>% 
  group_by(season, team_name) %>% 
  summarise(total_goals = sum(team_goal)) 


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
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("About", icon = icon("info-circle"), tabName = "about", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("See also", icon = icon("send",lib='glyphicon'), 
               href = "https://github.com/mini-pw/WizualizacjaDanych2018/")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              titlePanel("Scored goals classification"),
              selectInput(inputId = "selected_season", label = "Select season", 
                          choices = levels(dat[["season"]])),
              plotOutput("total_goal_plot"),
              titlePanel("Home wins classification"),
              selectInput(inputId = "selected_season_h", label = "Select season", 
                          choices = levels(dat[["season"]])),
              plotOutput("total_home_wins_plot"),
              titlePanel("Away wins classification"),
              selectInput(inputId = "selected_season_a", label = "Select season", 
                          choices = levels(dat[["season"]])),
              plotOutput("total_away_wins_plot"),
              titlePanel("Wins, draws and loses proportion in season 2014/15"),
              selectInput(inputId = "selected_team", label = "Select season", 
                          choices = levels(dat1[["home_team"]])),
              plotOutput("circle_plot"),
              titlePanel("Wins, draws and loses proportion in season 2015/16"),
              selectInput(inputId = "selected_team2", label = "Select season", 
                          choices = levels(dat2[["home_team"]])),
              plotOutput("circle_plot2")
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
      theme(axis.text.x=element_text(angle = 90,hjust = 1,vjust = 0.5,size = 15)) +
      labs(y="Goals scored in season", x = ' ') 
  })
  
  output[["total_home_wins_plot"]] <- renderPlot({
    team_order_h <- filter(total_home_wins, season == input[["selected_season_h"]]) %>% 
      arrange(desc(n)) %>% 
      pull(team_name) %>% 
      as.character()
    
    full_team_order_h <- c(team_order_h,
                           setdiff(total_home_wins[["team_name"]], team_order_h))
    
    mutate(total_home_wins, 
           team_name2 = factor(team_name, levels = full_team_order_h)) %>%
      ggplot(aes(x = team_name2, y = n, 
                 fill = season)) +
      geom_col(position = "dodge") +
      theme(axis.text.x=element_text(angle = 90,hjust = 1,vjust = 0.5,size = 15)) +
      labs(y="Number of home wins in season", x = ' ') 
    
  }) 
  
  output[["total_away_wins_plot"]] <- renderPlot({
    team_order_a <- filter(total_away_wins, season == input[["selected_season_a"]]) %>% 
      arrange(desc(n)) %>% 
      pull(team_name) %>% 
      as.character()
    
    full_team_order_a <- c(team_order_a,
                           setdiff(total_away_wins[["team_name"]], team_order_a))
    
    mutate(total_away_wins, 
           team_name2 = factor(team_name, levels = full_team_order_a)) %>%
      ggplot(aes(x = team_name2, y = n, 
                 fill = season)) +
      geom_col(position = "dodge") +
      theme(axis.text.x=element_text(angle = 90,hjust = 1,vjust = 0.5,size = 15)) +
      labs(y="Number of away wins in season", x = ' ') 
    
  }) 
  
  output[["circle_plot"]] <- renderPlot({
    wins_home <- filter(dat1, home_team == input[["selected_team"]] & home_team_goal > away_team_goal) %>% 
      count(home_team) %>%
      pull(n) / 38
    
    wins_away <- filter(dat1, away_team == input[["selected_team"]] & home_team_goal < away_team_goal) %>% 
      count(away_team) %>%
      pull(n) / 38
    
    draws_home <- filter(dat1, home_team == input[["selected_team"]] & home_team_goal == away_team_goal) %>% 
      count(home_team) %>%
      pull(n) / 38
    
    draws_away <- filter(dat1, away_team == input[["selected_team"]] & home_team_goal == away_team_goal) %>% 
      count(away_team) %>%
      pull(n) / 38
    
    loses_home <- filter(dat1, home_team == input[["selected_team"]] & home_team_goal < away_team_goal) %>% 
      count(home_team) %>%
      pull(n) / 38
    
    loses_away <- filter(dat1, away_team == input[["selected_team"]] & home_team_goal > away_team_goal) %>% 
      count(away_team) %>%
      pull(n) / 38
    
    labels <- c("results", "values")
    raw1 <- c("wins",wins_home + wins_away)
    raw2 <- c("draws",draws_home + draws_away)
    raw3 <- c("loses",loses_home + loses_away)
    
    df <- rbind(raw1, raw2, raw3)
    colnames(df) <- labels
    df <- data.frame(df)
    
    df %>%
    ggplot(aes(x = "", y=values, fill = results)) + 
      geom_bar(width = 1, stat = "identity") +
      theme(axis.line = element_blank()) +
      labs(fill="Stats", 
           x=NULL, 
           y=NULL) +
      scale_fill_manual("legend", values = c("wins" = "limegreen", "draws" = "deepskyblue", "loses" = "red2")) + 
      coord_polar(theta = "y", start=0) +
      theme(axis.text.x = element_text(color = NA))
  }) 
  
  output[["circle_plot2"]] <- renderPlot({
    wins_home <- filter(dat2, home_team == input[["selected_team2"]] & home_team_goal > away_team_goal) %>% 
      count(home_team) %>%
      pull(n)
    
    wins_away <- filter(dat2, away_team == input[["selected_team2"]] & home_team_goal < away_team_goal) %>% 
      count(away_team) %>%
      pull(n)
    
    draws_home <- filter(dat2, home_team == input[["selected_team2"]] & home_team_goal == away_team_goal) %>% 
      count(home_team) %>%
      pull(n)
    
    draws_away <- filter(dat2, away_team == input[["selected_team2"]] & home_team_goal == away_team_goal) %>% 
      count(away_team) %>%
      pull(n)
    
    loses_home <- filter(dat2, home_team == input[["selected_team2"]] & home_team_goal < away_team_goal) %>% 
      count(home_team) %>%
      pull(n)
    
    loses_away <- filter(dat2, away_team == input[["selected_team2"]] & home_team_goal > away_team_goal) %>% 
      count(away_team) %>%
      pull(n)
    
    labels <- c("results", "values")
    raw1 <- c("wins",wins_home + wins_away)
    raw2 <- c("draws",draws_home + draws_away)
    raw3 <- c("loses",loses_home + loses_away)
    
    df <- rbind(raw1, raw2, raw3)
    colnames(df) <- labels
    df <- data.frame(df)
    
    df %>%
      ggplot(aes(x = "", y=values, fill = results)) + 
      geom_bar(width = 1, stat = "identity") +
      theme(axis.line = element_blank()) +
      labs(fill="Results", 
           x=NULL, 
           y=NULL) +
      scale_fill_manual("legend", values = c("wins" = "limegreen", "draws" = "deepskyblue", "loses" = "red2")) + 
      coord_polar(theta = "y", start=0) +
      theme(axis.text.x = element_text(color = NA))
  }) 
  
}

shinyApp(ui, server)