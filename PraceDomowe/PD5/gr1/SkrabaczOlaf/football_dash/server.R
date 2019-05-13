#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(dplyr)
library(shiny)
library(DT)
library(magrittr)

library(ggplot2)

library(tibble)
options(stringsAsFactors = FALSE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   data <- read.csv("PL_dat.csv", sep=";") %>% as_tibble

   data_processed <- reactive({
    print(input$stage_chooser)
    data %<>% mutate(win = home_team_goal > away_team_goal, draw = home_team_goal == away_team_goal, 
                     away_win = away_team_goal > home_team_goal)
    filter(data, (season==input$season_chooser) & (stage >= input$stage_chooser[1]) & (stage <= input$stage_chooser[2]))
   })
   
   points <- reactive({
     points1 <- data_processed() %>% group_by(home_team) %>% summarise(points = sum(win) * 3 + sum(draw))
     points2 <- data_processed() %>% group_by(away_team) %>% summarise(points = sum(1 - win) * 3 + sum(draw))
     points <- left_join(points1, points2, by=c("home_team" = "away_team"), suffix=c("_home", "_away"))
     mutate(points, points = points_home + points_away, team = home_team) %>% select(-home_team)
   })
   
   output$points_plot <- renderPlot({
     ggplot(data=points(), aes(x=reorder(team, points), y=points)) + geom_bar(stat='identity') +
       coord_flip() + xlab("Team") + ylab("Points") + ggtitle("Points earned")
   })
  
   
   output$pstwo_plot <- renderPlot({
     home_wins <- data_processed() %>% group_by(home_team) %>% summarise(win_probability=mean(win)) %>%
       mutate(pitch="home", team=home_team) %>% select(team, pitch, win_probability)
     away_wins <- data_processed() %>% group_by(away_team) %>% summarise(win_probability=mean(away_win)) %>%
       mutate(pitch="away", team=away_team) %>% select(team, pitch, win_probability)
     wins <- rbind(home_wins, away_wins)
     wins <- left_join(wins, points() %>% select(team, points), on="team")
     ggplot(data=wins, aes(x=reorder(team, points), y=win_probability, fill=pitch)) + 
       geom_bar(stat='identity', position="dodge", width=0.5) + coord_flip() + 
       ggtitle("Probability of winning") + xlab("Team") + ylab("Probability")
   })
   
   output$max_goals_scored_plot <- renderPlot({
     home_goals <- data_processed() %>% group_by(home_team) %>% summarise(max_goals=max(home_team_goal)) %>%
       mutate(pitch="home", team=home_team) %>% select(team, pitch, max_goals)
     away_goals <- data_processed() %>% group_by(away_team) %>% summarise(max_goals=max(away_team_goal)) %>%
       mutate(pitch="away", team=away_team) %>% select(team, pitch, max_goals)
     goals <- rbind(home_goals, away_goals)
     goals <- left_join(goals, points() %>% select(team, points), on="team")
     ggplot(data=goals, aes(x=reorder(team, points), y=max_goals, fill=pitch)) + geom_bar(stat='identity', position="dodge", width=0.5) + 
       coord_flip() + ggtitle("Max goals scored") + xlab("Team") + ylab("Goals")
   })
   
   output$mean_goals_scored <- renderPlot({
     home_goals <- data_processed() %>% group_by(home_team) %>% summarise(max_goals=mean(home_team_goal)) %>%
       mutate(pitch="home", team=home_team) %>% select(team, pitch, max_goals)
     away_goals <- data_processed() %>% group_by(away_team) %>% summarise(max_goals=mean(away_team_goal)) %>%
       mutate(pitch="away", team=away_team) %>% select(team, pitch, max_goals)
     goals <- rbind(home_goals, away_goals)
     goals <- left_join(goals, points() %>% select(team, points), on="team")
     ggplot(data=goals, aes(x=reorder(team, points), y=max_goals, fill=pitch)) + geom_bar(stat='identity', position="dodge", width=0.5) + 
       coord_flip() + ggtitle("Mean goals scored") + xlab("Team") + ylab("Goals")
   })
   
   output$points_in_time <- renderPlot({
     one_team <- data_processed() %>% filter(home_team == input$choosen_team | away_team == input$choosen_team) %>%
       mutate(points = case_when(home_team==input$choosen_team ~ win*3 + draw, TRUE ~(1-win) * 3 + draw), team=input$choosen_team) %>%
       arrange(stage) %>% mutate(cum_points = cumsum(points))
     ggplot(data=one_team, aes(x=stage, y=cum_points)) + geom_line() + geom_point() + ggtitle("Points") + ylab("Points")
   })
   
   output$goal_balance_in_time <- renderPlot({
     one_team <- data_processed() %>% filter(home_team == input$choosen_team | away_team == input$choosen_team) %>% 
       mutate(goals_balance = case_when(home_team == input$choosen_team ~ home_team_goal - away_team_goal,
                                        TRUE ~ away_team_goal - home_team_goal), team=input$choosen_team) %>% arrange(stage) %>%
       mutate(cum_goals = cumsum(goals_balance))
     ggplot(data=one_team, aes(x=stage, y=cum_goals)) + geom_line() + geom_point() + ggtitle("Goal balance") + ylab("Goal balance")
   })
   
   output$`Goals scored home` <- renderValueBox({
     one_team <- data_processed() %>% filter(home_team == input$choosen_team | away_team == input$choosen_team) %>% 
       mutate(goals_sum = case_when(home_team == input$choosen_team ~ home_team_goal,
                                        TRUE ~ 0L), team=input$choosen_team)
     valueBox(one_team$goals_sum %>% sum
       , "Goals scored home", icon = icon("home"),
       color = "green"
     )
   })
   
   output$`Goals lost home` <- renderValueBox({
     one_team <- data_processed() %>% filter(home_team == input$choosen_team | away_team == input$choosen_team) %>% 
       mutate(goals_sum = case_when(home_team == input$choosen_team ~ away_team_goal,
                                    TRUE ~ 0L), team=input$choosen_team)
     valueBox(one_team$goals_sum %>% sum
              , "Goals lost home", icon = icon("home"),
              color = "red"
     )
   })
   
   output$`Goals scored away` <- renderValueBox({
     one_team <- data_processed() %>% filter(home_team == input$choosen_team | away_team == input$choosen_team) %>% 
       mutate(goals_sum = case_when(away_team == input$choosen_team ~ away_team_goal,
                                    TRUE ~ 0L), team=input$choosen_team)
     valueBox(one_team$goals_sum %>% sum
              , "Goals scored away", icon = icon("plane"),
              color = "green"
     )
   })
   

   output$`Goals lost away` <- renderValueBox({
     one_team <- data_processed() %>% filter(home_team == input$choosen_team | away_team == input$choosen_team) %>%
       mutate(goals_sum = case_when(away_team == input$choosen_team ~ home_team_goal,
                                    TRUE ~ 0L), team=input$choosen_team)
     valueBox(as.integer(one_team$goals_sum %>% sum)
              , "Goals lost away", icon = icon("plane"),
              color = "red"
     )
   })
   
   output$spec_table <- renderDT({
     one_team <- data_processed() %>% filter(home_team == input$choosen_team | away_team == input$choosen_team) %>%
       mutate(points = case_when(home_team==input$choosen_team ~ win*3 + draw, TRUE ~(1-win) * 3 + draw), team=input$choosen_team) %>%
       arrange(stage) %>% mutate(cumulative_points = cumsum(points))
     one_team
   })
   
   
})
