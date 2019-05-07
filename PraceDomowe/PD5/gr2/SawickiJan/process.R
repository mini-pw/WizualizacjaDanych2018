library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)

data <-
  read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

write.csv(data, file = "data.csv")

data$winning_team = ifelse(
  data$home_team_goal > data$away_team_goal,
  as.character(data$home_team),
  as.character(data$away_team)
)
data$loosing_team = ifelse(
  data$home_team_goal <= data$away_team_goal,
  as.character(data$home_team),
  as.character(data$away_team)
)
data$winning_team_goals = ifelse(
  data$home_team_goal > data$away_team_goal,
  as.character(data$home_team_goal),
  as.character(data$away_team_goal)
)
data$loosing_team_goals = ifelse(
  data$home_team_goal <= data$away_team_goal,
  as.character(data$home_team_goal),
  as.character(data$away_team_goal)
)

data$home_team = NULL
data$away_team = NULL
data$home_team_goal = NULL
data$away_team_goal = NULL

data2 = data

data2$winning_team = data$loosing_team
data2$loosing_team = data$winning_team
data2$winning_team_goals = data2$loosing_team_goals
data2$loosing_team_goals = data2$winning_team_goals

data = rbind(data, data2)

colnames(data) =  c("season",
                    "stage",
                    "date",
                    "team1",
                    "team2",
                    "team1_goals",
                    "team2_goals")

data$team1_goals = as.numeric(data$team1_goals)
data$team2_goals = as.numeric(data$team2_goals)

data$team1_points = as.numeric(ifelse(
  data$team1_goals > data$team2_goals,
  3,
  ifelse(data$team1_goals == data$team2_goals, 1, 0)
))
data$team2_points = as.numeric(ifelse(
  data$team1_goals < data$team2_goals,
  3,
  ifelse(data$team1_goals == data$team2_goals, 1, 0)
))

data$team1_win = as.numeric(ifelse(
  data$team1_goals > data$team2_goals,
  1,
  ifelse(data$team1_goals == data$team2_goals, 0.5, 0)
))
data$team2_win = as.numeric(ifelse(
  data$team1_goals < data$team2_goals,
  1,
  ifelse(data$team1_goals == data$team2_goals, 0.5, 0)
))

team_results = data %>% group_by(team = team1, season) %>% summarise(
  wins = sum(team1_win),
  goals = sum(team1_goals),
  points = sum(team1_points)
) %>% arrange(desc(wins))

head(team_results)
