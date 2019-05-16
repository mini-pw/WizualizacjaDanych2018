library(dplyr)
library(tidyr)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

compute_points <- function(x, y) {
  ifelse(x > y, 3, ifelse(x == y, 1, 0))
}

point_dat <- mutate(dat, home_team_points = compute_points(home_team_goal, away_team_goal),
                    away_team_points = compute_points(away_team_goal, home_team_goal))

point_dat$result_home <- ifelse(point_dat$home_team_points=="3", "winner", 
                     ifelse(point_dat$home_team_points=="0", "loser", "draw"))

point_dat$result_away <- ifelse(point_dat$away_team_points=="3", "winner", 
                                ifelse(point_dat$away_team_points=="0", "loser", "draw"))

win_perc_dat <- rbind(select(point_dat, team = home_team, points = home_team_points, season), 
                      select(point_dat, team = away_team, points = away_team_points, season)) %>% 
  mutate(win = as.numeric(points == 3) + (points == 1)/2) %>% 
  group_by(team, season) %>% 
  summarise(win_perc = mean(win)) %>% 
  complete(season, nesting(team), fill = list(win_perc = NA)) %>% 
  ungroup()

series_dat <- mutate(dat, home_team_points = compute_points(home_team_goal, away_team_goal),
                     away_team_points = compute_points(away_team_goal, home_team_goal))


match_dat <- rbind(
  select(point_dat, team = home_team, points = home_team_points, scored_goals = home_team_goal, lost_goals = away_team_goal, season, result = result_home), 
  select(point_dat, team = away_team, points = away_team_points, scored_goals = away_team_goal, lost_goals = home_team_goal, season, result = result_away)) %>% 
  group_by(team, season) %>%
  summarise(points = as.integer(sum(points)), wins = (sum(result=="winner")), losses = (sum(result=="loser")), draws = (sum(result=="draw"))) %>% 
  ungroup()

match_dat_melt <- melt(match_dat[,-which(colnames(match_dat) %in% "points")],id.vars = c("team", "season"))
match_dat_melt$variable <- factor(match_dat_melt$variable, levels = c("wins", "draws", "losses"))
match_point_dat_melt <- melt(match_dat[,c("team","season","points")],id.vars = c("team", "season"))

match_dat_home <-
  select(point_dat, team = home_team, home_points = home_team_points,home_goals = home_team_goal,lost_home_goals = away_team_goal,season)  %>% 
  group_by(team, season) %>%
  summarise(home_points = as.integer(sum(home_points)), home_goals = as.integer(sum(home_goals )), lost_home_goals = as.integer(sum(lost_home_goals))) %>% 
  ungroup()

match_dat_away <-
  select(point_dat, team = away_team, away_points = away_team_points,away_goals = away_team_goal, lost_away_goals = home_team_goal, season)  %>% 
  group_by(team, season) %>%
  summarise(away_points = as.integer(sum(away_points)), away_goals = as.integer(sum(away_goals )), lost_away_goals = as.integer(sum(lost_away_goals))) %>% 
  ungroup()

match_dat_all<-left_join(match_dat_home, match_dat_away, by=c("team", "season"))

match_dat_all$points <- match_dat_all$home_points+match_dat_all$away_points
match_dat_all$'home goals difference' <- match_dat_all$home_goals- match_dat_all$lost_home_goals
match_dat_all$'away goals difference' <- match_dat_all$away_goals- match_dat_all$lost_away_goals
match_dat_all <- match_dat_all[,-which(colnames(match_dat_all) %in% c( "home_points", "home_goals", "lost_home_goals", "away_points",
                                                                       "away_goals", "lost_away_goals"))]

