library(dplyr)
library(tidyr)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

compute_points <- function(x, y) {
  ifelse(x > y, 3, ifelse(x == y, 1, 0))
}

point_dat <- mutate(dat, home_team_points = compute_points(home_team_goal, away_team_goal),
                    away_team_points = compute_points(away_team_goal, home_team_goal))

win_perc_dat <- rbind(select(point_dat, team = home_team, points = home_team_points, season), 
      select(point_dat, team = away_team, points = away_team_points, season)) %>% 
  mutate(win = as.numeric(points == 3) + (points == 1)/2) %>% 
  group_by(team, season) %>% 
  summarise(win_perc = mean(win)) %>% 
  complete(season, nesting(team), fill = list(win_perc = NA)) %>% 
  ungroup()
