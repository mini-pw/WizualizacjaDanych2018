library(dplyr)
library(tidyr)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")
dat %>% head

compute_points <- function(x, y) {
  ifelse(x > y, 3, ifelse(x == y, 1, 0))
}

point_dat <- mutate(
  dat,
  home_team_points = compute_points(home_team_goal, away_team_goal),
  away_team_points = compute_points(away_team_goal, home_team_goal)
)

win_sum_dat <- rbind(
    select(point_dat, team = home_team, points = home_team_points, scored_goals = home_team_goal, lost_goals = away_team_goal, season), 
    select(point_dat, team = away_team, points = away_team_points, scored_goals = away_team_goal, lost_goals = home_team_goal, season)
  ) %>%
  group_by(team, season) %>%
  summarise(points = as.integer(sum(points)), scored_goals = sum(scored_goals), lost_goals = sum(lost_goals)) %>%
  complete(season, nesting(team), fill = list(points = NA)) %>%
  ungroup()
