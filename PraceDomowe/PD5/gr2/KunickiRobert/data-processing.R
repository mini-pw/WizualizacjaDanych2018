library(dplyr)
library(tidyr)
library(ggplot2)

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


goals_dat <- rbind(select(point_dat, team = home_team, team_against = away_team, goals_scored = home_team_goal, goals_lost = away_team_goal), 
                        select(point_dat, team = away_team,  team_against = home_team, goals_scored = away_team_goal, goals_lost = home_team_goal)) %>%
  group_by(team, team_against) %>% summarise(goals_scored_sum = sum(goals_scored), goals_lost_sum = sum(goals_lost))  %>%
mutate (goals_diff = goals_scored_sum - goals_lost_sum)

#goals_dat <- goals_dat %>% filter(team == "Arsenal")

#ggplot(goals_dat, aes(x = team_against, y = goals_diff, fill=goals_diff)) +
#  geom_text(aes(label = paste0(goals_diff), vjust = ifelse(goals_diff > 0, -0.2, 1.2))) +
#  ggtitle("Różnica bramek zdobytych i straconych pomiędzy zespołami") +
#  xlab('Zespół') +
#  ylab('Różnica bramek')  +
#  labs(fill='Różnica bramek') +
#  geom_col(position = "dodge") +
#  geom_bar(stat="identity") +
#  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))

