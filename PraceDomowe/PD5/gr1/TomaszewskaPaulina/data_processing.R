library(dplyr)
library(tidyr)
library(reshape2)

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
  summarise(win_perc=mean(win))%>%
  complete(season, nesting(team), fill = list(win_perc = NA)) %>%
  ungroup()


match_result_at_home <- point_dat %>%  mutate(lost=ifelse (home_team_points==0 ,1,0) )%>% 
  mutate(win=ifelse (home_team_points==3 ,1,0) )%>% mutate(draw=ifelse (home_team_points==1 ,1,0) )%>% group_by(home_team,season) %>%
  summarise(draw_times=sum(draw),win_times=(sum(win)), lost_times=sum(lost) )
match_result <- rbind(select(point_dat, team = home_team, points = home_team_points, season), 
                      select(point_dat, team = away_team, points = away_team_points, season))  %>% mutate(lost=ifelse (points==0 ,1,0) )%>% 
  mutate(win=ifelse (points==3 ,1,0) )%>% mutate(draw=ifelse (points==1 ,1,0) )%>% group_by(team,season) %>%
  summarise(draw_times=sum(draw),win_times=(sum(win)), lost_times=sum(lost) ) 


match_result_away <- point_dat %>%  mutate(lost=ifelse (away_team_points==0 ,1,0) )%>%
  mutate(win=ifelse (away_team_points==3 ,1,0) )%>% mutate(draw=ifelse (home_team_points==1 ,1,0) )%>% group_by(away_team,season) %>%
  summarise(draw_times=sum(draw),win_times=(sum(win)), lost_times=sum(lost) )

diff_goals <- rbind(select(point_dat, team1 = home_team, team2=away_team,goals1 = home_team_goal,goals2=away_team_goal, season, date, stage))

final<- match_result %>% mutate(points=3*win_times+draw_times)
