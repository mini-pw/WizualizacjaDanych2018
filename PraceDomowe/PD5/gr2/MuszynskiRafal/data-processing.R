library(dplyr)
library(tidyr)
library(ggplot2)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

compute_points <- function(x, y) {
  ifelse(x > y, 3, ifelse(x == y, 1, 0))
}

point_dat <- mutate(dat, home_team_points = compute_points(home_team_goal, away_team_goal),
                    away_team_points = compute_points(away_team_goal, home_team_goal))

# stage | opponent | points

# stage | team | points

# time join teams filter by season
point_at_stage1 <- mutate(point_dat, team = home_team, points = home_team_points)
point_at_stage2 <- mutate(point_dat, team = away_team, points = away_team_points)
point_at_stage <- select(rbind(point_at_stage1, point_at_stage2), season, stage, team, points)

points_in_season <- point_at_stage %>% 
  group_by(season, team) %>% 
  summarise(season_points = sum(points))

cumulative_points <- point_at_stage %>% 
  group_by(season, team) %>% 
  arrange(stage) %>% 
  mutate(cumul_points = cumsum(points))

# points accumulated through time  
cumulative_points %>% 
  filter(season == "2015/2016") %>% 
  mutate(selection = (team == 'Arsenal')) %>% 
  ggplot(aes(x = stage, y = cumul_points, color = team, alpha = selection)) +
  geom_line() +
  scale_alpha_discrete(range = c(0.35,1), guide = FALSE)

# table position through time

get_pos <- function(desired_season, desired_stage, desired_team){
  ordered <- cumulative_points %>% 
    filter(stage == desired_stage & season == desired_season) %>% 
    arrange(desc(cumul_points))
  
  which(ordered$team == desired_team)
}

cumulative_points %>% 
  filter(stage == 35 & season == "2015/2016") %>% 
  arrange(desc(cumul_points))
  
get_pos("2014/2015", 33, "Lelcester City")

position_in_table <- cumulative_points %>% 
  group_by(season,stage,team) %>% 
  mutate(position = get_pos(season, stage, team))


# positions in table
selection <- position_in_table %>% 
  filter(season == "2015/2016") %>% 
  mutate(selection = ifelse(team == 'Arsenal', 1, 0.3)) %>% 
  pull(selection)

last_stage <- max( position_in_table %>% 
       filter(season == "2015/2016") %>% 
       pull(stage) )

final_positions <- position_in_table %>%
  filter(season == "2015/2016" & stage == last_stage)

position_in_table %>%
  filter(season == "2015/2016") %>%
  mutate(selection = (team == 'Arsenal')) %>% 
  ggplot(aes(x = stage, y = position, color = team, alpha = selection, label = team)) +
  geom_line() +
  scale_alpha_discrete(range = c(0.35,1), guide = FALSE)


fractions_dat <- 


win_perc_dat <- rbind(select(point_dat, team = home_team, points = home_team_points, season), 
                      select(point_dat, team = away_team, points = away_team_points, season)) %>% 
  mutate(win = as.numeric(points == 3)) %>% 
  group_by(team, season) %>% 
  summarise(win_perc = mean(win)) %>% 
  complete(season, nesting(team), fill = list(win_perc = NA)) %>% 
  ungroup()


## versus mode

# zestawienie
# gole
# wygrane 
# remisy
# ogólnie lub między sobą

team_a <- "Arsenal"
team_b <- "Liverpool"
chosen_season <- "2014/2015"
# get wins

comparision_dat <- filter(dat, home_team %in% c(team_a, team_b) & away_team %in% c(team_a, team_b))
comparision_points <- mutate(comparision_dat, home_team_points = compute_points(home_team_goal, away_team_goal),
                             away_team_points = compute_points(away_team_goal, home_team_goal))


point_at_stage1 <- mutate(point_dat, team = home_team, points = home_team_points)
point_at_stage2 <- mutate(point_dat, team = away_team, points = away_team_points)
point_at_stage <- select(rbind(point_at_stage1, point_at_stage2), season, stage, team, points)


direct_comparision <- point_at_stage %>%
  filter(season == chosen_season) %>%
  group_by(team, points) %>%
  summarise(occurences = n()) %>%
  mutate(result = ifelse(points == 0, 'lost', ifelse(points == 1, 'tied', 'won'))) %>%
  filter( team %in% c(team_a,team_b))

comparision <- point_at_stage %>%
  filter(season == chosen_season) %>%
  group_by(team, points) %>%
  summarise(occurences = n()) %>%
  mutate(result = ifelse(points == 0, 'lost', ifelse(points == 1, 'tied', 'won'))) %>%
  filter( team %in% c(team_a,team_b))

comparision %>% 
  ggplot(aes(x = result, y = occurences, fill = team)) +
  geom_col()

goals_home <- mutate(dat, team = home_team, goals = home_team_goal) %>% 
  group_by(season, team) %>% 
  summarise(goals_sum_home = sum(goals))

goals_away <- mutate(dat, team = away_team, goals = away_team_goal) %>% 
  group_by(season, team) %>% 
  summarise(goals_sum_away = sum(goals))

goals_sum <- inner_join(goals_home, goals_away, by = c('season', 'team')) %>% 
  mutate(goals_sum = goals_sum_away + goals_sum_home)

get_ratios_plot <- function(chosen_team, chosen_season){
  
  get_points_for_team <- function(team, home_team, away_team, home_goals, away_goals){
    ifelse(home_team == team, 
           ifelse(home_goals > away_goals, 3, ifelse(home_goals == away_goals, 1, 0))
          ,ifelse(home_goals > away_goals, 1, ifelse(home_goals == away_goals, 1, 3)))
  }
  
  ratios <- dat %>%
    filter(season == chosen_season) %>%
    filter(home_team == chosen_team | away_team == chosen_team) %>%
    mutate(home_char = as.character(home_team), away_char = as.character(away_team)) %>%
    mutate(opponent = ifelse(chosen_team == home_char, away_char, home_char)) %>%
    mutate(chosen_team_points = factor(get_points_for_team(chosen_team, home_team, away_team, home_team_goal, away_team_goal))) %>% 
    mutate(result = ifelse(chosen_team_points == 3, 'win', ifelse(chosen_team_points == 1,'tie','lost'))) %>% 
    select(result, opponent) %>% 
    group_by(result, opponent) %>% 
    summarise(occurence = n())
  
  # super weird way to order them, or maybe super smart?
  # but it works 
  
  team_order0 <- ratios %>% 
    filter(result == 'win' ) %>% 
    arrange(desc(occurence)) %>% 
    pull(opponent) %>% 
    as.character()
  
  team_order1 <- ratios %>% 
    filter(result == 'tie' ) %>% 
    arrange(desc(occurence)) %>% 
    pull(opponent) %>% 
    as.character()
  
  team_order2 <- ratios %>% 
    filter(result == 'lost' ) %>% 
    arrange(occurence) %>% 
    pull(opponent) %>% 
    as.character()
  
  team_order <- unique(c(team_order0, team_order1, team_order2))
  
  ratios %>% 
    ggplot(aes(x = factor(opponent, levels = team_order), fill = result)) +
    geom_bar(position = 'fill') +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
    
}

get_ratios_plot('Aston Villa', '2015/2016')


