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
    arrange(cumul_points)
  
  which(ordered$team == desired_team)
}

# test
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

# data for annotations
final_positions <- position_in_table %>%
  filter(season == "2015/2016" & stage == last_stage)

position_in_table %>%
  filter(season == "2015/2016") %>%
  mutate(selection = (team == 'Arsenal')) %>% 
  ggplot(aes(x = stage, y = position, color = team, alpha = selection, label = team)) +
  geom_line() +
  scale_alpha_discrete(range = c(0.35,1), guide = FALSE) +
  annotate("text", x= 38 + 2, y = final_positions[['position']], label = final_positions[['team']]) +
  theme_minimal() +
  xlim(c(0,45))

# ratios plot
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
    mutate(result = ifelse(chosen_team_points == 3, 'won', ifelse(chosen_team_points == 1,'tied','lost'))) %>% 
    select(result, opponent) %>% 
    group_by(result, opponent) %>% 
    summarise(occurence = n())
  
  # super weird way to order them, or maybe super smart?
  # but it works 
  
  team_order0 <- ratios %>% 
    filter(result == 'won' ) %>% 
    arrange(desc(occurence)) %>% 
    pull(opponent) %>% 
    as.character()
  
  team_order1 <- ratios %>% 
    filter(result == 'tied' ) %>% 
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
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(paste("Match result statistics for team: ", chosen_team)) +
    xlab('opponent') +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank())
    
    
}

# test
get_ratios_plot('Aston Villa', '2015/2016')


