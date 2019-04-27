library(ggplot2)

plot_win_percentage <- function(team_win_percentage) {
  team_order2015 <- team_win_percentage[order(-season, -win_percentage), team] %>%
    unique()
  
  ggplot(team_win_percentage,
         aes(x = factor(team, levels = rev(team_order2015)), y = win_percentage, fill = season)) +
    geom_col(position = "dodge") +
    xlab("Team") +
    ylab("Win Percentage") +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
}


plot_streaks <- function(team_streaks) {
  team_order <- team_streaks[order(streak_value, streak_length), team]
  
  plot_data <- team_streaks[,
    .(team = factor(team, levels = team_order),
      streak_length = streak_length,
      streak_value = streak_value)
  ]
  
  ggplot(plot_data,
         aes(x = team,
             y = streak_length,
             fill = streak_value)) +
    facet_wrap(streak_value ~ ., scales = "free_y") +
    geom_col() +
    xlab("Team") +
    ylab("Streak length") +
    coord_flip()
}


plot_goals_over_time <- function(football_data_team_view,
                                 selected_team,
                                 selected_opponent = NULL) {
  
  team_goals <- football_data_team_view[
    team == selected_team
  ]
  
  if (!is.na(selected_opponent)) {
    team_goals <- team_goals[opponent == selected_opponent]
  }
  
  p <- ggplot(team_goals, aes(x = as.character(date), y = goals_scored)) +
    geom_col() +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank()) +
    ylab("Goals scored")
  
  
  return(p)
}

plot_goal_differences <- function(football_data_team_view,
                                  selected_team,
                                  selected_opponent) {
  
  team_goals <- football_data_team_view[
    team %in% c(selected_opponent, selected_team)
  ]
  
  goal_differences <- team_goals[,
    .(goal_difference = mean(goals_scored - goals_lost)),
    by = .(team, opponent)
  ] 
  
  ggplot(goal_differences,
         aes(x = team,
             y = opponent,
             fill = goal_difference)) +
    geom_raster() +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
    coord_flip()
}

plot_matches_heat_map <- function(football_data) {
  
  goal_differences <- football_data[,
    .(goal_difference = mean(home_team_goal - away_team_goal)),
    by = .(home_team, away_team)
  ] 
  
  ggplot(goal_differences,
         aes(x = home_team,
             y = away_team,
             fill = goal_difference)) +
    geom_raster() +
    xlab("Home team") +
    ylab("Road team") +
    labs(fill = "Goal difference") +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
}
