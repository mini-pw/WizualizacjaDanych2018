library(ggplot2)

#' Plots the win percentages of all teams
#' over the course of the last two seasons
#'
#' @param team_win_percentage data.table containing
#' win percentages for each team for each season
#' separately
#'
plot_win_percentage <- function(team_win_percentage) {
  team_order2015 <- team_win_percentage[order(-season, -win_percentage), team] %>%
    unique()

  ggplot(
    team_win_percentage,
    aes(x = factor(team, levels = rev(team_order2015)), y = win_percentage, fill = season)
  ) +
    geom_col(position = "dodge") +
    xlab("Team") +
    ylab("Win Percentage") +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
}


#' Plots the current winning, drawing, losing streaks
#'
#' @param team_streaks data.table containing information
#' about the length of the current teams streak and whether
#' the streak is a winning, drawing or losing one
#'
plot_streaks <- function(team_streaks) {
  team_order <- team_streaks[order(streak_value, streak_length), team]

  plot_data <- team_streaks[,
    .(
      team = factor(team, levels = team_order),
      streak_length = streak_length,
      streak_value = streak_value
    )
  ]

  ggplot(
    plot_data,
    aes(
      x = team,
      y = streak_length,
      fill = streak_value
    )
  ) +
    facet_wrap(streak_value ~ ., scales = "free_y") +
    geom_col() +
    xlab("Team") +
    ylab("Streak length") +
    coord_flip()
}

#' Plots the amount of goals over time of one team
#' against a selected opponent
#'
#' @param football_data_team_view data.table containing
#' football data from the teams perspective
#'
#' @param selected_team teams whose goals should be counted
#'
#' @param selected_opponent opponent against which goals
#' should be counted. If null then all goals of the selected_team
#' are included (default: NULL)
#'
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
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    ) +
    ylab("Goals scored")


  return(p)
}

#' Plots a heatmap of goal differences for the selected team
#' and the opponent team against all other teams in the league.
#'
#' The goal of this plot is to enable the comparison of the
#' performance of two selected teams against all other teams
#' e.g. we selected Arsenal and Chelsea, we want to see how
#' they both did against Manchaster and we can see that even though
#' Chelsea had a better performance against Manchester than Arsenal,
#' Arsenal still managed to win against chelsea.
#'
#' @param football_data_team_view data.table containing
#' football data from the teams perspective
#'
#' @param selected_team teams whose goals should be counted
#'
#' @param selected_opponent opponent against which goals
#' should be counted
#'
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

  ggplot(
    goal_differences,
    aes(
      x = team,
      y = opponent,
      fill = goal_difference
    )
  ) +
    geom_raster() +
    xlab("Team") +
    ylab("Opponent") +
    labs(fill = "Goal difference") +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
    coord_flip()
}

#' Creates a heat map visualizing goal differences
#' between all teams in the league.
#'
#' @football_data data.table containing original football
#' data
#'
plot_matches_heat_map <- function(football_data) {
  goal_differences <- football_data[,
    .(goal_difference = mean(home_team_goal - away_team_goal)),
    by = .(home_team, away_team)
  ]

  ggplot(
    goal_differences,
    aes(
      x = home_team,
      y = away_team,
      fill = goal_difference
    )
  ) +
    geom_raster() +
    xlab("Home team") +
    ylab("Road team") +
    labs(fill = "Goal difference") +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
}
