library(data.table)

football_data <- fread("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

#' Transforms the football data to make it more from
#' a teams perspective. All of the matcher are doubled in
#' order to always have a single perspective of the match e.g.
#'
#' match machester - arsenal in the original data offered
#' two different perspectives
#'
#' This function transforms this specific record into
#' two records:
#'
#' manchester - arsenal - home
#' arsenal - manchester - away
#'
#' @football_data data.table containing original football
#' data
#'
#' @return data.table with a view of the football data
#' from the teams perspective
#'
transform_to_team_view <- function(football_data) {
  home_team_matches <- football_data[, .(
    team = home_team,
    opponent = away_team,
    date = as.Date(date),
    goals_scored = home_team_goal,
    goals_lost = away_team_goal,
    season = season,
    stage = stage,
    match_type = "home"
  )]

  away_team_matches <- football_data[, .(
    team = away_team,
    opponent = home_team,
    date = as.Date(date),
    goals_scored = away_team_goal,
    goals_lost = home_team_goal,
    season = season,
    stage = stage,
    match_type = "away"
  )]

  return(rbind(home_team_matches, away_team_matches))
}


#' Calculates match results based ont football team view data
#'
#' @param football_data_team_view data.table containing
#' football data from the teams perspective
#'
#' @return data.table with the `result` column containg
#' `win`/`lost`/`draw` values
#'
calculate_match_results <- function(football_data_team_view) {
  get_match_result <- function(goals_scored, goals_lost) {
    ifelse(
      goals_scored > goals_lost,
      "win",
      ifelse(goals_scored == goals_lost, "draw", "lost")
    )
  }

  match_results <- football_data_team_view[, .(
    team = team,
    opponent = opponent,
    date = date,
    season = season,
    result = get_match_result(goals_scored, goals_lost)
  )]

  return(match_results)
}

#' Calculates the current streaks of all teams
#'
#' @param football_data_team_view data.table containing
#' football data from the teams perspective
#'
#' @return data.table with the `streak_length` and
#' `streak_value` columns which signify how long the
#' current streak is and whether the streak is
#' a winning/drawing/losing one.
#'
calculate_streak <- function(football_data_team_view) {
  get_last_streaks <- function(football_data_match_results) {
    football_streaks <- football_data_match_results[order(date)][,
      .(
        streak_length = rle(result)$lengths %>% tail(1),
        streak_value = rle(result)$values %>% tail(1)
      ),
      by = team
    ]

    football_streaks$streak_value <- factor(
      football_streaks$streak_value,
      c("win", "draw", "lost")
    )

    return(football_streaks)
  }

  football_data_result_streaks <- football_data_team_view %>%
    calculate_match_results() %>%
    get_last_streaks()

  return(football_data_result_streaks)
}

#' Calculates the record of the selected team against the
#' selected opponent.
#'
#' @param football_data_team_view data.table containing
#' football data from the teams perspective
#'
#' @param selected_team selected team for which the
#' record should be calculated
#'
#' @param selected_opponent opponents against which
#' the record shoudl be calculated
#'
#' @return list with wins/draw/losses/amounts
#'
calculate_win_loss_amounts <- function(football_data_team_view,
                                       selected_team,
                                       selected_opponent) {
  match_results <- calculate_match_results(
    football_data_team_view[
      team == selected_team &
        opponent == selected_opponent
    ]
  )

  list(
    wins = sum(match_results$result == "win"),
    draws = sum(match_results$result == "draw"),
    losses = sum(match_results$result == "lost")
  )
}

#' Calculates the winning percentages of all teams
#' for each season separately.
# '
calculate_win_percentage <- function(football_data_team_view) {
  match_results <- calculate_match_results(football_data_team_view)

  get_win_percentage <- function(result) {
    (sum(result == "win") + 0.5 * sum(result == "draw")) / length(result)
  }

  win_percentage <- match_results[,
    .(win_percentage = get_win_percentage(result)),
    by = .(team, season)
  ][CJ(
    team = unique(football_data_team_view$team),
    season = unique(football_data_team_view$season)
  ),
  on = .(team, season)
  ]

  return(win_percentage)
}
