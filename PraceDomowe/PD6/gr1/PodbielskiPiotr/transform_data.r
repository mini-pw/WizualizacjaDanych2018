library(jsonlite)
library(dplyr)
library(tidyr)
#library(stringr)

count_points.function <- function(winner, for_team) {
  if (winner == "DRAW") {
    return(1)
  }
  else if (winner == for_team) {
    return(3)
  }
  else {
    return(0)
  }
}

result <- fromJSON("result.json")
matches <- result$matches


get_laliga_accumulated_results <- function(laliga_matches) {
  matchday <- laliga_matches$matchday
  winner <- sapply(laliga_matches$score$winner, function(x) ifelse(is.null(x), NA, x))
  hometeam <- laliga_matches$homeTeam$name
  awayteam <- laliga_matches$awayTeam$name
  
  laliga_results <- data.frame(matchday, winner, hometeam, awayteam)
  
  laliga_results <- laliga_results %>%
    drop_na() %>%
    mutate(homeTeam.points = mapply(count_points.function, winner, "HOME_TEAM"),
           awayTeam.points = mapply(count_points.function, winner, "AWAY_TEAM")) %>%
    select(-winner)
  
  laliga_results_host <- laliga_results %>%
    select(matchday, hometeam, homeTeam.points) %>%
    rename(team = hometeam,
           points = homeTeam.points)
  
  laliga_results_guest <- laliga_results %>%
    select(matchday, awayteam, awayTeam.points) %>%
    rename(team = awayteam,
           points = awayTeam.points)
  
  laliga_results_per_matchday <- bind_rows(laliga_results_host, laliga_results_guest)
  
  laliga_results_accumulated <- laliga_results_per_matchday %>%
    group_by(team) %>%
    arrange(matchday) %>%
    mutate(total_points = cumsum(points)) %>%
    ungroup() %>%
    select(-points)
  
  laliga_results_accumulated
}

laliga_results_accumulated <- get_laliga_accumulated_results(matches)

write.table(laliga_results_accumulated, file = "results.csv", sep = ",", quote = FALSE)
