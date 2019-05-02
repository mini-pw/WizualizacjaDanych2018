library(shiny)
library(jsonlite)
library(dplyr)
library(tidyr)
library(httr)
library(ggplot2)
library(stringr)

# get yours at https://www.football-data.org/client/login
API_KEY <- ""

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

load_laliga_matches <- function(API_KEY) {
  if (str_length(API_KEY) == 0) {
    print("Loading from local file.")
    result = fromJSON("result.json")
  }
  else {
    print("Downloading from internet.")
    url <- paste0("http://api.football-data.org/v2/competitions/2014/matches?season=2018")
    httpResponse <- GET(url, add_headers("X-Auth-Token" = API_KEY), accept_json())
    result = fromJSON(content(httpResponse, "text"))
    write(content(httpResponse, "text"), "result.json")
  }
  
  return(result$matches)
}

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

laliga_matches <- load_laliga_matches(API_KEY)
laliga_results_accumulated <- get_laliga_accumulated_results(laliga_matches)

ui <- fluidPage(
  titlePanel("Piotr Podbielski's Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_team",
                         label = "Select teams:",
                         choices = unique(laliga_results_accumulated[with(laliga_results_accumulated, order(team)), ][["team"]]),
                         selected = unique(laliga_results_accumulated[["team"]]))
    ),
    mainPanel(
      h2("Plot"),
      plotOutput("laliga_plot", height = 600, hover = hoverOpts(id="laliga_hover", delay=100)),
      h2("Information about hover"),
      tableOutput("laliga_table")
    )
  )
)

server <- function(input, output) {
  
  
  max_points <- max(laliga_results_accumulated$total_points)
  max_matchday <- max(laliga_results_accumulated$matchday)

  c25 <- c("dodgerblue2","#E31A1C", # red
           "green4",
           "#6A3D9A", # purple
           "#FF7F00", # orange
           "black","gold1",
           "skyblue2","#FB9A99", # lt pink
           "palegreen2",
           "#CAB2D6", # lt purple
           "#FDBF6F", # lt orange
           "gray70", "khaki2",
           "maroon","orchid1","deeppink1","blue1","steelblue4",
           "darkturquoise","green1","yellow4","yellow3",
           "darkorange4","brown")

  myColors <- c25[1:length(unique(laliga_results_accumulated$team))]
  names(myColors) <- levels(laliga_results_accumulated$team)
  colScale <- scale_colour_manual(name = "team", values = myColors)

  laliga_results_accumulated_r <- reactive({
    filter(laliga_results_accumulated, team %in% input[["chosen_team"]])
  })

  output[["laliga_plot"]] <- renderPlot({
    my_plot <- ggplot(data = laliga_results_accumulated_r(), aes(matchday, total_points, colour = team)) + # Skąd i co rysować
      geom_point() + # Rysuj punkty
      geom_line(alpha=0.4) + # Rysuj linie
      scale_x_continuous(breaks = seq(1, max_matchday, by=1), limits=c(1, max_matchday)) + # Oś X podpisana co 1
      scale_y_continuous(breaks = seq(0, max_points, by=10), limits=c(0, max_points)) +
      colScale +
      labs(title="Points for teams in La Liga", x="Match day", y="Total points", color="Team") + # Zmiana oznaczeń osi i legendy
      theme_bw() # Wybór stylu

    my_plot
  })


  output[["laliga_table"]] <- renderTable({
    shiny::validate(
      need(input[["laliga_hover"]], message = FALSE)
    )
    output <- nearPoints(laliga_results_accumulated_r(), input[["laliga_hover"]], threshold = 3)

    shiny::validate(
      need(nrow(output) > 0, message = FALSE)
    )

    output
  })
}

shinyApp(ui = ui, server = server)
