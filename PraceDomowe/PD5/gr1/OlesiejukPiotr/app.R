library(shinydashboard)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

ui <- dashboardPage(
  dashboardHeader(title = "Soccer assistant"),
  dashboardSidebar(
    sidebarMenu(
      tags$script(HTML("$('body').addClass('fixed');")),
      title = "Analysis panel",
      selectInput("season", label = "Season", choices = c("2014/2015", "2015/2016"), width = 220),
      selectInput("team", label = "Team", choices = c("Arsenal","Aston Villa","Bournemouth","Burnley","Chelsea","Crystal Palace","Everton",
                                                      "Hull City","Leicester City","Liverpool","Manchester City","Manchester United",
                                                      "Newcastle United","Norwich City","Queens Park Rangers","Southampton","Stoke City","Sunderland",
                                                      "Swansea City", "Tottenham Hotspur","Watford","West Bromwich Albion", "West Ham United" ),
                  width = 220),
      width = 3,
      height = 450
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Goals",
        status = 'warning',
        solidHeader = TRUE,
        plotOutput("goalsPlot"),
        width = 9
      ),
      box(
        title = "Team Statistics",
        status = "primary",
        solidHeader = TRUE,
        plotOutput("statisticsPlot"),
        width = 3
      )
    ),
    fluidRow(
      box(
        title = "Points",
        status = "info",
        solidHeader = TRUE,
        plotOutput("pointsPlot"),
        width = 8,
        height = 550
      ),
      box(
        title = "Comparison",
        status = "info",
        solidHeader = TRUE,
        selectInput("opponent", label = "Opponent", choices = c("Arsenal","Aston Villa","Bournemouth","Burnley","Chelsea","Crystal Palace","Everton",
                                                                "Hull City","Leicester City","Liverpool","Manchester City","Manchester United",
                                                                "Newcastle United","Norwich City","Queens Park Rangers","Southampton","Stoke City","Sunderland",
                                                                "Swansea City", "Tottenham Hotspur","Watford","West Bromwich Albion", "West Ham United" ),
                    selected = "Manchester City"),
        plotOutput("comparisonPlot"),
        width = 4,
        height = 550
      )
    ),
    fluidRow(
      box(
        title = "Detailed performance",
        status = "info",
        solidHeader = TRUE,
        plotOutput("detailedPerformancePlot"),
        width = 12
      )
    )
  )
)

server <- function(input, output) {
  
  dat <- read.table("PL_dat.csv", sep = ';', header = TRUE)
  dat$date <- sapply(dat$date, function(d) { strsplit(as.character(d), " ")[[1]][1]})
  
  output$goalsPlot <- renderPlot(
    {
      season_to_filter<- input[["season"]]
      goals_home <- dat %>% filter(season == season_to_filter) %>% group_by(home_team) %>% 
        summarize(scored_home = sum(home_team_goal), 
                  lost_home = sum(away_team_goal)) %>% data.frame
      
      goals_away <- dat %>% filter(season == season_to_filter) %>% 
        group_by(away_team) %>% summarize(scored_away = sum(away_team_goal),
                                          lost_away = sum(home_team_goal)) %>% data.frame
      
      summary_table <- cbind(goals_home, goals_away[-1])
      summary_table <- summary_table %>% mutate(scored_sum = scored_home + scored_away,
                                                lost_sum = lost_home + lost_away)
      summary_table$home_team <- reorder(summary_table$home_team, summary_table$scored_sum, sort)
      head(summary_table)
      
      p <- ggplot(data = summary_table, aes(x = home_team, y = scored_sum, fill = home_team, label = home_team)) + geom_col() +
        theme_test() +
        ylab(NULL) +
        scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Spectral"))(nrow(summary_table))) +
        ggtitle(paste0("Goals scored in season ", season_to_filter)) +
        theme(axis.text.x = element_blank(), 
              axis.title.x = element_blank(), 
              axis.ticks.x = element_blank(), 
              plot.title = element_text(hjust = 0.5, size = 17),
              legend.position = "none", panel.border = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_text(size = 14),
              panel.grid.major = element_line(color = "#f0f0f0")) +
        geom_text(angle = 90, y = 1, hjust = 0, size = 5, color = "#252525")
      p
    })
  output$pointsPlot <- renderPlot(
    {
      team_points <- input[["team"]]
      season_points <- input[["season"]]
      
      dat_points <- dat %>% filter(season == season_points) %>% filter(home_team == team_points | away_team == team_points) %>%
        mutate(home = if_else(home_team == team_points, TRUE, FALSE)) %>%
        mutate(points = 0)
      
      points_fun <- function(x, y) {
        if (x == y) {return(1)}
        else if (x < y) {return(0)}
        else return(3)
      }
      
      for (i in seq(dat_points$points)) {
        if (dat_points$home[i] == TRUE) {
          dat_points$points[i] <- points_fun(dat_points$home_team_goal[i], dat_points$away_team_goal[i])
        }
        else
        {
          dat_points$points[i] <- points_fun(dat_points$away_team_goal[i], dat_points$home_team_goal[i])
        }
      }
      
      dat_points <- dat_points[order(dat_points$stage) ,]
      
      date_fun <- function(d) {
        d <- substr(d, start = 6, stop = 10)
        return(d)
      }
      
      dat_points$date <- sapply(dat_points$date, FUN = date_fun)
      head(dat_points)
      
      cols <- rep('Away', nrow(dat_points))
      cols[which(dat_points$home == TRUE)] <- 'Home'
      dat_points <- cbind(dat_points, cols)
      #browser()
      colors = rep("#fec44f", nrow(dat_points))
      colors[which(dat_points$home == TRUE)] = "#2ca25f"
      #browser()
      p <- ggplot(data = dat_points, aes(x = stage, y = points, color = cols))  +
        ylim(c(0,3)) +
        theme_bw() +
        xlab("Stage") +
        ylab("Points") +
        ggtitle(paste0("History of ", team_points, " points in season ", season_points)) +
        #https://stackoverflow.com/questions/16350720/using-geom-line-with-x-axis-being-factors
        geom_point( size = 5) +
        scale_color_manual(values = c("Home" = "#2ca25f", "Away" = '#fec44f')) +
        #geom_point(stat='summary', fun.y=sum, col = colors, size = 4) +
        #stat_summary(fun.y=sum, geom="line", col = "#43a2ca", size = 0.8) +
        theme(plot.title = element_text(hjust = 0.5, size = 17), 
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text  = element_text(size = 14),
              legend.position = "bottom", legend.title = element_blank(),
              legend.spacing.x = unit(1, 'cm'),
              legend.text = element_text(size = 13))
      
      p2 <- p + geom_line(aes(y = points), col = "#43a2ca", size = 1)
      p2
    }
  )
  output$statisticsPlot <- renderPlot(
    {
      team_points <- input[["team"]]
      season_points <- input[["season"]]
      
      dat_points <- dat %>% filter(season == season_points) %>% filter(home_team == team_points | away_team == team_points) %>%
        mutate(home = if_else(home_team == team_points, TRUE, FALSE)) %>%
        mutate(points = 0)
      
      points_fun <- function(x, y) {
        if (x == y) {return(1)}
        else if (x < y) {return(0)}
        else return(3)
      }
      
      for (i in seq(dat_points$points)) {
        if (dat_points$home[i] == TRUE) {
          dat_points$points[i] <- points_fun(dat_points$home_team_goal[i], dat_points$away_team_goal[i])
        }
        else
        {
          dat_points$points[i] <- points_fun(dat_points$away_team_goal[i], dat_points$home_team_goal[i])
        }
      }
      
      points <- dat_points$points
      
      wins <- sum(points == 3)
      draws <- sum(points == 1)
      loses <- sum(points == 0)
      
      df <- data.frame(value = c(wins, draws, loses),
                       result = c("wins", "draws", "loses"))
      
      plt <- ggplot(data = df, aes(x = result, y = value, fill = result, label = value)) + geom_col() + 
        geom_text(vjust = -0.5, size = 5) +
        theme(panel.background = element_rect(fill = "white"),
              axis.title.x = element_blank(), axis.title.y = element_blank(),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14),
              panel.grid.major = element_line("#f0f0f0"),
              legend.position = "none")
      
      plt
    }
  )
  output$detailedPerformancePlot <-  renderPlot(
    {
      t1 <- input[["team"]]
      season1 <- input[["season"]]
      
      dat_comparison <- dat %>% filter(season == season1) %>% 
        filter((home_team == t1 | away_team == t1)) %>%
        mutate(home = if_else(home_team == t1, TRUE, FALSE)) %>%
        mutate(points = 0)
      
      
      
      points_fun <- function(x, y) {
        if (x == y) {return(1)}
        else if (x < y) {return(0)}
        else return(3)
      }
      
      for (i in seq(dat_comparison$points)) {
        if (dat_comparison$home[i] == TRUE) {
          dat_comparison$points[i] <- points_fun(dat_comparison$home_team_goal[i], dat_comparison$away_team_goal[i])
        }
        else
        {
          dat_comparison$points[i] <- points_fun(dat_comparison$away_team_goal[i], dat_comparison$home_team_goal[i])
        }
      }
      
      dat_comparison <- dat_comparison[order(dat_comparison$stage) ,]
      n <- nrow(dat_comparison)
      
      goals_value <- numeric(2 * n)
      team_value <- character(2 * n)
      j = 1
      k = 1
      for (i in 1:(2*n)) {
        if (i %% 2 == 1) {
          goals_value[i] <- dat_comparison$home_team_goal[j]
          team_value[i] <- as.character(dat_comparison$home_team[j])
          if (dat_comparison$home_team[j] != t1)
          {goals_value[i] <- goals_value[i] * - 1}
          j = j + 1
        }
        else
        {
          goals_value[i] <- dat_comparison$away_team_goal[k]
          team_value[i] <- as.character(dat_comparison$away_team[k])
          if (dat_comparison$away_team[k] != t1)
          {goals_value[i] <- goals_value[i] * - 1}
          k = k + 1
        }
      }
      
      
      dat_comparison <- rbind(dat_comparison, dat_comparison)
      dat_comparison <- dat_comparison[order(dat_comparison$date), ]
      dat_comparison$home_team_goal <- goals_value
      dat_comparison$away_team_goal <- team_value
      colnames(dat_comparison)[4] <- "Goals_value"
      colnames(dat_comparison)[5] <- "Team_scored"
      dat_comparison$Team_scored[dat_comparison$Team_scored != t1] <- "Opponent"
      
      plt <- ggplot(data = dat_comparison, aes(x = date, y = Goals_value, fill = Team_scored)) + 
        geom_bar(stat = "identity", width = 0.2) +
        theme_test() + 
        labs(x = "Stage") +
        labs(y = "Goals scored") + 
        ggtitle(paste0("History of ", t1, " games in season ", season1)) +
        scale_fill_manual(values = c("#2c7fb8","#d95f0e")) + #c("#2ca25f", "#de2d26")) + 
        scale_x_discrete(breaks = unique(dat_comparison$date), labels = as.character(seq(n))) +
        theme(legend.title = element_blank(), legend.text = element_text(size = 14), 
              plot.title = element_text(hjust = 0.5, size = 18),
              axis.text = element_text(size = 14),
              panel.border = element_blank(),
              axis.title = element_text(size = 15), panel.grid.major = element_line(color = "#f0f0f0"))
      plt
    }
  )
  output$comparisonPlot <- renderPlot(
    {
      t1 <- input[["team"]]
      t2 <- input[["opponent"]]
      season1 <- input[["season"]]
      
      dat_comparison <- dat %>% filter(season == season1) %>% 
        filter((home_team == t1 & away_team == t2) | 
                 (home_team == t2 & away_team == t1)) %>%
        mutate(home = if_else(home_team == t1, TRUE, FALSE)) %>%
        mutate(points = 0)
      
      
      
      points_fun <- function(x, y) {
        if (x == y) {return(1)}
        else if (x < y) {return(0)}
        else return(3)
      }
      
      for (i in seq(dat_comparison$points)) {
        if (dat_comparison$home[i] == TRUE) {
          dat_comparison$points[i] <- points_fun(dat_comparison$home_team_goal[i], dat_comparison$away_team_goal[i])
        }
        else
        {
          dat_comparison$points[i] <- points_fun(dat_comparison$away_team_goal[i], dat_comparison$home_team_goal[i])
        }
      }
      
      dat_comparison <- dat_comparison[order(dat_comparison$stage) ,]
      n <- nrow(dat_comparison)
      
      goals_value <- numeric(2 * n)
      team_value <- character(2 * n)
      j = 1
      k = 1
      for (i in 1:(2*n)) {
        if (i %% 2 == 1) {
          goals_value[i] <- dat_comparison$home_team_goal[j]
          team_value[i] <- as.character(dat_comparison$home_team[j])
          if (dat_comparison$home_team[j] == t2)
          {goals_value[i] <- goals_value[i] * - 1}
          j = j + 1
        }
        else
        {
          goals_value[i] <- dat_comparison$away_team_goal[k]
          team_value[i] <- as.character(dat_comparison$away_team[k])
          if (dat_comparison$away_team[k] == t2)
          {goals_value[i] <- goals_value[i] * - 1}
          k = k + 1
        }
      }
      
      
      dat_comparison <- rbind(dat_comparison, dat_comparison)
      dat_comparison <- dat_comparison[order(dat_comparison$date), ]
      dat_comparison$home_team_goal <- goals_value
      dat_comparison$away_team_goal <- team_value
      colnames(dat_comparison)[4] <- "Goals_value"
      colnames(dat_comparison)[5] <- "Team_scored"
      lab <- rep("HOME", n / 2)
      lab <- c(lab, rep("AWAY", n / 2))
      heigths <- dat_comparison$Goals_value[dat_comparison$Team_scored == t1] + 0.5
      
      plt <- ggplot(data = dat_comparison, aes(x = date, y = Goals_value, fill = Team_scored)) + 
        geom_bar(stat = "identity", width = 0.2) +
        theme_test() + 
        xlab("Match date") +
        ylab("Goals scored") +
        ggtitle(paste0("History of ", t1, " vs. ", t2, " in season ", season1)) +
        scale_fill_manual(values = c("#2ca25f", "#de2d26")) + 
        scale_y_continuous(breaks = seq(-4,4), labels = as.character(c(seq(4,0), seq(1,4)))) +
        theme(legend.title = element_blank(), legend.text = element_text(size = 14), 
              plot.title = element_text(size = 14),
              axis.text = element_text(size = 14), panel.border = element_blank(),
              axis.title = element_text(size = 15), axis.text.x = element_text(size = 14),
              legend.position = "bottom", legend.spacing.x = unit(1.0, 'cm'),
              panel.grid.major = element_line(color = "#f0f0f0")) +
        annotate("text", x = seq(n), y = heigths, label = lab)
      plt
    }
  )
}

shinyApp(ui = ui, server = server)