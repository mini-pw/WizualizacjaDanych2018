# Football analysis
library(ggplot2)
library(dplyr)


# liczba goli w sezonie dla danej drużyny
plot_1 <- function(chosen_season, chosen_team){
  
  football <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")
  
  football$date <- as.Date(football$date)
  
  football$year <- as.numeric(format(football$date, format = "%Y"))
  football$month <- as.numeric(format(football$date, format = "%m"))
  football$day <- as.numeric(format(football$date, format = "%d"))
  
  home <- football %>% 
    filter(season == chosen_season & home_team == chosen_team) %>%
    select(home_team, home_team_goal, year, month) %>% 
    group_by(year,month) %>% 
    summarise(total = sum(home_team_goal)) 
  
  away <- football %>% 
    filter(season == chosen_season & away_team == chosen_team) %>%
    select(home_team, away_team_goal, year, month) %>% 
    group_by(year,month) %>% 
    summarise(total = sum(away_team_goal)) 
  
  if(nrow(home)==0 & nrow(away)==0){
    
    ggplot() + 
      labs(title = "Chosen team did not appear in chosen season", x = "Stage", y = "Points") +
      theme_bw() 
  }
  else {
    
    goals <- merge(home,away,by=c('year','month'))
    goals$total <- goals$total.x + goals$total.y
    goals$date <- with(goals, sprintf("%d-%02d", year, month))
    goals <- goals[c('date','total')]
    
    ggplot(data=goals, aes(x=date, y=total,group=1)) +
      geom_line(color='light coral',size=1.5) +
      geom_point(color='brown') +
      labs(x = "Date", y = "Goals") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
}

count_goals <- function(football_data,chosen_season, chosen_team)
{
  home <- football_data %>% 
    filter(season == chosen_season & home_team == chosen_team) %>%
    select(home_team, home_team_goal, year, month) %>% 
    group_by(year,month) %>% 
    summarise(total = sum(home_team_goal)) 
  
  away <- football_data %>% 
    filter(season == chosen_season & away_team == chosen_team) %>%
    select(home_team, away_team_goal, year, month) %>% 
    group_by(year,month) %>% 
    summarise(total = sum(away_team_goal)) 
  
  goals <- merge(home,away,by=c('year','month'))
  
  if(nrow(goals)!=0){
    goals$team_name <- chosen_team
    goals$date <- with(goals, sprintf("%d-%02d", year, month))
    
    goals <- goals %>% 
      rename(
        total_home = total.x,
        total_away = total.y
      )
    
    goals <- goals[c('date','total_home','total_away','team_name')]
    
    return(goals)
  }
  
}


# wykres dla wszystkich - liczba goli na wyjezdzie i u siebie
plot_2 <- function(chosen_season) {
  
  football <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")
  
  football$date <- as.Date(football$date)
  
  football$year <- as.numeric(format(football$date, format = "%Y"))
  football$month <- as.numeric(format(football$date, format = "%m"))
  football$day <- as.numeric(format(football$date, format = "%d"))
  
  all_teams <- unique(football$home_team)
  
  result <- data.frame(date=as.Date(character()),
                       total_home=integer(), 
                       total_away=integer(), 
                       team_name=character(),
                       stringsAsFactors=FALSE) 
  
  for(team in all_teams) {
    temp <- count_goals(football,chosen_season,team)
    result <- rbind(result,temp)
  }
 
  home_away_goals <- result %>% 
    group_by(team_name) %>% 
    summarise(home_goals = sum(total_home),away_goals=sum(total_away)) 
  
  goals_for_plot <- melt(home_away_goals[,c('team_name','home_goals','away_goals')],id.vars = 1)
  
  ggplot(goals_for_plot,aes(x = team_name,y = value)) + 
    geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
    labs(x = "Team", y = "Goals") +
    ylim(0,50) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}



# wykres dla wszystkich drużyn - która miała najwięcej zwycięstw w danym sezonie
plot_3 <- function(chosen_season) {
  
  football <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")
  
  who_won_df <- football %>% 
    select(season,home_team_goal,away_team_goal,home_team,away_team)
  
  who_won_df = within(who_won_df, {
    home_team_win = ifelse(home_team_goal > away_team_goal, 1, 0)
  })
  
  
  results_1 <- who_won_df %>% 
    filter(season==chosen_season) %>% 
    group_by(home_team) %>% 
    summarise(total = sum(home_team_win)) 
  
  ggplot(results_1,aes(x = reorder(home_team,-total),y = total)) + 
    geom_bar(stat = "identity",position = "dodge",fill="light coral") +
    labs(title = paste("Matches won in season ",chosen_season), x = "Team", y = "Matches won") +
    ylim(0,15) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}

# wykres dla wybranej drużyny - ile razy wygrali a ile razy przegrali z każdą pozostałą
plot_4 <- function(chosen_team){
  
  football <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")
  
  who_won_df <- football %>% 
    select(season,home_team_goal,away_team_goal,home_team,away_team)
  
  who_won_df <- who_won_df %>%
    mutate(home_team_win = case_when(home_team_goal > away_team_goal ~ 1,
                                     home_team_goal < away_team_goal ~ -1,
                                     home_team_goal == away_team_goal ~ 0))
  
  won_matches_1 <- who_won_df %>% 
    filter(home_team==chosen_team & home_team_win==1) %>% 
    group_by(away_team) %>% summarise(won=n())
  
  won_matches_2 <- who_won_df %>% 
    filter(away_team==chosen_team & home_team_win==-1) %>% 
    group_by(home_team) %>% summarise(won=n()) %>% 
    rename(
      away_team = home_team
    )
  
  lost_matches_1 <- who_won_df %>% 
    filter(home_team==chosen_team & home_team_win==-1) %>% 
    group_by(away_team) %>% summarise(lost=n())
  
  lost_matches_2 <- who_won_df %>% 
    filter(away_team==chosen_team & home_team_win==1) %>% 
    group_by(home_team) %>% summarise(lost=n()) %>% 
    rename(
      away_team = home_team
    )
  
  result_1 <-merge(won_matches_1,lost_matches_1,all = TRUE)
  result_2 <-merge(won_matches_2,lost_matches_2,all = TRUE)
  result <- merge(result_1,result_2,all = TRUE)
  
  result[is.na(result)] <- 0
  
  result <- result %>% 
    group_by(away_team) %>% 
    summarise(won = sum(won), lost = sum(lost))
  
  goals_for_plot <- melt(result[,c('away_team','won','lost')],id.vars = 1)
  
  ggplot(goals_for_plot,aes(x = away_team,y = value)) + 
    geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
    labs(x = "Team", y = "Matches") +
    #ylim(0,2) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}


prepare_plot_5 <- function(data,chosen_season,chosen_team) {
  
  only_chosen_team <- data %>% 
    filter((home_team==chosen_team | away_team==chosen_team) & season==chosen_season) %>% 
    select(stage,home_team,away_team,home_team_win,away_team_win)
  
  only_chosen_team <- only_chosen_team[with(only_chosen_team, order(stage)), ]
  
  only_chosen_team$summary <- 0
  sum = 0
  
  for(i in 1:nrow(only_chosen_team)) {
    
    if(only_chosen_team$home_team[i] == chosen_team){
      sum = sum + only_chosen_team$home_team_win[i]
    } else {
      sum = sum + only_chosen_team$away_team_win[i]
    }
    only_chosen_team$summary[i] = sum
    
  }
  
  return(only_chosen_team)
}


# tendencja w kolejnych kolejkach
plot_5 <- function(chosen_season, chosen_team,chosen_team_2){
  
  football <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")
  
  who_won_df <- football %>% 
    select(season,stage,home_team_goal,away_team_goal,home_team,away_team) %>% 
    filter(season==chosen_season)
  
  if(chosen_team %in% who_won_df$home_team & chosen_team_2 %in% who_won_df$away_team 
     & chosen_team_2 %in% who_won_df$home_team & chosen_team %in% who_won_df$away_team) {
    
    who_won_df <- who_won_df %>%
      mutate(home_team_win = case_when(home_team_goal > away_team_goal ~ 3,
                                       home_team_goal < away_team_goal ~ 0,
                                       home_team_goal == away_team_goal ~ 1)) %>% 
      mutate(away_team_win = case_when(home_team_goal < away_team_goal ~ 3,
                                       home_team_goal > away_team_goal ~ 0,
                                       home_team_goal == away_team_goal ~ 1))
    
    only_chosen_team_1 <- prepare_plot_5(who_won_df,chosen_season,chosen_team)
    only_chosen_team_2 <- prepare_plot_5(who_won_df,chosen_season,chosen_team_2)
    
    result <- merge(only_chosen_team_1,only_chosen_team_2,by='stage')
    
    result <- result %>% 
      rename(
        main_team = summary.x,
        second_team = summary.y
      )
    
    result <- result[c('stage','main_team','second_team')]
    
    result_for_plot <- melt(result[,c('stage','main_team','second_team')],id.vars = 1)
    
    ggplot(result_for_plot,aes(x = stage,y = value,color=variable)) + 
      geom_line(size=1.5) + 
      scale_color_discrete(name = "", labels = c("Main Team", "Second Team")) +
      labs(x = "Stage", y = "Points") +
      theme_bw() +
      theme(legend.position = "bottom")
  }
  else {
    ggplot() + 
      labs(title = "Chosen team did not appear in chosen season", x = "Stage", y = "Points") +
      theme_bw() 
  }
  
}



func_1 <- function(argument,chosen_season) {
  
  football <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")
  
  football$date <- as.Date(football$date)
  
  football$year <- as.numeric(format(football$date, format = "%Y"))
  football$month <- as.numeric(format(football$date, format = "%m"))
  football$day <- as.numeric(format(football$date, format = "%d"))
  
  all_teams <- unique(football$home_team)
  
  result <- data.frame(date=as.Date(character()),
                       total_home=integer(), 
                       total_away=integer(), 
                       team_name=character(),
                       stringsAsFactors=FALSE) 
  
  for(team in all_teams) {
    temp <- count_goals(football,chosen_season,team)
    result <- rbind(result,temp)
  }
  
  home_away_goals <- result %>% 
    group_by(team_name) %>% 
    summarise(home_goals = sum(total_home),away_goals=sum(total_away)) 
  
  if(argument=="home"){
    i = which(home_away_goals$home_goals == max(home_away_goals$home_goals), arr.ind = TRUE)  
    return(paste0(home_away_goals$team_name[i],": ",home_away_goals$home_goals[i]))
  } 
  if(argument=="away") {
    i = which(home_away_goals$away_goals == max(home_away_goals$away_goals), arr.ind = TRUE)
    return(paste0(home_away_goals$team_name[i],": ",home_away_goals$away_goals[i],"\n"))
  }
  if(argument=="total") {
    i = which(home_away_goals$home_goals+home_away_goals$away_goals == max(home_away_goals$home_goals+home_away_goals$away_goals), arr.ind = TRUE)
    return(paste0(home_away_goals$team_name[i],": ",home_away_goals$home_goals[i]+home_away_goals$away_goals[i]))
  }
}

