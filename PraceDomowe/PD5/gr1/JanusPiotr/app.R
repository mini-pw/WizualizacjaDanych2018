library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(ggthemes)

dat <- read.csv2("https://raw.githubusercontent.com/michbur/soccer-data/master/PL_dat.csv")

dat <- dat %>% mutate(date = as.Date(date))

total_goal <- rbind(select(dat, season, team_goal = home_team_goal, 
                           team_name = home_team), 
                    select(dat, season, team_goal = away_team_goal, 
                           team_name = away_team)) %>% 
  group_by(season, team_name) %>% 
  summarise(total_goals = sum(team_goal)) 

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Piłko nożex 2000"),
  dashboardSidebar(
    sidebarUserPanel(Sys.info()[["effective_user"]],
                     subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
    ),
    sidebarMenu(
      id = "tabs",
      menuItem("Podsumowanie", tabName = "summary", icon = icon("dashboard")),
      menuItem("Kluby", tabName = "clubs", icon = icon("info-circle")),
      menuItem("Pojedynki", tabName = "versus", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("summary",
              selectInput(inputId = "selected_season", label = "Wybierz sezon", 
                          choices = levels(dat[["season"]])),
              fluidRow(column(6,box(width = 12,plotOutput("total_goal_plot"), title = "Strzelone bramki", status = "primary", solidHeader = TRUE)),
                       column(6,box(width = 12,plotOutput("compare_plot"), title = "Mecze rozgrywane u siebie",status = "primary", solidHeader = TRUE))),
              fluidRow(box(width=12, plotOutput("best_teams"), status = "warning", solidHeader = TRUE))),
      tabItem("clubs",
              fluidRow(style = "height:50vh;",
                column(4,box(width =12,
                             selectInput(inputId = "selected_season_club", label = "Wybierz sezon", 
                                         choices = levels(dat[["season"]]),
                                         multiple = TRUE,
                                         selected = levels(dat[["season"]])),
                             selectInput(inputId = "clubs_list", label = "Wybierz klub", 
                              choices = levels(total_goal$team_name), selected = total_goal$team_name[1])
                             )),
                column(3,box(width = 12,
                       fluidRow(valueBoxOutput(width = 12,"wins")),
                       fluidRow(valueBoxOutput(width = 12,"loses")),
                       fluidRow(valueBoxOutput(width = 12,"goals")))),
                column(5,box(width = 12,
                             plotOutput('points_plot')))
                ),
              fluidRow(
                tabBox(
                  title = "Wyniki ostatnich spotkań", width = 12, id = "tabset1",
                  tabPanel("U siebie", dataTableOutput("last_matches_home")),
                  tabPanel("Na wyjeździe", dataTableOutput("last_matches_away"))
                  )
                )
              ),
      tabItem("versus",
              fluidRow(
                column(2, plotOutput("goals_club1")),
                column(4, align="center", box(width =12, selectInput(inputId = "club1", label = "Klub I",  
                                                   choices = levels(total_goal$team_name), selected = total_goal$team_name[1]))),

                column(4, align="center", box(width =12,selectInput(inputId = "club2", label = "Klub II", 
                                                   choices = levels(total_goal$team_name), selected = total_goal$team_name[2]))),
                column(2, plotOutput("goals_club2"))
              ),
              fluidRow(box(width=12,plotOutput("match_stats")))
              )
      
    )
  )
)

server <- function(input, output, session) {
  
  output$total_goal_plot <- renderPlot({
    team_order <- filter(total_goal, season == input[["selected_season"]]) %>% 
      arrange(desc(total_goals)) %>% 
      pull(team_name) %>% 
      as.character()
    
    full_team_order <- c(team_order,
                         setdiff(total_goal[["team_name"]], team_order))
    mutate(total_goal, 
           team_name = factor(team_name, levels = full_team_order)) %>%
      complete(season, nesting(team_name), fill = list(total_goals = 0)) %>% 
      ggplot(aes(x = team_name, y = total_goals, 
                 fill = season)) +
      geom_col(position = "dodge") + labs(y="", x='')+
      theme_tufte()+
      theme(axis.text.x=element_text(angle=45, hjust=1))+
      scale_fill_discrete(name = "Sezon")
  })
  observeEvent(input$club1, {
    updateSelectInput(session, "club2", choices = setdiff(levels(total_goal$team_name),input$club1), selected = input$club2)
  })
  observeEvent(input$club2, {
    updateSelectInput(session, "club1", choices = setdiff(levels(total_goal$team_name),input$club2), selected = input$club1 )
  })
  team_summary <- reactive({
    home <- dat %>% 
      mutate(win = (home_team_goal>away_team_goal)*3+(home_team_goal==away_team_goal)*1) %>% 
      group_by(season, home_team) %>% summarize(points = sum(win))
    away <- dat %>% 
      mutate(win = (home_team_goal<away_team_goal)*3+(home_team_goal==away_team_goal)*1) %>% 
      group_by(season, away_team) %>% summarize(points = sum(win))
    df <- merge(home, away, by=c(1,2)) %>% group_by(home_team) %>% mutate(total = points.x+points.y)
    names(df) <- c("season","team",'home', 'away','total')
    return(df)
  })
  
  compare_teams <- reactive({
    home <- dat %>% filter(season == input$selected_season) %>% 
      mutate(win = (home_team_goal>away_team_goal)*1+(home_team_goal==away_team_goal)*0.5) %>%  select(6,7,8) %>% mutate()
    wins_home <- home %>% filter(win == 1) %>%  summarize(wins = n())
    draws_home <- home %>% filter(win == 0.5) %>%  summarize(draws = n())
    loses_home <- home %>% filter(win == 0) %>% summarize(loses = n())
    summa <- cbind(wins_home, loses_home, draws_home)/ sum(wins_home, loses_home, draws_home)
    return(summa)
  })
  
  output$compare_plot <- renderPlot({
    df <- compare_teams()
    df <- data.frame(perc = t(df))
    brks <- seq(0, max(df$perc), length.out = 4)
    lbls = paste0(as.character(abs(brks)))
    row.names(df) <- c('zwycięstwa', 'porażki','remisy')
    ggplot(df, aes(x = row.names(df), y = perc ) )+
      geom_col(fill = '#90A9B7')+
      scale_y_continuous(limits=c(0,max(df$perc)+0.05),expand = c(0,0))+
      labs(y= "Odsetek wszystkich meczów",x='')+
      theme_bw()+
      theme(panel.grid.major.x = element_blank(), 
            axis.text.y = element_text(size=11),
            axis.title.y = element_text(size=13, face='bold'),
            axis.text.x = element_text(size=13, face='bold'),
            panel.border = element_blank(),
            axis.line = element_line(size = 0.7),
            axis.ticks.x = element_blank())
  })
  
  output$best_teams <- renderPlot({
    df <- team_summary() 
    df <- df[df$season == input$selected_season,]
    df <- df[order(df$total, decreasing = T),]
    df$team <- factor(df$team, levels = df$team)
    
    ggplot(df, aes(x = team, y = total))+
      geom_col(fill = '#0d0091', width = 0.8 )+
      scale_y_discrete(expand = c(0,0))+
      theme_bw() +
      geom_text(aes(label=total),
                colour ="White", fontface="bold", 
                size = 5, hjust =0.6,
                vjust = 1) +
      theme(panel.grid.major.x = element_blank(), 
            plot.title = element_text(face='bold', hjust = 0.5),
            axis.text.x = element_text(angle = 45,size=12, face='bold', hjust = 1),
            panel.border = element_blank(),
            axis.line = element_line(size = 0.7),
            axis.ticks.x = element_blank())+
      ggtitle(paste("Zdobyte punkty w sezonie",input$selected_season)) +
      labs(y="Punkty", x='Drużyna') 
  })
  
  output$last_matches_home <- renderDataTable({
    df <- dat %>% filter(home_team == input$clubs_list, season %in% input$selected_season_club) %>%  arrange(desc(date)) %>% 
      mutate(win = (home_team_goal>away_team_goal)*1+(home_team_goal==away_team_goal)*0.5,
             result = paste0(home_team_goal, ':',away_team_goal)) %>%  head(10)
    colnames(df) = c("Sezon","Kolejka", "Data", "home","away","Gospodarze", "Goście", "win", "Wynik")
    datatable(df, rownames = FALSE, options=list(columnDefs = list(list(visible=FALSE, targets=c(0,1,3,4,7) )),
                                                 dom = "t")) %>% formatStyle(
      'Wynik','win',
      backgroundColor = styleEqual(c(0, 1,0.5), c('rgb(255,100,100)', 'rgb(100,255,100)','orange'))
    )
  })
  
  output$last_matches_away <- renderDataTable({
    df <- dat %>% filter(away_team == input$clubs_list) %>%  arrange(desc(date)) %>% 
      mutate(win = (home_team_goal<away_team_goal)*1+(home_team_goal==away_team_goal)*0.5,
             result = paste0(home_team_goal, ':',away_team_goal)) %>%  head(10)
    colnames(df) = c("Sezon","Kolejka", "Data", "home","away","Gospodarze", "Goście", "win", "Wynik")
    datatable(df, rownames = FALSE, options=list(columnDefs = list(list(visible=FALSE, targets=c(0,1,3,4,7) )),
                               dom = "t")) %>% formatStyle(
      'Wynik','win',
      backgroundColor = styleEqual(c(0, 1,0.5), c('rgb(255,100,100)', 'rgb(100,255,100)','orange'))
    )
  })
  output$wins <- renderValueBox({
    valueBox(
      longest_streak_win(), "Najdłuższa seria zwycięstw", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "purple")
  })
  
  output$loses <- renderValueBox({
    valueBox(
      longest_streak_lose(), "Najdłuższa seria porażek", icon = icon("thumbs-down", lib = "glyphicon"),
      color = "yellow")})
  output$goals <- renderValueBox({
    valueBox(
      goals_scored(), "Liczba strzelonych goli",
      color = "green")})

  team_info <- reactive({
    df <- dat %>% filter(season %in% input$selected_season_club & (away_team == input$clubs_list | home_team == input$clubs_list)) %>%
                  mutate(win = if_else((home_team ==input$clubs_list & home_team_goal>away_team_goal) |
                                       (away_team ==input$clubs_list & home_team_goal<away_team_goal), 1, 
                                       if_else(home_team_goal == away_team_goal, 0, -1))) %>%  arrange(season,stage)
    df <- df %>% mutate(points =if_else(win == 1, 3, if_else(win==0,1,0)))
    return(df)
  })
  longest_streak_win <- reactive({
    df <- team_info()
    if(nrow(df)>0)
    {
      score <- rle(df$win)
      return(max(score$lengths[score$values==1]))
    }
    else
    {
      return('Brak danych')
    }
  })
  longest_streak_lose <- reactive({
    df <- team_info()
    if(nrow(df)>0)
    {
      score <- rle(df$win)
      return(max(score$lengths[score$values==-1]))
    }
    else
    {
      return('Brak danych')
    }
  })
  
  goals_scored <- reactive({
    df <- total_goal %>% filter(team_name == input$clubs_list & season %in% input$selected_season_club) %>% summarize(total = sum(total_goals))
    if(nrow(df)>0)
    {
      return(sum(df$total))
    }
    else
    {
      return('Brak danych')
    }
  })
  
  output$points_plot <- renderPlot({
    df <- team_info()
    df <- df %>% group_by(season) %>% mutate(cum_point = cumsum(points))
    ggplot(df, aes( x= stage, y = cum_point, color=season)) +
      geom_point(size=3) + geom_line() +
      theme_bw() + xlab('Kolejka') +ylab('Punkty')+
      ggtitle('Zdobyte punkty') +
      theme(plot.title = element_text(hjust = 0.5))+
      scale_color_discrete(name = "Sezon")
    
  })
  
  onevsone <- reactive({
    df <- dat %>%  filter(home_team %in% c(input$club1, input$club2) & away_team %in% c(input$club1, input$club2))
    ind <- (df[,6:7]== input$club1)
    scored_club1 <- sum(df[,4:5]*ind)
    scored_club2 <- sum(df[,4:5]*!ind)
    return(c(scored_club1, scored_club2))
  })
  
  
  output$goals_club1 <- renderPlot({
    goals <- onevsone()
    df <- data.frame(club = input$club1, goal = goals[1] )
    ggplot(df, aes(x = club, y = goal ))+
      geom_col(fill = '#1B9D77') +
      geom_text(aes(label=goal), size = 15, vjust=-0.1) +
      ylim(0, 15)+
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank()) +xlab(paste0("Gole strzelone przeciw ",input$club2))
  },bg="transparent")
  
  
  output$goals_club2 <- renderPlot({
    goals <- onevsone()
    df <- data.frame(club = input$club2, goal = goals[2] )
    ggplot(df, aes(x = club, y = goal ))+
      geom_col(fill='#DA5F02') +
      geom_text(aes(label=goal), size = 15, vjust=-0.1) +
      ylim(0, 15) + 
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank()) +xlab(paste0("Gole strzelone przeciw ",input$club1))
  },bg="transparent")
  
  
  output$match_stats <- renderPlot({
    df <- team_summary()
    df <- df[df$team %in% c(input$club1, input$club2),] %>% group_by(team) %>% summarise(home = sum(home),
                                                                                          away = sum(away),
                                                                                          total = sum(total))
    names(df) <- c('team','U siebie', 'Na wyjeździe', 'Ogólnie')
    df <- df %>% group_by()
    df <- melt(df, id=c("team"))
    numb <- round(max(df$value)+5,-1)
    df[df$team ==input$club1, "value"] <- df[df$team ==input$club1, "value"]*-1
    brks <- seq(-numb, numb, 20)
    lbls = paste0(as.character(abs(brks)))
    
    ggplot(df, aes(x = variable, y = value, fill = team)) +   # Fill column
      geom_bar(stat = "identity", width = .6) +   # draw the bars
      scale_y_continuous(breaks = brks,   # Breaks
                         labels = lbls,
                         limits = c(-numb,numb)) + # Labels
      annotate("text", label = 'U siebie', color='white',size=6, fontface =2,
               x = 1, y = 0)+
      annotate("text", label = "Na wyjeździe",  color='white',size=6, fontface =2,
               x = 2, y = 0)+
      annotate("text", label = "Ogólnie",  color='white',size=6, fontface =2,
               x = 3, y = 0)+
      coord_flip() + 
      labs(title="Statystyka strzelonych bramek", y='',x='') +
      theme_bw() + 
      theme(plot.title = element_text(size = 12, hjust = .5),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size=13, face='bold'),
            legend.position="none", plot.background = element_blank(),
            panel.background=element_blank(),panel.border=element_blank(),
            panel.grid.major.y = element_blank()) +  # Centre plot title
      scale_fill_brewer(palette = "Dark2")  # Color palette


  })

}


# Run the application 
shinyApp(ui = ui, server = server)

