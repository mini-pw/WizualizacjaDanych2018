library(magrittr)
library(shinydashboard)
library(shinyjs)
source("./data_manipulation.R")
source("./ui_components.R")
source("./visualization.R")

football_data_team_view <- football_data %>%
  transform_to_team_view()


ui <- dashboardPage(
  dashboardHeader(title = "Football analyzer"),
  dashboardSidebar(sidebar_menu_component()),
  dashboardBody(
    useShinyjs(),
    tabItems(
      overview_tab_item(),
      analyzer_tab_item(football_data_team_view)
    )
  )
)

server <- function(input, output, session) {
  output$win_percentage_plot <- renderPlot({
    football_data_team_view %>%
      calculate_win_percentage() %>%
      plot_win_percentage()
  })

  output$head2head_plot <- renderPlot({
    football_data %>%
      plot_matches_heat_map()
  })

  output$streaks_plots <- renderPlot({
    football_data_team_view %>%
      calculate_streak() %>%
      plot_streaks()
  })

  output$streak_box <- renderValueBox({
    team_streaks <- football_data_team_view %>%
      calculate_streak()

    selected_team_streak <- team_streaks[team == input$selected_team]

    create_value_box_from_team_streak(selected_team_streak)
  })

  output$home_win_percentage <- renderValueBox({
    selected_team_win_percentage <- football_data_team_view[
      team == input$selected_team &
        match_type == "home"
    ] %>% calculate_win_percentage()

    valueBox(
      subtitle = "Home win percentage",
      icon = icon("home"),
      value = sprintf("%s %%", round(selected_team_win_percentage$win_percentage * 100, 2))
    )
  })

  output$road_win_percentage <- renderValueBox({
    selected_team_win_percentage <- football_data_team_view[
      team == input$selected_team &
        match_type == "away"
    ] %>% calculate_win_percentage()

    valueBox(
      subtitle = "Road win percentage",
      color = "yellow",
      icon = icon("road"),
      value = sprintf("%s %%", round(selected_team_win_percentage$win_percentage * 100, 2))
    )
  })


  output$goals_over_time_label <- renderText({
    ifelse(input$opponent_selection_allowed,
      sprintf("Goals against %s", input$selected_opponent),
      "Goals"
    )
  })

  output$goals_over_time_plot <- renderPlot({
    opponent <- ifelse(input$opponent_selection_allowed,
      input$selected_opponent,
      NA
    )

    football_data_team_view %>%
      plot_goals_over_time(
        selected_team = input$selected_team,
        selected_opponent = opponent
      )
  })

  output$goal_difference_plot <- renderPlot({
    opponent <- ifelse(input$opponent_selection_allowed,
      input$selected_opponent,
      NA
    )

    football_data_team_view %>%
      plot_goal_differences(
        selected_team = input$selected_team,
        selected_opponent = opponent
      )
  })

  output$win_amount_box <- renderValueBox({
    results <- football_data_team_view %>%
      calculate_win_loss_amounts(
        input$selected_team,
        input$selected_opponent
      )

    valueBox(
      subtitle = "Wins",
      value = results$wins,
      color = "green"
    )
  })

  output$draw_amount_box <- renderValueBox({
    results <- football_data_team_view %>%
      calculate_win_loss_amounts(
        input$selected_team,
        input$selected_opponent
      )

    valueBox(
      subtitle = "Draws",
      value = results$draws,
      color = "light-blue"
    )
  })

  output$loss_amount_box <- renderValueBox({
    results <- football_data_team_view %>%
      calculate_win_loss_amounts(
        input$selected_team,
        input$selected_opponent
      )

    valueBox(
      subtitle = "Losses",
      value = results$losses,
      color = "red"
    )
  })

  observe({
    toggleState("selected_opponent", input$opponent_selection_allowed)
  })

  observe({
    if (input$opponent_selection_allowed) {
      show("specific_stats_box")
    } else {
      hide("specific_stats_box")
    }
  })

  observe({
    updateSelectInput(
      session = session,
      inputId = "selected_opponent",
      choices = setdiff(
        unique(football_data_team_view$team),
        input$selected_team
      )
    )
  })
}

shinyApp(ui, server)
