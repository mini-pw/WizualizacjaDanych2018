library(ggplot2)
library(shiny)
library(shinydashboard)
library(shinyjs)
source("./components.R")


#' Creates a builder for creating visualization cases
#'
create_visualization_case_builder <- function() {
  list(
    good_plot = NULL,
    bad_plot = NULL,
    questions = NULL,
    answers = NULL,
    description = NULL
  ) %>% structure(class = "visualization_case_builder")
}


#' Checks whether the given object is a visualization case builder
#'
#' @param obj object to validate
#'
#' @return TRUE if obj is a visualization case builder
#'
is_visualization_case_builder <- function(obj) {
  class(obj) == "visualization_case_builder"
}


add_good_plot <- function(builder, plot) {
  stopifnot(is_visualization_case_builder(builder))
  stopifnot("ggplot" %in% class(plot))

  builder$good_plot <- plot
  return(builder)
}


add_bad_plot <- function(builder, plot) {
  stopifnot(is_visualization_case_builder(builder))
  stopifnot("ggplot" %in% class(plot))

  builder$bad_plot <- plot
  return(builder)
}


add_qa_case <- function(builder, question, answer) {
  stopifnot(is_visualization_case_builder(builder))

  builder$questions <- c(builder$questions, question)
  builder$answers <- c(builder$answers, answer)

  return(builder)
}


build <- function(my_builder) {
  ui <- dashboardPage(
    header = dashboardHeader(disable = TRUE),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      useShinyjs(),
      fluidPage(
        fluidRow(
          create_questions_box(my_builder),
          create_bad_plot_box(),
          div(id="good_plot_box", create_good_plot_box()),
          create_results_box()
        )
      )
    )
  )

  server <- function(input, output, session) {
    my_builder # needs to be added so it can be referenced in the server function
    answer_comparison <- reactive({
      user_answers <- sapply(seq_along(my_builder$questions), function(id) input[[sprintf("question_%s", id)]])
      data.frame(
        yours = user_answers,
        correct = my_builder$answers
      )
    })
    
    hide("results")
    
    hide(id = "good_plot_box")
    hide(id = "fun_fact_box")
    
    onclick("check", {
      toggle("results")
      toggle("good_plot_box")
      toggle("fun_fact_box")
    })
    
    output$bad_plot <- renderPlot({
      my_builder$bad_plot
    })
    
    output$good_plot <- renderPlot({
      my_builder$good_plot
    })
    
    output$results <- DT::renderDataTable({
      DT::datatable(answer_comparison())
    })
  }

  list(ui = ui, server = server)
}
