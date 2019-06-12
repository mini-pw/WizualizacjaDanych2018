library(shiny)
library(shinyjs)


create_bad_plot_box <- function() {
  box(
    title = "Bad Plot",
    solidHeader = TRUE,
    status = "danger",
    plotOutput("bad_plot")
  )
}


create_good_plot_box <- function() {
  box(
    title = "Proper Plot",
    solidHeader = TRUE,
    status = "success",
    plotOutput("good_plot")
  )
}


create_questions_box <- function(builder) {
  box(
    title = "Questions",
    solidHeader = TRUE,
    status = "info",
    create_form(builder$questions, builder$answers)
  )
}


create_form <- function(questions, answers) {
    create_form_part <- function(id, q, a) {
      splitLayout(
        cellWidths = c("60%", "40%"),
          textInput(inputId = sprintf("question_%s", id), label = q)
      )
    }
    
    stopifnot(length(questions) == length(answers))
    ids <- seq_along(questions)
    div(
      id = "form",
      Map(create_form_part, ids, questions, answers, USE.NAMES = FALSE),
      actionButton(inputId = "check", label = "Check", class = "btn-primary")
    )
}


create_results_box <- function() {
  div(
    id = "results",
    box(
      title = "Results",
      solidHeader = TRUE,
      status = "info",
      DT::dataTableOutput("results")
    )
  )
}


create_fun_fact_box <- function(description) {
  box(
    title = "Fun fact",
    solidHeader = TRUE,
    status = "info",
    p(description)
  )
}

