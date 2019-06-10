
baselineUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    
    box(
      title = 'Bad visualization',
      status = 'danger',
      solidHeader = TRUE,
      plotOutput(ns('baseline'))
    ),
    box(
      title = "How much you have been lied to?",
      status = 'warning',
      solidHeader = TRUE,
      fluidRow(
        box(
          width = 12,
          sliderInput(ns('axis_range_slider'),label = 'Choose y-axis baseline',value = 10, min=0, max = 10)
        ),
      fluidRow(
        column(
          width=12, 
          helpText('Choose continents to compare'),
          align="center",
          column(
            width = 6,
            selectInput(ns('continent_1'),label = "Continent I", selected = 'Europe', choices = birth$continent ),
            valueBoxOutput(width = 12, ns("real_diff_box"))),
          column(
            width = 6,
            selectInput(ns('continent_2'),label = "Continent II", selected = 'Africa', choices = birth$continent ),
            valueBoxOutput(ns("curr_diff_box"), width = 12))
          )
        )
      )
    )
  )
}

