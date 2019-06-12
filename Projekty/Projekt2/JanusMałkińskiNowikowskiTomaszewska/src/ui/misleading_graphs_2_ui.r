
axisUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    fluidRow(
      column(width = 3),
      column(
        width=6,
        align="center",
        box(
          width = 12,
          align = "center",
          title = 'Bad visualization',
          status = 'danger',
          solidHeader = TRUE,
          plotOutput(ns('all_currency'))
        )
      ),
      column(width = 3)
    ),
    fluidRow(
      column(
        width=6,
        box(
          width = 12,
          title = 'Good visualization',
          status = 'success',
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          plotOutput(ns('thb_plot'))
        )
      ),
      column(
        width=6,
        box(
          width = 12,
          title = 'Good visualization',
          status = 'success',
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          plotOutput(ns('gbp_plot'))
        )
      )
    )
  )
}

