polarUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    fluidRow(
      #column(width = 3),
      column(
        width=6,
        #align="center",
        box(
          width = 12,
          #align = "center",
          title = 'Bad visualization',
          status = 'danger',
          solidHeader = TRUE,
          plotOutput(ns('bad_polar'))
        )
      ),
      
      
      column(width=6,
             box(width=12,
        title = 'Question to you',
        status = 'warning',
        solidHeader = TRUE,
        selectInput("Question to you", "Which one has bigger death rate - Nauru vs. Vanuatu?",
                    c("","the same","Nauru","Vanuatu")))
      )),
    
    
    fluidRow(
      box(
        width = 12,
        actionButton('action_button_polar', 'Verify', width = '100%', style = 'color: #fff; background-color: #337ab7; border-color: #2e6da4')
      )
    ),
    
    
    fluidRow(
      column(
        width=6,
        box(
          id = 'box_good_polar',
          width = 12,
          title = 'Good visualization',
          status = 'success',
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          plotOutput(ns('good_polar'))
        )
      ),
      column(width=6,
             box(
               id="answer_polar",
               width=12,
               title = 'Correct answer',
               status = 'success',
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               selectInput("Correct answer", "Which one has bigger death rate - Nauru vs. Vanuatu?",
                           c("Vanuatu")))
               
        
      )

      
    )
  )
}
