
dimensionsUI <- function(id) {
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
          plotOutput(ns('bad_3D_barplot'))
        )
      ),
      
      
      column(width=6,
             box(width=12,
                 title = 'Question to you',
                 status = 'warning',
                 solidHeader = TRUE,
                 textInput('question',"Acceleration of cars with 6 cylinders produced in 1982:",""),
             verbatimTextOutput("value1"))
      )),
    
    
    fluidRow(
      box(
        width = 12,
        actionButton('action_button_dim', 'Verify', width = '100%', style = 'color: #fff; background-color: #337ab7; border-color: #2e6da4')
      )
    ),
    
    
    fluidRow(
      column(
        width=6,
        box(
          id = 'box_good_dim',
          width = 12,
          title = 'Good visualization',
          status = 'success',
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          plotOutput(ns('good_3D_barplot'))
        )
      ),
      column(width=6,
             box(
               id="answer_dim",
               width=12,
               title = 'Correct answer',
               status = 'success',
               solidHeader = TRUE,
               collapsible = TRUE,
               collapsed = TRUE,
               textInput('question',"Acceleration of cars with 6 cylinders produced in 1982:","16"),
               verbatimTextOutput("value2"))
             
             
      )
      
      
    )
  )
}

# tab_item_pie_chart_1 <- tabItem(
#   'pie_chart_1',
#   fluidRow(
#     box(
#       title = 'Bad visualization',
#       status = 'danger',
#       solidHeader = TRUE,
#       plotOutput('bad_pie_chart_1')
#     ),
#     box(
#       title = 'Estimated values',
#       status = 'warning',
#       solidHeader = TRUE,
#       textInput('text_input_pie_chart_1_estimated_1', 'Asia:'),
#       textInput('text_input_pie_chart_1_estimated_2', 'Africa:'),
#       textInput('text_input_pie_chart_1_estimated_3', 'Americas:'),
#       textInput('text_input_pie_chart_1_estimated_4', 'Europe:'),
#       textInput('text_input_pie_chart_1_estimated_5', 'Oceania:')
#     )
#   ),
#   fluidRow(
#     box(
#       width = 12,
#       actionButton('action_button_pie_chart_1', 'Verify', width = '100%', style = 'color: #fff; background-color: #337ab7; border-color: #2e6da4')
#     )
#   ),
#   fluidRow(
#     box(
#       id = 'box_pie_chart_1_good_viz',
#       title = 'Good visualization',
#       status = 'success',
#       solidHeader = TRUE,
#       collapsible = TRUE,
#       collapsed = TRUE,
#       plotOutput('good_pie_chart_1')
#     ),
#     box(
#       id = 'box_pie_chart_1_actual_values',
#       title = 'Actual values',
#       status = 'success',
#       solidHeader = TRUE,
#       collapsible = TRUE,
#       collapsed = TRUE,
#       textInput('text_input_pie_chart_1_actual_1', 'Asia:', 61),
#       textInput('text_input_pie_chart_1_actual_2', 'Africa:', 14),
#       textInput('text_input_pie_chart_1_actual_3', 'Americas:', 13),
#       textInput('text_input_pie_chart_1_actual_4', 'Europe:', 11),
#       textInput('text_input_pie_chart_1_actual_5', 'Oceania:', 1)
#     )
#   )
# )
# 
# 
