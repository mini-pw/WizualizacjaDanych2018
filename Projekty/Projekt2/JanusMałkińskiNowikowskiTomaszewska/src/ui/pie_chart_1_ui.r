tab_item_pie_chart_1 <- tabItem(
    'pie_chart_1',
    fluidRow(
        box(
            title = 'Bad visualization',
            status = 'danger',
            solidHeader = TRUE,
            plotOutput('bad_pie_chart_1')
        ),
        box(
            title = 'Estimated values',
            status = 'warning',
            solidHeader = TRUE,
            textInput('text_input_pie_chart_1_estimated_1', 'Asia:'),
            textInput('text_input_pie_chart_1_estimated_2', 'Africa:'),
            textInput('text_input_pie_chart_1_estimated_3', 'Americas:'),
            textInput('text_input_pie_chart_1_estimated_4', 'Europe:'),
            textInput('text_input_pie_chart_1_estimated_5', 'Oceania:')
        )
    ),
    fluidRow(
        box(
            width = 12,
            actionButton('action_button_pie_chart_1', 'Verify', width = '100%', style = 'color: #fff; background-color: #337ab7; border-color: #2e6da4')
        )
    ),
    fluidRow(
        box(
            id = 'box_pie_chart_1_good_viz',
            title = 'Good visualization',
            status = 'success',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            plotOutput('good_pie_chart_1')
        ),
        box(
            id = 'box_pie_chart_1_actual_values',
            title = 'Actual values',
            status = 'success',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            textInput('text_input_pie_chart_1_actual_1', 'Asia:', 61),
            textInput('text_input_pie_chart_1_actual_2', 'Africa:', 14),
            textInput('text_input_pie_chart_1_actual_3', 'Americas:', 13),
            textInput('text_input_pie_chart_1_actual_4', 'Europe:', 11),
            textInput('text_input_pie_chart_1_actual_5', 'Oceania:', 1)
        )
    )
)
