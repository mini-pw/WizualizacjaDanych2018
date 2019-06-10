tab_item_pie_chart_2 <- tabItem(
    'pie_chart_2',
    fluidRow(
        box(
            title = 'Bad visualization',
            status = 'danger',
            solidHeader = TRUE,
            width = 6,
            plotOutput('bad_pie_chart_2')
        ),
        box(
            title = 'Estimated values',
            status = 'warning',
            solidHeader = TRUE,
            width = 3,
            textInput('text_input_pie_chart_2_estimated_1', 'Poland:'),
            textInput('text_input_pie_chart_2_estimated_2', 'Czech Republic:'),
            textInput('text_input_pie_chart_2_estimated_3', 'Hungary:'),
            textInput('text_input_pie_chart_2_estimated_4', 'Slovakia:'),
            textInput('text_input_pie_chart_2_estimated_5', 'Lithuania:')
        ),
        box(
            title = 'Estimated values',
            status = 'warning',
            solidHeader = TRUE,
            width = 3,
            textInput('text_input_pie_chart_2_estimated_6', 'Latvia:'),
            textInput('text_input_pie_chart_2_estimated_7', 'Slovenia:'),
            textInput('text_input_pie_chart_2_estimated_8', 'Cyprus:'),
            textInput('text_input_pie_chart_2_estimated_9', 'Estonia:'),
            textInput('text_input_pie_chart_2_estimated_10', 'Malta:')
        )
    ),
    fluidRow(
        box(
            width = 12,
            actionButton('action_button_pie_chart_2', 'Verify', width = '100%', style = 'color: #fff; background-color: #337ab7; border-color: #2e6da4')
        )
    ),
    fluidRow(
        box(
            id = 'box_pie_chart_2_good_viz',
            title = 'Good visualization',
            status = 'success',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 6,
            plotOutput('good_pie_chart_2')
        ),
        box(
            id = 'box_pie_chart_2_actual_values_1',
            title = 'Actual values',
            status = 'success',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 3,
            textInput('text_input_pie_chart_2_actual_1', 'Poland:', 51),
            textInput('text_input_pie_chart_2_actual_2', 'Czech Republic:', 14),
            textInput('text_input_pie_chart_2_actual_3', 'Hungary:', 13),
            textInput('text_input_pie_chart_2_actual_4', 'Slovakia:', 7),
            textInput('text_input_pie_chart_2_actual_5', 'Lithuania:', 4)
        ),
        box(
            id = 'box_pie_chart_2_actual_values_2',
            title = 'Actual values',
            status = 'success',
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 3,
            textInput('text_input_pie_chart_2_actual_6', 'Latvia:', 3),
            textInput('text_input_pie_chart_2_actual_7', 'Slovenia:', 3),
            textInput('text_input_pie_chart_2_actual_8', 'Cyprus:', 2),
            textInput('text_input_pie_chart_2_actual_9', 'Estonia:', 2),
            textInput('text_input_pie_chart_2_actual_10', 'Malta:', 1)
        )
    )
)
