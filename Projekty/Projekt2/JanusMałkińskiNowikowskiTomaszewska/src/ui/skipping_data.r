tab_item_skipping_data <- tabItem(
  'skipping_data',
  fluidRow(
    box(
      title = 'Bad visualization',
      status = 'danger',
      solidHeader = TRUE,
      plotOutput('skipping_data_increasing')
    ),
    box(
      title = 'Bad visualization',
      status = 'danger',
      solidHeader = TRUE,
      plotOutput('skipping_data_decreasing')
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = 'Full data set',
      status = 'warning',
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      plotOutput('skipping_data_full')
    )
  ),
  fluidRow(box(
    width = 12,
    actionButton(
      'skipping_data_try_it',
      'Try it yourself',
      width = '100%',
      style = 'color: #fff; background-color: #337ab7; border-color: #2e6da4'
    )
  )),
  fluidRow(
    box(
      id = 'box_skipping_data_input',
      title = 'Parameters',
      status = 'success',
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      checkboxGroupInput(
        inputId = "accidents_year_selected",
        label = "Years to show:",
        choices = seq(2005, 2017),
        selected = c(2010, 2014)
      )
    ),
    box(
      id = 'box_skipping_data_user',
      title = 'Good visualization',
      status = 'success',
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      plotOutput('skipping_data_user')
    )
  ),
  fluidRow(
    box(
      width = 12,
      title = "Source",
      solidHeader = T,
      collapsible = T,
      collapsed = T,
      status = 'success',
      tags$body(p(
        a('SmarterPoland - worst plots of 2018', href = 'http://smarterpoland.pl/index.php/category/zly-wykres/'),
        br(),
        a('TVN - original article', href = 'https://www.tvn24.pl/wiadomosci-z-kraju,3/rosnie-liczba-zachorowan-na-odre,729159.html')
      ))
    )
  )
)
