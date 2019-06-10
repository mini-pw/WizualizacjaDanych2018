tab_item_zus_ofe <- tabItem(
  'zus_ofe',
  fluidRow(
    box(
      width = 12,
      title = 'Bad visualization',
      status = 'danger',
      solidHeader = TRUE,
      plotOutput('zus_ofe_orig')
    )
  ),
  fluidRow(box(
    width = 12,
    actionButton(
      'zus_ofe_action_button',
      'How much have you been deceived',
      width = '100%',
      style = 'color: #fff; background-color: #337ab7; border-color: #2e6da4'
    )
  )),
  fluidRow(
    box(
      id = "zus_ofe_parameters",
      width = 12,
      solidHeader = F,
      collapsed = T,
      collapsible = F,
      column(
        width = 3,
        sliderInput(
          "zus_ofe_ofe_range",
          label = "Select OFE x-limits",
          min = 20,
          max = 35,
          step = 1,
          value = 25
        )
      ),
      column(
        width = 3,
        selectInput(
          "zus_ofe_year",
          label = "Select year",
          choices = seq(2000, 2011),
          selected = 2000
        )
      ),
      column(width = 3,
             valueBoxOutput("zus_ofe_real_ratio", width = 12)),
      column(width = 3,
             valueBoxOutput("zus_ofe_current_ratio", width = 12)),
      column(width = 12,
             textOutput("zus_ofe_ratio"))
    )
  ),
  fluidRow(
    box(
      width = 12,
      solidHeader = T,
      title = "Good visualization",
      collapsible = T,
      collapsed = T,
      status = 'success',
      plotOutput("zus_ofe_good")
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
        a('Biecek - essays', href = 'http://www.biecek.pl/Eseje/indexPomylka.html')
      ))
    )
  )
)