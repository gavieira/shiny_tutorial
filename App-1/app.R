library(shiny)

ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(h1('Installation'),
                 p('Shiny is available on CRAN, so you can (...) R console:'),
                 code('install.packages("shiny")'),
                 br(),
                 br(),
                 br(),
                 br(),
                 img(src = 'rstudio.png', height = 70, width = 200),
                 'Shiny is a product of',
                 br(),
                 span("Rstudio", style = 'color:blue' )
              ),
    mainPanel(
      h1('Introducing Shiny'),
      p('Shiny is a new package that makes it',
        em('incredibly easy'),
        'to build (...) with R'),
      br(),
      br(),
      p('For an introduction (...)',
        a(href = 'https://shiny.posit.co/', 'Shiny homepage')),
      br(),
      br(),
      h2('Features'),
      p('- Build useful (...) - no JavaScript required.'),
      p("- Shiny applications are automatically 'live' in the same way that ",
                strong('spreadsheets'),
                'are live. Outputs (...)')
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
}

shinyApp(ui = ui, server = server)
