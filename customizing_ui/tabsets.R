ui <- fluidPage(
  mainPanel(
  tabsetPanel(
    type = "tabs",
    tabPanel("Plot", plotOutput("plot")),
    tabPanel("Summary", tableOutput("summary")),
    tabPanel("Data", DT::dataTableOutput("data")),
    tabPanel(
      "Reference",
      tags$p(
        "There data were obtained from",
        tags$a("IMDB", href = "http://www.imdb.com/"), "and",
        tags$a("Rotten Tomatoes", href = "https://www.rottentomatoes.com/"), "."
      ),
      tags$p(
        "The data represent", nrow(movies),
        "randomly sampled movies released between 1972 to 2014 in the United States."
      )
    )
  )
)
)


server <- function(input, output, session) { }

shinyApp(ui = ui, server = server)
