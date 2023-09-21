# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)

# Load data --------------------------------------------------------------------

load("../movie_browser/movies.RData")
max_rows <- nrow(movies)

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(
          "IMDB rating" = "imdb_rating",
          "IMDB number of votes" = "imdb_num_votes",
          "Critics Score" = "critics_score",
          "Audience Score" = "audience_score",
          "Runtime" = "runtime"
        ),
        selected = "audience_score"
      ),

      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(
          "IMDB rating" = "imdb_rating",
          "IMDB number of votes" = "imdb_num_votes",
          "Critics Score" = "critics_score",
          "Audience Score" = "audience_score",
          "Runtime" = "runtime"
        ),
        selected = "critics_score"
      ),

      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c(
          "Title Type" = "title_type",
          "Genre" = "genre",
          "MPAA Rating" = "mpaa_rating",
          "Critics Rating" = "critics_rating",
          "Audience Rating" = "audience_rating"
        ),
        selected = "mpaa_rating"
      ),

      checkboxGroupInput(
        inputId = "selected_type",
        label = "Select movie type(s):",
        choices = c("Documentary", "Feature Film", "TV Movie"),
        selected = "Feature Film"
      ),

      numericInput(
        inputId = "n_samp",
        label = "Sample size:",
        min = 1, max = max_rows,
        value = 3
      ),

      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ),

      sliderInput(
        inputId = "size",
        label = "Size:",
        min = 0, max = 5,
        value = 2
        ),

      textInput(
        inputId = 'plot_title',
        label = 'Plot title:',
        placeholder = 'Type the plot title here'
      ),

      actionButton("update_title", "Update Title"),

      br(),
      br(),

      HTML('The next section is for selecting a subset of rows and generating a table output with it'),

      numericInput(
        inputId = "n_rows",
        label = "How many rows do you want to see?",
        value = 10,
        min = 1,
        max = max_rows
      ),

      actionButton('table_output', 'Generate table output'),

      br(),

      #The next section is for testing observers
      HTML('The next section is for testing buttons, reactiveVal() and observeEvent():'),
      br(),
      actionButton("minus", "-1"),
      actionButton("plus", "+1"),
      br(),
      textOutput("value"),

      #The next section is for testing observers
      actionButton("double", "Double"),
      br(),
      textOutput("doubled_value")

    ),

    mainPanel(
      plotOutput(outputId = "scatterplot"),
      uiOutput(outputId = "n"),
      tableOutput(outputId = "table_rows"),
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {

  # Create new df that is n_samp obs from selected type movies
  movies_sample <- reactive({
    req(input$n_samp)
    sample_n(movies, input$n_samp)
  })

  # Create a subset of data filtering for selected title types
  movies_subset <- reactive({
    req(input$selected_type)
    filter(movies_sample(), title_type %in% input$selected_type)
  })

  new_plot_title <- eventReactive(
    eventExpr = input$update_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    }
  )

  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = movies_subset(), aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      #labs(title =  new_plot_title())
      labs(title = isolate({toTitleCase(input$plot_title)}))
  })

  value <- reactiveVal(2)

  observeEvent(input$minus,{
    newValue <- value() - 1 # rv <- reactiveValues(value = 0)
    if (newValue >= 2) { return(value(newValue)) }  # rv$value <- newValue
    return(value(2))

    value(newValue)
  })

  observeEvent(input$plus, {
    newValue <- value() + 1     # newValue <- rv$value + 1
    if (newValue <= 8) { return(value(newValue)) }  # rv$value <- newValue
    return(value(8))
  })

  output$value <- renderText({
    value()                     # rv$value
  })

  double_value <- reactiveVal(1)

  observeEvent(input$double, {
    double_value(double_value() * 2)
  })

  output$doubled_value <- renderText({
    double_value()
  })

  observeEvent(input$table_output, {
    cat("Showing", input$n_rows, "rows\n")
  })

  #Take a reactive dependency on input$button, but not on any other inputs
  df <- eventReactive(input$table_output, {
    head(movies, input$n_rows)
  })

  output$table_rows <- renderTable({
    df()
  })

  # Print number of movies plotted
  output$n <- renderUI({
    types <- factor(movies_subset()$title_type, levels = input$selected_type)
    counts <- table(types)
    HTML(paste("There are", counts, input$selected_type, "movies plotted in the plot above. <br>"))
  })
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
