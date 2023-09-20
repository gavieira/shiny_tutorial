# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)

# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(

    # Inputs: Select variables to plot
    sidebarPanel(

      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(
          "IMDB rating" = "imdb_rating",
          "IMDB number of votes" = "imdb_num_votes",
          "Critics score" = "critics_score",
          "Audience score" = "audience_score",
          "Runtime" = "runtime"
        ),
        selected = "audience_score"
      ),

      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(
          "IMDB rating" = "imdb_rating",
          "IMDB number of votes" = "imdb_num_votes",
          "Critics score" = "critics_score",
          "Audience score" = "audience_score",
          "Runtime" = "runtime"
        ),
        selected = "critics_score"
      ),

      # Select variable for color
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c(
          "Title type" = "title_type",
          "Genre" = "genre",
          "MPAA rating" = "mpaa_rating",
          "Critics rating" = "critics_rating",
          "Audience rating" = "audience_rating"
        ),
        selected = "mpaa_rating"
      ),

      # Select alpha
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 1
      )
    ),

    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot"),
      plotOutput(outputId = "densityplot", height = 200)
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({

    ggplot(data = movies, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha)

  })

  output$densityplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, color = input$z)) +
      geom_density(alpha = input$alpha)

  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
