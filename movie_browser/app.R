# Load packages

library(shiny)
library(ggplot2)

# Get the data

file <- "https://github.com/rstudio-education/shiny-course/raw/main/movies.RData"
destfile <- "movies.RData"

download.file(file, destfile)

# Load data

load("movies.RData")

# Define UI

ui <- fluidPage(

  sidebarLayout(

    # Inputs: Select variables to plot
    sidebarPanel(

      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c("IMDB Rating" = "imdb_rating",
                    "IMDB Number of Votes" = "imdb_num_votes",
                    "Critics Score" = "critics_score",
                    "Audience Score" = "audience_score",
                    "Runtime" = "runtime"),
        selected = "audience_score"
      ),
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c("IMDB Rating" = "imdb_rating",
                    "IMDB Number of Votes" = "imdb_num_votes",
                    "Critics Score" = "critics_score",
                    "Audience Score" = "audience_score",
                    "Runtime" = "runtime"),
        selected = "critics_score"
      ),
      # Select variable to color by
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c("Title Type" = "title_type",
                    "Genre" = "genre",
                    "MPAA Rating" = "mpaa_rating",
                    "Critics Rating" = "critics_rating",
                    "Audience Rating" = "audience_rating"),
        selected = "mpaa_rating"
      )
    ),

    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

# Define server

server <- function(input, output, session) {
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point()
  })
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)
