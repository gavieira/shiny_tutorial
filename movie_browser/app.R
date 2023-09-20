# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Load data --------------------------------------------------------------------

load("movies.RData")


# Vars outside the app

movies <- movies %>%
  mutate(thtr_rel_date = as.Date(thtr_rel_date))

n_total <- nrow(movies)

min_date <- min(movies$thtr_rel_date)
max_date <- max(movies$thtr_rel_date)



# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(

    # Inputs: Select variables to plot
    sidebarPanel(

      HTML(paste("Enter a value between 1 and", n_total)),

      numericInput(inputId = "n",
                   label = "Sample_size",
                   min = 1, max = n_total,
                   value = 30,
                   step = 1),

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
      ),

      # Select studios
      selectInput(
        inputId = 'studios',
        label = 'Select studios:',
        multiple = TRUE,
        choices = c('ALL', movies %>% distinct(studio)),
        selected = 'ALL'
      ),


      HTML(paste("Enter a date between", min_date, "and", max_date)),
      # Select date
      dateRangeInput(
        inputId = "daterange",
        label = "Select date range:",
        start = "01-01-13", end = "01-01-14",
        startview = "year"
      ),

      # Select data table visualization
      checkboxInput(inputId = "show_data",
                    label = "Show data table:",
                    value = TRUE)
    ),

    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot", height = 200),
      plotOutput(outputId = "densityplot", height = 200),
      dataTableOutput(outputId = "moviestable")
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  output$studios <- renderText({input$studios})

   sampled_n_input <- reactive({
     req(input$n)
      movies[1:input$n,]
    })

   sampled_studio_input <- reactive({
     req(input$studios)
      data <- sampled_n_input()
      selected <- input$studios
      if ('ALL' %in% selected) { return( data ) }
      data %>% filter(studio %in% selected)
    })

   sampledInput <- reactive({
     req(input$daterange)
     sampled_studio_input() %>%
       filter(thtr_rel_date >= input$daterange[1], thtr_rel_date <= input$daterange[2])
   })

  output$scatterplot <- renderPlot({
    ggplot(data = sampledInput(), aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha)
  })

  output$densityplot <- renderPlot({
    ggplot(data = sampledInput(), aes_string(x = input$x, color = input$z)) +
      geom_density()
  })

  # Print data table if checked
  output$moviestable <- renderDataTable({
    if(input$show_data){
      DT::datatable(data = sampledInput() %>% select(title:studio),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    }
  })
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
