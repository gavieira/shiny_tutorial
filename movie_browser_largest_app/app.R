# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Load data --------------------------------------------------------------------

load("movies.RData")


# Vars outside the app

movies <- movies %>%
  mutate(thtr_rel_date = as.Date(thtr_rel_date)) %>%
  mutate(score_ratio = audience_score / critics_score)

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
                   value = n_total,
                   step = 1),

      # Select which types of movies to plot
      selectInput(
        inputId = "movie_type",
        label = "Select movie type:",
        choices = c('ALL', levels(movies$title_type)),
        selected = "ALL"
      ),

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
        choices = c('ALL', levels(movies$studio)),
        selected = 'ALL'
      ),

      # Subset for title types
      checkboxGroupInput(inputId = "selected_title_type",
                         label = "Select title type:",
                         choices = levels(movies$title_type),
                         selected = levels(movies$title_type)),

      # Select date
      dateRangeInput(
        inputId = "daterange",
        label = "Select date range:",
        start = min_date, end = max_date,
        startview = "year"
      ),

      HTML(paste("Enter a date between", min_date, "and", max_date)),

      # Select data table visualization
      checkboxInput(inputId = "show_data",
                    label = "Show data table:",
                    value = TRUE),

      HTML("Select filtype and variables to be downloaded using the donwload button"),
      radioButtons(inputId = "filetype",
                   label = "Select filetype:",
                   choices = c("csv", "tsv"),
                   selected = "csv"),

      selectInput(inputId = "selected_var",
                         label = "Select variables:",
                         choices = names(movies),
                         multiple = TRUE,
                         selected = c("title"))
    ),

    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot", brush = "plot_brush", hover = "plot_hover"),
      verbatimTextOutput("info"),
      textOutput(outputId = "n_recs"),
      textOutput(outputId = "correlation"),
      htmlOutput(outputId = "avgs"), # avg of x and y
      verbatimTextOutput(outputId = "lmoutput"), # regression output
      plotOutput(outputId = "densityplot", height = 200),
      dataTableOutput(outputId = "moviestable"),
      # Show data table
      tableOutput(outputId = "summarytable"),
      # Show data table
      HTML('Table with selected data (brush) in scatterplot:'),
      dataTableOutput(outputId = "brushtable"),
      HTML("Select filetype and variables, then hit 'Download data'."),
      downloadButton("download_data", "Download data")
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

   sampled_title_type_input <- reactive({
     req(input$studios)
      data <- sampled_studio_input()
      data %>%
        filter(title_type %in% input$selected_title_type)
    })

   sampledInput <- reactive({
     req(input$daterange)
     sampled_title_type_input() %>%
       filter(thtr_rel_date >= input$daterange[1], thtr_rel_date <= input$daterange[2])
   })

  output$scatterplot <- renderPlot({
    ggplot(data = sampledInput(), aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha)
  })

  output$n_recs <- renderText({
    n_recs <- nrow(sampledInput())
    paste('Number of selected movies:', n_recs)
  })

  output$densityplot <- renderPlot({
    ggplot(data = sampledInput(), aes_string(x = input$x, color = input$z)) +
      geom_density()
  })

  # Print data table if checked
  output$moviestable <- renderDataTable({
    if(input$show_data){
      DT::datatable(data = sampledInput() %>% select(title:studio),
                    extensions = c('Buttons', 'ColReorder', 'Responsive'), options = list(pageLength = 10,
                                                           dom = 'Bfrtip',
                                                           colReorder = TRUE,
                                                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'colvis')),
                    rownames = FALSE)
    }
  })

  output$summarytable <- renderTable({
    sampledInput() %>%
      group_by(mpaa_rating) %>%
      summarise(mean_score_ratio = mean(score_ratio),
                SD = sd(score_ratio),
                n = n())
  },
    striped = TRUE,
    spacing = "l",
    align = "lccr",
    digits = 4,
    width = "90%",
    caption = "Score ratio (audience / critics' scores) summary statistics by MPAA rating.")

  output$correlation <- renderText({
    data <- sampledInput()
    r <- round(cor(data[,input$x], data[,input$y]), digits = 2)
    paste0('Correlation: ', r, ". Note: If the relationship between the two variables is not linear, the correlation coefficient will not be meaningful.")
  })

  output$brushtable <- renderDataTable({
    data <- brushedPoints(sampledInput(), input$plot_brush)
    data[, c('title', input$x, input$y)]
  })

  output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    data <- nearPoints(sampledInput(), input$plot_hover, xvar = input$x, yvar = input$y)
    data[, c('title', input$x, input$y)]
    # nearPoints() also works with hover and dblclick events
  })

  output$avgs <- renderText({
    data <- sampledInput()
    avg_x <- data %>% pull(input$x) %>% mean() %>% round(2)
    avg_y <- data %>% pull(input$y) %>% mean() %>% round(2)
    str_x <- paste("Average", input$x, "=", avg_x)
    str_y <- paste("Average", input$y, "=", avg_y)
    HTML(paste(str_x, str_y, sep = '<br/>'))
  })

  output$avg_y <- renderText({
    avg_y <- sampledInput() %>% pull(input$y) %>% mean() %>% round(2)
    paste("Average", input$y, "=", avg_y)
  })

  output$lmoutput <- renderPrint({
    data <- sampledInput()
    x <- data %>% pull(input$x)
    y <- data %>% pull(input$y)
    summ <- summary(lm(y ~ x, data = data))
    print(summ, digits = 3, signif.stars = FALSE)
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("movies.", input$filetype)
    },
    content = function(file) {
      if(input$filetype == "csv"){
        write.csv(sampledInput() %>% select(input$selected_var), file)
      }
      if(input$filetype == "tsv"){
        write.tsv(sampledInput() %>% select(input$selected_var), file)
      }
    }
  )

}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
