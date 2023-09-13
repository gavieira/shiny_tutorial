library(shiny)

ui <-fluidPage(
  titlePanel('censusVis'),

  sidebarLayout(
    sidebarPanel(
      helpText('Create demographic maps with information from the 2010 US Census.'),
      selectInput('var',
                  label = 'Choose a variable to display',
                  choices = list(
                     'Percent White',
                     'Percent Black',
                     'Percent Hispanic',
                     'Percent Asian'
                  ), selected = 'Percent White' ),
      sliderInput('range',
                  label =  'Range of interest:',
                  value = c(0,100),
                  min = 0,
                  max = 100),
      checkboxGroupInput("checkGroup", label = h3("Checkbox group"),
                         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                         selected = 1),
      radioButtons("radio", label = h3("Radio buttons"),
                   choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                   selected = 1)
    ),
    mainPanel()
  )
)


# Define server logic ----
server <- function(input, output) {

}

# Run the app ----
shinyApp(ui = ui, server = server)
