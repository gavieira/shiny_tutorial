library(shiny)

ui <- fluidPage(
  titlePanel("Add 2/Mult 3"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("x", "Select x", min = 1, max = 50, value = 30)
    ),
    mainPanel(
      textOutput("x_add2"),
      textOutput("x_mult3")
      )
  )
)

server <- function(input, output, session) {
  add_2 <- function(x) {
    x + 2
  }

  mult_3 <- function(x) {
    x * 3
  }

  current_add2_x <- reactive({
    add_2(input$x)
    })

  output$x_add2 <- renderText({
    paste("Current add 2:", current_add2_x())
  })


  current_mult3_x <- reactive({
    mult_3(input$x)
    })

  output$x_mult3 <- renderText({
    paste("Current mult 3:", current_mult3_x())
  })

}

shinyApp(ui, server)
