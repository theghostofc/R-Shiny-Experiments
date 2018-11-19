library(shiny)

ui <- fluidPage(
  numericInput("n", "n", 50),
  selectizeInput(
    'mainfoo', label = 'Select Value', choices = state.name,
    options = list(create = TRUE)
  ),
  textInput('t', 'User Selected'),
  actionButton("go", "Go"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  
  randomVals <- eventReactive(input$go, {
    showModal(modalDialog(
      selectizeInput(
        'foo', label = 'Select', choices = state.name,
        selected = input$mainfoo,
        options = list(create = TRUE)
      ),
      "Pre-selected value - ",
      input$mainfoo,
      actionButton('sel', 'Select'),
      easyClose = TRUE,
      footer = NULL
    ))
    runif(input$n)
  })
  observeEvent(input$sel, {
    removeModal()
    updateTextInput(session, 't', value = input$foo)
    print(input$foo)
  })
  output$plot <- renderPlot({
    w = randomVals()
    hist(w)
  })
}

shinyApp(ui, server)
