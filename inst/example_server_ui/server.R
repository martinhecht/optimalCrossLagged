# server <- shinyServer(function(input, output, session){
server <- function(input, output){

  # packages
  require(shiny)
  require(shinyMatrix)
  
  # sourced R files
  source( "R/f2.R" )
  
  # matrix input
  matrInput <- reactive({ matrixInput(inputId="ARCL",value=diag(2)) })
  output$matrInput <- renderUI({ matrInput() })

  # calculation with sourced f2
  matrCalc2 <- reactive({
    x <- reactiveValuesToList(input)
    matr.dim <- sqrt(length(sapply(x$ARCL,c)))    
    matr <- matrix( as.numeric(x$ARCL), matr.dim, matr.dim )
    f2(matr)
  })
  output$matrCalc2 <- renderText({ matrCalc2() })

}
