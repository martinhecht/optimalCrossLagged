ui <- fluidPage(
  navbarPage(title=NULL,
            tabPanel("matrix input",uiOutput("matrInput")),
            # tabPanel("f1 (defined in code)",uiOutput("matrCalc1")),
            tabPanel("f2 (sourced)",uiOutput("matrCalc2"))
            # tabPanel("f3 (from package)",uiOutput("matrCalc3"))
  )
)
