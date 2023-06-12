library(shiny)
library(shinyMatrix)
library(dplyr)
library(R.utils)
library(rgenoud)
library(Rcpp)
library(RcppArmadillo)
#library(devtools)
#devtools::install_github("martinhecht/optimalCrossLagged")
#library(optimalclpm)
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/calculate.F.diff.fast.R" )
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/calculate.F.diff.precise.R" )
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/calculate.from.cost.function.R" )
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/calculate.power.LRT.R" )
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/fn.R" )
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/generate.model.example.3.R" )
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/get_all_labels.R" )
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/kickstart.optimizer.R" )
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/optmze.R" )
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/prepare.input.R" )
source( "http://amor.cms.hu-berlin.de/~hechtmay/optimalclpm/0.0.24/prepare.results.R" )


ui <- tabPanel(
  title = HTML("Calculate <b>Power</b>"),
  icon = icon("magnifying-glass-chart"),
  fluidPage(
    fluidRow(
      column(
        id = "one",
        width = 3,
        div(class = "unit", uiOutput("modelClassInput"),
                            uiOutput("modelClassCurrent"),
                            uiOutput("matrInput"),
                            uiOutput("matrCurrent"),
                            uiOutput("minTInput"),
                            uiOutput("minTCurrent")
                            )
      ),
      column(
        id = "two",
        width = 3,
        div(class = "unit", uiOutput("availinputobj"),
                            uiOutput("optimalclpmversion") )
      ),
      column(
        id = "three",
        width = 3,
        div(class = "unit", uiOutput("res") )
      )
    )
  )
)



server <- shinyServer(function(input, output, session){
  
  # matrix input/output
  #matrInput <- reactive({ matrixInput(inputId="ARCL",label="ARCL matrix",value=matrix(c(0.9,0.1,0.4,0.3),2,2)) })
  matrInput <- reactive({ matrixInput(inputId="ARCL",label="ARCL matrix",value=matrix(c(0.5,0.1,0.1,0.5),2,2)) })
  output$matrInput <- renderUI({ matrInput() })
  matrCurrent <- reactive({
    x <- reactiveValuesToList(input)
    x$ARCL
  })
  output$matrCurrent <- renderText({ matrCurrent() })

  # modelClass
  modelClassInput <- reactive({ 
    selectInput(
      inputId = "modelClass",
      label = h5(strong("Model Class")),
      choices = list(
        "CLPM" = "clpm",
        "factor CLPM" = "fclpm",
        "RI-CPLM" = "ri-clpm",
        "STARTS" = "starts",
        "LCM-SR" = "lcm-sr",
        "ALT" = "alt",
        "LCS" = "lcs"
      ),
      selected = "ri-clpm"
    )
  })
  output$modelClassInput <- renderUI({ modelClassInput() })
  modelClassCurrent <- reactive({
    x <- reactiveValuesToList(input)
    x$modelClass
  })
  output$modelClassCurrent <- renderText({ modelClassCurrent() })

  # minT
  minTInput <- reactive({
    mc.val <- c(2,3,3,3,3,4,3)
    names(mc.val) <- c("clpm","fclpm","ri-clpm","starts","lcm-sr","alt","lcs")
    if( req( input$modelClass ) %in% names(mc.val) ) minTidf <- mc.val[input$modelClass] else minTidf <- 1234
    if( !is.null(input$minT) && input$minT >= minTidf ) minTval <- input$minT else minTval <- minTidf
    numericInput(
      inputId = "minT",
      label = HTML("T min"),
      value = minTval,
      min = minTidf,
      max = 10000
    )
  })
  output$minTInput <- renderUI({ minTInput() })
  minTCurrent <- reactive({
    x <- reactiveValuesToList(input)
    x$minT
  })
  output$minTCurrent <- renderText({ minTCurrent() })

  ### optimalclpm version
  optimalclpmversion <- reactive({
    vers <- try( installed.packages()["optimalclpm","Version"], silent=TRUE )
    if( inherits(vers,"try-error") ) vers <- ""
    return(paste0( "optimalclpm version: <br>", vers ))
  })
  output$optimalclpmversion <- renderPrint({ optimalclpmversion() })

  ### available input objects
  availinputobj <- reactive({
#browser()
     #shiny:::flushReact()
     x <- reactiveValuesToList(input)
    #str(x) 
    
    return(paste0( "available input objects: <br>", paste(names(x),collapse="<br>")  ))
  })
  output$availinputobj <- renderPrint({ availinputobj() })

  ### optimizer call
  res <- reactive({
    
    # example specs
    specs <- generate.model.example.3()
    
#browser()
    # ARCL matrix
    if( length(input$ARCL)>0 ){
      specs$input_H1$Gamma$values <- array(as.numeric(input$ARCL),dim=dim(input$ARCL))
    }
    
    # optimizer call
    res <- optmze( model=list(# model specifications und target parameter aus shiny app
      "specification"=specs,
      "target.parameters"=c("ARCL_2_1", "ARCL_1_2"),
      "target.parameters.values.H0"=rep(0,2)),
      # budget, l2.cost, l1.cost, alpha aus shiny app
      study=list("budget"=20000, "l2.cost"=10, "l1.cost"=10, alpha=0.05 ),
      optimize=list(
        "what"=c("power"),
        "direction"=c("max"),
        "via"=c("power"),
        "par"=c("T"),
        "via.function"=c("calculate.power.LRT"),
        "optimizer"=c("genoud"),
        "starting.values"="round(mean(c(par.min.set,par.max.set)))",
        "set.seed.value"="random"
      ),
      # T.min, T.max, N.min, N.max aus shiny app
      constraints=list("T.min"=req(input$minT), "T.max"=40, "N.min"=3, "N.max"=1000,
                       "T.integer"=TRUE,
                       "N.integer"=FALSE ),									
      genoud=list("pop.size"=16,"max.generations"=100,"wait.generations"=1,
                  "boundary.enforcement"=2,"solution.tolerance"=0.001),
      verbose=FALSE )
    
    #shiny:::flushReact()
    #x <- reactiveValuesToList(input)
    #str(x) 
    
    return(paste0( "optimal N / T: <br><b>", paste(c(res$N.opt,res$T.opt),collapse=" / ")  ))
  })
  output$res <- renderPrint({ res() })

})
shinyApp(ui = ui, server = server)
