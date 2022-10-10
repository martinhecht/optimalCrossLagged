# JW: 0.0.37 2022-20-10: new function for erro-warn differentiation; error output$maxPower fixed; T.<x>.set instead of T.<x>.bound
# JW: 0.0.36 2022-10-06: tryCatch for results to suppress printing of internal errors
# JW: 0.0.30 2022-10-05: diverse things added and bugs fixed
# JW: 0.0.29 2022-09-02: error in IS and AB matrices corrected; problem with html and internet browser tab fixed in css; new script sourced
# JW: 0.0.26 2022-08-31: typo in compute_results() input corrected

# only for local run
setwd("/Users/julia/Documents/Arbeit/Promotion/Forschung/Projects/Shiny_App_Optimal_Design/optDynMo_JK-main")

# (install and) load packages
packages <- c("shiny", # basic
              "shinyjs", # for delay in code execution
              "magrittr", # pipe operator
              "devtools", # for shinyMatrix on github
              "shinyWidgets", # for pickerInput widget
              "dplyr", # for case_when
              "R.utils",
              "rgenoud",
              "RcppArmadillo",
              "matrixcalc", # check_plausability()
              "stringr",
              "RMariaDB", # for data log
              "here",
              "config" # same
)
newPackages <-
  packages[!(packages %in% installed.packages()[, "Package"])]
if (length(newPackages))
  install.packages(newPackages)
packages <- packages[-length(packages)] # prevent config from loading; otherwise base::get() masked
lapply(packages, require, character.only = TRUE)
if (!("shinyMatrix" %in% installed.packages()[, "Package"])){
  install_github("INWTlab/shiny-matrix") # version on github is more recent (i.e., editableCells parameter only here)
}
require(shinyMatrix)
if (!("shinyMatrix" %in% installed.packages()[, "Package"])){
  install_github("mitchelloharawild/icons") # does not appear in "Packages"
  download_fontawesome()
}
require(icons)



# source all relevant functions
source("R/calculate.F.diff.R")
source("R/calculate.from.cost.function.R")
source("R/calculate.power.LRT.R")
source("R/calculate.power.R")
source("R/check_plausability.R")
source("R/compute.se.oertzen.R")
source("R/fn.R")
source("R/get_all_labels.R")
source("R/helper.functions.R")
source("R/kickstart.optimizer.R")
source("R/optmze.R")
source("R/prepare.input.R")
source("R/prepare.results.R")
source("R/RcppExports.R")

ui <-
  # include file-based css in shiny: https://shiny.rstudio.com/articles/css.html
  navbarPage(tags$link(rel = "stylesheet", type = "text/css", href = "ui.css"),
             title = "OptDynMo: Optimal Design for Dynamic Longitudinal Models",
             
             tabPanel(
               title = "Calculate Power",
               icon = icon("magnifying-glass-chart"),
               fluidPage(
                 fluidRow(
                   column(
                     id = "one",
                     width = 4,
                     #tags$details(
                     #'open' = "TRUE",
                     #tags$summary(
                     div(
                       class = "main-box",
                       tags$span(class = "heading", "Study Design"),
                       #),
                       tags$br(),
                       tags$br(),
                       
                       fluidRow(
                         column(
                           width = 6,
                           div(class = "input-box",
                               tags$span(
                                 class = "hovertext",
                                 'data-hover' = "In any monetary unit, e.g. € or $.",
                                 icon("circle-question")
                               )  %>% tagAppendAttributes(style = "left: 101%;"),
                               numericInput(
                                 inputId = "budget",
                                 label = "Budget",
                                 value = 10000,
                                 min = 0,
                                 max = 10000000,
                                 step = 2500 # wird verdoppelt in UI!?
                               )
                           )
                         ),
                         column(
                           width = 6,
                           div(class = "input-box",
                               tags$span(
                                 class = "hovertext",
                                 'data-hover' = "Used for the Likelihood-Ratio-Tests in which the optimal N and T are calculated.",
                                 icon("circle-question")
                               )  %>% tagAppendAttributes(style = "left: 101%;"),
                               ## with arrows
                               numericInput(
                                 inputId = "alpha",
                                 label =
                                   withMathJax(c("\\(\\alpha\\)-Level")),
                                 value = 0.05,
                                 min = 0,
                                 max = 1,
                                 step = 0.05
                               )
                               ## without arrows (but not able to give value to backend)
                               # autonumericInput(
                               #   inputId = "alpha",
                               #   label =
                               #     withMathJax(c("\\(\\alpha\\)-Level")),
                               #   value = 0.05,
                               #   minimumValue = 0,
                               #   maximumValue = 1,
                               #   ecimalCharacter = ".",
                               #   align = "left"
                               # )
                           )
                         )
                       ), ### fluidRow budget-alpha
                       
                       fluidRow(
                         column(
                           width = 6,
                           div(id="boxN",
                               class = "input-box",
                               tags$span(
                                 class = "hovertext",
                                 'data-hover' = "Indicate the costs per unit. Indicate minimum and maximum number of units in the white boxes; the gray boxes indicate the adapted number of minimum or maximum number of units for the optimizer, respectively. The adapted numbers differ from your input because the parameters depend on each other.",
                                 icon("circle-question")
                               )  %>% tagAppendAttributes(style = "left: 101%;"),
                               p(HTML("<strong><i>Persons N</i></strong>")),
                               numericInput(
                                 inputId = "costN",
                                 label = HTML("Costs"),
                                 value = 10,
                                 min = 0,
                                 max = 10000000,
                                 step = 5
                               ),
                               splitLayout(
                                 #cellWidths = c("75%", "25%"),
                                 numericInput(
                                   inputId = "minN",
                                   label = HTML("Min"),
                                   value = 3,
                                   min = 2,
                                   max = 10000,
                                   step = 1
                                 ),
                                 conditionalPanel(
                                   condition = "output.errorCond == false", 
                                   div(class = "unit", uiOutput(outputId = "minN_Output"))
                                 )
                               ),
                               splitLayout(
                                 numericInput(
                                   inputId = "maxN",
                                   label = HTML("Max"),
                                   value = 300,
                                   min = 2,
                                   max = 10000,
                                   step = 1
                                 ),
                                 conditionalPanel(
                                   condition = "output.errorCond == false", 
                                   div(class = "unit", uiOutput(outputId = "maxN_Output"))
                                 )
                               )
                           ) ### div input-box N
                         ), ### column N
                         
                         column(
                           width = 6,
                           div(id="boxT",
                               class = "input-box",
                               tags$span(
                                 class = "hovertext",
                                 'data-hover' = "Indicate the costs per unit. Indicate minimum and maximum number of units in the white boxes; the gray boxes indicate the adapted number of minimum or maximum number of units for the optimizer, respectively. The adapted numbers differ from your input because the parameters depend on each other. The default for the minimum number of units reflects the minimum number of units (given the number of parameters) that is required for model identification.",
                                 icon("circle-question")
                               )  %>% tagAppendAttributes(style = "left: 101%;"),
                               p(HTML("<strong><i>Time Points T</i></strong>")),
                               numericInput(
                                 inputId = "costT",
                                 label = HTML("Costs"),
                                 value = 10,
                                 min = 0,
                                 max = 10000000,
                                 step = 5
                               ),
                               splitLayout(
                                 uiOutput(outputId = "minTidentify_Output"),
                                 conditionalPanel(
                                   condition = "output.errorCond == false", 
                                   div(class = "unit", uiOutput(outputId = "minT_Output"))
                                 )
                               ),
                               splitLayout(
                                 numericInput(
                                   inputId = "maxT",
                                   label = HTML("Max"),
                                   value = 10,
                                   min = 1,
                                   max = 10000,
                                   step = 1
                                 ),
                                 conditionalPanel(
                                   condition = "output.errorCond == false", 
                                   div(class = "unit", uiOutput(outputId = "maxT_Output"))
                                 )
                               )
                           ) ### div input-box T
                         ) #### column T
                       ) ### fluidRow (N & T)
                     ),
                     ### div study design
                     
                     #tags$details('open' = "TRUE",
                     # tags$summary(
                     div(
                       class = "main-box",
                       tags$span(class = "heading", "Model Characteristics"),
                       #),
                       tags$br(),
                       tags$br(),
                       div(class = "input-box",
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
                             selected = "clpm"
                           )
                       ),
                       # p(HTML("<small>For guidance concerning model selection see Usami, S., Murayama, K., & Hamaker, E. L. (2019). A unified framework of longitudinal models to examine reciprocal relations. Psychological Methods, 24(5), 637–657. <a href=\"https://doi.org/10.1037/met0000210\" target=\"_blank\">https://doi.org/10.1037/met0000210</a>.</small>")),
                       # tags$br(),
                       div(class = "input-box",
                           textAreaInput(
                             inputId = "procNames",
                             label = HTML("Process Names"),
                             placeholder = "proc1, proc2",
                             value = "proc1, proc2"
                           )
                       ),
                       
                       # only single-indicator for now
                       conditionalPanel(
                         condition = "input.modelClass == 'fclpm' | input.modelClass == 'starts' | input.modelClass == 'lcs'",
                         div(class = "input-box",
                             uiOutput(outputId = "measModel_Output")
                         )
                       )
                       # conditionalPanel(condition = "(input.modelClass == 'fclpm' | input.modelClass == 'starts' | input.modelClass == 'lcs') & (input.measModel.length > 0)",
                       #                  tags$br(),
                       #                  uiOutput(outputId = "indicat_Output"))
                     ) ### div model characteristics
                   ),
                   ### column one
                   
                   column(
                     id = "two",
                     class = "main-box",
                     #style = "left: 8px;",
                     width = 3,
                     #tags$details('open' = "TRUE",
                     #tags$summary(
                     tags$span(class = "heading", "Model Parameters"),
                     # ),
                     tags$br(),
                     tags$br(),
                     p(
                       HTML(
                         "First, <strong>set the model parameters in the matrices</strong>. Set them to 0 to exclude them from the model. Note that variances cannot be set to 0 for a power analysis."
                       )
                     ),
                     p(
                       HTML(
                         "Second, <strong>choose all target parameters in the drop-down menu</strong> that you want to include in the single likelihood ratio test and the joint power analysis. Note that it is not possible to specify every parameter as target parameter."
                       )
                     ),
                     tags$br(),
                     
                     ### removed for now: first measurement occasion params
                     # div(
                     #   class = "input-box", style="background-color: #c1c1c1",
                     #   span(
                     #     tags$span(
                     #       class = "hovertext",
                     #       'data-hover' = "Per default, parameters for the first measurement occasion are computed from your parameter inputs for all subsequent measurement occasions. You can also chose to set them by yourself.",
                     #       icon("circle-question")
                     #     ) %>% tagAppendAttributes(style = "left: 30%;"),
                     #     checkboxInput(
                     #       inputId = "firstCheck",
                     #       label = HTML("<strong>Fixed Parameters for <i>T = 1</i></strong>"),
                     #       value = TRUE
                     #     )
                     #   ),
                     #   tags$details(
                     #     tags$summary(class = "firstMeas", span("See/set parameters")),
                     #     tags$br(),
                     #
                     #     # fixed values
                     #
                     #     # standard
                     #     conditionalPanel(
                     #       condition = "input.firstCheck==1",
                     #       div(
                     #         style = "font-weight:normal; font-size:small;",
                     #         HTML(
                     #           "<strong>See</strong> the parameters for <i>T = 1</i>:"
                     #         )
                     #       ),
                     #       tags$br(),
                     #       p(HTML(
                     #         "<strong>Autoregressive Effects</strong>"
                     #       )),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "ARfixT1_Output")
                     #       ),
                     #       p(HTML("<strong>Cross-Lagged Effects</strong>")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "CLfixT1_Output")
                     #       ),
                     #       p(
                     #         HTML("<strong>Dynamic Residuals</strong>")
                     #       ),
                     #         p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "RESfixT1_Output")
                     #       )
                     #     ),
                     #
                     #     # Measurement Model
                     #     conditionalPanel(
                     #       condition = "input.firstCheck==1 & (input.modelClass == 'fclpm' | input.modelClass == 'starts' | input.modelClass == 'lcs') & (input.measModel.length > 0)",
                     #       p(HTML("<strong>Factor Loadings</strong>")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "LOADfixT1_Output")
                     #       ),
                     #       p(HTML("<strong>Unique Residuals</strong>")),
                     #       p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "UNIQfixT1_Output")
                     #       )
                     #     ),
                     #
                     #     # Intercept
                     #     conditionalPanel(
                     #       condition = "input.firstCheck==1 & (input.modelClass == 'ri-clpm' | input.modelClass == 'starts' | input.modelClass == 'lcm-scr')",
                     #       p(HTML("<strong>Random Intercepts</strong>")),
                     #       p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "IfixT1_Output")
                     #       )
                     #     ),
                     #
                     #     # Slope
                     #     conditionalPanel(
                     #       condition = "input.firstCheck==1 & input.modelClass == 'lcm-sr'",
                     #       p(HTML("<strong>Random Slopes</strong>")),
                     #       p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "SfixT1_Output")
                     #       )
                     #     ),
                     #
                     #     # Cov Int - Slope
                     #     conditionalPanel(
                     #       condition = "input.firstCheck==1 & input.modelClass == 'lcm-sr'",
                     #       p(HTML("<strong>Covariance Random Intercept and Random Slope</strong>")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "ISfixT1_Output")
                     #       )
                     #     ),
                     #
                     #     # constant accumulating factor A
                     #     conditionalPanel(
                     #       condition = "input.firstCheck==1 & (input.modelClass == 'alt' | input.modelClass == 'lcs')",
                     #       p(HTML("<strong>Constant Accumulating Factor A</strong>")),
                     #       p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "AfixT1_Output")
                     #       )
                     #     ),
                     #
                     #     # changing accumulating factor B
                     #     conditionalPanel(
                     #       condition = "input.firstCheck==1 & input.modelClass == 'alt'",
                     #       p(HTML("<strong>Changing Accumulating Factor B</strong>")),
                     #       p(HTML("Means")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "BmeanfixT1_Output")
                     #       ),
                     #       p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "BfixT1_Output")
                     #       )
                     #     ),
                     #
                     #     # coupling factor AB
                     #     conditionalPanel(
                     #       condition = "input.firstCheck==1 & input.modelClass == 'alt'",
                     #       p(HTML("<strong>Covariances Accumulating Factors A and B</strong>")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "ABfixT1_Output")
                     #         )
                     #       ),
                     #
                     #
                     #     # set parameters (T=1)
                     #
                     #     # standard
                     #     conditionalPanel(
                     #       condition = "input.firstCheck!=1",
                     #       div(
                     #         style = "font-weight:normal; font-size:small;",
                     #         HTML(
                     #           "<strong>Set</strong> the parameters for <i>T = 1</i>:´"
                     #         )
                     #       ),
                     #       tags$br(),
                     #       p(HTML(
                     #         "<strong>Autoregressive Effects</strong>"
                     #       )),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "ART1_Output")
                     #       ),
                     #       p(HTML("<strong>Cross-Lagged Effects</strong>")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "CLT1_Output")
                     #       ),
                     #       p(
                     #         HTML("<strong>Dynamic Residual</strong>")
                     #       ),
                     #         p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "REST1_Output")
                     #       ),
                     #       tags$span(
                     #         style = "font-weight:normal; font-size:small;",
                     #         "Please only fill in covariances in the lower triangular matrix."
                     #       ),
                     #       tags$br()
                     #     ),
                     #
                     #     # Measurement Model
                     #     conditionalPanel(
                     #       condition = "input.firstCheck!=1 & (input.modelClass == 'fclpm' | input.modelClass == 'starts' | input.modelClass == 'lcs') & (input.measModel.length > 0)",
                     #       p(HTML("<strong>Factor Loadings</strong>")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "LOADT1_Output")
                     #       ),
                     #       p(HTML("<strong>Unique Residuals</strong>")),
                     #       p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "UNIQT1_Output")
                     #       ),
                     #       tags$span(
                     #         style = "font-weight:normal; font-size:small;",
                     #         "Please only fill in covariances in the lower triangular matrix."
                     #       ),
                     #       tags$br()
                     #     ),
                     #
                     #     # Intercept
                     #     conditionalPanel(
                     #       condition = "input.firstCheck!=1 & (input.modelClass == 'ri-clpm' | input.modelClass == 'starts' | input.modelClass == 'lcm-scr')",
                     #       p(HTML("<strong>Random Intercepts</strong>")),
                     #       p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "IT1_Output")
                     #       ),
                     #       tags$span(
                     #         style = "font-weight:normal; font-size:small;",
                     #         "Please only fill in covariances in the lower triangular matrix."
                     #       ),
                     #       tags$br()
                     #     ),
                     #
                     #     # Slope
                     #     conditionalPanel(
                     #       condition = "input.firstCheck!=1 & input.modelClass == 'lcm-sr'",
                     #       p(HTML("<strong>Random Slopes</strong>")),
                     #       p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "ST1_Output")
                     #       ),
                     #       tags$span(
                     #         style = "font-weight:normal; font-size:small;",
                     #         "Please only fill in covariances in the lower triangular matrix."
                     #       ),
                     #       tags$br()
                     #     ),
                     #
                     #     # Cov Int - Slope
                     #     conditionalPanel(
                     #       condition = "input.firstCheck!=1 & input.modelClass == 'lcm-sr'",
                     #       p(HTML("<strong>Covariances Random Intercepts and Random Slopes</strong>")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "IST1_Output")
                     #       ),
                     #       tags$span(
                     #         style = "font-weight:normal; font-size:small;",
                     #         "Please only fill in the lower triangular matrix."
                     #       ),
                     #       tags$br()
                     #     ),
                     #
                     #     # constant accumulating factor A
                     #     conditionalPanel(
                     #       condition = "input.firstCheck!=1 & (input.modelClass == 'alt' | input.modelClass == 'lcs')",
                     #       p(HTML("<strong>Constant Accumulating Factor A</strong>")),
                     #       p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "AT1_Output")
                     #       ),
                     #       tags$span(
                     #         style = "font-weight:normal; font-size:small;",
                     #         "Please only fill in covariances in the lower triangular matrix."
                     #       ),
                     #       tags$br()
                     #     ),
                     #
                     #     # changing accumulating factor B
                     #     conditionalPanel(
                     #       condition = "input.firstCheck!=1 & input.modelClass == 'alt'",
                     #       p(HTML("<strong>Changing Accumulating Factor B</strong>")),
                     #       p(HTML("Means")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "BmeanT1_Output")
                     #       ),
                     #       p(HTML("Variances and Covariances")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "BT1_Output")
                     #       ),
                     #       tags$span(
                     #         style = "font-weight:normal; font-size:small;",
                     #         "Please only fill in covariances in the lower triangular matrix."
                     #       ),
                     #       tags$br()
                     #     ),
                     #
                     #     # coupling AB
                     #     conditionalPanel(
                     #       condition = "input.firstCheck!=1 & input.modelClass == 'alt'",
                     #       p(HTML("<strong>Covariances Accumulating Factors A and B</strong>")),
                     #       div(
                     #         class = "matrixWidget",
                     #         uiOutput(outputId = "ABT1_Output")),
                     #       tags$span(
                     #         style = "font-weight:normal; font-size:small;",
                     #         "Please only fill in the lower triangular matrix."
                     #       ),
                     #       tags$br()
                     #       )
                     #   )
                     # ),
                     
                     ### set parameters (T>2)
                     div(
                       class = "input-box",
                       tags$span(
                         class = "hovertext",
                         'data-hover' = "The AR effect is a partial regression coefficient from a variable at t-1 to the variable at t, after controlling for the CL effect of other variables on this variable at t-1. The CL effect is a partial regression coefficient from the predictor at t-1 to the outcome variable at t, after controlling for the AR effect of the outcome variable at t-1.",
                         icon("circle-question") # CLPM: rank order stability (i.e., between level effect) - Hamaker: between and within conflated, vs RI-CLPM: within-person carry-over
                       ) %>% tagAppendAttributes(style = "left: 30%; font-weight:normal;"),
                       p(
                         HTML("<strong>Autoregressive and Cross-Lagged Effects (AR & CL)</strong>")
                       ),
                       div(class = "matrixWidget", uiOutput(outputId = "ARCL_Output")),
                       tags$span(
                         style = "font-weight:normal; font-size:small;",
                         "Specfiy AR effects in the diagonal; CL effects in the off-diagonal. For the direction of CL effects: columns → rows"
                       ),
                       tags$br(),
                       tags$br(),
                       uiOutput(outputId = "targetARCL_Output")
                     ),
                     
                     div(
                       class = "input-box",
                       tags$span(
                         class = "hovertext",
                         'data-hover' = "The dynamic residuals ... . They affect future scores through the lagged regression effects (i.e., AR and CL effects).",
                         icon("circle-question")
                       ) %>% tagAppendAttributes(style = "left: 30%; font-weight:normal;"),
                       p(HTML("<strong>Dynamic Residuals (RES)</strong>")),
                       p(style = "font-weight:normal;", "Variances and Covariances"),
                       div(class = "matrixWidget", uiOutput(outputId = "RES_Output")),
                       tags$span(
                         style = "font-weight:normal; font-size:small;",
                         "Please only fill in covariances in lower triangular matrix."
                       ),
                       tags$br(),
                       tags$br(),
                       uiOutput(outputId = "targetRES_Output")
                     ),
                     
                     # Measurement Model
                     conditionalPanel(
                       condition = "(input.modelClass == 'fclpm' | input.modelClass == 'starts' | input.modelClass == 'lcs') & (input.measModel.length > 0)",
                       ### removed for now: Factor Loadings
                       # div(
                       #   class = "input-box",
                       #   tags$span(
                       #     class = "hovertext",
                       #     'data-hover' = "...",
                       #     icon("circle-question")
                       #   ) %>% tagAppendAttributes(style = "left: 30%; font-weight:normal;"),
                       #   p(HTML("<strong>Factor Loadings</strong>")),
                       #   uiOutput(outputId = "LOAD_Output"),
                       #   uiOutput(outputId = "targetLOAD_Output")
                       # ),
                       div(
                         class = "input-box",
                         tags$span(
                           class = "hovertext",
                           'data-hover' = "The unique residuals are the measurement errors which are unique for each time point. In contrast to dynamic residuals, they do not have a temporal effect.",
                           icon("circle-question")
                         ) %>% tagAppendAttributes(style = "left: 30%; font-weight:normal;"),
                         p(HTML("<strong>Unique Residuals (UNIQ)<strong>")),
                         p(style = "font-weight:normal;", "Variances and Covariances"),
                         div(class = "matrixWidget", uiOutput(outputId = "UNIQ_Output")),
                         tags$span(
                           style = "font-weight:normal; font-size:small;",
                           "Please only fill in covariances in lower triangular matrix."
                         ),
                         tags$br(),
                         tags$br(),
                         uiOutput(outputId = "targetUNIQ_Output")
                       )
                     ),
                     
                     # Intercept (Persons means for ri-clpm and starts, growth curve for lcm-sr)
                     conditionalPanel(
                       condition = "input.modelClass == 'ri-clpm' | input.modelClass == 'starts' | input.modelClass == 'lcm-sr'",
                       div(
                         class = "input-box",
                         tags$span(
                           class = "hovertext",
                           'data-hover' = "...",
                           icon("circle-question")
                         ) %>% tagAppendAttributes(style = "left: 30%; font-weight:normal;"),
                         p(HTML("<strong>Random Intercepts (I)</strong>")),
                         p(style = "font-weight:normal;", "Variances and Covariances"),
                         div(class = "matrixWidget", uiOutput(outputId = "I_Output")),
                         tags$span(
                           style = "font-weight:normal; font-size:small;",
                           "Please only fill in covariances in lower triangular matrix."
                         ),
                         tags$br(),
                         tags$br(),
                         uiOutput(outputId = "targetI_Output")
                       )
                     ),
                     
                     ### Slope
                     conditionalPanel(
                       condition = "input.modelClass == 'lcm-sr'",
                       div(
                         class = "input-box",
                         tags$span(
                           class = "hovertext",
                           'data-hover' = "...",
                           icon("circle-question")
                         ) %>% tagAppendAttributes(style = "left: 30%; font-weight:normal;"),
                         p(HTML("<strong>Random Slopes (S)</strong>")),
                         p(style = "font-weight:normal;", "Variances and Covariances"),
                         div(class = "matrixWidget", uiOutput(outputId = "S_Output")),
                         tags$span(
                           style = "font-weight:normal; font-size:small;",
                           "Please only fill in covariances in lower triangular matrix."
                         ),
                         tags$br(),
                         tags$br(),
                         uiOutput(outputId = "targetS_Output")
                       )
                     ),
                     
                     ### Cov Int - Slope
                     conditionalPanel(
                       condition = "input.modelClass == 'lcm-sr'",
                       div(
                         class = "input-box",
                         tags$span(
                           class = "hovertext",
                           'data-hover' = "...",
                           icon("circle-question")
                         ) %>% tagAppendAttributes(style = "left: 30%; font-weight:normal;"),
                         p(
                           HTML(
                             "<strong>Covariances of Random Intercepts and Random Slopes (IS)</strong>"
                           )
                         ),
                         div(class = "matrixWidget", uiOutput(outputId = "IS_Output")),
                         tags$br(),
                         uiOutput(outputId = "targetIS_Output")
                       )
                     ),
                     
                     ### constant Accumulating Factor A
                     conditionalPanel(
                       condition = "(input.modelClass == 'alt' | input.modelClass == 'lcs')",
                       div(
                         class = "input-box",
                         tags$span(
                           class = "hovertext",
                           'data-hover' = "...",
                           icon("circle-question")
                         ) %>% tagAppendAttributes(style = "left: 30%; font-weight:normal;"),
                         p(HTML(
                           "<strong>Constant Accumulating Factor (A)</strong>"
                         )),
                         p(style = "font-weight:normal;", "Variances and Covariances"),
                         div(class = "matrixWidget", uiOutput(outputId = "A_Output")),
                         uiOutput(outputId = "targetA_Output")
                       )
                     ),
                     
                     ### changing Accumulating Factor B
                     conditionalPanel(
                       condition = "input.modelClass == 'alt'",
                       div(
                         class = "input-box",
                         tags$span(
                           class = "hovertext",
                           'data-hover' = "...",
                           icon("circle-question")
                         ) %>% tagAppendAttributes(style = "left: 30%; font-weight:normal;"),
                         p(HTML(
                           "<strong>Changing Accumulating Factor (B)</strong>"
                         )),
                         ### removed for now:
                         # p(style = "font-weight:normal;", "Means"),
                         # div(class="matrixWidget", uiOutput(outputId = "Bmean_Output")),
                         # uiOutput(outputId = "targetBmean_Output"),
                         p(style = "font-weight:normal;", "Variances and Covariances"),
                         div(class = "matrixWidget", uiOutput(outputId = "B_Output")),
                         uiOutput(outputId = "targetB_Output")
                       )
                     ),
                     
                     # Cov AB
                     conditionalPanel(
                       condition = "input.modelClass == 'alt'",
                       div(
                         class = "input-box",
                         tags$span(
                           class = "hovertext",
                           'data-hover' = "...",
                           icon("circle-question")
                         ) %>% tagAppendAttributes(style = "left: 30%; font-weight:normal;"),
                         p(
                           HTML("<strong>Covariance of Accumulating Factors (AB)</strong>")
                         ),
                         div(class = "matrixWidget", uiOutput(outputId = "AB_Output")),
                         tags$br(),
                         uiOutput(outputId = "targetAB_Output")
                       )
                     )
                   ),
                   #### column two
                   
                   column(
                     id = "three",
                     width = 5,
                     
                     # main-box(
                     ### removed for now (not working yet, too): SEM Plot
                     # div(style = "min-height: 20px; padding: 19px; margin-bottom: 20px; background-color: #f5f5f5; border: 1px solid #e3e3e3; border-radius: 4px; box-shadow: inset 0 1px 1px rgba(0,0,0,.05);",
                     #     plotOutput("platzhalter")),
                     
                     
                     div(
                       class = "main-box",
                       tags$p(class = "heading", "Results"),
                       # tags$span(
                       #   class = "hovertext",
                       #   'data-hover' = "lorem ipsum",
                       #   icon("circle-question")
                       # )  %>% tagAppendAttributes(style = "left: 40%;"),
                       
                       div(class = "output-box", style="border: 3px solid black;",
                           tags$span(class = "hovertext", style="font-weight:normal;",
                                     'data-hover' = "Likelihood-Ratio Test of Single Target Parameters",
                                     icon("circle-question")
                           )  %>% tagAppendAttributes(style = "left: 10%;"),
                           #div(class = "heading", "Likelihood-Ratio Test of Single Target Parameters"),
                           #tags$br(),
                           span(style="font-weight:normal; font-variant:small-caps;", HTML("Optimal number of Persons <i>N</i>:")), textOutput("optN", inline=T), 
                           tags$br(),
                           span(style="font-weight:normal; font-variant:small-caps;", HTML("Optimal number of Time Points <i>T</i>:")), textOutput("optT", inline=T)
                       ),
                       
                       div(class = "output-box", 
                           tags$span(
                             class = "hovertext", style="font-weight:normal;",
                             'data-hover' = "lorem ipsum 2",
                             icon("circle-question")
                           )  %>% tagAppendAttributes(style = "left: 40%;"),
                           div(class = "heading", "Maximum Power For all Target Parameters"),
                           span(style="font-weight:normal; font-variant:small-caps;",
                                HTML(
                                  "Given the optimal number of <i>N</i> and <i>T</i>"
                                )), 
                           tags$br(),
                           tags$br(),
                           DT::dataTableOutput("maxPowerTable")
                       ),
                       
                       conditionalPanel(
                         condition = "output.errorCond == true",
                         div(class = "error-box", htmlOutput("error", inline=T)
                         )
                       ),
                       
                       conditionalPanel(
                         condition = "output.warningCond == true",
                         div(class = "warn-box", htmlOutput("warn", inline=T)
                         )
                       )
                     ),
                     
                     div(class="main-box",
                         tags$p(class = "heading", "Technical Details"),
                         
                         fluidRow(
                           
                           column(
                             width = 6,
                             div(class = "input-box", style="padding-bottom: 5px;",
                                 sliderInput(
                                   inputId = "popSize",
                                   label = "Precision of Optimizer",
                                   value = 16,
                                   min = 16,
                                   max = 1000,
                                   ticks=FALSE
                                 )
                             ),
                             div(class = "input-box", style="padding-bottom: 2px; padding-top: 2px; font-size:12px;",
                                 checkboxInput(
                                   inputId = "dbLog",
                                   label = HTML("<b>Log results for scientific optimization of this app?</b>"),
                                   value=TRUE
                                 )
                             )
                           ),
                           
                           
                           column(
                             width = 6,
                             div(class = "output-box",
                                 span(style="font-weight:normal; font-variant:small-caps;", "Run Time (in sec):"), textOutput("runTime", inline=T),
                                 br(), span(style="font-weight:normal; font-variant:small-caps;", "Number of Runs:"), textOutput("optRuns", inline=T),
                                 # necessary bc https://github.com/rstudio/shiny/issues/1318
                                 #br(), span(style="font-weight:normal; font-variant:small-caps;", "Errors:"), textOutput("errorCond", inline=T),
                                 br(), 
                                 br(), span(style="font-weight:normal; font-variant:small-caps;", "Log Run Time (in sec):"), textOutput("runTimeDB", inline=T),
                                 br(), span(style="font-weight:normal; font-variant:small-caps;", "Log ID:"), textOutput("logID", inline=T),
                                 br(), span(style="font-weight:normal; font-variant:small-caps;", "Log Status:"), textOutput("logDB", inline=T)
                             )
                           )
                           
                         )
                     ),
                     
                     div(
                       class = "main-box",
                       p(class = "heading", "Citation"),
                       p(style="font-weight:normal;", HTML("This shiny app is based on the following article: ..."))
                     ),
                     tags$details(#'open' = "FALSE",
                       tags$summary(span("Dev Ouput")),
                       verbatimTextOutput("results")
                     ),
                     
                     # have to stay!!! otherwise JS using errorCond and warningCond won't evaluate
                     span(style="color:white;", textOutput("errorCond", inline=T)), 
                     span(style="color:white;", textOutput("warningCond", inline=T)) 
                     
                   ) ### column three
                 ) ### fluidRow POWER
               ) ### fluidpage POWER
             )
             ### tabpanel POWER
             
             ### removed for now: other panels, "compute budget" and "how to cite" (not implemented yet and problems with tab panels)
             # tabPanel(
             #   title = "How To Cite",
             #   icon = icon("pen-fancy"),
             #   fluidPage(
             #   fluidRow(
             #     column(
             #       class = "main-box",
             #       width = 3,
             #       p("Coming soon"),
             #       HTML("lolo")
             #     ),
             #     column(class = "main-box", width = 3,
             #            div(fluidRow(
             #              p("muhu"), HTML("kl")
             #            )))
             #   )
             # )
             # )
  )

#################################################################################################
#################################################################################################

server <- function(input, output, session) {
  ### zum debuggen, um zu schauen welche werte input hat
  #output$value <- renderPrint({ error_messages_translation(res()$res$error_codes) })
  # in kombi mit:
  # verbatimTextOutput("value")
  
  ### for study design tab:
  
  minTidentify <- reactive({
    mc <- input$modelClass
    minTidf <- case_when(mc == "clpm" ~ 2,
                         mc == "fclpm" ~ 3,
                         mc == "ri-clpm" ~ 3,
                         mc == "starts" ~ 3,
                         mc == "lcm-sr" ~ 3,
                         mc == "alt" ~ 4,
                         mc == "lcs" ~ 3
    )
    
  })
  
  output$minTidentify_Output <- renderUI({
    numericInput(
      inputId = "minT",
      label = HTML("Min"),
      value = minTidentify(),
      min = minTidentify(),
      max = 10000
    )
  })
  
  output$minT_Output <- renderText({
    tryCatch(round(res()$res$constraints$T.min.set, 0), error = function(e){""})
  })
  
  output$maxT_Output <- renderText({
    tryCatch(round(res()$res$constraints$T.max.set, 0), error = function(e){""})
  })
  
  output$minN_Output <- renderText({
    tryCatch(round(res()$res$constraints$N.min.bound, 0), error = function(e){""})
  })
  
  output$maxN_Output <- renderText({
    tryCatch(round(res()$res$constraints$N.max.bound, 0), error = function(e){""})
  })
  
  ### for model characteristics tab
  
  output$measModel_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| "))) # get values
    # patterns strsplit: https://stackoverflow.com/questions/10738729/r-strsplit-with-multiple-unordered-split-arguments
    # as.list(unlist(..)) to get sublist
    names(procNames_List) <-
      unlist(strsplit(input$procNames, split = "\\, |\\,| ")) # named list needed for checkboxGroupInput(choices)
    checkboxGroupInput(
      inputId = "measModel",
      label = HTML("Which Processes have a Measurement Model?"),
      choices = procNames_List,
      selected = procNames_List #--> NULL damit indic conditional worked
    )
  })
  
  # output$indicat_Output <- renderUI({
  #   measModelNb <-
  #     length(input$measModel) # nb of processses with measurement model
  #   
  #   div(p(HTML("<b>Number of indicators</b>")),
  #       lapply(seq(measModelNb), function(i) {
  #         div(
  #           style = "width: 14vw; font-weight: lighter;",
  #           numericInput(
  #             inputId = paste0("indicat_", input$measModel[i]),
  #             label = input$measModel[i],
  #             # names of indicators
  #             value = 1,
  #             min = 1,
  #             max = 20
  #           )
  #         )
  #       }))
  # })
  
  ### for model parameters tab
  
  ## for fixed T1
  
  # output$ARfixT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "ARfixT1",
  #               value = preCovmat,
  #               cols = list(names = FALSE),
  #               cells = list(editableCells = FALSE))
  # })
  # 
  # 
  # output$CLfixT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "CLfixT1",
  #               value = preCovmat,
  #               cols = list(names = FALSE),
  #               cells = list(editableCells = FALSE))
  # })
  # 
  # # output$ARCLfixT1_Output <- renderUI({
  # #   # ...
  # # })
  # # 
  # # output$RESfixT1_Output <- renderUI({
  # #   procNames_List <-
  # #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  # #   procNb <- length(procNames_List)
  # #   preCovmat <-
  # #     matrix(
  # #       0.5,
  # #       nrow = procNb,
  # #       ncol = procNb,
  # #       dimnames = list(procNames_List, procNames_List)
  # #     )
  # #   diag(preCovmat) <- 1
  # #   preCovmat[upper.tri(preCovmat)] <- ""
  # #   matrixInput(inputId = "RESfixT1",
  # #               value = preCovmat,
  # #               cells = list(editableCells = FALSE))
  # # })
  # 
  # output$LOADfixT1_Output <- renderUI({
  #   measModelNb <- length(input$measModel) 
  #   lapply(seq(measModelNb), function(i) {
  #     indNb <- get("input")[[paste0("indicat_", input$measModel[i])]] # get() is hard to handle with shiny; https://stackoverflow.com/questions/56127727/using-get-to-call-input-objects-indirectly-within-shiny-r
  #     indNames <- c()
  #     for (n in 1:indNb){
  #       indNames[n] <- paste0("ind_", n)
  #     }
  #     preLOAD <-
  #       matrix(
  #         1,
  #         nrow = indNb,
  #         ncol = 1,
  #         dimnames = list(indNames, input$measModel[i])
  #       )
  #     if (i < measModelNb){
  #       div(class = "matrixWidget", style="margin-bottom:-16px;",
  #           matrixInput(inputId = paste0("LOADfixT1_", input$measModel[i]),
  #                       value = preLOAD,
  #                       cells = list(editableCells = FALSE)
  #           )
  #       )
  #     } else {
  #       div(class = "matrixWidget",
  #           matrixInput(inputId = paste0("LOADfixT1_", input$measModel[i]),
  #                       value = preLOAD,
  #                       cells = list(editableCells = FALSE)
  #           )
  #       )
  #     }
  #   })
  # })
  # 
  # output$UNIQfixT1_Output <- renderUI({
  #   measModelNb <- length(input$measModel) 
  #   indNames <- c
  #   for (i in seq(measModelNb)) {
  #     indNb <- get("input")[[paste0("indicat_", input$measModel[i])]] 
  #     temp <- c()
  #     for (n in 1:indNb){
  #       temp[n] <- paste0(input$measModel[i], " ind_", n)
  #     }
  #     indNames <- append(indNames, temp)
  #   }
  #   indNames <- indNames[-1] # otherwise first element is primitive("c")
  #   indNb <- length(indNames)
  #   preCovmat <-
  #     matrix(
  #       diag(indNb),
  #       nrow = indNb,
  #       ncol = indNb,
  #       dimnames = list(indNames, indNames)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "UNIQfixT1",
  #               value = preCovmat,
  #               cells = list(editableCells = FALSE))
  # })
  # 
  # output$IfixT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       diag(indNb),
  #       nrow = procNb,
  #       ncol = procNb,
  #       dimnames = list(procNames_List, procNames_List)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "IfixT1",
  #               value = preCovmat,
  #               cells = list(editableCells = FALSE))
  # })
  # 
  # output$SfixT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       diag(indNb),
  #       nrow = procNb,
  #       ncol = procNb,
  #       dimnames = list(procNames_List, procNames_List)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "SfixT1",
  #               value = preCovmat,
  #               cells = list(editableCells = FALSE))
  # })
  # 
  # output$ISfixT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = procNb,
  #       dimnames = list(procNames_List, procNames_List)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   diag(preCovmat) <- ""
  #   matrixInput(inputId = "ISfixT1",
  #               value = preCovmat,
  #               cells = list(editableCells = FALSE))
  # })
  # 
  # output$AfixT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "AfixT1",
  #               value = preCovmat,
  #               cols = list(names = FALSE),
  #               cells = list(editableCells = FALSE))
  # })
  # 
  # output$BmeanfixT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "BmeanfixT1",
  #               value = preCovmat,
  #               cols = list(names = FALSE),
  #               cells = list(editableCells = FALSE))
  # })
  # 
  # output$BfixT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "BfixT1",
  #               value = preCovmat,
  #               cols = list(names = FALSE),
  #               cells = list(editableCells = FALSE))
  # })
  # 
  # output$ABfixT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   ABNames <- c()
  #   count <- 0
  #   for (i in 1:procNb){
  #     count <- count + 1
  #     ABNames[count] <- paste(procNames_List[i], "A")
  #     count <- count + 1
  #     ABNames[count] <- paste(procNames_List[i], "B")
  #   }
  #   ABall <- length(ABNames)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = ABall,
  #       ncol = ABall,
  #       dimnames = list(ABNames, ABNames)
  #     )
  #   diag(preCovmat) <- ""
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "ABfixT1",
  #               value = preCovmat,
  #               cells = list(editableCells = FALSE))
  # })
  # 
  # ## for setting params T1
  # 
  # output$ART1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "ART1",
  #               value = preCovmat,
  #               cols = list(names = FALSE))
  # })
  # 
  # output$CLT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "CLT1",
  #               value = preCovmat,
  #               cols = list(names = FALSE))
  # })
  # 
  # 
  # output$REST1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = procNb,
  #       dimnames = list(procNames_List, procNames_List)
  #     )
  #   diag(preCovmat) <- 1
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "REST1",
  #               value = preCovmat)
  # })
  # 
  # # das sind keine gekoppelten params wie AB und IS, sondern gamma und beta (AR und CL) mit Startwerten, die wir auch zu diesen packen
  # # output$ARCLT1_Output <- renderUI({
  # #   # ...
  # # })
  # 
  # output$LOADT1_Output <- renderUI({
  #   measModelNb <- length(input$measModel) 
  #   lapply(seq(measModelNb), function(i) {
  #     indNb <- get("input")[[paste0("indicat_", input$measModel[i])]] # get() is hard to handle with shiny; https://stackoverflow.com/questions/56127727/using-get-to-call-input-objects-indirectly-within-shiny-r
  #     indNames <- c()
  #     for (n in 1:indNb){
  #       indNames[n] <- paste0("ind_", n)
  #     }
  #     preLOAD <-
  #       matrix(
  #         1,
  #         nrow = indNb,
  #         ncol = 1,
  #         dimnames = list(indNames, input$measModel[i])
  #       )
  #     if (i < measModelNb){
  #       div(class = "matrixWidget", style="margin-bottom:-16px;",
  #           matrixInput(inputId = paste0("LOADT1_", input$measModel[i]),
  #                       value = preLOAD)
  #       )
  #     } else {
  #       div(class = "matrixWidget", 
  #           matrixInput(inputId = paste0("LOADT1_", input$measModel[i]),
  #                       value = preLOAD)
  #       )
  #     }
  #   })
  # })
  # 
  # output$UNIQT1_Output <- renderUI({
  #   measModelNb <- length(input$measModel) 
  #   indNames <- c
  #   for (i in seq(measModelNb)) {
  #     indNb <- get("input")[[paste0("indicat_", input$measModel[i])]] 
  #     temp <- c()
  #     for (n in 1:indNb){
  #       temp[n] <- paste0(input$measModel[i], " ind_", n)
  #     }
  #     indNames <- append(indNames, temp)
  #   }
  #   indNames <- indNames[-1] # otherwise first element is primitive("c")
  #   indNb <- length(indNames)
  #   preCovmat <-
  #     matrix(
  #       diag(indNb),
  #       nrow = indNb,
  #       ncol = indNb,
  #       dimnames = list(indNames, indNames)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "UNIQT1",
  #               value = preCovmat)
  # })
  # 
  # output$IT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       diag(indNb),
  #       nrow = procNb,
  #       ncol = procNb,
  #       dimnames = list(procNames_List, procNames_List)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "IT1",
  #               value = preCovmat)
  # })
  # 
  # output$S1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       diag(indNb),
  #       nrow = procNb,
  #       ncol = procNb,
  #       dimnames = list(procNames_List, procNames_List)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "ST1",
  #               value = preCovmat)
  # })
  # 
  # output$IST1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = procNb,
  #       dimnames = list(procNames_List, procNames_List)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   diag(preCovmat) <- ""
  #   matrixInput(inputId = "IST1",
  #               value = preCovmat)
  # })
  # 
  # output$AT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "AT1",
  #               value = preCovmat,
  #               cols = list(names = FALSE))
  # })
  # 
  # output$BmeanT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "BmeanT1",
  #               value = preCovmat,
  #               cols = list(names = FALSE))
  # })
  # 
  # output$BT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "BT1",
  #               value = preCovmat,
  #               cols = list(names = FALSE))
  # })
  # 
  # output$ABT1_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   ABNames <- c()
  #   count <- 0
  #   for (i in 1:procNb){
  #     count <- count + 1
  #     ABNames[count] <- paste(procNames_List[i], "A")
  #     count <- count + 1
  #     ABNames[count] <- paste(procNames_List[i], "B")
  #   }
  #   ABall <- length(ABNames)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = ABall,
  #       ncol = ABall,
  #       dimnames = list(ABNames, ABNames)
  #     )
  #   diag(preCovmat) <- ""
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "ABT1",
  #               value = preCovmat)
  # })
  
  
  ## for setting params (T>1)
  
  output$ARCL_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    procNb <- length(procNames_List)
    preCovmat <-
      matrix(
        0.1,
        nrow = procNb,
        ncol = procNb,
        dimnames = list(procNames_List, procNames_List)
      )
    diag(preCovmat) <- 0.5
    matrixInput(inputId = "ARCL",
                value = preCovmat)
  })
  
  output$targetARCL_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| "))) # 2, 3, 4
    ARCLnames <- labelsL(procNames_List, "ARCL") # 2, 6, 12
    ARCLsel <- ARCLnames[-c(1:length(procNames_List))]
    pickerInput(
      inputId = "targetARCL", 
      choices = ARCLnames, 
      selected = ARCLsel, # all CL effects
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  output$RES_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    procNb <- length(procNames_List)
    preCovmat <-
      matrix(
        diag(procNb),
        nrow = procNb,
        ncol = procNb,
        dimnames = list(procNames_List, procNames_List)
      )
    preCovmat[upper.tri(preCovmat)] <- ""
    matrixInput(inputId = "RES",
                value = preCovmat)
  })
  
  output$targetRES_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    RESnames <- labelsL(procNames_List, "RES")
    pickerInput(
      inputId = "targetRES", 
      choices = RESnames, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  # output$LOAD_Output <- renderUI({ # erstmal nur single-indicator!
  #   measModelNb <- length(input$measModel) 
  #   lapply(seq(measModelNb), function(i) {
  #     indNb <- 1
  #     #indNb <- get("input")[[paste0("indicat_", input$measModel[i])]] # get() is hard to handle with shiny; https://stackoverflow.com/questions/56127727/using-get-to-call-input-objects-indirectly-within-shiny-r
  #     indNames <- c()
  #     for (n in 1:indNb){
  #       indNames[n] <- paste0("ind_", n)
  #     }
  #     preLOAD <-
  #       matrix(
  #         1,
  #         nrow = indNb,
  #         ncol = 1,
  #         dimnames = list(indNames, input$measModel[i])
  #       )
  #     if (i < measModelNb){
  #       div(class = "matrixWidget", style="margin-bottom:-16px; cursor: default;",
  #           matrixInput(inputId = paste0("LOADfixT1_", input$measModel[i]),
  #                       value = preLOAD,
  #                       cells = list(editableCells = FALSE)
  #           )
  #       )
  #     } else {
  #       div(class = "matrixWidget",
  #           matrixInput(inputId = paste0("LOAD_", input$measModel[i]),
  #                       value = preLOAD,
  #                       cells = list(editableCells = FALSE)
  #           )
  #       )
  #     }
  #   })
  # })
  # 
  # output$targetLOAD_Output <- renderUI({ # erstmal nur single-indicator!
  #   measModelNb <- length(input$measModel) 
  #   indNamesAll <- c()
  #   count <- 0
  #   for (i in seq(measModelNb)){
  #     indNb <- 1
  #     #indNb <- get("input")[[paste0("indicat_", input$measModel[i])]]
  #     for (j in 1:indNb){
  #       count <- count + 1
  #       indNamesAll[count] <- paste0(input$measModel[i], " ind_", j)
  #     }
  #   }
  #   pickerInput(
  #     inputId = "targetLOAD", 
  #     choices = indNamesAll,
  #     options = list(
  #       `actions-box` = TRUE, 
  #       size = 10,
  #       `selected-text-format` = "count > 3"
  #     ), 
  #     multiple = TRUE
  #   )
  # })
  
  output$UNIQ_Output <- renderUI({ # erstmal nur single-indicator!
    measModelNb <- length(input$measModel) 
    indNames <- c
    for (i in seq(measModelNb)) {
      indNb <- 1
      #indNb <- get("input")[[paste0("indicat_", input$measModel[i])]] 
      temp <- c()
      for (n in 1:indNb){
        temp[n] <- input$measModel[i] #paste0(input$measModel[i], " ind_", n)
      }
      indNames <- append(indNames, temp)
    }
    indNames <- indNames[-1] # otherwise first element is primitive("c")
    indNb <- length(indNames)
    preCovmat <-
      matrix(
        diag(indNb),
        nrow = indNb,
        ncol = indNb,
        dimnames = list(indNames, indNames)
      )
    preCovmat[upper.tri(preCovmat)] <- ""
    
    matrixInput(inputId = "UNIQ",
                value = preCovmat)
  })
  
  output$targetUNIQ_Output <- renderUI({ # erstmal nur single-indicator!
    UNIQnames <- labelsL(NULL, "UNIQ", input$measModel)
    pickerInput(
      inputId = "targetUNIQ", 
      choices = UNIQnames, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  output$I_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    procNb <- length(procNames_List)
    preCovmat <-
      matrix(
        diag(procNb),
        nrow = procNb,
        ncol = procNb,
        dimnames = list(procNames_List, procNames_List)
      )
    preCovmat[upper.tri(preCovmat)] <- ""
    matrixInput(inputId = "I",
                value = preCovmat)
  })
  
  output$targetI_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    Inames <- labelsL(procNames_List, "I")
    pickerInput(
      inputId = "targetI", 
      choices = Inames, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  output$S_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    procNb <- length(procNames_List)
    preCovmat <-
      matrix(
        diag(procNb),
        nrow = procNb,
        ncol = procNb,
        dimnames = list(procNames_List, procNames_List)
      )
    preCovmat[upper.tri(preCovmat)] <- ""
    matrixInput(inputId = "S",
                value = preCovmat)
  })
  
  output$targetS_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    Snames <- labelsL(procNames_List, "S")
    pickerInput(
      inputId = "targetS", 
      choices = Snames, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  output$IS_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    procNb <- length(procNames_List)
    INames <- c()
    SNames <- c()
    for (i in 1:procNb){
      INames[i] <- paste("I ", procNames_List[i])
      SNames[i] <- paste("S ", procNames_List[i])
    }
    preCovmat <-
      matrix(
        0,
        nrow = procNb,
        ncol = procNb,
        dimnames = list(INames, SNames)
      )
    matrixInput(inputId = "IS",
                value = preCovmat)
  })
  
  output$targetIS_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    ISnames <- labelsL(procNames_List, "IS")
    pickerInput(
      inputId = "targetIS", 
      choices = ISnames, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  output$A_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    procNb <- length(procNames_List)
    preCovmat <-
      matrix(
        diag(procNb),
        nrow = procNb,
        ncol = procNb,
        dimnames = list(procNames_List, procNames_List)
      )
    preCovmat[upper.tri(preCovmat)] <- ""
    matrixInput(inputId = "A",
                value = preCovmat)
  })
  
  output$targetA_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    Anames <- labelsL(procNames_List, "A")
    pickerInput(
      inputId = "targetA", 
      choices = Anames, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  # output$Bmean_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   procNb <- length(procNames_List)
  #   preCovmat <-
  #     matrix(
  #       0.5,
  #       nrow = procNb,
  #       ncol = 1,
  #       dimnames = list(procNames_List, NULL)
  #     )
  #   preCovmat[upper.tri(preCovmat)] <- ""
  #   matrixInput(inputId = "Bmean",
  #               value = preCovmat,
  #               cols = list(names = FALSE))
  # })
  
  # output$targetBmean_Output <- renderUI({
  #   procNames_List <-
  #     as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
  #   pickerInput(
  #     inputId = "targetBmean", 
  #     choices = procNames_List, 
  #     options = list(
  #       `actions-box` = TRUE, 
  #       size = 10,
  #       `selected-text-format` = "count > 3"
  #     ), 
  #     multiple = TRUE
  #   )
  # })
  
  output$B_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    procNb <- length(procNames_List)
    preCovmat <-
      matrix(
        diag(procNb),
        nrow = procNb,
        ncol = procNb,
        dimnames = list(procNames_List, procNames_List)
      )
    preCovmat[upper.tri(preCovmat)] <- ""
    matrixInput(inputId = "B",
                value = preCovmat)
  })
  
  output$targetB_Output <- renderUI({
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    Bnames <- labelsL(procNames_List, "B")
    pickerInput(
      inputId = "targetB", 
      choices = Bnames, 
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  output$AB_Output <- renderUI({ 
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    procNb <- length(procNames_List)
    ANames <- c()
    BNames <- c()
    for (i in 1:procNb){
      ANames[i] <- paste("A ", procNames_List[i])
      BNames[i] <- paste("B ", procNames_List[i])
    }
    preCovmat <-
      matrix(
        0,
        nrow = procNb,
        ncol = procNb,
        dimnames = list(ANames, BNames)
      )
    matrixInput(inputId = "AB",
                value = preCovmat)
  })
  
  output$targetAB_Output <- renderUI({ 
    procNames_List <-
      as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| ")))
    ABnames <- labelsL(procNames_List, "AB")
    pickerInput(
      inputId = "targetAB",
      choices = ABnames,
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ),
      multiple = TRUE
    )
  })
  
  ### compute results
  
  res <- reactive({
    compute_results(
      budget = input$budget,
      alpha = input$alpha,
      costN = input$costN,
      minN = input$minN,
      maxN = input$maxN,
      costT = input$costT,
      minT = input$minT,
      maxT = input$maxT,
      modelClass = input$modelClass,
      procNames = input$procNames,
      measModel = input$measModel,
      ARCL = input$ARCL,
      targetARCL = input$targetARCL,
      RES = input$RES,
      targetRES = input$targetRES,
      UNIQ = input$UNIQ,
      targetUNIQ = input$targetUNIQ,
      I = input$I,
      targetI = input$targetI,
      S = input$S,
      targetS = input$targetS,
      IS = input$IS,
      targetIS = input$targetIS,
      A = input$A,
      targetA = input$targetA,
      B = input$B,
      targetB = input$targetB,
      AB = input$AB,
      targetAB = input$targetAB,
      popSize = input$popSize,
      dbLog = input$dbLog
    )})
  
  output$optN <- renderText({ tryCatch(as.integer(res()$res$N.opt), error = function(e){""})
  }) 
  
  output$optT <- renderText({ tryCatch(as.integer(res()$res$T.opt), error = function(e){""})
  })
  
  maxPower <- reactive({ 
    # besser wäre es names von power.max zu nehmen
    target.param <- tryCatch(res()$target.parameters, error = function(e){target.param <- NULL})
    power.max <- tryCatch(res()$res$power.max, error = function(e){power.max <- NULL})
    
    if (!is.null(target.param)){
      par <- c()
      vars_O <- c("CL_", "IS_", "AB_") # -->
      vars_N <- c("CL ", "IS ", "AB ")
      covs_0 <- c("RES_", "UNIQ_", "I_", "S_", "A_", "B_") # <-->
      covs_N <- c("RES ", "UNIQ ", "I ", "S ", "A ", "B ")
      for (i in 1:length(target.param)){ # if only one _ then variance (including AR)
        if (lengths(regmatches(target.param[i], gregexpr("_", target.param[i]))) == 1){ 
          par[i] <- gsub("_", " ", target.param[i])
        } else {
          tmp <- c()
          if (any(str_detect(target.param[i], vars_O))){
            id <- str_which(target.param[i], vars_O)
            tmp <- gsub(vars_O[id], vars_N[id], target.param[i])
            par[i] <- gsub("_", " → ", tmp)
          } else if (any(str_detect(target.param[i], covs_O))){
            id <- str_which(target.param[i], covs_O)
            tmp <- gsub(covs_O[id], covs_N[id], target.param[i])
            par[i] <- gsub("_", " ↔ ", tmp)
          }
        }
      }
      #par <- gsub("_", " ", target.param)
      pow <- unname(round(power.max, 5))
      data <- data.frame(par, pow) # later whitespace turned to points 
      return(data)
    }
  })
  
  output$maxPowerTable <- DT::renderDataTable({ 
    tryCatch(DT::datatable(maxPower(), options = list(pageLength=nrow(maxPower()), 
                                                      dom = 't'),
                           class='cell-border',
                           colnames = c('Target Parameter', 'Maximum Power')),
             error = function(e){""})
  })
  
  output$errorCond <- reactive({
    tryCatch(length( res()$res$error_codes[  error_type(res()$res$error_codes) %in%  "error" ] ) > 0, error = function(e){""}) 
  })
  
  output$error <- renderUI({ # print linebreaks https://groups.google.com/g/shiny-discuss/c/8GmXV-UfTm4?pli=1
    tryCatch(HTML(paste0("<span style=\"font-variant: small-caps;\">Error!</span><br/>", paste0(error_messages_translation( res()$res$error_codes[  error_type(res()$res$error_codes) %in%  "error" ] ), 
                         collapse = "<br/>"))), error = function(e){""}) 
  })
  
  output$warningCond <- reactive({ 
    tryCatch(length( res()$res$error_codes[  error_type(res()$res$error_codes) %in%  "warning" ] ) > 0, error = function(e){""}) 
  })
  
  output$warn <- renderUI({ # print linebreaks https://groups.google.com/g/shiny-discuss/c/8GmXV-UfTm4?pli=1
    tryCatch(HTML(paste0("<span style=\"font-variant: small-caps;\">Warning!</span><br/>", paste0(error_messages_translation( res()$res$error_codes[  error_type(res()$res$error_codes) %in%  "warning" ] ), 
                         collapse = "<br/>"))), error = function(e){""}) 
  })
  
  output$runTime <- renderText({ 
    tryCatch(round(res()$res$run.time.optimizer.secs, 3), error = function(e){""})
  })
  
  output$optRuns <- renderText({ 
    tryCatch(res()$res$optimizer.runs, error = function(e){""}) 
  })
  
  output$runTimeDB <- renderText({ 
    tryCatch(round(res()$res$run.time.log.data.secs, 3), error = function(e){""}) 
  })
  
  output$logID <- renderText({ tryCatch(as.integer(res()$res$logid), error = function(e){""}) 
  })
  
  output$logDB <- renderText({ 
    tryCatch(res()$res$log.data.status, error = function(e){""}) 
  })
  
  # only for testing phase:
  output$results <- renderPrint({ 
    tryCatch(res(), error = function(e){""})
  })
  
  ### deprecated bzw muss geändert werden später
  # Platzhalter für SEM Plot
  # output$platzhalter <- renderImage({
  #   outfile <- tempfile(fileext = '.png')
  #   # größe nicht verstellbar:
  #   png(outfile,
  #       width = 200,
  #       height = 200,
  #       units = "px")
  #   list(src = "img/platzhalter.png")
  # }, deleteFile = F) # constant img
  #dev.off()
  
  ### deprecated: selfmade ICONS
  # output$cite <- renderImage({
  #   outfile <- tempfile(fileext='.png')
  #   # größe nicht verstellbar:
  #   png(outfile, width=20, height=2000, units="px", res=120)
  #   list(src="img/cite.png")
  # }, deleteFile=F) # constant img
  # dev.off()
  # output$logo <- renderImage({
  #   l <- tempfile(fileext='.png')
  #   # größe nicht verstellbar:
  #   png(l, width=20, height=20, units="px", res=120)
  #   list(src="img/Logo.png")
  # }, deleteFile=F) # constant img
  #dev.off()
}

shinyApp(ui = ui, server = server)