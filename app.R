# JW: 0.2.1 2023-06-23: reference and link updated
# JW: 0.1.76 2023-06-23: tick marks N/T in steps of 1, roudning of optimizer N/T (gray) rounded to 0
# MA 0.1.75 2023-06-15: added source("locally_redefine_OpenMx_functions.R")
# MH 0.1.74 2023-06-12: setwd commented out
# JW: 0.1.71 2023-06-08: package OpenMx included
# JW: 0.1.70 2023-06-06: lorem ipsum citation
# JW: 0.1.69 2023-05-17: error that prevented tabpanels from appearing (a missing / in HTML()...) in row 699 fixed,
#                        two new tabs added (use, cite),
#                        all fluidTab() etc removed bc deprecated bc navbarpage(fluid=TRUE, ...),
#                        included full names of model classes,
#                        numbering of steps,
#                        both types of results in one box, with progress bar (package shinycssloaders),
#                        new column Example Model with static path diagrams for each model class
#                        all main columns are equally wide now
#                        included more information on usage of the app
#                        changed display form of optimal numbers to table (like maxPower)
#                        note: no solution for url in hovertext & force to print 3 digits in maxPowTable left aligns them..
#                        hover text refering to Usami for model selection
#                        amended hover text for budget, larger budgets
# JW: 0.1.00 2022-11-18: some hover texts changed, problem with error 1 when changing modelClass persists
# JW: 0.0.48 2022-11-17: new infos in hover boxes
# JW: 0.0.46 2022-11-14: added default target params in univariate changed to AR (before: none)
# JW: 0.0.45 2022-11-04: minimal changes suggested by Martins Mail 22-11-03
# JW: 0.0.43 2022-11-02: implemented change suggestion from meeting on 22-10-28, see google docs
# JW: 0.0.38 2022-10-14: removed guthib packages (icons at all, shinyMatrix now from CRAN)
# JW: 0.0.37 2022-10-10: new function for erro-warn differentiation; error output$maxPower fixed; T.<x>.set instead of T.<x>.bound
# JW: 0.0.36 2022-10-06: tryCatch for results to suppress printing of internal errors
# JW: 0.0.30 2022-10-05: diverse things added and bugs fixed
# JW: 0.0.29 2022-09-02: error in IS and AB matrices corrected; problem with html and internet browser tab fixed in css; new script sourced
# JW: 0.0.26 2022-08-31: typo in compute_results() input corrected

# only for local run
#setwd("/Users/julia/Documents/Arbeit/Promotion/Forschung/Projects/Shiny_App_Optimal_Design/optimalCrossLagged-main")
#setwd("C:/Users/manue/OneDrive/Forschung/Optimal Cross Lagged/optimalCrossLagged")
# setwd("c:/Users/martin/Dropbox/84_optimalclpm/04b_martinhecht/optimalCrossLagged")


# (install and) load packages
packages <- c("shiny", # basic
              "shinyjs", # for delay in code execution
              "magrittr", # pipe operator
              "devtools", # for shinyMatrix on github
              "shinyWidgets", # for pickerInput widget
              "shinyMatrix",
              "dplyr", # for case_when
              "R.utils",
              "rgenoud",
              "RcppArmadillo",
              "matrixcalc", # check_plausability()
              "stringr",
              "RMariaDB", # for data log
              "here",
              "shinycssloaders",
              "config",
              "numDeriv"
)
newPackages <-
  packages[!(packages %in% installed.packages()[, "Package"])]
if (length(newPackages))
  install.packages(newPackages)
packages <- packages[!packages %in% "config"] # prevent config from loading; otherwise base::get() masked
lapply(packages, require, character.only = TRUE)
# if (!("shinyMatrix" %in% installed.packages()[, "Package"])){
#   install_github("INWTlab/shiny-matrix") # version on github is more recent (i.e., editableCells parameter only here)
# }
# require(shinyMatrix)
# if (!("shinyMatrix" %in% installed.packages()[, "Package"])){
#   install_github("mitchelloharawild/icons") # does not appear in "Packages"
#   download_fontawesome()
# }
# require(icons)



# source all relevant functions
source("R/locally_redefine_OpenMx_functions.R", local = TRUE)
source("R/calculate.F.diff.fast.R", local = TRUE)
source("R/calculate.F.diff.precise.R", local = TRUE)
source("R/calculate.from.cost.function.R", local = TRUE)
source("R/calculate.power.LRT.R", local = TRUE)
source("R/calculate.power.R", local = TRUE)
source("R/check_plausability.R", local = TRUE)
source("R/compute.se.oertzen.R", local = TRUE)
source("R/error_messages_translation.R", local = TRUE)
source("R/fn.R", local = TRUE)
source("R/get_all_labels.R", local = TRUE)
source("R/helper.functions.R", local = TRUE)
source("R/kickstart.optimizer.R", local = TRUE)
source("R/optmze.R", local = TRUE)
source("R/prepare.input.R", local = TRUE)
source("R/prepare.results.R", local = TRUE)
source("R/RcppExports.R", local = TRUE)

ui <- 
  # include file-based css in shiny: https://shiny.rstudio.com/articles/css.html
  navbarPage(header = includeCSS('www/ui.css'),
             #title = "OptDynMo: Finding the optimal number of persons (N) and time points (T) for maximal power in dynamic longitudinal models given a fixed budget",
             title = HTML("<strong>OptDynMo</strong>: Optimal Design for Dynamic Longitudinal Models"),
             
             tabPanel(
               title = "Maximize Power",
               icon = icon("magnifying-glass-chart"),
               
               div(class= "what", "This app helps you to plan a study with the optimal number of persons (n) and time Points (t) for maximal power given your budget", br(), span(style="font-size:18px", HTML("Simply follow Steps (<font color=#7000a8>1</font>), (<font color=#7000a8>2</font>) and (<font color=#7000a8>3</font>)"))),
               br(), 
               
               column(
                 id = "one",
                 width = 4,
                 div(
                   class = "main-box",
                   tags$span(class = "heading", HTML("(<font color=#7000a8>1</font>) Study Design")),
                   tags$br(),
                   tags$br(),
                   p(HTML("First, indicate key features of your planned study.")),
                   tags$br(),
                   
                   fluidRow(
                     column(
                       width = 6,
                       div(class = "input-box mini-left",
                           tags$span(
                             class = "hovertext",
                             'data-hover' = "Indicate the maximum budget that you have available for the study (in any monetary unit). Note that larger budgets lead to larger computation times (because the search space for the optimizer gets larger).",
                             icon("circle-question")
                           )  %>% tagAppendAttributes(style = "left: 4%;"),
                           
                           numericInput(
                             inputId = "budget",
                             label = "Budget",
                             value = 10000,
                             min = 0,
                             max = 1000000, # otherwise no solution
                             step = 2500 # wird verdoppelt in UI!?
                           )
                       )
                     ),
                     column(
                       width = 6,
                       div(class = "input-box mini-right",
                           tags$span(
                             class = "hovertext",
                             'data-hover' = "This is used for the Likelihood-Ratio-Tests in which the optimal N and T are calculated.",
                             icon("circle-question")
                           )  %>% tagAppendAttributes(style = "left: 4%;"),
                           ## with arrows
                           numericInput(
                             inputId = "alpha",
                             label =
                               withMathJax(c("\\(\\alpha\\)-Level")),
                             value = 0.05,
                             min = 0.01,
                             max = 1,
                             step = 0.005
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
                       ))
                   ),
                   fluidRow(
                     column(
                       width = 6,
                       div(id="boxN",
                           class = "input-box mini-left",
                           tags$span(
                             class = "hovertext",
                             'data-hover' = "Indicate the cost to include one person in the study and the desired minimum and maximum number of persons in the white boxes. As the min-max range of number of persons and number of time points is interdependent due to the cost function, the app will perform adjustments to your entered minimum and maximum values. The resulting adjusted values used in the optimization process are printed in gray boxes on the right-hand side of the input boxes.",
                             icon("circle-question")
                           )  %>% tagAppendAttributes(style = "left: 4%;"),
                           p(HTML("<strong><i>Persons N</i></strong>")),
                           numericInput(
                             inputId = "costN",
                             label = HTML("Costs"),
                             value = 100,
                             min = 0,
                             max = 10000000,
                             step = 5
                           ),
                           splitLayout(
                             #cellWidths = c("75%", "25%"),
                             numericInput(
                               inputId = "minN",
                               label = HTML("Min"),
                               value = 10,
                               min = 2,
                               max = 10000,
                               step = 1
                             ),
                             conditionalPanel(
                               condition = "output.errorCond == false", 
                               uiOutput(outputId = "minN_Output_Backend")
                             )
                           ),
                           splitLayout(
                             numericInput(
                               inputId = "maxN",
                               label = HTML("Max"),
                               value = 50,
                               min = 2,
                               max = 10000,
                               step = 1
                             ),
                             conditionalPanel(
                               condition = "output.errorCond == false", 
                               uiOutput(outputId = "maxN_Output_Backend")
                             )
                           )
                       ) ### div input-box N
                     ), ### column N
                     
                     column(
                       width = 6,
                       div(id="boxT",
                           class = "input-box mini-right",
                           tags$span(
                             class = "hovertext",
                             'data-hover' = "Indicate the cost for measuring one person at one time point and the desired minimum and maximum number of time points in the white boxes. As the min-max range of number of persons and number of time points is interdependent due to the cost function, the app will perform adjustments to your entered minimum and maximum values. The resulting adjusted values used in the optimization process are printed in gray boxes on the right-hand side of the input boxes. The default for the minimum number of time points reflects the minimum number of time points that are required for model identification (see Usami et al., 2019).",
                             icon("circle-question")
                           )  %>% tagAppendAttributes(style = "left: 4%;"),
                           p(HTML("<strong><i>Time Points T</i></strong>")),
                           numericInput(
                             inputId = "costT",
                             label = HTML("Costs"),
                             value = 50,
                             min = 0,
                             max = 10000000,
                             step = 5
                           ),
                           splitLayout(
                             uiOutput(outputId = "minTidentify_Output"),
                             conditionalPanel(
                               condition = "output.errorCond == false", 
                               uiOutput(outputId = "minT_Output_Backend")
                             )
                           ),
                           splitLayout(
                             numericInput(
                               inputId = "maxT",
                               label = HTML("Max"),
                               value = 18,
                               min = 1,
                               max = 10000,
                               step = 1
                             ),
                             conditionalPanel(
                               condition = "output.errorCond == false", 
                               uiOutput(outputId = "maxT_Output_Backend")
                             )
                           )
                       ) ### div input-box T
                     ) #### column T
                   ),
                   br(),
                   column(width=6, "", style="margin-left:-0.5vw"),
                   icon("arrow-down")
                 ),
                 ### div study design
                 
                 div(
                   class = "main-box",
                   tags$span(class = "heading", HTML("(<font color=#7000a8>2</font>) Model Characteristics")),
                   tags$br(),
                   tags$br(),
                   p(HTML("Second, indicate the model class that you plan to analyse the data with and the names of the manifest processes that you plan to investigate.")),
                   tags$br(),
                   
                   div(class = "input-box",
                       tags$span(
                         class = "hovertext",
                         'data-hover' = "For guidance on model selection see Usami et al. (2019).",
                         icon("circle-question")
                       )  %>% tagAppendAttributes(style = "left: 1%;"),
                       selectInput(
                         inputId = "modelClass",
                         label = h5(strong("Model Class")),
                         choices = list(
                           "Cross-Lagged Panel Model (CLPM)" = "clpm",
                           "Factor Cross-Lagged Panel Model (factor CLPM)" = "fclpm",
                           "Random Intercept Cross-Lagged Panel Model (RI-CPLM)" = "ri-clpm",
                           "Stable Trait Autoregressive Trait and State Model (STARTS)" = "starts",
                           "Latent Curve Model With Structured Residuals (LCM-SR)" = "lcm-sr",
                           "Autoregressive Latent Trajectory Model (ALT)" = "alt",
                           "Latent Change Score Model (LCS)" = "lcs"
                         ),
                         selected = "clpm"
                       )
                   ),
                   
                   fluidRow(column(width=10,
                                   div(class = "input-box", 
                                       splitLayout(
                                         cellWidths = c("80%", "20%"),
                                         textAreaInput(
                                           inputId = "procNames",
                                           label = HTML("Process Names"),
                                           placeholder = "y1, y2",
                                           value = "y1, y2"
                                         ),
                                         uiOutput(outputId = "nbProc")
                                       ),
                                       tags$span(style = "font-weight:normal; font-size:small;",
                                                 "Please indicate at least one process name. Seperate multiple names with comma. The number of processes for a given model is inferred from the number of names. It is displayed right to the input field."
                                       ),
                                       tags$br(),
                                       tags$br()
                                   ),
                                   
                                   # only single-indicator for now
                                   conditionalPanel(
                                     condition = "input.modelClass == 'fclpm' | input.modelClass == 'starts' | input.modelClass == 'lcs'",
                                     div(class = "input-box",
                                         uiOutput(outputId = "measModel_Output")
                                     )
                                   )), column(width=1, br(), br(), icon("arrow-turn-up")))
                   #column(width=9, ""), icon("arrow-turn-up")
                   # conditionalPanel(condition = "(input.modelClass == 'fclpm' | input.modelClass == 'starts' | input.modelClass == 'lcs') & (input.measModel.length > 0)",
                   #                  tags$br(),
                   #                  uiOutput(outputId = "indicat_Output"))
                 ), ### div model characteristics
                 div(
                   class = "main-box",
                   
                   HTML("<p>The example path diagram below helps you select a model class and set model parameters. It shows your currently selected model class and the non-zero model parameters of the defaults of the app.</p>"),
                   br(),
                   span(style="text-align:center;",
                        conditionalPanel(
                          condition = "input.modelClass == 'clpm' ",
                          imageOutput(outputId = "figCLPM")
                        ),
                        conditionalPanel(
                          condition = "input.modelClass == 'fclpm' ",
                          imageOutput(outputId = "figfCLPM")
                        ),
                        conditionalPanel(
                          condition = "input.modelClass == 'ri-clpm' ",
                          imageOutput(outputId = "figRICLPM")
                        ),
                        conditionalPanel(
                          condition = "input.modelClass == 'starts' ",
                          imageOutput(outputId = "figSTARTS")
                        ),
                        conditionalPanel(
                          condition = "input.modelClass == 'lcm-sr' ",
                          imageOutput(outputId = "figLCMSR")
                        ),
                        conditionalPanel(
                          condition = "input.modelClass == 'alt' ",
                          imageOutput(outputId = "figALT")
                        ),
                        conditionalPanel(
                          condition = "input.modelClass == 'lcs' ",
                          imageOutput(outputId = "figLCS")
                        )))
               ),
               ### column one
               
               column(
                 id = "two",
                 class = "main-box",
                 #style = "left: 8px;",
                 width = 4,
                 #tags$details('open' = "TRUE",
                 #tags$summary(
                 tags$span(class = "heading", HTML("(<font color=#7000a8>3</font>) Model Parameters")),
                 # ),
                 tags$br(),
                 tags$br(),
                 p(
                   HTML(
                     "Third, set model parameters and choose target parameters for which you want to maximize the joint power."
                   )
                 ),
                 p(style="text-indent: -1.4em;",
                   HTML(
                     "<li>Set the model parameters in the matrices.<br/><small>(Note that variance parameters must be larger than zero)</small></li>"
                   )
                 ),
                 p(
                   HTML(
                     "<li>Choose the target parameters from the drop-down menu.</li>"
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
                 #     ) %>% tagAppendAttributes(style = "left: 3%;"),
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
                   conditionalPanel(
                     condition = "input.modelClass == 'clpm' | input.modelClass == 'fclpm'",
                     tags$span(
                       class = "hovertext",
                       'data-hover' = "The AR parameters indicate the stability of the processes. Hamaker et al. (2015) give the following interpretation: “The closer these autoregressive parameters are to one, the more stable the rank order of individuals is from one occasion to the next”. (p. 104). The CL parameters indicate the extent to which change in one process can be predicted from the individual’s prior deviation from the group mean of another process (Hamaker et al., 2015, p. 104).",
                       icon("circle-question") # CLPM: rank order stability (i.e., between level effect) - Hamaker: between and within conflated, vs RI-CLPM: within-person carry-over
                     ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;")
                   ),
                   conditionalPanel(
                     condition = "input.modelClass == 'ri-clpm' | input.modelClass == 'starts'| input.modelClass == 'lcm-sr'| input.modelClass == 'lcs'| input.modelClass == 'alt'",
                     tags$span(
                       class = "hovertext",
                       'data-hover' = "The AR parameters represent the amount of within-person carry-over effect. If it is positive, it implies that measurement occasions on which a person scored above his or her expected score are likely to be followed by occasions on which he or she still scores above the expected score again, and vice versa. (Hamaker et al., 2015, p. 104-105). The CL parameters indicate the extent to which variables influence each other. Specifically, a CL parameter indicates the degree by which deviations from an individual’s expected score on one variable can be predicted from preceding deviations from one’s expected score on another variable (Hamaker et al., 2015, p. 104-105).",
                       icon("circle-question") # CLPM: rank order stability (i.e., between level effect) - Hamaker: between and within conflated, vs RI-CLPM: within-person carry-over
                     ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;")
                   ),
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
                     'data-hover' = "The dynamic residuals represent the parts of the processes that cannot be explained by the lagged relations (i.e., AR and CL effects) and other common factors. The influence of the dynamic residuals feeood forward through the lagged relations and affects subsequent measurement occasions . They are also referred to as innovations or dynamic errors (Usami et al., 2019, p. 7).",
                     icon("circle-question")
                   ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;"),
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
                   #   ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;"),
                   #   p(HTML("<strong>Factor Loadings</strong>")),
                   #   uiOutput(outputId = "LOAD_Output"),
                   #   uiOutput(outputId = "targetLOAD_Output")
                   # ),
                   div(
                     class = "input-box",
                     tags$span(
                       class = "hovertext",
                       'data-hover' = "Unique residuals, also referred to as measurement errors, do not affect future measurements and are only associated with a single measurement occasion (Usami et al., 2019, p. 3).",
                       icon("circle-question")
                     ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;"),
                     p(HTML("<strong>Unique Residuals (UNIQ)</strong>")),
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
                       'data-hover' = "The random intercepts are the individual’s trait-like deviations from the group means (Hamaker et al., 2015, p. 104).",
                       icon("circle-question")
                     ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;"),
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
                       'data-hover' = "Random slopes represent the linear slopes of the individual developmental trajectories. (Usami et al., 2019, p. 3).",
                       icon("circle-question")
                     ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;"),
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
                       'data-hover' = "The covariances of the random intercepts and slopes. For example, a positive covariance between the random intercept of process x and the random slope of process y indicates that an individual who starts the study with a larger-than-average value of process x is likely to exhibit more growth in process y over time.",
                       icon("circle-question")
                     ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;"),
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
                       'data-hover' = "The constant accumulating factors A have a constant effect on the processes that accumulate over time through the lagged relations (Usami et al., 2019, p. 7).",
                       icon("circle-question")
                     ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;"),
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
                       'data-hover' = "The changing factors B have a changing effect on the processes that accumulate over time through the lagged relations. The direct effect of the changing accumulating factors grows with each measurement occasion (Usami et al., 2019, p. 5).",
                       icon("circle-question")
                     ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;"),
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
                       'data-hover' = "The covariance of the constant changing accumulating factors A and B. For example, a positive covariance between the constant accumuliting factor of process x and the changing accumulating factor of process y indicates that an individual who starts the study with a larger-than-average value of process x is likely to exhibit more growth in process y over time.",
                       icon("circle-question")
                     ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;"),
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
                 width = 4,
                 
                 div(
                   class = "main-box",
                   tags$span(class = "heading", "Results"),
                   br(),
                   br(),
                   HTML("<p>Finally, have a look at the results. <br>Note that the results change immediately when you change any input.</p>"),
                   br(),
                   div(class = "output-box", style="border: 3px solid black;",
                       # tags$span(class = "hovertext", style="font-weight:normal;",
                       #           'data-hover' = "Optimal number of persons N and optimal number of time points T for which the power of the Likelihood-Ratio-Tests of the target parameters is maximal.",
                       #           icon("circle-question")
                       # )  %>% tagAppendAttributes(style = "left: 1%;"),
                       span(style="font-weight:bold; font-variant:small-caps;", HTML("Optimal Number of Units for Maximal Power<br>")),
                       span(style="font-weight:normal; font-size:12px; font-variant:small-caps;", "for which the power of the likelihood-ratio tests of the target parameters is maximal"),
                       br(),
                       br(),
                       shinycssloaders::withSpinner(DT::dataTableOutput("optNumTable", width="auto"), type=7, color="#7000a8", hide.ui=FALSE),
                       # span(style="font-weight:normal; font-variant:small-caps;", HTML("Persons <i>N</i>:")), textOutput("optN", inline=T),
                       # tags$br(),
                       # span(style="font-weight:normal; font-variant:small-caps;", HTML("Time Points <i>T</i>:")), textOutput("optT", inline=T),
                       br(),
                       br(),
                       span(style="font-weight:bold; font-variant:small-caps;", HTML("Maximum Power for All Target Parameters<br>")),
                       span(style="font-weight:normal; font-size:12px; font-variant:small-caps;", HTML("for all target parameters based on the optimal <i>n</i> and <i>t</i> solution")),
                       tags$br(),
                       tags$br(),
                       shinycssloaders::withSpinner(DT::dataTableOutput("maxPowerTable", width="auto"), type=7, color="#7000a8", hide.ui=FALSE)
                   ),
                   
                   
                   conditionalPanel(
                     condition = "output.errorCond == true",
                     htmlOutput("error", inline=T)
                   ),
                   
                   conditionalPanel(
                     condition = "output.errorCondFrontend == true",
                     htmlOutput("errorFrontend", inline=T)
                   ),
                   
                   conditionalPanel(
                     condition = "output.warningCond == true",
                     htmlOutput("warn", inline=T)
                   ),
                   
                   conditionalPanel(
                     condition = "output.noteCond == true",
                     htmlOutput("note", inline=T)
                   )
                 ),
                 
                 div(class="main-box",
                     tags$span(class = "heading", "Technical Details"),
                     br(),
                     br(),
                     p("Here you have some further options."),
                     p(HTML("<li>Change precision of the optimizer.<br/><small>(“Precision” is a user-friendly term for what the argument pop.size of the genoud optimizer from the package <a href=\"https://cran.r-project.org/web/packages/rgenoud/rgenoud.pdf\" target=\"_blank\">rgenoud</a> adjusts, see Mebane and Sekhon, 2011.)</small></li>")),
                     p(HTML("<li>Disallow logging of your results.<br/><small>(This helps us improving this app!)</small></li>")),
                     p(HTML("<li>Get technical information on your last results.</li>")),
                     br(),
                     
                     column(
                       width = 5,
                       div(class = "input-box mini-left mini-right",
                           
                           # tags$span(
                           #   class = "hovertext",
                           #   'data-hover' = "The term “precision” is used as an user-friendly transcription of what the argument pop.size of the genoud optimizer adjusts, see Mebane and Sekhon (2011).",
                           #   icon("circle-question") 
                           # ) %>% tagAppendAttributes(style = "left: 1%; font-weight:normal;"),
                           style="padding-bottom: 5px;",
                           sliderInput(
                             inputId = "popSize",
                             label = "Precision",
                             value = 16,
                             min = 16,
                             max = 1000, # used in backend optmze() "pop.size.max"
                             ticks=FALSE
                           ),
                           # span(style="font-size:small;", HTML("“Precision” is a user-friendly term for what the argument pop.size of the genoud optimizer from the package <a href=\"https://cran.r-project.org/web/packages/rgenoud/rgenoud.pdf\" target=\"_blank\">rgenoud</a> adjusts (see Mebane and Sekhon, 2011).</small>")),
                       ),
                       
                       div(class = "input-box mini-left mini-right", style="padding-bottom: 2px; padding-top: 2px; font-size:12px;",
                           checkboxInput(
                             inputId = "dbLog",
                             label = HTML("<b>Log Results</b>"),
                             value=TRUE
                           )
                       )
                     ),
                     
                     
                     column(
                       width = 7,
                       div(class = "output-box mini-left mini-right", 
                           style="font-weight: small; font-variant:small-caps;",
                           span("Run Time (in sec):"), textOutput("runTime", inline=T),
                           br(), span("Number of Iterations:"), textOutput("optRuns", inline=T),
                           # necessary bc https://github.com/rstudio/shiny/issues/1318
                           #br(), span(style="font-weight:normal; font-variant:small-caps;", "Errors:"), textOutput("errorCond", inline=T),
                           br(), 
                           br(), span("Log Run Time (in sec):"), textOutput("runTimeDB", inline=T),
                           br(), span("Log ID:"), textOutput("logID", inline=T),
                           br(), span("Log Status:"), textOutput("logDB", inline=T),
                           br(), span("App Version:"), textOutput("appVersion", inline=T)
                       )
                     )
                 ),
                 
                 div(class="main-box",
                     br(),
                     br(),
                     
                     tags$details(style="font-weight: lighter;",
                                  tags$summary(span(class = "heading", "References")),
                                  br(), 
                                  HTML("Hamaker, E. L., Kuiper, R. M., & Grasman, R. P. P. P. (2015). A critique of the cross-lagged panel model. <i>Psychological Methods</i>, 20, 102–116. <a href=\"https://doi.org/10.1037/a0038889\" target=\"_blank\">https://doi.org/10.1037/a0038889</a>"),
                                  br(), 
                                  br(), 
                                  HTML("Mebane, W. R., & Sekhon, J. S. (2011). Genetic optimization using derivatives: The rgenoud package for R. <i>Journal of Statistical Software</i>, 42. <a href=\"https://doi.org/10.18637/jss.v042.i11\" target=\"_blank\">https://doi.org/10.18637/jss.v042.i11</a>"),
                                  br(), 
                                  br(), 
                                  HTML("Usami, S., Murayama, K., & Hamaker, E. L. (2019). A unified framework of longitudinal models to examine reciprocal relations. <i>Psychological Methods</i>, 24, 637–657. <a href=\"https://doi.org/10.1037/met0000210\" target=\"_blank\">https://doi.org/10.1037/met0000210</a>")
                     )),
                 
                 
                 # tags$details(#'open' = "FALSE",
                 #  tags$summary(span("Dev Output")),
                 #  verbatimTextOutput("results")
                 # ),
                 
                 # have to stay!!! otherwise JS using errorCond and warningCond won't evaluate
                 span(style="color:white;", textOutput("errorCond", inline=T)), 
                 span(style="color:white;", textOutput("errorCondFrontend", inline=T)),  
                 span(style="color:white;", textOutput("warningCond", inline=T)),
                 span(style="color:white;", textOutput("noteCond", inline=T))
                 
               ) ### column three
               
             ),
             
             
             # tabPanel(
             #   title = "How To Use",
             #   icon = icon("question"),
             #   img(src='exCLPM.jpg', align = "middle", width="50%", height="50%")
             #   # uiOutput(outputId = "tutorial")
             # ),
             
             tabPanel(
               title = "How To Cite",
               icon = icon("pen-fancy"),
               div(
                 class = "main-box",
                 p(style="font-weight:normal;", HTML("If you use the app for publications, please cite the corresponding article:")),
                 br(), 
                 p(style="font-weight: lighter;", HTML("Hecht, M., Walther, J.-K., Arnold, M., & Zitzmann, S. (2023). Finding the Optimal Number of Persons (N) and Time Points (T) for Maximal Power in Dynamic Longitudinal Models Given a Fixed Budget. <i>Structural Equation Modeling: A Multidisciplinary Journal.</i> <a href=\"https://doi.org/10.1080/10705511.2023.2230520\" target=\"_blank\">https://doi.org/10.1080/10705511.2023.2230520</a>"))
               )
             )
             
  )


#################################################################################################
#################################################################################################

server <- function(input, output, session) {
  ### zum debuggen, um zu schauen welche werte input hat
  # output$value <- renderPrint({ 
  #   tryCatch(res()$res$error_codes, error = function(e){0L}) 
  # })
  # # in kombi mit:
  # verbatimTextOutput("value")
  
  ### for study design tab:
  
  minTidentify <- reactive({
    mc <- req(input$modelClass)
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
      value = req(minTidentify()),
      min = req(minTidentify()),
      max = 10000,
      step = 1
    )
  })
  
  observeEvent(input$minT, { # without !is.na() app crashes when value deleted
    if (!is.na(input$minT) && req(input$minT) > input$maxT){
      updateNumericInput(session, "maxT",
                         value = input$minT)
    }
  })
  
  observeEvent(input$maxT, {
    # req() ensures that values are available ("truthy") before proceeding with a calculation or action
    if (!is.na(input$maxT) && input$maxT < req(input$minT)){ 
      if (input$maxT > minTidentify()){
        updateNumericInput(session, "minT",
                           value = input$maxT) 
      } else {
        updateNumericInput(session, "minT",
                           value = minTidentify())
        updateNumericInput(session, "maxT",
                           value = minTidentify())
      }
      
    }
  })
  
  
  observeEvent(input$minN, {
    if (!is.na(input$minN) && input$minN > input$maxN){
      updateNumericInput(session, "maxN",
                         value = input$minN)
    }
  })
  
  observeEvent(input$maxN, {
    if (!is.na(input$maxN) && input$maxN < input$minN){
      updateNumericInput(session, "minN",
                         value = input$maxN)
    }
  })
  
  # maxT <- reactive({
  #   init <- 10
  #   if (input$minT > input$maxT){
  #     init <- input$minT
  #   }
  # })
  # 
  # output$maxT_Output <- renderUI({
  #   numericInput(
  #     inputId = "maxT",
  #     label = HTML("Max"),
  #     value = maxT(),
  #     min = 1,
  #     max = 10000,
  #     step = 1
  #   )
  # })
  
  output$minT_Output_Backend <- renderUI({ 
    tryCatch(div(class="unit", round(res()$res$constraints$T.min.set, 0)), error = function(e){""})
  })
  
  output$maxT_Output_Backend <- renderUI({
    tryCatch(div(class="unit", round(res()$res$constraints$T.max.set, 0)), error = function(e){""})
  })
  
  output$minN_Output_Backend <- renderUI({
    tryCatch(div(class="unit", round(res()$res$constraints$N.min.bound, 0)), error = function(e){""})
  })
  
  output$maxN_Output_Backend <- renderUI({
    tryCatch(div(class="unit", round(res()$res$constraints$N.max.bound, 0)), error = function(e){""})
  })
  
  ### for model characteristics tab
  
  output$nbProc <- renderUI({ 
    tryCatch(procNames_List <- as.list(unlist(strsplit(input$procNames, split = "\\, |\\,| "))), error = function(e){""})
    tryCatch(div(class="unit", length(procNames_List) ), error = function(e){""})
    
  })
  
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
    if (length(procNames_List) > 1){
      ARCLsel <- ARCLnames[-c(1:length(procNames_List))]
    } else {
      ARCLsel <- ARCLnames
    }
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
      minTidentify = minTidentify(),
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
  
  
  # output$optN <- renderText({ tryCatch(as.integer(res()$res$N.opt), error = function(e){""})
  # })
  # 
  # output$optT <- renderText({ tryCatch(as.integer(res()$res$T.opt), error = function(e){""})
  # })
  
  
  optNum <- reactive({
    optN <- tryCatch(as.integer(res()$res$N.opt), error = function(e){optN <- NULL})
    optT <- tryCatch(as.integer(res()$res$T.opt), error = function(e){optT <- NULL})
    if (length(optN) == 1){ # otherwise error data must be 2 dimensional
      if (is.na(optN)){
        optN <- NULL
      }
    }
    if (length(optT) == 1){ # otherwise error data must be 2 dimensional
      if (is.na(optT)){
        optT <- NULL
      }
    }
    if (!is.null(optN) & !is.null(optT)){
      unitNam <- c("Persons <i>N</i>", "Time Points <i>T</i>")
      unitVal <- c(optN, optT)
      data <- data.frame(unitNam, unitVal)
      return(data)
    } else {
      return(data=NULL)
    }
  })
  
  output$optNumTable <- DT::renderDataTable({
    wtf <- optNum() # otherwise shortly appearing: error data must be 2 dimensional
    tryCatch(DT::datatable(data=wtf, options = list(pageLength=2,
                                                    dom = 't'),
                           class='cell-border', escape=FALSE, 
                           colnames = c('', 'Optimal Number')),
             error = function(e){""})
    
  })
  
  
  maxPower <- reactive({ 
    # besser wäre es names von power.max zu nehmen
    target.param <- tryCatch(req(res()$target.parameters), error = function(e){target.param <- NULL})
    power.max <- tryCatch(req(res()$res$power.max), error = function(e){power.max <- NULL})
    #ARCL <- tryCatch(req(res()$res$ARCL), error = function(e){ARCL <- NULL})
    if (length(power.max) == 1){ # otherwise error data must be 2 dimensional
      if (is.na(power.max)){
        power.max <- NULL
      }
    }
    if (!is.null(target.param) & !is.null(power.max)){ 
      par <- c()
      vars_O <- c("CL_", "IS_", "AB_") # -->
      vars_N <- c("CL ", "IS ", "AB ")
      covs_O <- c("RES_", "UNIQ_", "I_", "S_", "A_", "B_") # <-->
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
          } # error wenn alle als target.params aber funzt trotzdem (ie alle angezeigt): Warning in gsub(covs_O[id], covs_N[id], target.param[i]) : argument 'pattern' has length > 1 and only the first element will be used
        }
      }
      #par <- gsub("_", " ", target.param)
      pow <- unname(round(power.max, 3))
      data <- data.frame(par, pow) # later whitespace turned to points 
      return(data)
    } else {
      return(data=NULL)
    }
  })
  
  output$maxPowerTable <- DT::renderDataTable({ 
    tryCatch(DT::datatable(maxPower(), options = list(pageLength=nrow(maxPower()), 
                                                      dom = 't'),
                           class='cell-border',
                           colnames = c('Target Parameter', 'Maximum Power')),
             error = function(e){""})
  })
  
  output$errorCondFrontend <- reactive({
    # TRUE if errors
    #tryCatch(length( error_function_fe(procNames=input$procNames, budget=input$budget, costT=input$costT, costN=input$costN, minN=input$minN, minT=input$minT, minTidentify=minTidentify()) ) > 0, error = function(e){""})
    tryCatch(input$procNames == "", error = function(e){""})
  })
  
  output$errorFrontend <- renderUI({ 
    #tryCatch(HTML(paste0("<span style=\"font-variant: small-caps;\">Error!</span><br/>", error_function_fe(procNames=input$procNames, budget=input$budget, costT=input$costT, costN=input$costN, minN=input$minN, minT=input$minT, minTidentify=minTidentify()) ) ), error = function(e){""})
    tryCatch(div(class = "error-box", HTML(paste0("<span style=\"font-variant: small-caps;\">Error!</span><br/> Please indicate at least one process name."))), error = function(e){""})
  })
  
  # output$errorFrontend <- renderUI({ 
  #   #tryCatch(HTML(paste0("<span style=\"font-variant: small-caps;\">Error!</span><br/>", error_function_fe(procNames=input$procNames, budget=input$budget, costT=input$costT, costN=input$costN, minN=input$minN, minT=input$minT, minTidentify=minTidentify()) ) ), error = function(e){""})
  #   tryCatch(HTML(paste0("<span style=\"font-variant: small-caps;\">Error!</span><br/> Please indicate at least one process name.")), error = function(e){""})
  # })
  
  output$errorCond <- reactive({
    # TRUE if errors
    tryCatch(length( res()$res$error_codes[  error_type(res()$res$error_codes) %in%  "error" ] ) > 0, error = function(e){""})
  })
  
  output$error <- renderUI({ # print linebreaks https://groups.google.com/g/shiny-discuss/c/8GmXV-UfTm4?pli=1
    tryCatch(div(class = "error-box", HTML(paste0("<span style=\"font-variant: small-caps;\">Error(s)!</span><br/>", paste0(error_messages_translation( res()$res$error_codes[  error_type(res()$res$error_codes) %in%  "error" ], minTidentify=req(minTidentify()) ),
                                                                                                                            collapse = "<br/>")))), error = function(e){""})
  })
  
  output$warningCond <- reactive({ 
    tryCatch(length( res()$res$error_codes[  error_type(res()$res$error_codes) %in%  "warning" ] ) > 0, error = function(e){""}) 
  })
  
  output$warn <- renderUI({ # print linebreaks https://groups.google.com/g/shiny-discuss/c/8GmXV-UfTm4?pli=1
    tryCatch(div(class = "warn-box", HTML(paste0("<span style=\"font-variant: small-caps;\">Warning!</span><br/>", paste0(error_messages_translation( res()$res$error_codes[  error_type(res()$res$error_codes) %in%  "warning" ], minTidentify=minTidentify() ), 
                                                                                                                          collapse = "<br/>")))), error = function(e){""}) 
  })
  
  output$noteCond <- reactive({ 
    tryCatch(length( res()$res$error_codes[  error_type(res()$res$error_codes) %in%  "note" ] ) > 0, error = function(e){""}) 
  })
  
  output$note <- renderUI({ 
    tryCatch(div(class = "note-box", HTML(paste0("<span style=\"font-variant: small-caps;\">Note!</span><br/>", paste0(error_messages_translation( res()$res$error_codes[  error_type(res()$res$error_codes) %in%  "note" ], minTidentify=minTidentify() ), 
                                                                                                                       collapse = "<br/>")))), error = function(e){""}) 
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
  
  output$appVersion <- renderText({ 
    tryCatch(res()$res$optimalclpm.version.str, error = function(e){""}) 
  })
  
  # only for testing phase:
  output$results <- renderPrint({ 
    tryCatch(res(), error = function(e){""})
  })
  
  output$figCLPM <- renderImage({
    list(src="./img/figCLPM-no.jpg",
         contentType = "image/jpeg",
         width="75%", height="100%"
    )
  }, deleteFile=FALSE) 
  
  output$figfCLPM <- renderImage({
    list(src="./img/figfCLPM-no.jpg",
         contentType = "image/jpeg",
         width="70%", height="100%"
    )
  }, deleteFile=FALSE) 
  
  output$figRICLPM <- renderImage({
    list(src="./img/figRI-CLPM-no.jpg",
         contentType = "image/jpeg",
         width="95%", height="100%"
    )
  }, deleteFile=FALSE) 
  
  output$figSTARTS <- renderImage({
    list(src="./img/figSTARTS-no.jpg",
         contentType = "image/jpeg",
         width="95%", height="100%"
    )
  }, deleteFile=FALSE) 
  
  output$figLCMSR <- renderImage({
    list(src="./img/figLCM-SR-no.jpg",
         contentType = "image/jpeg",
         width="100%", height="100%"
    )
  }, deleteFile=FALSE) 
  
  output$figALT <- renderImage({
    list(src="./img/figALT-no.jpg",
         contentType = "image/jpeg",
         width="100%", height="100%"
    )
  }, deleteFile=FALSE) 
  
  output$figLCS <- renderImage({
    list(src="./img/figLCS-no.jpg",
         contentType = "image/jpeg",
         width="95%", height="100%"
    )
  }, deleteFile=FALSE) 
  
}

shinyApp(ui = ui, server = server)
