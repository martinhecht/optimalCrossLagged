# MA 0.1.75 2023-06-15: redefined OpenMx functions to my_mx functions

my_mxModel <- eval(parse(text = "OpenMx::mxModel"), envir = .GlobalEnv)
my_mxMatrix <- eval(parse(text = "OpenMx::mxMatrix"), envir = .GlobalEnv)
my_mxExpectationRAM <- eval(parse(text = "OpenMx::mxExpectationRAM"),
                            envir = .GlobalEnv)
my_mxData <- eval(parse(text = "OpenMx::mxData"), envir = .GlobalEnv)
my_mxFitFunctionML <- eval(parse(text = "OpenMx::mxFitFunctionML"),
                           envir = .GlobalEnv)
my_mxRun <- eval(parse(text = "OpenMx::mxRun"), envir = .GlobalEnv)
my_mxGetExpected <- eval(parse(text = "OpenMx::mxGetExpected"),
                         envir = .GlobalEnv)

