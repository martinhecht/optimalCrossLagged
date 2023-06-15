# MA 0.1.75 2023-06-15: redefined OpenMx functions to my_mx functions
# problem:
#   -- function for deploying (upload to shiny server) the app threw error 200
#      and crashed the shiny server while trying to auto-install R package
#      OpenMx from cran
#   -- OpenMx had been manually installed into an R library folder on the shiny
#      server (by Carlos/HU IT) with to us unknown path
#   -- the deployer function (possibly deployApp()) from the rsconnect package
#      auto-checks and tries to install R packages, the check is possibly done
#      by appDependencies(), in the manual it reads:
#        Dependencies are determined by parsing application source code and
#        looking for calls to library, require, ::, and :::
#   -- this auto-check/auto-install seems not being possible to turn off
# solution/workaround:
#   -- as OpenMx wouldn't auto-install when deploying the app, we removed all
#      keywords (library, require, ::, :::) from the entire code so that the
#      deployer wouldn't recognize OpenMx as a package that needs to be installed
#   -- needed functions could be accessed via ::, however we needed to hide ::
#      from the deployer, so we redefined the needed functions by calling an
#      evaluation (eval()) on a string that contained "::" (this is ignored by
#      the deployer), see below
# by-products/insights:
#   -- OpenMx still needs to be accessible, .libPaths contained three paths,
#      /usr/lib/R/site-library contained OpenMx
#   -- just for testing we added .Rprofile to the app directory in which we added
#      another path to .libPaths
#      https://community.rstudio.com/t/change-default-loading-of-shiny-to-specific-libpaths-on-shinyserver/18211
#      https://community.rstudio.com/t/change-default-loading-of-shiny-to-specific-libpaths-on-shinyserver/18211/8
#      this seemed to work (.libPaths had 4 entries)
#   -- lib.loc argument of library() seems to being ignored by the deployer,
#      it still tried to install OpenMx

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

