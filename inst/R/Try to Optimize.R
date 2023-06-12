setwd("~/GitHub/optimalCrossLagged/R")

source("generate.model.example.3.R")
source("optmze.R")
source("prepare.input.R")
source("prepare.results.R")
source("calculate.from.cost.function.R")
source("kickstart.optimizer.R")
source("fn.R")
source("calculate.power.LRT.R")
source("get_all_labels.R")
source("calculate.F.diff.fast.R")
source("calculate.F.diff.precise.R")

library(R.utils)
library(rgenoud)
library(RcppArmadillo)


specs <- generate.model.example.3()
specs$N <- NULL
specs$timepoints <- NULL
specs$names_ov <- NULL
specs$names_process <- NULL



 while(TRUE){
 res <- optmze(
   model = list("specification" = specs,
 "target.parameters" = c("ARCL_2_1", "ARCL_1_2")),
 study=list("budget" = 20000, "target.power" = 0.80, "l2.cost" = 10,
            "l1.cost" = 10, alpha = 0.05, T = 8),
 optimize=list(
   "what"=c("power"),
   ## "what"=c("budget"),
   ## "what"=c("target.power"),
   "direction"=c("max"),
   ## "direction"=c("min"),
   "via"=c("power"),
   "par"=c("T"),
   ## "par"=c("N"),
   "via.function"=c("calculate.power.LRT"),
   "optimizer"=c("genoud"),
   "starting.values"="round(mean(c(par.min.set,par.max.set)))",
   "set.seed.value"="random"
 ),
## constraints
 constraints=list("T.min"=3, "T.max"=40, "N.min"=3, "N.max"=1000,
 "T.integer"=TRUE,
 "N.integer"=FALSE ),									
 genoud=list("pop.size"=16,"max.generations"=100,"wait.generations"=1,
 "boundary.enforcement"=2,"solution.tolerance"=0.001),
 verbose=FALSE )

 str( res ); flush.console()
 