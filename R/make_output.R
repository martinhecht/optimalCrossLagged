make_output <- function (N.opt = NA, T.opt = NA, power.max = NA,
                         budget.opt = NA, run.time.optimizer.secs = NA,
                         optimizer.runs = NA, constraints = NA, Sigma_H1 = NA,
                         Sigma_H0 = NA, error_codes = NA) {
  
  list(N.opt = N.opt,
       T.opt = T.opt,
       power.max = power.max,
       budget.opt = budget.opt,
       run.time.optimizer.secs = run.time.optimizer.secs,
       optimizer.runs = optimizer.runs,
       constraints = constraints,
       Sigma_H1 = Sigma_H1,
       Sigma_H0 = Sigma_H0,
       error_codes = error_codes)
  
}

