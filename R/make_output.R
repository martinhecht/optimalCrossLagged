## Changelog:
# MH 0.0.34 2022-09-15: log.data.status, logid, run.time.log.data.secs
# MH 0.0.30 2022-09-02:
#    -- modification for stability checks
#    -- changed error_codes default from NA to NULL, this seems more in line
#       with the current programming of check_plausability() where
#       error_codes <- c() (=NULL) in line 3

make_output <- function (N.opt = NA, T.opt = NA, power.max = NA,
                         budget.opt = NA, run.time.optimizer.secs = NA,
                         optimizer.runs = NA,
						 # MH 0.0.30 2022-09-02: added par.opts, stable.solution
						 par.opts = as.numeric(NA),
						 stable.solution = as.logical(NA),
						 constraints = NA, Sigma_H1 = NA,
                         Sigma_H0 = NA,
						 # MH 0.0.30 2022-09-02: changed error_codes default from NA to NULL
						 error_codes = NULL,
						 # MH 0.0.34 2022-09-15
						 log.data.status=as.character(NA),
						 logid=as.integer(NA),
						 run.time.log.data.secs=as.numeric(NA)
						 ) {
  
  list(N.opt = N.opt,
       T.opt = T.opt,
       power.max = power.max,
       budget.opt = budget.opt,
       run.time.optimizer.secs = run.time.optimizer.secs,
       optimizer.runs = optimizer.runs,
	   # MH 0.0.30 2022-09-02: added par.opts, stable.solution
	   par.opts = par.opts,
	   stable.solution = stable.solution,	   
       constraints = constraints,
       Sigma_H1 = Sigma_H1,
       Sigma_H0 = Sigma_H0,
       error_codes = error_codes,
	   # MH 0.0.34 2022-09-15
	   log.data.status = log.data.status,
	   logid = logid,
	   run.time.log.data.secs = run.time.log.data.secs
	   )
  
}

