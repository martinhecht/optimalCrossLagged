## Changelog:
# MH 0.0.2 2021-11-25: update
# MH 0.0.1 2021-11-03: copy from multi level optimal design project

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return
#' @keywords internal

## Function definition
foptim <- function( N, study, model, target.parameter, envs, verbose ){
		
		# start time
		start.time <- Sys.time()
		
		# number of optim runs
		n.optim.runs_current <- get( "n.optim.runs", envir=envs$optmz.env )
		n.optim.runs_new <- n.optim.runs_current + 1
		assign( "n.optim.runs", n.optim.runs_new, envir = envs$optmz.env, inherits = FALSE, immediate = TRUE )
		if ( verbose ) {
				cat( "optimizer run: ", n.optim.runs_new, "\n" )
				cat( "number of persons: ", N, "\n" )
				flush.console()
		}

		
		# Constraint / Cost function: compute T from N
		T <- round( (study$budget-study$l2.cost*N)/(study$l1.cost*N) )

		# console output
		if ( verbose ) {
				cat( "number of time points: ", T, "\n" )
				flush.console()
		}

# browser()
		# compute standard error
		se <- compute_se_oertzen( N=N,
								  timepoints=T,
								  n_ov=model$n_ov,
								  #names_ov=model$names_ov,
								  n_process=model$n_process,
								  #names_process=model$names_process,
								  matrices=model$matrices,
								  cppf.env=envs$cppf.env, 
								  target.parameters=target.parameter )

		# console output
		if ( verbose ) {
			cat( "se of target parameter: ", se, "\n" )
			cat( "run time: ", Sys.time() - start.time, "\n" )
			flush.console()
		}
		
		# return
		return( se^2 ) # Varianz statt SE, Gespräch 25.11.2021
}

### development
# Rdir <- "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R"
# Rfiles <- list.files( Rdir, pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("foptim.R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R","Make RAM matrices.R") ]
# Rfiles <- file.path( Rdir, Rfiles )
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
