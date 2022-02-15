## Changelog:
# MH 0.0.4 2022-01-15: renamed foptim to fn
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
fn <- function( N, study, model, target.parameters, envs, verbose=TRUE ){
		
		# start time
		start.time <- Sys.time()
		
		# number of optim runs
		n.optim.runs.current <- get( "n.optim.runs", envir=envs$optmz.env )
		n.optim.runs.new <- n.optim.runs.current + 1
		assign( "n.optim.runs", n.optim.runs.new, envir=envs$optmz.env, inherits=FALSE, immediate=TRUE )
		if ( verbose ) {
				cat( "======= OPTIMIZER RUN: ", n.optim.runs.new, "=======\n" )
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
		se <- compute.se.oertzen( N=N,
								  timepoints=T,
								  n_ov=model$n_ov,
								  #names_ov=model$names_ov,
								  n_process=model$n_process,
								  #names_process=model$names_process,
								  matrices=model$matrices,
								  # cppf.env=envs$cppf.env, 
								  target.parameters=target.parameters,
								  verbose=verbose )

		# console output
		if ( verbose ) {
			cat( "se of target parameter: ", se, "\n" )
			cat( "run time: ", Sys.time() - start.time, "\n" )
			flush.console()
		}
		
		# return
		return( se^2 ) # Varianz statt SE, Gespräch 25.11.2021
}
