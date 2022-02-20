## Changelog:
# MH 0.0.6 2022-01-20
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
fn <- function( N, optimize, study, constraints, model, target.parameters, envs, verbose=TRUE ){
		
		# start time
		start.time <- Sys.time()
		
		# number of optimizer runs
		optimizer.runs.current <- get( "optimizer.runs", envir=envs$optmz.env )
		optimizer.runs.new <- optimizer.runs.current + 1
		assign( "optimizer.runs", optimizer.runs.new, envir=envs$optmz.env, inherits=FALSE, immediate=TRUE )
		
		# Constraint / Cost function: compute T from N
		T <- calculate.T( budget=study$budget, l2.cost=study$l2.cost, l1.cost=study$l1.cost, N=N )
		if( constraints$T.integer ) T <- as.integer( round( T ) )
		
		# console output
		if ( verbose ) {
				cat( "======= OPTIMIZER RUN: ", optimizer.runs.new, "=======\n" )
				cat( "number of persons: ", N, "\n" )
				cat( "number of time points: ", T, "\n" )
				flush.console()
		}

# browser()
		
		# compute standard error
		if( optimize$se.function %in% "compute.se.oertzen" ){
			se <- compute.se.oertzen( N=round(N),
									  timepoints=round(T),
									  n_ov=model$n_ov,
									  #names_ov=model$names_ov,
									  n_process=model$n_process,
									  #names_process=model$names_process,
									  matrices=model$matrices,
									  # cppf.env=envs$cppf.env, 
									  target.parameters=target.parameters,
									  verbose=verbose )
		}
		
		# console output
		if ( verbose ) {
			cat( "se of target parameter: ", se, "\n" )
			cat( "run time: ", Sys.time() - start.time, "\n" )
			flush.console()
		}

		# prepare return
		if( optimize$via %in% c("se","se^2") ) { ret <- eval( parse( text=optimize$via ) ) } 
		
		# return
		return( ret )
}
