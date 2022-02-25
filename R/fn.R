## Changelog:
# MH 0.0.7 2022-01-20
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
fn <- function( pr, optimize, study, constraints, model, envs, verbose=TRUE ){

		# start time
		start.time <- Sys.time()
		
		# either N or T
		par <- optimize$par
		oth <- c("N","T")[! c("N","T") %in% optimize$par ]
		
		# parameter value
		eval( parse( text=paste0( par, ' <- pr' ) ) )
		
		# number of optimizer runs
		optimizer.runs.current <- get( "optimizer.runs", envir=envs$optmz.env )
		optimizer.runs.new <- optimizer.runs.current + 1
		assign( "optimizer.runs", optimizer.runs.new, envir=envs$optmz.env, inherits=FALSE, immediate=TRUE )

		# Constraint / Cost function: compute T from N
		#                                   or
		#                             compute N from T
		eval( parse( text=paste0( oth, ' <- calculate.from.cost.function( what="',oth,'", budget=study$budget, l2.cost=study$l2.cost, l1.cost=study$l1.cost, ',par,'=',par,' )' ) ) )
		
		# if integer is required, make integer
		eval( parse( text=paste0( 'if( constraints$',oth,'.integer ) ',oth,' <- as.integer( round( ',oth,' ) )' ) ) )
		
		# console output
		if ( verbose ) {
				cat( "======= OPTIMIZER RUN: ", optimizer.runs.new, "=======\n" )
				cat( "number of persons: ", N, "\n" )
				cat( "number of time points: ", T, "\n" )
				flush.console()
		}

# browser()
		# initialize default return
		value <- NULL
		
		# via se/se^2
		if( optimize$via %in% c("se^2","se") ) {
		
				# compute standard error
				if( optimize$via.function %in% "compute.se.oertzen" ){
					se <- compute.se.oertzen( 	N=round(N),
												timepoints=round(T),
												n_ov=model$specification$n_ov,
												#names_ov=model$specification$names_ov,
												n_process=model$specification$n_process,
												#names_process=model$specification$names_process,
												matrices=model$specification$matrices,
												target.parameters=model$target.parameters,
												# cppf.env=envs$cppf.env, 
												verbose=verbose )
				}
				
		}
		
		# via power
		if( optimize$via %in% c("power") ) {
				
				# compute power
				if( optimize$via.function %in% "calculate.power.LRT" ){
						
						power <- calculate.power.LRT( alpha=0.05,
													  N=N,
													  timepoints=T,
									n_ov=model$specification$n_ov,
									n_process=model$specification$n_process,
									matrices=model$specification$matrices, 
									target.parameters=model$target.parameters,
									pwrLRT.env=envs$pwrLRT.env,
									verbose=verbose )
				}
		}

		# prepare return
		value <- eval( parse( text=optimize$via ) )

		# console output
		if ( verbose ) {
			cat( paste0( optimize$via, " of target parameters: ", paste( round( value, 5), collapse=", " ), "\n" ) )
			cat( "run time: ", Sys.time() - start.time, "\n" )
			flush.console()
		}
		
		# return
		return( value )
}
