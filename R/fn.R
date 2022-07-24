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
fn <- function( pr, optimize, study, constraints, model, genoud, envs, timeout, verbose=TRUE ){

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

		# calculate other (<Y>) with cost function
		if( optimize$what %in% c("power","budget") ){
			# Constraint / Cost function: compute T from N
			#                                   or
			#                             compute N from T
			eval( parse( text=paste0( oth, ' <- calculate.from.cost.function( what="',oth,'", budget=study$budget, l2.cost=study$l2.cost, l1.cost=study$l1.cost, ',par,'=',par,' )' ) ) )
		
			# if integer is required, make integer
			eval( parse( text=paste0( 'if( constraints$',oth,'.integer ) ',oth,' <- as.integer( round( ',oth,' ) )' ) ) )
		}
		if( optimize$what %in% c("target.power") ){
			# T is set by user
			T <- study$T
		}
		
		# console output
		if ( verbose ) {
				cat( "======= OPTIMIZER RUN: ", optimizer.runs.new, "=======\n" )
				cat( "number of persons: ", N, "\n" )
				cat( "number of time points: ", T, "\n" )
				flush.console()
		}

		# initialize default return
		value <- NULL
		
		# power optimization
		if( optimize$what %in% c("power","target.power") ){
		
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
								
						  power <- calculate.power.LRT(
						    alpha=study$alpha,
						    N=N,
						    timepoints=T,
						    input_H1 = model$specification$input_H1,
						    target.parameters = model$target.parameters,
						    target.parameters.values.H0 = model$target.parameters.values.H0,
						    pwrLRT.env = envs$pwrLRT.env,
						    verbose=verbose)
						}
				}

				## prepare return
				# if power is optimized, then return power of target parameters
				if( optimize$what %in% "power" ) value <- eval( parse( text=optimize$via ) )
				# if target.power should be achieved, then return is quadratic loss
				if( optimize$what %in% "target.power" ) value <- ( power - study$target.power )^2
		}
		
		# budget optimization
		if( optimize$what %in% "budget" ){

				## optimize N to achieve target power
				# set T (current T as varied by optimizer)
				study$T <- T

				## starting value for N
				# get current budget
				budget.current <- get( "budget.current", envir=envs$optmz.env )
				N.start <- calculate.from.cost.function( "what"="N", budget=budget.current, T=T, l2.cost=study$l2.cost, l1.cost=study$l1.cost )
				if( N.start > constraints$N.max.bound ) N.start <- constraints$N.max.bound
				if( N.start < constraints$N.min.bound ) N.start <- constraints$N.min.bound
				
				# optimize N
				res <- optmze(	optimize=list(
									"what"=c("target.power"),
									"direction"=c("min"),
									"via"=optimize$via,
									"par"=c("N"),
									"via.function"=optimize$via.function,
									"optimizer"=c("genoud"),
									"starting.values"=N.start,
									"set.seed.value"=optimize$set.seed.value
									),
								study=study,
								constraints=constraints,
								model=model,
								genoud=genoud,
								timeout=timeout,
								verbose=verbose )
				# new N what closest achieves target power
				N <- res$N.opt
				if ( verbose ) {
						cat( "optimized N to achieve target power: ", N, "\n" )
						flush.console()
				}
				
				# calculate the budget based on T (varied by optimizer (outer run))
				# and N (optimized to achieve target power (inner run))
				value <- calculate.from.cost.function( "what"="budget", N=N, T=T, l2.cost=study$l2.cost, l1.cost=study$l1.cost )
				
				# write current budget to envs$optmz.env
				assign( "budget.current", value, envir=envs$optmz.env )
		}

		# console output
		if ( verbose ) {
			cat( paste0( optimize$via, " of target parameters: ", paste( round( value, 5), collapse=", " ), "\n" ) )
			cat( "run time: ", Sys.time() - start.time, "\n" )
			flush.console()
		}
		
		# return
		return( value )
}
