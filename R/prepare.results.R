## Changelog:
# MH 0.0.44 2022-11-04: added get.clpm.info
# MH 0.0.31 2022-09-05: error code 13 implementation
# MH 0.0.30 2022-09-02: modification for stability checks
# MH 0.0.23 2022-07-27: bugfix, model$target.parameters
# MH 0.0.21 2022-07-24: now returns Sigma_H1/Sigma_H0
# MH 0.0.1 2022-01-20: copied chunks from optmze

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
prepare.results <- function( res, run.time.optimizer.secs, input, clpm.info.list,
							 verbose=TRUE, error_codes ){

		# put elements from input list on this environment
		do <- paste0( names( input ), " <- input$", names( input ) )
		eval( parse( text=do ) )
		
		# either N or T
		par <- optimize$par
		oth <- c("N","T")[! c("N","T") %in% optimize$par ]
		
		# if timeouted or error, results are NA
		if( c(inherits(res,"try-error") ) ){
			eval( parse( text=paste0( par,'.opt. <- as.numeric(NA)' ) ) )
			# MH 0.0.23 2022-07-27: bugfix, _model$_target.parameters
			values.opt <- rep( as.numeric(NA), length( model$target.parameters ) )
		} else {
			eval( parse( text=paste0( par,'.opt. <- res$par.opt' ) ) )
			values.opt <- res$values.opt
		}

		# rounding of N.opt and calculating of T.opt
		#                  or
		# rounding of T.opt and calculating of N.opt
		eval( parse( text=paste0('
		if( !is.na(',par,'.opt.) ){
			if( constraints$',par,'.integer ){
				# <X>.opt as integer
				',par,'.opt <- as.integer( ',par,'.opt. )
				# warning if not integer
				# (because optimizer should have returned an integer)
				if( !',par,'.opt == ',par,'.opt. ) warning( "',par,'.opt seems not to be integer" )
			} else {
				',par,'.opt <- ',par,'.opt.
			}
			
			# optimal <Y>
			if( optimize$what %in% c("power","budget") ){
				',oth,'.opt. <- calculate.from.cost.function( what="',oth,'",
														budget=study$budget, ',par,'=',par,'.opt,
														l2.cost=study$l2.cost,
														l1.cost=study$l1.cost )
				if( constraints$',oth,'.integer ){
					# if <Y> should be integer then round it (floor)
					',oth,'.opt <- as.integer( floor( ',oth,'.opt. ) )
				} else {
					',oth,'.opt <- ',oth,'.opt.
				}
			}
			if( optimize$what %in% c("target.power") ){
				# <Y> is set by user
				',oth,'.opt <- study$',oth,'
			}
		} else {
			# <X>.opt is also NA as <X>.opt.
			',par,'.opt <- ',par,'.opt.
			# <Y>.opt defined as NA
			',oth,'.opt <- as.numeric(NA)
		}
		' ) ) )
		
		## optimal power
		# if via power, then return values are the power
		if( optimize$what %in% "power" && optimize$via %in% c("power") ) {
			power.opt <- values.opt
		}
		# if power optimization was with se (or se^2), optimal power needs to be calculated
		if( optimize$what %in% "power" && optimize$via %in% c("se^2","se") ) {
			
			# console output
			if( verbose ) { cat( "call calculate.power", "\n" ); flush.console() }

			# if power was computed via se^2 or se, then se are already available
			# and do not need to be computed again
			if( optimize$via %in% c("se^2","se") ){
				if( optimize$via %in% "se^2" ){
					se.target.parameters <- sqrt( values.opt )
				} else if( optimize$via %in% "se" ) {
					se.target.parameters <- values.opt
				}
			} else {
				se.target.parameters <- NULL
			}

			# optimal power
			power.opt <- calculate.power( 	N=N.opt,
											T=T.opt,
											model=model,
											alpha=study$alpha,
											se.target.parameters=se.target.parameters,
											via.function=optimize$via.function,
											verbose=verbose
										)
		}
		# also if budget is optimized (or target.power should be achieved)
		# then power is calculated to be outputted fyi (to see whether target power was achieved)
		if( optimize$what %in% c("budget","target.power") ) {
				if( optimize$via.function %in% "calculate.power.LRT" ){
						power.opt <- calculate.power.LRT( 	alpha=study$alpha,
															N=N.opt,
															timepoints=T.opt,
															n_ov=model$specification$n_ov,
															n_process=model$specification$n_process,
															matrices=model$specification$matrices, 
															target.parameters=model$target.parameters,
															pwrLRT.env=envs$pwrLRT.env,
															verbose=verbose )
				}
				if( optimize$via %in% c("se^2","se") ){
						power.opt <- calculate.power( 	N=N.opt,
														T=T.opt,
														model=model,
														alpha=study$alpha,
														se.target.parameters=se.target.parameters,
														via.function=optimize$via.function,
														verbose=verbose
						)
				}
		}

		
		## optimal budget
		# if budget was optimized, then return values are optimal budget
		if( optimize$what %in% "budget" ){
			# optimal budget from optimization
			budget.opt <- unname( values.opt )
			
			## enforce target power (it can happen that power is not hit perfectly)
			# optimize N
			# study$T <- T.opt
			# rs <- optmze(	optimize=list(
								# "what"=c("target.power"),
								# "direction"=c("min"),
								# "via"=optimize$via,
								# "par"=c("N"),
								# "via.function"=optimize$via.function,
								# "optimizer"=c("genoud"),
								# "starting.values"=optimize$starting.values,
								# "set.seed.value"=optimize$set.seed.value
								# ),
							# study=study,
							# constraints=constraints,
							# model=model,
							# genoud=genoud,
							# timeout=timeout,
							# verbose=verbose )
			# new N what closest achieves target power
			# N.opt.pwr.enforced <- rs$N.opt			
			# new budget
			# budget.opt.pwr.enforced <- calculate.from.cost.function( "what"="budget", N=N.opt.pwr.enforced, T=T.opt, l2.cost=study$l2.cost, l1.cost=study$l1.cost )
		}
		# if power was optimized or if target.power was tried to being achieved, calculate budget
		if( optimize$what %in% c("power","target.power") ){
			budget.opt <- calculate.from.cost.function( "what"="budget", N=N.opt, T=T.opt, l2.cost=study$l2.cost, l1.cost=study$l1.cost )
		}
		
	
		### results list
		
		# N.opt/T.opt
		res2 <- list( "N.opt"=N.opt, "T.opt"=T.opt )
		
		# power
		if( optimize$what %in% c("power","budget","target.power") ){
			res.power <- list( power.opt )
			if( optimize$what %in% "power" && optimize$direction %in% c("max","min") ){
				res.power.name <- paste0( "power.", optimize$direction )
			} else {
				res.power.name <- "power.opt"
			}
			names( res.power ) <- res.power.name
			res2 <- c( res2, res.power )
		}
		
		# se
		if( optimize$via %in% c("se^2","se") ){
			if( optimize$via %in% "se^2" ){
				se.opt <- sqrt( values.opt )
			} else {
				se.opt <- values.opt
			}
			res.se <- list( values.opt )
			
			if( optimize$direction.optimizer %in% c("max","min") ){
				res.se.name <- paste0( "se.", optimize$direction.optimizer )
			} else {
				# should never be the case
				res.se.name <- "se.opt"
			}
			names( res.se ) <- res.se.name
			res2 <- c( res2, res.se )
		}
		
		# optimal budget
		if( optimize$what %in% c("power","budget","target.power") ){
			res.budget <- list( budget.opt )
			if( optimize$what %in% "budget" && optimize$direction %in% c("max","min") ){
				res.budget.name <- paste0( "budget.", optimize$direction )
			} else {
				res.budget.name <- "budget.opt"
			}
			names( res.budget ) <- res.budget.name
			res2 <- c( res2, res.budget )
		}
		# optimal budget power enforced
		# if( optimize$what %in% c("budget") ){
			# res2.budget <- list( N.opt.pwr.enforced, budget.opt.pwr.enforced )
			# names( res2.budget ) <- c( "N.opt.pwr.enforced", paste0( res.budget.name, ".pwr.enforced" ) )
			# res2 <- c( res2, res2.budget )
		# }
		
		# number of optimizer runs
		optimizer.runs <- get( "optimizer.runs", pos=envs$optmz.env )
		
		# add run time and optimizer runs to results list
		res2 <- c( res2, list( "run.time.optimizer.secs"=run.time.optimizer.secs,
							 "optimizer.runs"=optimizer.runs ) )

		# MH 0.0.30 2022-09-02
		# add optimized parameter values from runs and stability check result
		res2 <- c( res2, list( "par.opts"=res$par.opts,
							   "stable.solution"=res$stable.solution ) )

		# add constraints
		res2 <- c( res2, list( "constraints"=constraints ) )

		## MH 0.0.21 2022-07-24: covariance matrice of optimized model
		F_diff <- calculate.F.diff(
          timepoints = res2$T.opt,
          input_H1 = model$specification$input_H1,
          target.parameters = model$target.parameters,
          target.parameters.values.H0 = model$target.parameters.values.H0,
		  return.Sigma=TRUE
        )[c("Sigma_H1","Sigma_H0")]
		res2 <- c( res2, F_diff )
		
		# MH 0.0.31 2022-09-05:
		pop.size.max.reached <- genoud$pop.size>=genoud$pop.size.max
		
		# MH 0.0.30 2022-09-02:
		# add stability error (code: 12)
		if( !is.na(res$stable.solution) && !res$stable.solution && !pop.size.max.reached ) error_codes <- c( error_codes, 12 ) 
		
		# MH 0.0.31 2022-09-05:
		# stability error if pop.size is max (code: 13)
		if( !is.na(res$stable.solution) && !res$stable.solution && pop.size.max.reached ) error_codes <- c( error_codes, 13 ) 
		
		# add error_codes to results list
		res2 <- c( res2, list( "error_codes"=error_codes ) )
		
		# MH 0.0.44 2022-11-04: added get.clpm.info
		res2 <- c( res2, list( "optimalclpm.version.str"=clpm.info.list$optimalclpm.version.str ) )
		
		# return
		return( res2 )

}