## Changelog:
# MH 0.0.1 2022-01-20: copied chunks from optmze

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
prepare.results <- function( res, run.time.optimizer.secs, input, verbose=TRUE ){

		# put elements from input list on this environment
		do <- paste0( names( input ), " <- input$", names( input ) )
		eval( parse( text=do ) )

		# if timeouted or error, results are NA
		if( c(inherits(res,"try-error") ) ){
			N.opt. <- as.numeric(NA)
			values.opt <- rep( as.numeric(NA), length( target.parameters ) )
		} else {
			N.opt. <- res$N.opt
			values.opt <- res$values.opt
		}

		
		# rounding of N.opt and calculating of T.opt
		if( !is.na(N.opt.) ){
			if( constraints$N.integer ){
				# N.opt as integer
				N.opt <- as.integer( N.opt. )
				# warning if not integer
				# (because optimizer should have returned an integer)
				if( !N.opt == N.opt. ) warning( "N.opt seems not to be integer" )
			} else {
				N.opt <- N.opt.
			}
			
			# optimal T
			T.opt. <- calculate.from.cost.function( what="T",
													budget=study$budget, N=N.opt,
													l2.cost=study$l2.cost,
													l1.cost=study$l1.cost )
			if( constraints$T.integer ){
				# if T should be integer then round it (floor)
				T.opt <- as.integer( floor( T.opt. ) )
			} else {
				T.opt <- T.opt.
			}
		} else {
			# N.opt is also NA as N.opt.
			N.opt <- N.opt.
			# T.opt defined as NA
			T.opt <- as.numeric(NA)
		}
		
		# maximal power
		# (if power optimization was with se (or se^2), max power needs to be calculated) 
		if( optimize$what %in% "power" && optimize$via %in% c("se^2","se") ){
			
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
											se.target.parameters=se.target.parameters,
											se.function=optimize$se.function,
											verbose=verbose
											# cppf.env=envs$cppf.env
										)
		}
	
		# results vector for checking
		# v <- c( N.opt, T.opt, power.max )
		
		# if( any(is.na(v)) || any(v[1:2]<1) || v[3]<0 || v[3]>100 ){
				#nothing
		# } else {

		# }
		
		
		### results list
		
		# N.opt/T.opt
		res <- list( "N.opt"=N.opt, "T.opt"=T.opt )
		
		# power
		if( optimize$what %in% "power" ){
			res.power <- list( power.opt )
			if( optimize$direction %in% c("max","min") ){
				res.power.name <- paste0( "power.", optimize$direction )
			} else {
				# should never be the case
				res.power.name <- "power.opt"
			}
			names( res.power ) <- res.power.name
			res <- c( res, res.power )
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
			res <- c( res, res.se )
		}
		
		
		# number of optimizer runs
		optimizer.runs <- get( "optimizer.runs", pos=envs$optmz.env )
		
		# add run time and optimizer runs to results list
		res <- c( res, list( "run.time.optimizer.secs"=run.time.optimizer.secs,
							 "optimizer.runs"=optimizer.runs ) )

		# return
		return( res )

}