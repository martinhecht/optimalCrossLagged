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
		
		# either N or T
		par <- optimize$par
		oth <- c("N","T")[! c("N","T") %in% optimize$par ]
		
		# if timeouted or error, results are NA
		if( c(inherits(res,"try-error") ) ){
			eval( parse( text=paste0( par,'.opt. <- as.numeric(NA)' ) ) )
			values.opt <- rep( as.numeric(NA), length( target.parameters ) )
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
		} else {
			# <X>.opt is also NA as <X>.opt.
			',par,'.opt <- ',par,'.opt.
			# <Y>.opt defined as NA
			',oth,'.opt <- as.numeric(NA)
		}
		' ) ) )
		
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
											via.function=optimize$via.function,
											verbose=verbose
										)
		}
		# via power
		if( optimize$what %in% "power" && optimize$via %in% c("power") ) {
			power.opt <- values.opt
		}
	
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