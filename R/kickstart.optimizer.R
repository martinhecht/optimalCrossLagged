## Changelog:
# MH 0.0.4 2022-01-15: renamed calc.power. to kickstart.optimizer
# MH 0.0.3 2022-01-10:
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
kickstart.optimizer <- function( study, constraints, genoud, model, target.parameters, envs, verbose=TRUE ){

# browser()
		# initialize output as NA
		res <- list( "N.opt"=NA, "T.opt"=NA, "power.max"=NA )
		
		# input vector
		i <- c( study$budget, study$l2.cost, study$l1.cost )
		if( any(is.na(i)) || any(i[1:3]<0) ){
				#nothing 
		} else {
			
			# console output
			if( verbose ) { cat( "starting optim","\n" ); flush.console() }
		
# browser()			
			# constraints
			( N.min <- constraints$N.min )
			( N.max <- constraints$N.max )
			( T.min <- constraints$T.min )
			( T.max <- constraints$T.max )

			# maximal N possible by budget and T.min
			( N.max.bound <- floor(study$budget/((study$l2.cost+study$l1.cost)*T.min)) )
			
			# if user N.min is greater than maximal possible N.max.bound
			# warning and set N.min to N.max.bound
			if( N.min > N.max.bound ){
				msg <- paste0( "user-specified N.min (",N.min,") is greater than maximal possible N.max.bound (",N.max.bound,") determined by T.min;\n N.min set equal to N.max.bound, this however will result in the optimized N = N.min;\n set N.min to lower value than ",N.max.bound,"." )
				if( verbose ) cat( paste0( msg, "\n" ) ); flush.console()
				warning( msg )
				N.min <- N.max.bound
			}
			
			# set maximal N based on user input and boundaries
			( N.max.set <- min( c(N.max, N.max.bound) ) )
			## => T.min has priority over N.max
			
			# minimal N allowed by budget and T.max
			( N.min.bound <- ceiling(study$budget/((study$l2.cost+study$l1.cost)*T.max)) )

			# set minimal N based on user input and boundaries
			( N.min.set <- max( c(N.min, N.min.bound) ) )
			## => N.min has priority over T.max
			
			## for checking
			# maximal T possible by budget and N.min.set
			( T.max.bound <- floor((study$budget-study$l2.cost*N.min)/(study$l1.cost*N.min.set)) )
			
			
			
			# maximal T possible by budget and N.min
			# ( T.max.bound <- floor((study$budget-study$l2.cost*N.min)/(study$l1.cost*N.min)) )

			# set 
			# ( N.max.set <- min( c(N.max, N.max.bound) ) )			

			# determine necessary T (not used further, only for checks)
			# ( T.max.set <- max( c(T.max, T.max.bound) ) )
			
			# enforce.T.max <- FALSE
			# if( !enforce.T.max ){
				# if user T.max is smaller than the T.max boundary, warning
				# if( T.max < T.max.bound ) warning( paste0( "user input T.max=",T.max," is smaller than the T.max.bound (",T.max.bound,") necessary by N.min (",N.min,"). ") )
			# } else {
				# if user T.max should be enforced then N.min needs to be lifted
				
			# }

			# set minimal N/T allowed by budget and T/N.max.set
			# ( N.min.set <- ceiling(study$budget/((study$l2.cost+study$l1.cost)*T.max.set)) )
			# ( T.min.set <- ceiling((study$budget-study$l2.cost*N.max.set)/(study$l1.cost*N.max.set)) )
			
			
			# console output
			if( verbose ) {
				cat( paste0( "N.min: ",N.min,"\n" ) )
				cat( paste0( "N.max: ",N.max,"\n" ) )
				cat( paste0( "T.min: ",T.min,"\n" ) )
				cat( paste0( "T.max: ",T.max,"\n" ) )
				cat( paste0( "N.max.bound: ",N.max.bound,"\n" ) )
				cat( paste0( "N.max.set: "  ,N.max.set,"\n" ) )
				cat( paste0( "N.min.bound: ",N.min.bound,"\n" ) )
				cat( paste0( "N.min.set: "  ,N.min.set,"\n" ) )
				cat( paste0( "   Note:  T.min has priority over N.max\n" ) )
				cat( paste0( "          N.min has priority over T.max\n" ) )
				cat( paste0( "   fyi:\n" ) )
				cat( paste0( "   T.max.bound: "  ,T.max.bound,"\n" ) )
				flush.console()
			}
			
			# genoud has no verbosity argument, manual shutoff
			# https://github.com/AnotherSamWilson/ParBayesianOptimization/issues/3
			# if( !verbose ) sink("/dev/null")
			# but it doesn't work in Windows. In Windows you can use:
			# if( !verbose ) sink("nul:")
			# but that works only in Windows. A cross-OS solutions might be:
			# if( !verbose ) sink(tempfile())
			if( !verbose ){
				platform <- .Platform$OS.type
				if ( platform %in% "windows" ) { sink("nul:") }
				else if ( platform %in% "unix" ) { sink("/dev/null") }
				else { sink(tempfile()) }
			}
			
# browser()
			# starting values
			starting.values <- eval(parse(text=genoud$starting.values))
			
			# start optimizer
			res.opt <- genoud( fn = fn,
							   nvars = 1,
							   lexical = length( target.parameters ),
							   max = FALSE,
							   data.type.int = TRUE,
							   Domains = matrix( c( N.min.set, N.max.set ), 1, 2 ),
							   pop.size = genoud$pop.size,
							   max.generations = genoud$max.generations, 
							   wait.generations = genoud$wait.generations,
							   boundary.enforcement = genoud$boundary.enforcement,
							   solution.tolerance = genoud$solution.tolerance,
							   starting.values = starting.values,
							   # ... fn arguments
							   study = study,
							   model = model,
							   target.parameters = target.parameters,
							   envs = envs,
							   verbose = verbose )
			
			# end of sink
			if( !verbose ) sink()
			
			# optimal N
			N.opt. <- res.opt$par
			# as integer
			N.opt <- as.integer( N.opt. )
			# warning if not integer
			if( !N.opt == N.opt. ) warning( "N.opt seems not to be integer" )
			
			# console output
			if( verbose ) { cat( "end of optim","\n" ); flush.console()}
			
			# post processing
			if( any( c(inherits(N.opt,"try-error") ))){
					#nothing
			} else {
				
				# optimal T
				T.opt. <- (study$budget-study$l2.cost*N.opt)/(study$l2.cost*N.opt)
				T.opt <- as.integer( floor( T.opt. ) )
				
				# console output
				if( verbose ) { cat( "calculate.power se call", "\n" ); flush.console() }
				
				# maximal power
				power.max <- calculate.power( N=N.opt,
								   T=T.opt,
								   model=model,
								   target.parameters=target.parameters,
								   verbose=verbose
								   # cppf.env=envs$cppf.env
								   )
		
				# results vector for checking
				v <- c( N.opt, T.opt, power.max )
				
				if( any(is.na(v)) || any(v[1:2]<1) || v[3]<0 || v[3]>100 ){
						#nothing
				} else {
						res <- list( "N.opt"=N.opt, "T.opt"=T.opt, "power.max"=power.max )
				}
			}
		}
		
		# return
		return( res )
}
