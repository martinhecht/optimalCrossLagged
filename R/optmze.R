## Changelog:
# MH 0.0.6 2022-01-20
# MH 0.0.5 2022-01-19: renamed maximize.power to optmze
# MH 0.0.4 2022-01-15: renamed calc.power to maximize.power
# MH 0.0.3 2022-01-10
# MH 0.0.2 2021-11-25: update
# MH 0.0.1 2021-11-03: copy from multi level optimal design project

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
optmze <- function( optimize=list(	"what"=c("power"),
									"direction"=c("min"),
									"via"=c("se^2","se"), 
									"se.function"=c("compute.se.oertzen"),
									"optimizer"=c("genoud"),
									"starting.values"="round(mean(c(N.min.set,N.max.set)))",
									"set.seed.value"="random"
									),
					study=list("budget"=20000, "l2.cost"=10, "l1.cost"=10),
					constraints=list("N.min"=250, "N.max"=300, "T.min"=3, "T.max"=300,
									 "N.integer"=TRUE,
									 "T.integer"=TRUE ),
					genoud=list("pop.size"=20,"max.generations"=10,"wait.generations"=1,
								"boundary.enforcement"=2,"solution.tolerance"=0.001	),
					model, target.parameters=NULL,
					timeout=60, verbose=TRUE ){
		
		# packages
		pkgs <- "require( R.utils ); # withTimeout()
				 require( rgenoud ); # genoud()
				 require( Rcpp );
				 require( RcppArmadillo )"
		
		# suppress package loading outputs or not
		if( !verbose ) {
			suppressMessages( eval( parse( text=pkgs ) ) )
		} else {
			eval( parse( text=pkgs ) )
		}

		# handle optimize$what optimize$via optimize$se.function optimize$optimizer
		# i.e. if more than one value is given, take the first one
		optimize[c("what","direction","via","se.function","optimizer")] <- 
			sapply( optimize[c("what","direction","via","se.function","optimizer")],
			function( x ){
				if( length( x ) > 1 ) x[1] else x
			} )
		
		# direction of optimizer
		direction.optimizer <- optimize$direction
		# if power should be optimized via se method, then flip direction
		if( optimize$what %in% "power" && optimize$via %in% c("se^2","se") ){
			direction.optimizer <- c("max","min")[ !c("max","min") %in% optimize$direction ]
		}
		optimize$direction.optimizer <- direction.optimizer
		
		# set random seed in case it is requested
		if( optimize$set.seed.value %in% "random" ){
			optimize$set.seed.value <- sample.int( 999999999, 1 )
		}

		## target parameters
		all.tp.named <- do.call( "c", sapply( model$matrices, "[[", "labels" ))
		all.tp.named <- all.tp.named[!is.na( all.tp.named )]
		all.tp <- unname( all.tp.named )
		
		# set all if target.parameters is NULL
		if( is.null( target.parameters ) ){
			target.parameters <- all.tp
		}
		
		# check whether all specified target parameters are in the model object
		tp.log <- target.parameters %in% all.tp
		
		# if none specified parameter is valid then set all
		if( !any( tp.log ) ){
			msg <- "none of the specified target parameters are parameters in the model object;\n all model parameters are used."
			if( verbose ) cat( paste0( msg, "\n" ) ); flush.console()
			warning( msg )
			target.parameters <- all.tp
		}
		
		# if some are wrongly specified, kick them out
		if( !all( tp.log ) ){
			msg <- paste0( "the following target parameters are not in the model object and are discarded: ", paste( target.parameters[ !tp.log ], collapse=", " ) )
			if( verbose ) cat( paste0( msg, "\n" ) ); flush.console()
			warning( msg )
			target.parameters <- target.parameters[ tp.log ]
		}

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

		# put into constraints list
		constraints$N.max.bound <- N.max.bound
		constraints$N.max.set <- N.max.set
		constraints$N.min.bound <- N.min.bound
		constraints$N.min.set <- N.min.set
		constraints$T.max.bound <- T.max.bound
	
		## starting value for N
		# if character, try to evaluate
		if( is.character( optimize$starting.values ) ){
			# save equation for documentation
			optimize$starting.values.equation <- optimize$starting.values
			optimize$starting.values <- eval(parse(text=optimize$starting.values))
		}
	
		# environment to track statistics of optimizer (e.g., number of optim runs)
		optmz.env <- new.env()
		assign( "optimizer.runs", 0, pos = optmz.env,
											  inherits = FALSE, immediate=TRUE)
		
		# environments lists
		envs <- list( optmz.env )
		names( envs ) <- c( "optmz.env" )
		
		# with timeout (or not)
		wt <- !is.na( timeout ) && !is.null( timeout ) && is.numeric( timeout ) && timeout > 0 

		# start time 
		start.time.optimizer <- Sys.time()
		
		# call kickstart.optimizer
		res <- eval( parse( text=paste0( ifelse(wt,'withTimeout(',""),
													'try(kickstart.optimizer(
														optimize=optimize,
														study=study,
														constraints=constraints,
														genoud=genoud,
														model=model,
														target.parameters=target.parameters,
														envs=envs,
														verbose=verbose ) )',
														ifelse(wt,',
														timeout = timeout,
														onTimeout = "error" )', "" ) ) ) )

		# runtime in seconds
		run.time.optimizer.difftime <- Sys.time() - start.time.optimizer
		run.time.optimizer.secs <- as.double( run.time.optimizer.difftime, units="secs" )

		# if timeouted or error, results are NA
		if( c(inherits(res,"try-error") ) ){
			N.opt. <- as.numeric(NA)
			values.opt <- rep( as.numeric(NA), length( target.parameters ) )
		} else {
			N.opt. <- res$N.opt
			values.opt <- res$values.opt
		}
# browser()
		
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
			T.opt. <- calculate.T( budget=study$budget, l2.cost=study$l2.cost, l1.cost=study$l1.cost, N=N.opt )
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
											target.parameters=target.parameters,
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
		
		# add optimizer runs to results list
		res <- c( res, list( 
							 # "run.time.cppf"=run.time.cppf,
							 "run.time.optimizer.secs"=run.time.optimizer.secs,
							 "optimizer.runs"=optimizer.runs ) )
		
		# return
		return( res )
}

### development
# optimalclpm needs to be loaded for compiled C++ functions
# else they are defined locally in compute.se.oertzen()
# library( optimalclpm ); mm( matrix(1:4,2,2), matrix(1:4,2,2) )

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/84_optimalclpm/04_martinhecht/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("optmze.R","RcppExports.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }


# example 2
# model <- generate.model.example.2()

# res <- optmze( model=model,target.parameters="arcl_eta1eta2",timeout=6000,verbose=TRUE)
# res <- optmze( model=model,timeout=6000,verbose=TRUE)
# res <- optmze( model=model,target.parameters=c("arcl_eta1eta2","arcl_eta2eta1"), verbose=TRUE )

# print( res )
# str( res )






### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
