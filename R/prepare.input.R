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
prepare.input <- function( optimize, study,	constraints, model, genoud, verbose=TRUE ){

		# handle optimize$what $direction $via $se.function $optimizer
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

		### target parameters
		
		# all possible target parameters from model specification
		all.tp.named <- do.call( "c", sapply( model$specification$matrices, "[[", "labels" ))
		all.tp.named <- all.tp.named[!is.na( all.tp.named )]
		all.tp <- unname( all.tp.named )
		
		# get target parameters from model list
		target.parameters <- model$target.parameters
		
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
		
		# set target parameters in model list
		model$target.parameters <- target.parameters
		
		# constraints
		N.min <- constraints$N.min
		N.max <- constraints$N.max
		T.min <- constraints$T.min
		T.max <- constraints$T.max

		# maximal N possible by budget and T.min
		N.max.bound <- calculate.from.cost.function( what="N",
													 budget=study$budget, T=T.min,
													 l2.cost=study$l2.cost,
													 l1.cost=study$l1.cost )
		if( constraints$N.integer ) N.max.bound <- floor( N.max.bound )
		
		# if user N.min is greater than maximal possible N.max.bound
		# warning and set N.min to N.max.bound
		if( N.min > N.max.bound ){
			msg <- paste0( "user-specified N.min (",N.min,") is greater than maximal possible N.max.bound (",N.max.bound,") determined by T.min;\n N.min set equal to N.max.bound, this however will result in the optimized N = N.min;\n set N.min to lower value than ",N.max.bound,"." )
			if( verbose ) cat( paste0( msg, "\n" ) ); flush.console()
			warning( msg )
			N.min <- N.max.bound
		}
		
		# set maximal N based on user input and boundaries
		N.max.set <- min( c(N.max, N.max.bound) )
		## => T.min has priority over N.max
		if( constraints$N.integer ) N.max.set <- floor( N.max.set )
		
		# minimal N allowed by budget and T.max
		# N.min.bound <- study$budget/(study$l2.cost+study$l1.cost*T.max)
		N.min.bound <-  calculate.from.cost.function( what="N",
													  budget=study$budget, T=T.max,
													  l2.cost=study$l2.cost,
													  l1.cost=study$l1.cost )
		if( constraints$N.integer ) N.min.bound <- ceiling( N.min.bound )

		# set minimal N based on user input and boundaries
		N.min.set <- max( c(N.min, N.min.bound) )
		## => N.min has priority over T.max
		if( constraints$N.integer ) N.min.set <- ceiling( N.min.set )
		
		## for checking
		# maximal T possible by budget and N.min.set
		# T.max.bound <- (study$budget/N.min.set-study$l2.cost)/(study$l1.cost)
		T.max.bound <- calculate.from.cost.function( what="T",
													 budget=study$budget, N=N.min.set,
													 l2.cost=study$l2.cost,
													 l1.cost=study$l1.cost )
		if( constraints$T.integer ) T.max.bound <- floor( T.max.bound )
		
		# minimal T possible by budget and N.max.set
		# T.min.bound <- (study$budget/N.max.set-study$l2.cost)/(study$l1.cost)
		T.min.bound <- calculate.from.cost.function( what="T",
													 budget=study$budget, N=N.max.set,
													 l2.cost=study$l2.cost,
													 l1.cost=study$l1.cost )
		if( constraints$T.integer ) T.min.bound <- ceiling( T.min.bound )
		
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
			cat( paste0( "   T.min.bound: "  ,T.min.bound,"\n" ) )
			flush.console()
		}

		# put into constraints list
		constraints$N.max.bound <- N.max.bound
		constraints$N.max.set <- N.max.set
		constraints$N.min.bound <- N.min.bound
		constraints$N.min.set <- N.min.set
		constraints$T.max.bound <- T.max.bound
		constraints$T.min.bound <- T.min.bound
	
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

		# return list
		list.elements <- c( "optimize", "study", "constraints", "model", "envs" )
		if( optimize$optimizer %in% "genoud" ) list.elements <- c( list.elements, "genoud" )
		ret <- eval( parse( text=paste0("list(",paste(list.elements,collapse=","),")" ) ) )
		names( ret ) <- list.elements
		
		# return
		return( ret )
}
