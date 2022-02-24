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

		# handle optimize$what $direction $via $par $via.function $optimizer
		# i.e. if more than one value is given, take the first one
		optimize[c("what","direction","via","par","via.function","optimizer")] <- 
			sapply( optimize[c("what","direction","via","par","via.function","optimizer")],
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

	
		### boundaries for parameter over which optimization runs
		# either N or T
		par <- optimize$par
		oth <- c("N","T")[! c("N","T") %in% optimize$par ]

		# maximal N possible by budget and T.min
		#                  or
		# maximal T possible by budget and N.min
		eval( parse( text=paste0( par,'.max.bound <- calculate.from.cost.function( what="',par,'",
													 budget=study$budget, ',oth,'=',oth,'.min,
													 l2.cost=study$l2.cost,
													 l1.cost=study$l1.cost )' ) ) )
		# if integer is required, make integer
		eval( parse( text=paste0( 'if( constraints$',par,'.integer ) ',par,'.max.bound <- as.integer( floor( ',par,'.max.bound ) )' ) ) )
		
		# if user N.min is greater than maximal possible N.max.bound
		# warning and set N.min to N.max.bound
		#                  or
		#          [other way around]
		eval( parse( text=paste0( '
		if( ',par,'.min > ',par,'.max.bound ){
			msg <- paste0( "user-specified ',par,'.min (",',par,'.min,") is greater than maximal possible ',par,'.max.bound (",',par,'.max.bound,") determined by ',oth,'.min;\n ',par,'.min set equal to ',par,'.max.bound, this however will result in the optimized ',par,' = ',par,'.min;\n set ',par,'.min to lower value than ",',par,'.max.bound,"." )
			if( verbose ) cat( paste0( msg, "\n" ) ); flush.console()
			warning( msg )
			',par,'.min <- ',par,'.max.bound
		}'
		) ) )
		
		# set maximal N based on user input and boundaries
		#                  or
		#          [other way around]
		eval( parse( text=paste0( par,'.max.set <- min( c(',par,'.max, ',par,'.max.bound) )' ) ) )
		## => T.min has priority over N.max [or other way around]
		
		# if integer is required, make integer
		eval( parse( text=paste0( 'if( constraints$',par,'.integer ) ',par,'.max.set <- as.integer( floor( ',par,'.max.set ) )' ) ) )
		
		# minimal N allowed by budget and T.max
		#                  or
		#          [other way around]				
		eval( parse( text=paste0( par, '.min.bound <-  calculate.from.cost.function( what="',par,'",
													  budget=study$budget, ',oth,'=',oth,'.max,
													  l2.cost=study$l2.cost,
													  l1.cost=study$l1.cost )' ) ) )
		
		# if integer is required, make integer
		eval( parse( text=paste0( 'if( constraints$',par,'.integer ) ',par,'.min.bound <- as.integer( ceiling( ',par,'.min.bound ) )' ) ) )

		# set minimal N based on user input and boundaries
		#                  or
		#          [other way around]				
		eval( parse( text=paste0( par,'.min.set <- max( c(',par,'.min, ',par,'.min.bound) )' ) ) )
		## => N.min has priority over T.max [or other way around]
		
		# if integer is required, make integer
		eval( parse( text=paste0( 'if( constraints$',par,'.integer ) ',par,'.min.set <- as.integer( ceiling( ',par,'.min.set ) )' ) ) )
		
		## for checking
		# maximal T possible by budget and N.min.set
		#                  or
		#          [other way around]				
		eval( parse( text=paste0( oth, '.max.bound <- calculate.from.cost.function( what="',oth,'",
													 budget=study$budget, ',par,'=',par,'.min.set,
													 l2.cost=study$l2.cost,
													 l1.cost=study$l1.cost )' ) ) )
		
		# if integer is required, make integer				
		eval( parse( text=paste0( 'if( constraints$',oth,'.integer ) ',oth,'.max.bound <- as.integer( floor( ',oth,'.max.bound ) )' ) ) )
		
		# minimal T possible by budget and N.max.set
		#                  or
		#          [other way around]				
		eval( parse( text=paste0( oth,'.min.bound <- calculate.from.cost.function( what="',oth,'",
													 budget=study$budget, ',par,'=',par,'.max.set,
													 l2.cost=study$l2.cost,
													 l1.cost=study$l1.cost )' ) ) )

		# if integer is required, make integer
		eval( parse( text=paste0( 'if( constraints$',oth,'.integer ) ',oth,'.min.bound <- as.integer( ceiling( ',oth,'.min.bound ) )' ) ) )
		
		# console output
		if( verbose ) {
			eval( parse( text=paste0( 'cat( paste0( "',par,'.min: ",',par,'.min,"\n" ) )                       ') ) )
			eval( parse( text=paste0( 'cat( paste0( "',par,'.max: ",',par,'.max,"\n" ) )                       ') ) )
			eval( parse( text=paste0( 'cat( paste0( "',oth,'.min: ",',oth,'.min,"\n" ) )                       ') ) )
			eval( parse( text=paste0( 'cat( paste0( "',oth,'.max: ",',oth,'.max,"\n" ) )                       ') ) )
			eval( parse( text=paste0( 'cat( paste0( "',par,'.max.bound: ",',par,'.max.bound,"\n" ) )           ') ) )
			eval( parse( text=paste0( 'cat( paste0( "',par,'.max.set: "  ,',par,'.max.set,"\n" ) )             ') ) )
			eval( parse( text=paste0( 'cat( paste0( "',par,'.min.bound: ",',par,'.min.bound,"\n" ) )           ') ) )
			eval( parse( text=paste0( 'cat( paste0( "',par,'.min.set: "  ,',par,'.min.set,"\n" ) )             ') ) )
			eval( parse( text=paste0( 'cat( paste0( "   Note:  ',oth,'.min has priority over ',par,'.max\n" ) )') ) )
			eval( parse( text=paste0( 'cat( paste0( "          ',par,'.min has priority over ',oth,'.max\n" ) )') ) )
			eval( parse( text=paste0( 'cat( paste0( "   fyi:\n" ) )                                ') ) )
			eval( parse( text=paste0( 'cat( paste0( "   ',oth,'.max.bound: "  ,',oth,'.max.bound,"\n" ) )      ') ) )
			eval( parse( text=paste0( 'cat( paste0( "   ',oth,'.min.bound: "  ,',oth,'.min.bound,"\n" ) )      ') ) )
			flush.console()
		}

		# put into constraints list
		eval( parse( text=paste0( 'constraints$',par,'.max.bound <- ',par,'.max.bound ') ) )
		eval( parse( text=paste0( 'constraints$',par,'.max.set   <- ',par,'.max.set   ') ) )
		eval( parse( text=paste0( 'constraints$',par,'.min.bound <- ',par,'.min.bound ') ) )
		eval( parse( text=paste0( 'constraints$',par,'.min.set   <- ',par,'.min.set   ') ) )
		eval( parse( text=paste0( 'constraints$',oth,'.max.bound <- ',oth,'.max.bound ') ) )
		eval( parse( text=paste0( 'constraints$',oth,'.min.bound <- ',oth,'.min.bound ') ) )
		
		## starting value for N
		# if character, try to evaluate
		if( is.character( optimize$starting.values ) ){
			# substitute "par" for par
			optimize$starting.values <- gsub( "par", par, optimize$starting.values )
			# save equation for documentation
			optimize$starting.values.equation <- optimize$starting.values
			# calculate starting values
			optimize$starting.values <- eval(parse(text=optimize$starting.values))
		}
		# if integer is required, make integer
		eval( parse( text=paste0( 'if( constraints$',par,'.integer ) optimize$starting.values <- as.integer( optimize$starting.values )' ) ) )

		
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