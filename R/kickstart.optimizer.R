## Changelog:
# MH 0.0.7 2022-01-20
# MH 0.0.6 2022-01-20
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
kickstart.optimizer <- function( input, verbose=TRUE ){

		# put elements from input list on this environment
		do <- paste0( names( input ), " <- input$", names( input ) )
		eval( parse( text=do ) )

		# console output
		if( verbose ) { cat( "starting optimization","\n" ); flush.console() }

		# set seed
		set.seed( optimize$set.seed.value )
		
		# for the case that the optimizer has no verbosity
		# argument (as genoud), manual shutoff
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
		
		# start optimizer
		if( optimize$optimizer %in% "genoud" ){
			
			# direction
			max <- ifelse( optimize$direction.optimizer %in% "max", TRUE, FALSE )

			# data.type.int
			data.type.int <- eval( parse( text=paste0('constraints$',optimize$par,'.integer' ) ) )
			
			# Domains
			Domains <- eval( parse( text=paste0('matrix( c( as.numeric(constraints$',optimize$par,'.min.set), as.numeric(constraints$',optimize$par,'.max.set) ), 1, 2 )' ) ) )

			# call genoud optimizer
			res.opt <- try( genoud( fn = fn,
							   nvars = 1,
							   lexical = length( model$target.parameters ),
							   max = max,
							   data.type.int = data.type.int,
							   # seems to be bug/inconsistent, although when data.type.int==TRUE, Domains need to be numeric (not integer)
							   Domains = Domains,
							   pop.size = genoud$pop.size,
							   max.generations = genoud$max.generations, 
							   wait.generations = genoud$wait.generations,
							   boundary.enforcement = genoud$boundary.enforcement,
							   solution.tolerance = genoud$solution.tolerance,
							   starting.values = optimize$starting.values,
							   # ... fn arguments
							   optimize = optimize,
							   study = study,
							   constraints = constraints,
							   model = model,
							   envs = envs,
							   verbose = verbose ) )
			if( c(inherits(res.opt,"try-error") ) ){
				par.opt <- as.numeric(NA)
			} else {
				# optimal N
				par.opt <- res.opt$par
				# optimal value
				values.opt <- res.opt$value
				names( values.opt ) <- model$target.parameters
			}
		}
		
		# end of sink
		if( !verbose ) sink()
		
		# console output
		if( verbose ) { cat( "end of optimizing","\n" ); flush.console()}

		# return list
		res <- list( "par.opt"=par.opt, "values.opt"=values.opt )
		
		# return
		return( res )
}
