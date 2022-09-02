## Changelog:
# MH 0.0.30 2022-09-02: modification for stability checks
# MH 0.0.11 2022-04-25: disabled seed values
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
		if( verbose ) { cat( "seed: ",.Random.seed,"\n" ); flush.console() }
		# cat( "seed: ",.Random.seed,"\n" ); flush.console()
		
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

			# number of optimization values (length of return vector of objective function)
			# in genoud: argument "lexical"
			# for power optimization: number of target parameters (for each parameter the power)
			# for budget optimization: 1
			if( ( optimize$what %in% c("power","target.power") ) && ( (ltp <- length( model$target.parameters ) ) > 1 ) ){
					lexical <- ltp }
			# else if( optimize$what %in% c("budget") ){
					# lexical <- length( model$target.parameters ) + 1 }
			else{
					lexical <- FALSE
			}

			# MH 0.0.30 2022-09-02 stability.check / runs
			# empty result list
			res.opt.l <- sapply( 1:input$runs, function(x) NULL, simplify=FALSE )
			
			# run optimizer input$runs times
			for( i in 1:input$runs ){
					if( verbose ) cat( paste0( "=====================\n optimizer run: ", i, "/", input$runs, "\n =====================\n" ) )
			
					# call genoud optimizer
					res.opt.l[[i]] <- try( 
					# res.opt <- withr::with_seed( optimize$set.seed.value,
									genoud( fn = fn,
									   nvars = 1,
									   lexical = lexical,
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
									   # unif.seed = sample( 1:999999, 1 ),
									   # int.seed =  sample( 1:999999, 1 ),
									   # ... fn arguments
									   optimize = optimize,
									   study = study,
									   constraints = constraints,
									   model = model,
									   genoud = genoud,
									   envs = envs,
									   timeout = timeout,
									   verbose = verbose ) )

					# after each run, environment for buffering F_diff values need to be emptied
					rm(list = ls( env=envs$pwrLRT.env ), envir=envs$pwrLRT.env)
					
			}
			
			# MH 0.0.30 2022-09-02
			# try error vector of runs
			tryerror <- sapply( res.opt.l, function(x) inherits(x,"try-error") )
			
			# MH 0.0.30 2022-09-02
			# first run must not have error
			# if( c(inherits(res.opt,"try-error") ) ){
			if( tryerror[1] ){
				par.opt <- as.numeric(NA)
			} else {
				# optimal N/T
				# par.opt <- res.opt$par
				# MH 0.0.30 2022-09-02
				par.opt <- res.opt.l[[1]]$par
				# optimal value
				# values.opt <- res.opt$value
				# MH 0.0.30 2022-09-02
				values.opt <- res.opt.l[[1]]$value
				# name(s) of optimal value(s)
				
				if( optimize$what %in% "power" )        names( values.opt ) <- model$target.parameters
			    if( optimize$what %in% "budget" )       names( values.opt ) <- "budget"
			    if( optimize$what %in% "target.power" ) names( values.opt ) <- paste0( "quadraticLoss.", model$target.parameters )
			}
			
			# MH 0.0.30 2022-09-02
			# if stability check, check if there is variance in the solutions 
			# CURRENTLY ONLY FOR 1 optimization parameter
			# optimal parameter values of runs
			if( input$stability.check && input$runs >= 2 && all( !tryerror ) ){
				par.opts <- sapply( res.opt.l, "[[", "par" )
				stable.solution <- !as.logical( sd( par.opts ) )
			} else {
				par.opts <- as.numeric(NA)
				stable.solution <- as.logical(NA)
			}
		}
		
		# end of sink
		if( !verbose ) sink()
		
		# console output
		if( verbose ) { cat( "end of optimizing","\n" ); flush.console()}

		# return list
		# MH 0.0.30 2022-09-02
		# res <- list( "par.opt"=par.opt, "values.opt"=values.opt )
		res <- list( "par.opt"=par.opt, "values.opt"=values.opt, "par.opts"=par.opts, "stable.solution"=stable.solution )
		
		# return
		return( res )
}
