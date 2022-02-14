## Changelog:
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
calc.power. <- function( study, model, target.parameter, envs, verbose ){

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
			
			# MH 0.0.3 2022-01-10
			T.min <- 3
			##### TEST
			N.min <- 300
			# print( N.max <- floor( budget/((cost1+cost2)*T.min) ) )
			##### TEST
			N.max <- 333
			# console output
			if( verbose ) cat( paste0( N.max, "\n" ) ); flush.console()
			
			# genoud has no verbosity argument, manual shutoff
			# https://github.com/AnotherSamWilson/ParBayesianOptimization/issues/3
			# if( !verbose ) sink("/dev/null")
			# but it doesn't work in Windows. In Windows you can use:
			if( !verbose ) sink("nul:")
			# but that works only in Windows. A cross-OS solutions might be:
			# if( !verbose ) sink(tempfile())
			
			# start optimizer
			res.opt <- genoud( fn = foptim,
							   nvars = 1,
							   max = FALSE,
							   data.type.int = TRUE,
							   Domains = matrix( c( N.min, N.max ), 1, 2 ),
							   pop.size = 25,
							   max.generations = 10, 
							   wait.generations = 1,
							   boundary.enforcement = 2, # no trespassing
							   solution.tolerance = 0.001,
							   starting.values = round( mean(c(N.min,N.max)) ),
							   # ... foptim arguments
							   study = study,
							   model = model,
							   target.parameter = target.parameter,
							   envs = envs,
							   verbose = verbose )
			
			# end of sink
			if( !verbose ) sink()
			
			# optimal N
			N.opt. <- res.opt$par
			N.opt <- as.integer( N.opt. )
			if( !N.opt == N.opt. ) warning( "N.opt seems not to be integer" )
			
			# console output
			if( verbose ) { cat( "end of optim","\n" ); flush.console()}
			
			# post processing
			if( any( c(inherits(N.opt,"try-error") ))){
					#nothing
			} else {  
				
				## rounding not necessary
				# N.opt <- round( N.opt )
				
				# optimal T
				T.opt. <- (study$budget-study$l2.cost*N.opt)/(study$l2.cost*N.opt)
				T.opt <- as.integer( round( T.opt. ) )
				
				# console output
				if( verbose ) { cat( "fpow se call", "\n" ); flush.console() }
				
				# maximal power
				power.max <- fpow( N=N.opt,
								   T=T.opt,
								   model=model,
								   target.parameter=target.parameter,
								   cppf.env=envs$cppf.env )
		
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
		res
}

### development
# Rdir <- "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R"
# Rfiles <- list.files( Rdir, pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("calc.power..R","app.R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R","Make RAM matrices.R") ]
# Rfiles <- file.path( Rdir, Rfiles )
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
