## Changelog:
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
calc.power <- function( budget=20000, cost2=10, cost1=10, k.start=100, model, target_parameter, timeout = 60, verbose=TRUE ){
		
		require( R.utils )
		
		# environment for number of optim runs
		env <- new.env()
		assign( "n_optim_runs", 0, pos = env, inherits = FALSE, immediate = TRUE )
		
		# start time 
		start.time <- Sys.time()
		
# browser()		
		# res <- try( calc.power.( budget, cost2, cost1, icc.y, icc.x, b2, b1 ) )
		# res <- try_with_time_limit( calc.power.( budget, cost2, cost1, icc.y, icc.x, b2, b1 ), 1 )
		# res <- withTimeout( calc.power.( budget=budget, cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1 ), timeout = 1, onTimeout = "error" )
		res <- withTimeout( calc.power.( budget=budget, cost2=cost2, cost1=cost1, k.start=k.start, model, target_parameter, env, verbose=verbose ), timeout = timeout, onTimeout = "error" )
		if( !(!is.null(res) && !inherits( res, "try-error" )) ) res <- list( "optclass"=NA, "optstud"=NA, "power"=NA )
		
		# runtime
		run.time.difftime <- Sys.time() - start.time

		# in Sekunden
		run.time <- as.double( run.time.difftime, units="secs" )
		
# browser()
		# Anzahl optim runs
		n_optim_runs <- get( "n_optim_runs", pos=env )
		
		# an res ran
		res <- c( res, list( "run.time"=run.time, "n_optim_runs"=n_optim_runs ) )
		
		res
}

### development
Rdir <- "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R"
Rfiles <- list.files( Rdir, pattern="*.R" )
Rfiles <- Rfiles[ !Rfiles %in% c("calc.power.R","app.R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R","Make RAM matrices.R") ]
Rfiles <- file.path( Rdir, Rfiles )
for( Rfile in Rfiles ){
	# cat( Rfile, "\n" ); flush.console()
	source( Rfile )
	# Sys.sleep( 0.2 )
}

# example 2
model <- generate_model_example2()

res <- calc.power( k.start=200, model=model, target_parameter="arcl_eta1eta2", verbose=TRUE )




### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
