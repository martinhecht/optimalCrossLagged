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
calc.power <- function( budget=20000, cost2=100, cost1=10, model, target_parameter ){
		
		require( R.utils )
		
		# environment for number of optim runs
		env <- new.env()
		assign( "n_optim_runs", 0, pos = env, inherits = FALSE, immediate = TRUE )
		
# browser()		
		# res <- try( calc.power.( budget, cost2, cost1, icc.y, icc.x, b2, b1 ) )
		# res <- try_with_time_limit( calc.power.( budget, cost2, cost1, icc.y, icc.x, b2, b1 ), 1 )
		# res <- withTimeout( calc.power.( budget=budget, cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1 ), timeout = 1, onTimeout = "error" )
		res <- withTimeout( calc.power.( budget=budget, cost2=cost2, cost1=cost1, model, target_parameter, env ), timeout = 100000, onTimeout = "error" )
		if( !(!is.null(res) && !inherits( res, "try-error" )) ) res <- list( "optclass"=NA, "optstud"=NA, "power"=NA )
		res
}

### development
Rfiles <- list.files( "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R", pattern="*.R", include.dirs = TRUE )
Rfiles <- Rfiles[ !Rfiles %in% c("calc.power.R","app.R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R") ]
for( Rfile in Rfiles ){
	source( Rfile )
}

# example 2
model <- generate_model_example2()

res <- calc.power( model=model, target_parameter="arcl_eta1eta2" )




### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
