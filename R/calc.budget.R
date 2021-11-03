## Changelog:
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
calc.budget <- function( power, cost2, cost1, icc.y, icc.x, b2, b1 ){
		res <- withTimeout( calc.budget.( power=power, cost2=cost2, cost1=cost1, icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1 ), timeout = 0.5, onTimeout = "error" )
		if( !(!is.null(res) && !inherits( res, "try-error" )) ) res <- list( "optclass"=NA, "optstud"=NA, "budget"=NA )
		res
}

### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/68_causalSEM/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% "plot_density.R" ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
