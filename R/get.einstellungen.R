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
get.einstellungen <- function( y.criterion ){

		# Variable fuer x Achse
		xvars <- c( "power"="b2", "budget"="b2" )
		xvar <- unname( xvars[ y.criterion ] )
		
		# Variable fuer y Achse
		yvars <- c( "power"="power", "budget"="budget" )
		yvar <- unname( yvars[ y.criterion ] )
		
		# Beschriftung x Achse
		xlabs <- c( "power"="Standardized between slope", "budget"="Standardized between slope" )
		xlab <- unname( xlabs[ y.criterion ] )
		
		# Beschriftung y Achse
		ylabs <- c( "power"="Maximum power (%)", "budget"="Minimum required budget" )
		ylab <- unname( ylabs[ y.criterion ] )
		
		list( "xvar"=xvar, "yvar"=yvar, "xlab"=xlab, "ylab"=ylab )
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
