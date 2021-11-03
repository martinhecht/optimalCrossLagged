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
fpow <- function( icc.y, icc.x, b2, b1, k, n, w, b0 ){
		# Power according to the formula by Kelcey et al. (2017)
		pow <- 100*(1-pnorm( (1.96 - b2/sqrt(fvar.bayes.dir( icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1, k=k, n=n, w=w, b0=b0 ))), 0, 1) + pnorm( (-1.96 - b2/sqrt(fvar.bayes.dir( icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1, k=k, n=n, w=w, b0=b0 ))), 0, 1))
		return( pow )
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
