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
fv.var.x2 <- function( icc.y, icc.x, b2, b1, k, n ){
		var.y2 <- icc.y
		var.y1 <- 1 - var.y2
		var.x2 <- icc.x
		var.x1 <- 1 - icc.x
		cov.yx2 <- b2*var.x2
		cov.yx1 <- b1*var.x1
		v.var.x2 <- ((2*(n*var.x2+var.x1)^2)/(k-1)+(2*var.x1^2)/((n-1)*k))/(n^2)
		return( v.var.x2 )
}

### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% "plot_density.R" ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
