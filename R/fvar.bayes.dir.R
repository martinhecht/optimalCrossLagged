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
fvar.bayes.dir <- function( icc.y, icc.x, b2, b1, k, n, w, b0 ){
		var.y2 <- icc.y
		var.y1 <- 1 - var.y2
		var.x2 <- icc.x
		var.x1 <- 1 - icc.x
		cov.yx2 <- b2*var.x2
		cov.yx1 <- b1*var.x1

		bias.bayes.dir <- (1-w)*b0 + w*(cov.yx2/var.x2)*(1-(fcv.cov.yx2.var.x2(icc.y,icc.x,b2,b1,k,n)/(cov.yx2*var.x2))+(fv.var.x2(icc.y,icc.x,b2,b1,k,n)/(var.x2^2))) - b2
		var.bayes.dir <- w^2*(((cov.yx2)^2/(var.x2)^2)*((fv.cov.yx2(icc.y,icc.x,b2,b1,k,n)/(cov.yx2)^2)-2*(fcv.cov.yx2.var.x2(icc.y,icc.x,b2,b1,k,n)/(cov.yx2*var.x2))+(fv.var.x2(icc.y,icc.x,b2,b1,k,n)/(var.x2^2))))
		mse.bayes.dir <- bias.bayes.dir^2 + var.bayes.dir
		return( var.bayes.dir ) #!!!!!!!!!
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
