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
# aktuellen Plot mit "generating plot" ueberlagern
annotate.plot <- function(p,y.criterion,budget) {
		x <- 0
		if( y.criterion %in% "power" ) y <- 50
		if( y.criterion %in% "budget" )   y <- budget
		# wenn plot not available
		if( identical( p$theme, plot.not.avail()$theme ) ) { y <-  25; x <- 4 }
		p <- p + annotate("text", x = x, y = y, size=13, label = "...generating new plot...", color="#84e900")
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
