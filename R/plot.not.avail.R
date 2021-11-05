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
# Plot not available
plot.not.avail <- function() {
		p <- ggplot() + annotate("text", x = 4, y = 25, size=8, label = "Plot not available") + theme_void()
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
