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
# leerer Plot
gen.empty.plot <- function() {
		# "leere" Plots
		# https://stackoverflow.com/questions/19918985/r-plot-only-text
		p <- ggplot() + annotate("text", x = 4, y = 25, size=14, label = "...generating plot...", color="#84e900") + theme_void()
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
