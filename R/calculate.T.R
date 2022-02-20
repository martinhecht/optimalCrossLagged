## Changelog:
# MH 0.0.1 2022-01-20: initial programming

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
calculate.T <- function( budget, l2.cost, l1.cost, N ){
		
		T <- try( ( budget-l2.cost*N )/ ( l1.cost*N ) )
		if( inherits( T, "try-error" ) ) { T <- as.numeric( NA ) }

		# return
		return( T )
}
