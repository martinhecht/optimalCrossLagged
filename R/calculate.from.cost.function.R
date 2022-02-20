## Changelog:
# MH 0.0.7 2022-01-20: renamed calculate.T to calculate.from.cost.function
# MH 0.0.1 2022-01-20: initial programming

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
calculate.from.cost.function <- function( what=c("budget","N","T"), budget=NA, N=NA, T=NA, l2.cost=NA, l1.cost=NA ){
		
		# get first argument if what is vector
		if( length( what ) > 1 ) what[1] else what
		
		# equations
		eqn <- c(	"budget" = "N * ( l1.cost * T + l2.cost )",
					"N"     =  "budget / ( l1.cost * T + l2.cost )",
					"T"     =  "( budget/N - l2.cost ) / l1.cost"
				)
		
		# evaluate
		res <- try( eval( parse ( text=eqn[what] ) ) )
		
		# if error, NA
		if( inherits( res, "try-error" ) ) res <- as.numeric( NA )

		# return
		return( res )
}
