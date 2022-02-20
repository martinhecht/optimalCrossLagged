## Changelog:
# MH 0.0.7 2022-01-20
# MH 0.0.6 2022-01-20
# MH 0.0.4 2022-01-15: renamed fpow to calculate.power
# MH 0.0.2 2021-11-25: update
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
calculate.power <- function( N, T, model,
									se.target.parameters=NULL,
									se.function="compute.se.oertzen",
									# , cppf.env
									verbose=TRUE	){
# browser()

		# get parameter values
		values <- unlist( sapply( model$specification$matrices, "[[", "values" ) )
		names( values ) <- unlist( sapply( model$specification$matrices, "[[", "labels" ) )
		
		# value of target parameter
		value.target.parameters <- values[ model$target.parameters ]

		# se of target parameter
		if( is.null( se.target.parameters ) ){
			if( se.function %in% "compute.se.oertzen" ){
			
				se.target.parameters <- compute.se.oertzen(	N=round(N),
														timepoints=round(T),
														n_ov=model$n_ov,
														n_process=model$n_process,
														matrices=model$specification$matrices,
														# cppf.env=cppf.env,
														target.parameters=model$target.parameters,
														verbose=verbose )
			} else {
				se.target.parameters <- rep( NA, length( target.parameters ) )
			}
		}
		
		# for safety, sort (again)
		if( !is.null( names( se.target.parameters ) ) ) se.target.parameters <- se.target.parameters[ model$target.parameters ]
		
		# power according to the formula by Kelcey et al. (2017)
		power <- 100*(1-pnorm( qnorm(0.975) - value.target.parameters/se.target.parameters, 0, 1) + pnorm( qnorm(0.025) - value.target.parameters/se.target.parameters, 0, 1))
		
		# return
		return( power )
}
