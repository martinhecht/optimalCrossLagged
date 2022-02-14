## Changelog:
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
fpow <- function( N, T, model, target.parameter, cppf.env ){

		# get parameter values
		values <- unlist( sapply( model$matrices, "[[", "values" ) )
		names( values ) <- unlist( sapply( model$matrices, "[[", "labels" ) )
		
		# value of target parameter
		value_target.parameter <- values[ target.parameter ]

		# se of target parameter
		se_target.parameter <- compute_se_oertzen( N=N,
												   timepoints=T,
												   n_ov=model$n_ov,
												   n_process=model$n_process,
												   matrices=model$matrices,
												   cppf.env=cppf.env,
												   target.parameters=target.parameter )
		
		# power according to the formula by Kelcey et al. (2017)
		pow <- 100*(1-pnorm( qnorm(0.975) - value_target.parameter/se_target.parameter, 0, 1) + pnorm( qnorm(0.025) - value_target.parameter/se_target.parameter, 0, 1))
		
		# return
		return( pow )
}

### development
# Rdir <- "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R"
# Rfiles <- list.files( Rdir, pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("fpow.R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R","Make RAM matrices.R") ]
# Rfiles <- file.path( Rdir, Rfiles )
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
