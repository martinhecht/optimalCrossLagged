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
# fpow <- function( icc.y, icc.x, b2, b1, k, n, w, b0 ){
fpow <- function( k, n, model, target_parameter, verbose=TRUE ){
		# Power according to the formula by Kelcey et al. (2017)
		# pow <- 100*(1-pnorm( (1.96 - b2/sqrt(fvar.bayes.dir( icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1, k=k, n=n, w=w, b0=b0 ))), 0, 1) + pnorm( (-1.96 - b2/sqrt(fvar.bayes.dir( icc.y=icc.y, icc.x=icc.x, b2=b2, b1=b1, k=k, n=n, w=w, b0=b0 ))), 0, 1))
# browser()
		# get parameter values
		values <- unlist( sapply( model$matrices, "[[", "values" ) )
		names( values ) <- unlist( sapply( model$matrices, "[[", "labels" ) )
		
		# value of target parameter
		value_target_parameter <- values[ target_parameter ]

		# se of target parameter
		# se_target_parameter <- compute_se_mx( N=k,
											  # timepoints=n,
											  # n_ov=model$n_ov,
											  # names_ov=model$names_ov,
											  # n_process=model$n_process,
											  # names_process=model$names_process,
											  # matrices=model$matrices,
											  # target_parameters=target_parameter )
		
		if( verbose ) { cat( "fpow se call", "\n" ); flush.console() }
# browser()		
		se_target_parameter <- compute_se_oertzen( N=k,
												   timepoints=n,
												   n_ov=model$n_ov,
												   n_process=model$n_process,
												   matrices=model$matrices,
												   target_parameters=target_parameter )
		
		pow <- 100*(1-pnorm( 1.96 - value_target_parameter/se_target_parameter, 0, 1) + pnorm( -1.96 - value_target_parameter/se_target_parameter, 0, 1))
		
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
