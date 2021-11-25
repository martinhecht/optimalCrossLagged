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
# foptim <- function( k,   budget=20000, cost2=100, cost1=10,   icc.y=0.2, icc.x=0.2, b2=0.5, b1=0.2,   w=1, b0=0 ){
foptim <- function( k,   budget, cost2, cost1, model, target_parameter, env, verbose=TRUE ){
		
		# n Anzahl Zeitpunkt
		# k Anzahl Personen
		# Constraint / Cost function
		n <- (budget-cost2*k)/(cost1*k)

		# var.y2 <- icc.y
		# var.y1 <- 1 - var.y2
		# var.x2 <- icc.x
		# var.x1 <- 1 - icc.x
		# cov.yx2 <- b2*var.x2
		# cov.yx1 <- b1*var.x1

		# bias.bayes.dir <- (1-w)*b0 + w*(cov.yx2/var.x2)*(1-(fcv.cov.yx2.var.x2(icc.y,icc.x,b2,b1,k,n)/(cov.yx2*var.x2))+(fv.var.x2(icc.y,icc.x,b2,b1,k,n)/(var.x2^2))) - b2
		# var.bayes.dir <- w^2*(((cov.yx2)^2/(var.x2)^2)*((fv.cov.yx2(icc.y,icc.x,b2,b1,k,n)/(cov.yx2)^2)-2*(fcv.cov.yx2.var.x2(icc.y,icc.x,b2,b1,k,n)/(cov.yx2*var.x2))+(fv.var.x2(icc.y,icc.x,b2,b1,k,n)/(var.x2^2))))

		se <- compute_se_mx( N=k,
		                     timepoints=n,
							 n_ov=model$n_ov,
							 names_ov=model$names_ov,
							 n_process=model$n_process,
							 names_process=model$names_process,
							 matrices=model$matrices,
                             target_parameters=target_parameter )
		
		# mse.bayes.dir <- bias.bayes.dir^2 + var.bayes.dir
		
		# number of optim runs
		n_optim_runs_current <- get( "n_optim_runs", pos=env )
		n_optim_runs_new <- n_optim_runs_current + 1
		assign( "n_optim_runs", n_optim_runs_new, pos = env, inherits = FALSE, immediate = TRUE )
		if ( verbose ) {
				cat( "optim run: ", n_optim_runs_new, "\n" )
				cat( "se of target parameter: ", se, "\n" )
				cat( "number of persons: ", k, "\n" )
				cat( "number of time points: ", n, "\n" )
				flush.console()
		}
		
		return( se^2 ) # Varianz statt SE, Gespräch 25.11.2021
}

### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("foptim.R","app.R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R") ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
