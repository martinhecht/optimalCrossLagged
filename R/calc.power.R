## Changelog:
# MH 0.0.3 2022-01-10
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
calc.power <- function( study=list( "budget"=20000, "l2.cost"=10,"l1.cost"=10),
						model, target.parameter, timeout = 60, verbose=TRUE ){
		
		# packages
		pkgs <- "require( R.utils ); # withTimeout()
				 require( rgenoud ); # genoud()
				 require( Rcpp );
				 require( RcppArmadillo )"
		
		# suppress package loading outputs or not
		if( !verbose ) {
			suppressMessages( eval( parse( text=pkgs ) ) )
		} else {
			eval( parse( text=pkgs ) )
		}
		
		# MH 0.0.3 2022-01-10
		# packages & function definitions based on RcppArmadillo
		# https://stackoverflow.com/questions/31671026/how-to-write-rcpp-
		#                        function-for-simple-matrix-multiplication-in-r
		# https://scholar.princeton.edu/sites/default/files/q-aps/files/
		#                                                    slides_day4_am.pdf
		
		# console output
		# if( verbose ) {
			# cat( "defining Rcpp functions\n" )
			# flush.console()
		# }
		
		# environment for cpp functions
		# cppf.env <- new.env()
		
		# start time cppf
		# start.time.cppf <- Sys.time()		

		# definitions of Rcpp functions
		# cppFunction("arma::mat mm(arma::mat A, arma::mat B) { return A * B; }",
										# depends="RcppArmadillo", env=cppf.env)
		# cppFunction("arma::mat mmm(arma::mat A, arma::mat B, arma::mat C)
				# { return A * B * C; }", depends="RcppArmadillo", env=cppf.env)
		# cppFunction("arma::mat mmmm(arma::mat A, arma::mat B, arma::mat C,
									# arma::mat D) { return A * B * C * D; }",
									# depends="RcppArmadillo", env=cppf.env)
		# cppFunction("arma::mat minv(arma::mat A) { return inv(A); }",
										# depends="RcppArmadillo", env=cppf.env)
		
		# runtime in seconds
		# run.time.cppf.difftime <- Sys.time() - start.time.cppf
		# run.time.cppf <- as.double( run.time.cppf.difftime, units="secs" )		
		
		# console output
		# if( verbose ) {
			# cat("end of defining Rcpp functions, run time: ",run.time.cppf,
															# " seconds", "\n" )
			# flush.console()
		# }	
		
		# environment for statistics of optimizer (e.g., number of optim runs)
		optmz.env <- new.env()
		assign( "n.optim.runs", 0, pos = optmz.env,
											  inherits = FALSE, immediate=TRUE)
		
		# environments lists
		# envs <- list( optmz.env, cppf.env )
		envs <- list( optmz.env )
		# names( envs ) <- c( "optmz.env", "cppf.env" )
		names( envs ) <- c( "optmz.env" )
		
		# start time 
		start.time.calc.power. <- Sys.time()
		
		# call calc.power.() with timeout
		res <- withTimeout( try( calc.power.( study=study,
										 model=model,
										 target.parameter=target.parameter,
										 envs=envs,
										 verbose=verbose ) ),
										 timeout = timeout,
										 onTimeout = "error" )
		
		# if timeouted or error, results are NA
		if( !(!is.null(res) && !inherits( res, "try-error" )) ) 
						  res <- list( "N.opt"=NA, "T.opt"=NA, "power.max"=NA )
		
		# runtime in seconds
		run.time.calc.power..difftime <- Sys.time() - start.time.calc.power.
		run.time.calc.power. <- as.double( run.time.calc.power..difftime, units="secs" )
		
		# number of optimizer runs
		n.optim.runs <- get( "n.optim.runs", pos=envs$optmz.env )
		
		# add optimizer runs to results list
		res <- c( res, list( 
							 # "run.time.cppf"=run.time.cppf,
							 "run.time.calc.power."=run.time.calc.power.,
							 "n.optim.runs"=n.optim.runs ) )
		
		# return
		return( res )
}

### development
# optimalclpm needs to be loaded for compiled C++ functions
# else they are defined in compute_se_oertzen()
# library( optimalclpm ); mm( matrix(1:4,2,2), matrix(1:4,2,2) )

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/84_optimalclpm/04_martinhecht/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("calc.power.R","RcppExports.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }


# example 2
# model <- generate_model_example2()

# res <- calc.power( model=model,target.parameter="arcl_eta1eta2",verbose=TRUE)


# print( res )
# str( res )






### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
