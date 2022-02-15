## Changelog:
# MH 0.0.4 2022-01-15: renamed calc.power to maximize.power
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

## Function definition
maximize.power <- function( study=list("budget"=20000, "l2.cost"=10,"l1.cost"=10),
							constraints=list("N.min"=200, "N.max"=300, "T.min"=3, "T.max"=300),
							genoud=list("pop.size"=20,"max.generations"=10,"wait.generations"=1,
										"boundary.enforcement"=2,"solution.tolerance"=0.001,
										"starting.values"="round(mean(c(N.min.set,N.max.set)))"),
							model, target.parameters=NULL, timeout=60, verbose=TRUE ){
		
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
		

		## target parameters
		all.tp.named <- do.call( "c", sapply( model$matrices, "[[", "labels" ))
		all.tp.named <- all.tp.named[!is.na( all.tp.named )]
		all.tp <- unname( all.tp.named )
		
		# set all if target.parameters is NULL
		if( is.null( target.parameters ) ){
			target.parameters <- all.tp
		}
		
		# check whether all specified target parameters are in the model object
		tp.log <- target.parameters %in% all.tp
		
		# if none specified parameter is valid then set all
		if( !any( tp.log ) ){
			msg <- "none of the specified target parameters are parameters in the model object;\n all model parameters are used."
			if( verbose ) cat( paste0( msg, "\n" ) ); flush.console()
			warning( msg )
			target.parameters <- all.tp
		}
		
		# if some are wrongly specified, kick them out
		if( !all( tp.log ) ){
			msg <- paste0( "the following target parameters are not in the model object and are discarded: ", paste( target.parameters[ !tp.log ], collapse=", " ) )
			if( verbose ) cat( paste0( msg, "\n" ) ); flush.console()
			warning( msg )
			target.parameters <- target.parameters[ tp.log ]
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
		start.time.optimizer <- Sys.time()

		# call maximize.power.() with timeout
		if( !is.na( timeout ) && !is.null( timeout ) && is.numeric( timeout ) && timeout > 0 ){
			res <- withTimeout(try(kickstart.optimizer( study=study,
														constraints=constraints,
														genoud=genoud,
														model=model,
														target.parameters=target.parameters,
														envs=envs,
														verbose=verbose ) ),
														timeout = timeout,
														onTimeout = "error" )
		} else {
			res <- 			   try(kickstart.optimizer( study=study,
														constraints=constraints,
														genoud=genoud,
														model=model,
														target.parameters=target.parameters,
														envs=envs,
														verbose=verbose ) )
		}
		
		# if timeouted or error, results are NA
		if( !(!is.null(res) && !inherits( res, "try-error" )) ) 
						  res <- list( "N.opt"=NA, "T.opt"=NA, "power.max"=NA )
		
		# runtime in seconds
		run.time.optimizer.difftime <- Sys.time() - start.time.optimizer
		run.time.optimizer <- as.double( run.time.optimizer.difftime, units="secs" )
		
		# number of optimizer runs
		n.optim.runs <- get( "n.optim.runs", pos=envs$optmz.env )
		
		# add optimizer runs to results list
		res <- c( res, list( 
							 # "run.time.cppf"=run.time.cppf,
							 "run.time.optimizer"=run.time.optimizer,
							 "n.optim.runs"=n.optim.runs ) )
		
		# return
		return( res )
}

### development
# optimalclpm needs to be loaded for compiled C++ functions
# else they are defined locally in compute.se.oertzen()
# library( optimalclpm ); mm( matrix(1:4,2,2), matrix(1:4,2,2) )

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/84_optimalclpm/04_martinhecht/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("maximize.power.R","RcppExports.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }


# example 2
# model <- generate.model.example.2()

# res <- maximize.power( model=model,target.parameters="arcl_eta1eta2",timeout=6000,verbose=TRUE)
# res <- maximize.power( model=model,timeout=6000,verbose=TRUE)
# res <- maximize.power( model=model,target.parameters=c("arcl_eta1eta2","arcl_eta2eta1"),verbose=TRUE)

# print( res )
# str( res )






### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
