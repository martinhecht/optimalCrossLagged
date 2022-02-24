## Changelog:
# MH 0.0.7 2022-01-20
# MH 0.0.6 2022-01-20
# MH 0.0.5 2022-01-19: renamed maximize.power to optmze
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
optmze <- function( optimize=list(	"what"=c("power"),
									"direction"=c("max","min"),
									"via"=c("se","se^2","power"),
									"par"=c("T","N"),
									"via.function"=c("compute.se.oertzen"),
									"optimizer"=c("genoud"),
									"starting.values"="round(mean(c(par.min.set,par.max.set)))",
									"set.seed.value"="random"
									),
					study=list("budget"=20000, "l2.cost"=10, "l1.cost"=10),
					constraints=list("T.min"=3, "T.max"=10, "N.min"=3, "N.max"=300,
									 "T.integer"=TRUE,
									 "N.integer"=FALSE ),
					model=list("class"=c("SEM"), "name"=NULL,
								specification=NULL, target.parameters=NULL),
					genoud=list("pop.size"=20,"max.generations"=10,"wait.generations"=1,
								"boundary.enforcement"=2,"solution.tolerance"=0.001	),
					timeout=60, verbose=TRUE ){
		
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

		# prepare input
		input <- prepare.input( optimize=optimize,
								study=study,
								constraints=constraints,
								model=model,
								genoud=genoud,
								verbose=verbose )

		# optimize with timeout (or not)
		wt <- !is.na( timeout ) && !is.null( timeout ) && is.numeric( timeout ) && timeout > 0 

		# start time 
		start.time.optimizer <- Sys.time()

		# call kickstart.optimizer
		res <- eval( parse( text=paste0( ifelse(wt,'withTimeout(',""),
													'try(kickstart.optimizer(
														 input=input,
														 verbose=verbose ) )',
														 ifelse(wt,',
														 timeout = timeout,
														 onTimeout = "error" )', "" ) ) ) )

		# runtime in seconds
		run.time.optimizer.difftime <- Sys.time() - start.time.optimizer
		run.time.optimizer.secs <- as.double( run.time.optimizer.difftime, units="secs" )
		
		# prepare results
		results <- prepare.results( res=res,
									run.time.optimizer.secs=run.time.optimizer.secs,
									input=input,
									verbose=verbose )
		
		# return
		return( results )
}

### development
# optimalclpm needs to be loaded for compiled C++ functions
# else they are defined locally in compute.se.oertzen()
# library( optimalclpm ); mm( matrix(1:4,2,2), matrix(1:4,2,2) )

# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    # "Dropbox/84_optimalclpm/04_martinhecht/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("optmze.R","RcppExports.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }


# example 2
# specs <- generate.model.example.2()
# specs$N <- NULL
# specs$timepoints <- NULL
# specs$names_ov <- NULL
# specs$names_process <- NULL


# res <- optmze( model=list("class"=c("SEM"),
						  # "name"=NULL,
					      # "specification"=specs,
						  # "target.parameters"=c("arcl_eta1eta2","arcl_eta2eta1")),
						  # verbose=TRUE )


# print( res )
# str( res )






### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
