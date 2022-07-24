## Changelog:
# MA/MH 0.0.21 2022-07-24: adapted for new "example3" structure
# MH 0.0.20 2022-07-04: alpha as input implemented, constraints are outputted
# MH 0.0.19 2022-07-04: budget/target.power optimization implemented
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
optmze <- function( optimize=list(	"what"=c("power","budget","target.power"),
									"direction"=c("max","min"),
									"via"=c("power","se","se^2"),
									"par"=c("T","N"),
									"via.function"=c("calculate.power.LRT","compute.se.oertzen"),
									"optimizer"=c("genoud"),
									"starting.values"="round(mean(c(par.min.set,par.max.set)))",
									"set.seed.value"="random"
									),
					study=list("budget"=20000, "target.power"=0.80, "l2.cost"=10, "l1.cost"=10, alpha=0.05, T=NULL),
					constraints=list("T.min"=3, "T.max"=10, "N.min"=3, "N.max"=300,
									 "T.integer"=TRUE,
									 "N.integer"=FALSE ),
					model=list("specification"=NULL, "target.parameters"=NULL, "target.parameters.values.H0"=NULL ),
					genoud=list("pop.size"=20,"max.generations"=100,"wait.generations"=1,
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
								timeout=timeout,
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
# installed.packages()["optimalclpm",]["Version"]


# user.profile <- shell( "echo %USERPROFILE%", intern=TRUE )
# Rfiles.folder <- file.path( user.profile,
                                    ## "Dropbox/84_optimalclpm/04_martinhecht/R" )
                                    # "Dropbox/84_optimalclpm/04b_martinhecht/optimalCrossLagged/R" )
# Rfiles <- list.files( Rfiles.folder , pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("optmze.R","RcppExports.R","Examples with Different Inputs.R","Try to Optimize.R") ]
# for( Rfile in Rfiles ){
	# source( file.path( Rfiles.folder, Rfile ) )
# }


# specs <- generate.model.example.2()
# specs$N <- NULL
# specs$timepoints <- NULL
# specs$names_ov <- NULL
# specs$names_process <- NULL

# specs <- generate.model.example.3()


# while(TRUE){
# res <- optmze( model=list("specification"=specs,
						  # "target.parameters"=c("ARCL_2_1", "ARCL_1_2"),
						  # "target.parameters.values.H0"=rep(0,2)),
						  ### "target.parameters"=c("arcl_eta1eta2")),
						  # study=list("budget"=20000, "target.power"=0.80, "l2.cost"=10, "l1.cost"=10, alpha=0.05, T=8 ),
						  # optimize=list(
									# "what"=c("power"),
									# "what"=c("budget"),
									# "what"=c("target.power"),
									# "direction"=c("max"),
									# "direction"=c("min"),
									# "via"=c("power"),
									# "par"=c("T"),
									# "par"=c("N"),
									# "via.function"=c("calculate.power.LRT"),
									# "optimizer"=c("genoud"),
									# "starting.values"="round(mean(c(par.min.set,par.max.set)))",
									# "set.seed.value"="random"
									# ),
							### constraints
							# constraints=list("T.min"=3, "T.max"=40, "N.min"=3, "N.max"=1000,
											# "T.integer"=TRUE,
											# "N.integer"=FALSE ),									
						  # genoud=list("pop.size"=16,"max.generations"=100,"wait.generations"=1,
						  			  # "boundary.enforcement"=2,"solution.tolerance"=0.001),
						  # verbose=TRUE )

# str( res ); flush.console()
# }





### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
