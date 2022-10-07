## Changelog:
# MH 0.0.35 2022-10-05: new package required: here
# MH 0.0.34 2022-09-15: new argument: timeout.log.data
#						changed default of log.data from FALSE to TRUE
#                       new results list entries: log.data.status (character) ..."ok", "error" or "error or timed out"
#												  logid (integer) ... id of entry in db
#												  run.time.log.data.secs (numeric) ... time in seconds for logging data
# MH 0.0.33 2022-09-12: added comments on max nchar (corresponding to db) for some input variables
# MH 0.0.32 2022-09-11: development of logging data, new argument log.data (current default: FALSE)
# MH 0.0.31 2022-09-05: added argument pop.size.max in genoud list
#                       error code 13 (warning) is thrown if pop.size>=pop.size.max
# MH 0.0.30 2022-09-02:
#    added stability checks
#    new function parameters: stability.check (default: TRUE) ... whether stability check is performed
#                             runs (default: 1 if stability.check==FALSE, 2 if stability.check==TRUE ) ... number of optimizer runs
#    new elements of output list: par.opts ... vector of optimized parameter values of runs
#                                 stable.solution (logical) ... TRUE if no variance over optimized parameter values
#                                 par.opts/stable solution is NA if runs==1 or stability.check==FALSE
# MA 0.0.25 2022-07-28: added plausability checks of inputs
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
optmze <- function( optimize=list(	"what"=c("power","budget","target.power"), # max nchar: 12
									"direction"=c("max","min"),                # max nchar: 3
									"via"=c("power","se","se^2"),			   # max nchar: 5
									"par"=c("T","N"),						   # nchar: 1
									"via.function"=c("calculate.power.LRT","compute.se.oertzen"), # max nchar: 19
									"optimizer"=c("genoud"),				   # max nchar: 6
									"starting.values"="round(mean(c(par.min.set,par.max.set)))", # max nchar: 50
									"set.seed.value"="random"				   # max nchar: 10
									),
					study=list("budget"=20000, "target.power"=0.80, "l2.cost"=10, "l1.cost"=10, alpha=0.05, T=NULL),
					constraints=list("T.min"=3, "T.max"=10, "N.min"=3, "N.max"=300,
									 "T.integer"=TRUE,
									 "N.integer"=FALSE ),
					model=list("specification"=NULL, "target.parameters"=NULL, # max nchar: 50
							   "target.parameters.values.H0"=NULL ),
					genoud=list("pop.size"=20,
								# MH 0.0.31 2022-09-05: pop.size.max
								"pop.size.max"=1000,
								"max.generations"=100,"wait.generations"=1,
								"boundary.enforcement"=2,"solution.tolerance"=0.001	),
								# MH 0.0.30 2022-09-02 new argument stability.check
								timeout=60, stability.check=TRUE, runs=ifelse(stability.check,2,1), 
								log.data=TRUE, # MH 0.0.32 2022-09-11: log.data
											   # MH 0.0.34 2022-09-15: changed default of log.data from FALSE to TRUE
								timeout.log.data=5, # MH 0.0.34 2022-09-15
								verbose=TRUE ){
  
  # MA 0.0.25 2022-07-28: added plausability checks of inputs
  # plausability checks
  error_codes <- check_plausability(constraints = constraints, model = model)
  
  # if errors are detected: return output with error codes
  if(length(  error_codes[  error_type(error_codes) %in%  "error" ] ) > 0) {
  # if (length(error_codes) > 0) {
    return(make_output(error_codes = error_codes))
  }

		# packages
		pkgs <- "require( R.utils ); # withTimeout()
				 require( rgenoud ); # genoud()
				 require( Rcpp );
				 require( RcppArmadillo );
				 require( RMariaDB ); # MH 0.0.32 2022-09-11
				 require( here ) # MH 0.0.35 2022-10-05
				"   
		
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
								timeout.log.data=timeout.log.data,
								# MH 0.0.30 2022-09-02 new argument stability.check
								stability.check=stability.check,
								runs=runs,
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
									verbose=verbose,
									error_codes = error_codes)
		
		# MH 0.0.32 2022-09-11, log data
		if( log.data ){
			
				# MH 0.0.34 2022-09-15
				# with timeout (or not)
				wt2 <- !is.na( timeout.log.data ) && !is.null( timeout.log.data ) && is.numeric( timeout.log.data ) && timeout.log.data > 0 
				
				logret <- eval( parse( text=paste0( ifelse(wt2,'withTimeout(',""),
													'try(log.data(
														 input=input,
														 results=results,
														 verbose=verbose ) )',
														 ifelse(wt2,',
														 timeout = timeout.log.data,
														 onTimeout = "error" )', "" ) ) ) )
										 
				# log.data.status
				if( !inherits( logret, "try-error" ) ){
					log.data.status <- "ok"
					logid <- logret$logid
					run.time.log.data.secs <- logret$run.time.log.data.secs
				} else {
					if( wt2 ) log.data.status <- "error or timed out" else log.data.status <- "error"
					logid <- as.integer( NA )
					run.time.log.data.secs <- as.numeric( NA )
				}
		
				# add log.data.status to results
				results <- c( results, list( "log.data.status"=log.data.status, "logid"=logid, "run.time.log.data.secs"=run.time.log.data.secs ) )
		} else {
				
		}

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
						  # study=list("budget"=20000, "target.power"=0.80, "l2.cost"=10, "l1.cost"=10, alpha=0.05, T=8 ),
						  # optimize=list(
									# "what"=c("power"),
									# "direction"=c("max"),
									# "via"=c("power"),
									# "par"=c("T"),
									# "via.function"=c("calculate.power.LRT"),
									# "optimizer"=c("genoud"),
									# "starting.values"="round(mean(c(par.min.set,par.max.set)))",
									# "set.seed.value"="random"
									# ),
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
