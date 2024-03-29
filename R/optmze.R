## Changelog:
# MH 0.2.2 2023-10-11: added evaluation example
# MH/MA 0.1.74 2023-06-12: added precise power calculation ("pprec")
# MH 0.1.1 2023-05-26: prepare.input() now returns error_codes
# MH 0.0.44 2022-11-04: changed default of T.min.identify from NULL to 0
# JW 0.0.43 2022-11-02: check_plausability() got new parameter study, optmze() got new list element T.min.identify for error checking
# MH 0.0.42 2022-10-28: added test case "instable results"
#                       added test case "budget35000"
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
									# MH/MA 0.1.74 2023-06-12 option pprec (more precise (but also still approximated) power) added
									#              was necessary, because in Revision 1, we realized
									#              that the implemented power calculation (on which the optimization runs)
									#              was just a fast approximation
									#              Note: the power optimization runs not on the precise power (but still on the fast
									#                    power, therefore, it ist not really "via" but after the optimization
									"via"=c("pprec","power","se","se^2"),      # max nchar: 5
									"par"=c("T","N"),						   # nchar: 1
									"via.function"=c("calculate.power.LRT","compute.se.oertzen"), # max nchar: 19
									"optimizer"=c("genoud"),				   # max nchar: 6
									"starting.values"="round(mean(c(par.min.set,par.max.set)))", # max nchar: 50
									"set.seed.value"="random"				   # max nchar: 10
									),
					study=list("budget"=20000, "target.power"=0.80, "l2.cost"=10, "l1.cost"=10, alpha=0.05, T=NULL),
					constraints=list("T.min"=3, "T.max"=10, "N.min"=3, "N.max"=300, "T.min.identify"=0,
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
  error_codes <- check_plausability(constraints = constraints, model = model, study = study)
  
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
		# MH 0.1.1 2023-05-26: error_codes
		# if errors are detected: return output with error codes
		if(length(  input$error_codes[  error_type(input$error_codes) %in%  "error" ] ) > 0) {
		# if (length(error_codes) > 0) {
		  return(make_output(error_codes = input$error_codes))
		}		
		
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
		
		# MH 0.0.44 2022-11-04: get package version
		clpm.info.list <- get.clpm.info()

		# prepare results
		results <- prepare.results( res=res,
									run.time.optimizer.secs=run.time.optimizer.secs,
									input=input,
									clpm.info.list=clpm.info.list,
									verbose=verbose,
									error_codes = error_codes )

		## MH/MA 0.1.74 2023-06-12: error_codes
		# if errors are detected: return output with error codes
		if(length(  results$error_codes[  error_type(results$error_codes) %in%  "error" ] ) > 0) {
		# if (length(error_codes) > 0) {
		  return(make_output(error_codes = results$error_codes))
		}
		
		
		# MH 0.0.32 2022-09-11, log data
		if( log.data ){
			
				# MH 0.0.34 2022-09-15
				# with timeout (or not)
				wt2 <- !is.na( timeout.log.data ) && !is.null( timeout.log.data ) && is.numeric( timeout.log.data ) && timeout.log.data > 0 
				
				logret <- eval( parse( text=paste0( ifelse(wt2,'withTimeout(',""),
													'try(log.data(
														 input=input,
														 results=results,
														 clpm.info.list=clpm.info.list,
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

### MH 0.2.2 2023-10-11: evaluation example
# specs <- generate.model.example.3()
# specs$input_H1$model <- "clpm"
# specs$input_H1$Gamma$values <- matrix( c(0.5,0.1,0.1,0.5), 2, 2 )
# specs$input_H1$Omega$values <- matrix( c(1,0,0,1), 2, 2 )
# specs$input_H1$Psi <- NULL

# set.seed(12345)
# res <- optmze( model=list("specification"=specs,
						  # "target.parameters"=c("ARCL_2_1", "ARCL_1_2"),
						  # "target.parameters.values.H0"=rep(0,2)),
						  # study=list("budget"=50000, "target.power"=0.80, "l2.cost"=100, "l1.cost"=50, alpha=0.05, T=8 ),
						  # optimize=list(
									# "what"=c("power"),
									# "direction"=c("max"),
									# "via"=c("pprec"),
									## "via"=c("power"),
									# "par"=c("T"),
									# "via.function"=c("calculate.power.LRT"),
									# "optimizer"=c("genoud"),
									# "starting.values"="round(mean(c(par.min.set,par.max.set)))",
									# "set.seed.value"="random"
									# ),
							# constraints=list("T.min"=4, "T.max"=50, "N.min"=20, "N.max"=50, "T.min.identify"=0,
											# "T.integer"=TRUE,
											# "N.integer"=FALSE ),									
						  # genoud=list("pop.size"=16,"pop.size.max"=1000,"max.generations"=100,"wait.generations"=1,
						  			  # "boundary.enforcement"=2,"solution.tolerance"=0.001),
						  # log.data=FALSE,
						  # verbose=TRUE )

# str( res ); flush.console()


### MH 0.1.74 2023-06-12
# specs <- generate.model.example.3()
# specs$input_H1$model <- "fclpm"

# set.seed(12345)
# res <- optmze( model=list("specification"=specs,
						  # "target.parameters"=c("ARCL_2_1", "ARCL_1_2"),
						  # "target.parameters.values.H0"=rep(0,2)),
						  # study=list("budget"=10000, "target.power"=0.80, "l2.cost"=100, "l1.cost"=50, alpha=0.05, T=8 ),
						  # optimize=list(
									# "what"=c("power"),
									# "direction"=c("max"),
									# "via"=c("pprec"),
									# "par"=c("T"),
									# "via.function"=c("calculate.power.LRT"),
									# "optimizer"=c("genoud"),
									# "starting.values"="round(mean(c(par.min.set,par.max.set)))",
									# "set.seed.value"="random"
									# ),
							# constraints=list("T.min"=3, "T.max"=18, "N.min"=10, "N.max"=50, "T.min.identify"=0,
											# "T.integer"=TRUE,
											# "N.integer"=FALSE ),									
						  # genoud=list("pop.size"=16,"pop.size.max"=1000,"max.generations"=100,"wait.generations"=1,
						  			  # "boundary.enforcement"=2,"solution.tolerance"=0.001),
						  # verbose=TRUE )

# str( res ); flush.console()


### MH 0.1.00/0.1.1 2023-05-26 test case "error_largeCL"
# specs <- generate.model.example.3()
# specs$input_H1$Gamma$values <- matrix( c(0.5,0.1,5,0.5), 2, 2 )
# specs$input_H1$Psi <- NULL
# specs$input_H1$model <- "CLPM"

# set.seed(12345)
# res <- optmze( model=list("specification"=specs,
						  # "target.parameters"=c("ARCL_2_1", "ARCL_1_2"),
						  # "target.parameters.values.H0"=rep(0,2)),
						  # study=list("budget"=10000, "target.power"=0.80, "l2.cost"=100, "l1.cost"=50, alpha=0.05, T=8 ),
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
							# constraints=list("T.min"=2, "T.max"=18, "N.min"=10, "N.max"=50, "T.min.identify"=0,
											# "T.integer"=TRUE,
											# "N.integer"=FALSE ),									
						  # genoud=list("pop.size"=16,"pop.size.max"=1000,"max.generations"=100,"wait.generations"=1,
						  			  # "boundary.enforcement"=2,"solution.tolerance"=0.001),
						  # verbose=TRUE )

# str( res ); flush.console()


### MH 0.1.00/0.1.1 2023-05-26 test case "error_costs"
# specs <- generate.model.example.3()
# specs$input_H1$Psi <- NULL
# specs$input_H1$model <- "CLPM"

# set.seed(12345)
# res <- optmze( model=list("specification"=specs,
						  # "target.parameters"=c("ARCL_2_1", "ARCL_1_2"),
						  # "target.parameters.values.H0"=rep(0,2)),
						  # study=list("budget"=200, "target.power"=0.80, "l2.cost"=0.1, "l1.cost"=50, alpha=0.05, T=8 ),
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
							# constraints=list("T.min"=2, "T.max"=18, "N.min"=10, "N.max"=50, "T.min.identify"=0,
											# "T.integer"=TRUE,
											# "N.integer"=FALSE ),									
						  # genoud=list("pop.size"=16,"pop.size.max"=1000,"max.generations"=100,"wait.generations"=1,
						  			  # "boundary.enforcement"=2,"solution.tolerance"=0.001),
						  # verbose=TRUE )

# str( res ); flush.console()


# ===================================

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
											# "N.integer"=FALSE, "T.min.identify"=0 ),
						  # genoud=list("pop.size"=16,"max.generations"=100,"wait.generations"=1,
						  			  # "boundary.enforcement"=2,"solution.tolerance"=0.001),
						  # verbose=TRUE )

# str( res ); flush.console()
# }


### MH 0.0.42 2022-10-28 test case "instable results"
# specs <- generate.model.example.3()
# specs$input_H1$Gamma$values <- matrix( c(0.9,0.1,0.5,0.3), 2, 2 )
# specs$input_H1$Omega$values <- matrix( c(1,0,0,1), 2, 2 )
# specs$input_H1$Psi <- NULL
# specs$input_H1$model <- "CLPM"

# set.seed(12345)
# res <- optmze( model=list("specification"=specs,
						  # "target.parameters"=c("ARCL_2_1", "ARCL_1_2"),
						  # "target.parameters.values.H0"=rep(0,2)),
						  # study=list("budget"=10000, "target.power"=0.80, "l2.cost"=10, "l1.cost"=10, alpha=0.05, T=8 ),
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
							# constraints=list("T.min"=9, "T.max"=36, "N.min"=3, "N.max"=300,
											# "T.integer"=TRUE,
											# "N.integer"=FALSE ),									
						  # genoud=list("pop.size"=16,"pop.size.max"=1000,"max.generations"=100,"wait.generations"=1,
						  			  # "boundary.enforcement"=2,"solution.tolerance"=0.001),
						  # verbose=TRUE )

# str( res ); flush.console()


### MH 0.0.42 2022-10-28 test case "budget35000"
# specs <- generate.model.example.3()
# specs$input_H1$Gamma$values <- matrix( c(0.5,0.1,0.1,0.5), 2, 2 )
# specs$input_H1$Omega$values <- matrix( c(1,0,0,1), 2, 2 )
# specs$input_H1$Psi <- NULL
# specs$input_H1$model <- "CLPM"


# set.seed(12345)
# res <- optmze( model=list("specification"=specs,
						  # "target.parameters"=c("ARCL_2_1", "ARCL_1_2"),
						  # "target.parameters.values.H0"=rep(0,2)),
						  # study=list("budget"=35000, "target.power"=0.80, "l2.cost"=10, "l1.cost"=10, alpha=0.05, T=8 ),
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
							# constraints=list("T.min"=2, "T.max"=10, "N.min"=3, "N.max"=300,
											# "T.integer"=TRUE,
											# "N.integer"=FALSE ),									
						  # genoud=list("pop.size"=16,"pop.size.max"=1000,"max.generations"=100,"wait.generations"=1,
						  			  # "boundary.enforcement"=2,"solution.tolerance"=0.001),
						  # verbose=TRUE )

# str( res ); flush.console()


### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
