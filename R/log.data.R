## Changelog:
# MH 0.0.34 2022-09-15: further variables added
# MH 0.0.33 2022-09-12: further variables added
# MH 0.0.32 2022-09-11: initial programming

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definition
log.data <- function( input, results, verbose=TRUE ){
	
	# start time 
	start.time.log.data <- Sys.time()	
	
	# get config file
	dw <- config::get("datawarehouse")
	
	# connect to data base
	con <- dbConnect( 	eval(parse(text=dw$driver)),
						host = dw$server,
						port = dw$port,
						username = dw$uid,
						password = dw$pwd,
						dbname = dw$database )
	
	## clpm version and date
	# read lines from description file
	lns <- readLines( "../DESCRIPTION" )
	# which line has version
	wV <- which( sapply( lns, function(ln) grepl( "Version:", ln, fixed=TRUE ) ) )
	# which line has date
	wD <- which( sapply( lns, function(ln) grepl( "Date:", ln, fixed=TRUE ) ) )
	# get version
	optimalclpm.version.str <- sub( "^.*\\s(\\d+\\.\\d+\\.\\d+).*$", "\\1", lns[wV] )
	version <-       as.integer( sub( "^(\\d+)\\.\\d+\\.\\d+$", "\\1", optimalclpm.version.str ) )
	subversion <-    as.integer( sub( "^\\d+\\.(\\d+)\\.\\d+$", "\\1", optimalclpm.version.str ) )
	subsubversion <- as.integer( sub( "^\\d+\\.\\d+\\.(\\d+)$", "\\1", optimalclpm.version.str ) )
	# get date
	optimalclpm.version.date.str <- sub( "^.*\\s(\\d+\\-\\d+\\-\\d+).*$", "\\1", lns[wD] )
	optimalclpm.version.year  <- as.integer( sub( "^(\\d+)\\-\\d+\\-\\d+$", "\\1", optimalclpm.version.date.str ) )
	optimalclpm.version.month <- as.integer( sub( "^\\d+\\-(\\d+)\\-\\d+$", "\\1", optimalclpm.version.date.str ) )
	optimalclpm.version.day   <- as.integer( sub( "^\\d+\\-\\d+\\-(\\d+)$", "\\1", optimalclpm.version.date.str ) )
	
	# get date time
	posix <- Sys.time()
	
	# change time zone
	attr( posix, "tzone" ) <- "UTC"
	
	# get time date values
	datetimestr <- as.character(posix, usetz = TRUE)
	datetime <- as.character(posix)
	year <-   as.integer( strftime(posix, format="%Y") )
	month <-  as.integer( strftime(posix, format="%m") )
	day <-    as.integer( strftime(posix, format="%d") )
	hour <-   as.integer( strftime(posix, format="%H") )
	min <-    as.integer( strftime(posix, format="%M") )
	sec <-    as.integer( strftime(posix, format="%S") )
	
	
	########### logs ###########
	
	# create insert command
	insert <- paste0(
		"INSERT INTO logs (datetimestr,",
						   "datetime,",
						   "year,",
						   "month,",
						   "day,",
						   "hour,",
						   "min,",
						   "sec,",
						   "optimalclpm_version_str,",
						   "version,",
						   "subversion,",
						   "subsubversion,",
						   "optimalclpm_version_date_str,",
						   "optimalclpm_version_date,",
						   "optimalclpm_version_year,",
						   "optimalclpm_version_month,",
						   "optimalclpm_version_day,",
						   "what,",
						   "direction,",
						   "via,",
						   "par,",
						   "via_function,",
						   "optimizer,",
						   "starting_values,",
						   "set_seed_value,",
						   "direction_optimizer,",
						   "starting_values_equation,",
						   "budget,",
						   "target_power,",
						   "l2_cost,",
						   "l1_cost,",
						   "alpha,",
						   "T,",
						   "T_min,",
						   "T_max,",
						   "N_min,",
						   "N_max,",
						   "T_integer,",
						   "N_integer,",
						   "T_max_bound,",
						   "T_max_set,",
						   "T_min_bound,",
						   "T_min_set,",
						   "N_max_bound,",
						   "N_min_bound,",
						   "model,",
						   "n_ov,",
						   "timeout,",
						   "timeout_log_data,",
						   "stability_check,",
						   "runs,",
						   "pop_size,",
						   "max_generations,",
						   "wait_generations,",
						   "boundary_enforcement,",
						   "solution_tolerance,",
						   "N_opt,",
						   "T_opt,",
						   "budget_opt,",
						   "run_time_optimizer_secs,",
						   "optimizer_runs,",
						   "stable_solution",
						   ") ",
		"VALUES (" ,'"',datetimestr,'"',","
		           ,'"',datetime,'"',","
		           ,year,","
				   ,month,","
				   ,day,","
				   ,hour,","
				   ,min,","
				   ,sec,","
				   ,'"',optimalclpm.version.str,'"',","
				   ,version,","
				   ,subversion,","
				   ,subsubversion,","
				   ,'"',optimalclpm.version.date.str,'"',","
				   ,'"',optimalclpm.version.date.str,'"',","
				   ,optimalclpm.version.year,","
				   ,optimalclpm.version.month,","
				   ,optimalclpm.version.day,","
				   ,'"',input$optimize$what,'"',","
				   ,'"',input$optimize$direction,'"',","
				   ,'"',input$optimize$via,'"',","
				   ,'"',input$optimize$par,'"',","
				   ,'"',input$optimize$via.function,'"',","
				   ,'"',input$optimize$optimizer,'"',","
				   ,input$optimize$starting.values,","
				   ,input$optimize$set.seed.value,","
				   ,'"',input$optimize$direction.optimizer,'"',","
				   ,'"',input$optimize$starting.values.equation,'"',","
				   ,input$study$budget,","
				   ,input$study$target.power,","
				   ,input$study$l2.cost,","
				   ,input$study$l1.cost,","
				   ,input$study$alpha,","
				   ,input$study$T,","
				   ,input$constraints$T.min,","
				   ,input$constraints$T.max,","
				   ,input$constraints$N.min,","
				   ,input$constraints$N.max,","
				   ,input$constraints$T.integer,","
				   ,input$constraints$N.integer,","
				   ,input$constraints$T.max.bound,","
				   ,input$constraints$T.max.set,","
				   ,input$constraints$T.min.bound,","
				   ,input$constraints$T.min.set  ,","
				   ,input$constraints$N.max.bound,","
				   ,input$constraints$N.min.bound,","
				   ,'"',input$model$specification$input_H1$model,'"',","
				   ,input$model$specification$input_H1$n_ov,","
				   ,input$timeout,","
				   ,input$timeout.log.data,","
				   ,input$stability.check,","
				   ,input$runs,","
				   ,input$genoud$pop.size,","
				   ,input$genoud$max.generations,","
				   ,input$genoud$wait.generations,","
				   ,input$genoud$boundary.enforcement,","
				   ,input$genoud$solution.tolerance,","
				   ,results$N.opt,","
				   ,results$T.opt,","
				   ,results$budget.opt,","
				   ,results$run.time.optimizer.secs,","
				   ,results$optimizer.runs,","
				   ,results$stable.solution,
		 ");"
	)
	
	# execute
	dbExecute(con, insert)
	# check
	# ( d <- dbGetQuery(con, "SELECT * FROM logs") )	

	# get current logid
	logid <- dbGetQuery(con, "SELECT LAST_INSERT_ID();")[,1]	
	
	
	########### target_parameters ###########

	# create insert command
	insert2 <- paste0(
		"INSERT INTO target_parameters (logid,",
						   "target_parameters,",
						   "power_max",
						   ") ",
		"VALUES (" ,logid,","
				   ,paste0( '"', names( results$power.max ), '"' ),","
				   ,unname( results$power.max ),
		 ");"
	)	
	
	# execute
	sapply( insert2, function( ins ) dbExecute(con, ins) )
	# check
	# ( d2 <- dbGetQuery(con, "SELECT * FROM target_parameters") )


	########### par_opts ###########

	# create insert command
	insert3 <- paste0(
		"INSERT INTO par_opts (logid,",
						   "par_opts",
						   ") ",
		"VALUES (" ,logid,","
				   ,results$par.opts,
		 ");"
	)
	
	# execute
	sapply( insert3, function( ins ) dbExecute(con, ins) )
	# check
	# ( d3 <- dbGetQuery(con, "SELECT * FROM par_opts") )


	########### error_codes ###########

	# create insert command
	insert4 <- paste0(
		"INSERT INTO error_codes (logid,",
						   "error_codes",
						   ") ",
		"VALUES (" ,logid,","
				   ,if( is.null(results$error_codes) ) "NULL" else results$error_codes,
		 ");"
	)
	
	# execute
	sapply( insert4, function( ins ) dbExecute(con, ins) )
	# check
	# ( d4 <- dbGetQuery(con, "SELECT * FROM error_codes") )


	########### model_matrices ###########

	# create insert command
	do.insert5 <- function( matr, input ){
		insert5 <- paste0(
			"INSERT INTO model_matrices (logid,",
							   "matrix,",
							   "value,",
							   "labels",
							   ") ",
			"VALUES (" ,logid,","
					   ,'"',matr,'"',","
					   ,as.vector(input$model$specification$input_H1[[matr]]$values),","
					   ,paste0( '"', input$model$specification$input_H1[[matr]]$labels, '"'),
			 ");"
		)
	
		# execute
		sapply( insert5, function( ins ) dbExecute(con, ins) )
	}
	
	# matrices
	matrices <- names(input$model$specification$input_H1)
	# delete not matrices names
	matrices <- matrices[!matrices %in% c("model","n_ov")]
	# delete all NULL matrices
	matrices <- matrices[!sapply( matrices, function( matr ) is.null( input$model$specification$input_H1[[matr]] ) )]

	# do insert
	if( length( matrices ) > 0 ) sapply( matrices, do.insert5, input )
	
	# check
	# ( d5 <- dbGetQuery(con, "SELECT * FROM model_matrices") )

	# runtime in seconds
	run.time.log.data.difftime <- Sys.time() - start.time.log.data
	run.time.log.data.secs <- as.double( run.time.log.data.difftime, units="secs" )

	# write runtime into db
	# dbExecute(con, paste0( "INSERT INTO logs (run_time_log_data_secs) VALUES (",run.time.log.data.secs,") WHERE (logid=",logid,"));" ) )
	dbExecute(con, paste0( "UPDATE logs SET run_time_log_data_secs=",run.time.log.data.secs," WHERE logid=",logid,";" ) )
	
	# disconnect db
	dbDisconnect(con)
	
	return( list("logid"=logid,"run.time.log.data.secs"=run.time.log.data.secs ) )
}
