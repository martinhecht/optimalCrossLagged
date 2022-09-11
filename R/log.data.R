## Changelog:
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
						   "timeout,",
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
				   ,input$timeout,","
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
	
	# disconnect db
	dbDisconnect(con)
	
	return( logid )
}
