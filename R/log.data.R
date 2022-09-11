## Changelog:
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
		"INSERT INTO logs (datetimestr,datetime,year,month,day,hour,min,sec) ",
		"VALUES (" ,'"',datetimestr,'"',","
		           ,'"',datetime,'"',","
		           ,year,","
				   ,month,","
				   ,day,","
				   ,hour,","
				   ,min,","
				   ,sec
		 ,");"
	)
	
	# execute
	dbExecute(con, insert)
	
	# get current logid
	logid <- dbGetQuery(con, "SELECT LAST_INSERT_ID();")[,1]
	
	# disconnect db
	dbDisconnect(con)
	
	return( logid )
}
