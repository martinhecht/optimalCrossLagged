
# install.packages("RMariaDB")
library("RMariaDB")

# get config file
dw <- config::get("datawarehouse")

# connect to data base
con <- dbConnect( 	eval(parse(text=dw$driver)),
					host = dw$server,
					port = dw$port,
					username = dw$uid,
					password = dw$pwd,
					dbname = dw$database )

dbExecute(con, 				  
"DROP TABLE IF EXISTS logs;"
) 

dbExecute(con, 				  
"CREATE TABLE logs (
    logid BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
	datetimestr CHAR(23),
	datetime DATETIME,
	year TINYINT UNSIGNED,
	month TINYINT UNSIGNED,
	day TINYINT UNSIGNED,
	hour TINYINT UNSIGNED,
	min TINYINT UNSIGNED,
	sec TINYINT UNSIGNED
);"
)  
				  
# Run query to get results as dataframe
( d <- dbGetQuery(con, "SELECT * FROM logs") )


# Send query to pull requests in batches
# res <- dbSendQuery(con, "SELECT * FROM logs")
# data <- dbFetch(res, n = 2)
# data
# dbHasCompleted(res)
# dbClearResult(res)

dbDisconnect(con)
