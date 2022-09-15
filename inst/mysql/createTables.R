## Changelog:
# MH 0.0.34 2022-09-15: further variables added
# MH 0.0.33 2022-09-12: further variables added
# MH 0.0.32 2022-09-11: initial programming

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


########### logs ###########

dbExecute(con, 				  
"DROP TABLE IF EXISTS logs;"
) 

dbExecute(con, 				  
"CREATE TABLE logs (
    logid BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
	datetimestr CHAR(23),
	datetime DATETIME,
	year SMALLINT UNSIGNED,
	month TINYINT UNSIGNED,
	day TINYINT UNSIGNED,
	hour TINYINT UNSIGNED,
	min TINYINT UNSIGNED,
	sec TINYINT UNSIGNED,
	optimalclpm_version_str VARCHAR(11),
	version TINYINT UNSIGNED,
	subversion TINYINT UNSIGNED,
	subsubversion TINYINT UNSIGNED,
    optimalclpm_version_date_str CHAR(10),
    optimalclpm_version_date DATE,
    optimalclpm_version_year SMALLINT UNSIGNED,
    optimalclpm_version_month TINYINT UNSIGNED,
    optimalclpm_version_day TINYINT UNSIGNED,
	what VARCHAR(12),
	direction VARCHAR(3),
	via VARCHAR(5),
	par CHAR(1),
	via_function VARCHAR(19),
	optimizer VARCHAR(6),
	starting_values SMALLINT UNSIGNED,
	set_seed_value BIGINT,
	direction_optimizer VARCHAR(3),
	starting_values_equation VARCHAR(50),
	budget INT UNSIGNED,
	target_power DEC(3,2) UNSIGNED,
	l2_cost FLOAT(24) UNSIGNED,
	l1_cost FLOAT(24) UNSIGNED,
	alpha DEC(3,2) UNSIGNED,
	T TINYINT UNSIGNED,
	T_min FLOAT(24) UNSIGNED,
	T_max FLOAT(24) UNSIGNED,
	N_min FLOAT(24) UNSIGNED,
	N_max FLOAT(24) UNSIGNED,
	T_integer BOOL,
	N_integer BOOL,
	T_max_bound FLOAT(24) UNSIGNED,
	T_max_set FLOAT(24) UNSIGNED,
	T_min_bound FLOAT(24) UNSIGNED,
	T_min_set FLOAT(24) UNSIGNED,
	N_max_bound FLOAT(24) UNSIGNED,
	N_min_bound FLOAT(24) UNSIGNED,
	model VARCHAR(200),
	n_ov SMALLINT UNSIGNED,
	timeout SMALLINT UNSIGNED,
	timeout_log_data SMALLINT UNSIGNED,
	stability_check BOOL,
	runs TINYINT UNSIGNED,
	pop_size SMALLINT UNSIGNED,
	max_generations SMALLINT UNSIGNED,
	wait_generations TINYINT UNSIGNED,
	boundary_enforcement TINYINT UNSIGNED,
	solution_tolerance FLOAT(24) UNSIGNED,
	N_opt FLOAT(24) UNSIGNED,
	T_opt FLOAT(24) UNSIGNED,
	budget_opt INT UNSIGNED,
	run_time_optimizer_secs FLOAT(24) UNSIGNED,
	run_time_log_data_secs FLOAT(24) UNSIGNED,
	optimizer_runs SMALLINT UNSIGNED,
	stable_solution BOOL
);"
)  

# Run query to get results as dataframe
( d <- dbGetQuery(con, "SELECT * FROM logs") )


########### target_parameters ###########

dbExecute(con, 				  
"DROP TABLE IF EXISTS target_parameters;"
) 

dbExecute(con, 				  
"CREATE TABLE target_parameters (
    logid BIGINT UNSIGNED NOT NULL,
	target_parameters VARCHAR(50),
	power_max DEC(3,2) UNSIGNED
);"
)  

# Run query to get results as dataframe
( d <- dbGetQuery(con, "SELECT * FROM target_parameters") )


########### par_opts ###########

dbExecute(con, 				  
"DROP TABLE IF EXISTS par_opts;"
) 

dbExecute(con, 				  
"CREATE TABLE par_opts (
    logid BIGINT UNSIGNED NOT NULL,
	par_opts FLOAT(24) UNSIGNED
);"
)  

# Run query to get results as dataframe
( d <- dbGetQuery(con, "SELECT * FROM par_opts") )


########### error_codes ###########

dbExecute(con, 				  
"DROP TABLE IF EXISTS error_codes;"
) 

dbExecute(con, 				  
"CREATE TABLE error_codes (
    logid BIGINT UNSIGNED NOT NULL,
	error_codes SMALLINT UNSIGNED
);"
)  

# Run query to get results as dataframe
( d <- dbGetQuery(con, "SELECT * FROM error_codes") )


########### model_matrices ###########

dbExecute(con, 				  
"DROP TABLE IF EXISTS model_matrices;"
) 

dbExecute(con, 				  
"CREATE TABLE model_matrices (
    logid BIGINT UNSIGNED NOT NULL,
	matrix VARCHAR(10),
	value FLOAT(24),
	labels VARCHAR(50)
);"
)  



# Run query to get results as dataframe
( d <- dbGetQuery(con, "SELECT * FROM model_matrices") )




# Send query to pull requests in batches
# res <- dbSendQuery(con, "SELECT * FROM logs")
# data <- dbFetch(res, n = 2)
# data
# dbHasCompleted(res)
# dbClearResult(res)

dbDisconnect(con)
