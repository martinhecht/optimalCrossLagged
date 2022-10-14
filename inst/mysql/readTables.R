## Changelog:
# MH 0.0.34 2022-09-15: initial programming

# install.packages("RMariaDB")
library("RMariaDB")

# get config file
dw <- config::get("datawarehouse",file = Sys.getenv("R_CONFIG_FILE", "config.pikepi12.yml"))
# dw <- config::get("datawarehouse")

# connect to data base
con <- dbConnect( 	eval(parse(text=dw$driver)),
					host = dw$server,
					port = dw$port,
					username = dw$uid,
					password = dw$pwd,
					dbname = dw$database )

########### logs ###########
( logs.dfr <- dbGetQuery(con, "SELECT * FROM logs") )

########### target_parameters ###########
( target.parameters.dfr <- dbGetQuery(con, "SELECT * FROM target_parameters") )

########### par_opts ###########
( par.opts.dfr <- dbGetQuery(con, "SELECT * FROM par_opts") )

########### error_codes ###########
( error.codes.dfr <- dbGetQuery(con, "SELECT * FROM error_codes") )

########### model_matrices ###########
( model.matrices.dfr <- dbGetQuery(con, "SELECT * FROM model_matrices") )

# disconnect db
dbDisconnect(con)
