## Changelog:
# MH 0.0.44 2022-11-04: moved from log.data()

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return

## Function definitions
get.clpm.info. <- function(){ 
	## clpm version and date
	# read lines from description file
	lns <- readLines( here( "DESCRIPTION" ) )
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
	return( list(  "optimalclpm.version.str"=optimalclpm.version.str,
				   "version"=version,
				   "subversion"=subversion,
				   "subsubversion"=subsubversion,
				   "optimalclpm.version.date.str"=optimalclpm.version.date.str,
				   "optimalclpm.version.year"=optimalclpm.version.year,
				   "optimalclpm.version.month"=optimalclpm.version.month,
				   "optimalclpm.version.day"=optimalclpm.version.day ) )
}

get.clpm.info <- function(){ 
	clpm.info.list <- try( get.clpm.info.() )
	if( inherits( clpm.info.list, "try-error" ) ){
		clpm.info.list <- list( "optimalclpm.version.str"=NULL,
								"version"=NULL,
								"subversion"=NULL,
								"subsubversion"=NULL,								
								"optimalclpm.version.date.str"=NULL,
								"optimalclpm.version.year"=NULL,
								"optimalclpm.version.month"=NULL,
								"optimalclpm.version.day"=NULL )
	}
	return( clpm.info.list )
}

