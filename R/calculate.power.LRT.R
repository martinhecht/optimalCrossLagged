## Changelog:
# MH 0.0.2 2022-02-25: added caching of T-specific F_diff calculations
# MA 0.0.1 2022-02-24: initial programming

## Documentation
#' @title Calculate power
#' @description Function calculates the power of a likelihood ratio test.
#' @param
#' @param
#' @param
#' @return
#' @keywords internal

calculate.power.LRT <- function(alpha, N, timepoints, n_ov, n_process,
										  matrices, target.parameters=NULL,
										  pwrLRT.env=NULL,
										  verbose=TRUE ) {

  # MH 0.0.2 2022-02-25
  # default F_diff
  F_diff <- NULL

  # if env exists
  if( !is.null( pwrLRT.env ) ){

		# get previous F_diff_values if exist
		if( "F_diff_values" %in% ls( envir=pwrLRT.env ) ) {
			F_diff_values <- get( "F_diff_values", envir=pwrLRT.env )
			
			# if existing, try to get F_diff for timepoint
			if( as.character(timepoints) %in% colnames(F_diff_values) ){
				F_diff_values. <- F_diff_values[ , as.character(timepoints) ]
				# create original F_diff structure
				F_diff <- list( "values" = F_diff_values., "df"=get( "df", envir=pwrLRT.env ) )
			}
		}
  }

  # if F_diff for timepoint has not yet been calculated, do it
  #    (and put it into env, if caching is desired)
  if( is.null( F_diff ) ){
	  
	  # calculate F_diff
	  F_diff <- calculate.F.diff( timepoints=timepoints,
								  n_ov=n_ov,
								  n_process=n_process,
								  matrices=matrices, 
								  target.parameters=target.parameters )

	  
	  # if env exists (=caching is desired)
	  if( !is.null( pwrLRT.env ) ){
			# matrix
			F_diff_matrix <- matrix( F_diff$values, nrow=length( F_diff$values ), ncol=1 )
			rownames( F_diff_matrix ) <- names( F_diff$values )
			colnames( F_diff_matrix ) <- as.character(timepoints)
		
			# attach F_diff to cumulative F_diff_values matrix
			if( !"F_diff_values" %in% ls( envir=pwrLRT.env ) ) {
					F_diff_values <- F_diff_matrix
			} else {
					F_diff_values <- cbind( F_diff_values, F_diff_matrix )
			}
			
			# assign into environment
			assign( "F_diff_values", F_diff_values, pos = pwrLRT.env,
													inherits = FALSE, immediate=TRUE)
			assign( "df", F_diff$df, pos = pwrLRT.env, inherits = FALSE, immediate=TRUE)
			
			if( verbose ){
				cat( paste0( "new F_diff values added", "\n" ) )
				cat( paste0( "F_diff values in cache: ", "\n" ) )
				print( F_diff_values )
				cat( paste0( "df in cache: ", F_diff$df, "\n" ) )
				flush.console()
			}
	  }
  }

  # Critical value
  critical_value <- qchisq(p = 1 - alpha, df = F_diff$df)
  
  # prepare outcome
  power <- F_diff$values
  
  # calculate power
  for (i in seq_along(power)) {
    power[i] <- pchisq(q = critical_value, df = F_diff$df, lower.tail = FALSE,
                       ncp = N * F_diff$values[i])
  }

  # return outcome
  power

}
