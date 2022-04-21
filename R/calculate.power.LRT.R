## Changelog:
# MH 0.0.3 2022-04-22: more lean T-specific F_diff calculations
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

		# get previous F_diff_list if exist
		if( "F_diff_list" %in% ls( envir=pwrLRT.env ) ) {
			F_diff_list <- get( "F_diff_list", envir=pwrLRT.env )
			
			# if existing, try to get F_diff for timepoint
			if( as.character(timepoints) %in% names(F_diff_list) ){
				F_diff <- F_diff_list[[ as.character(timepoints) ]]
				
				if( verbose ){
					cat( paste0( "F_diff for time point ", timepoints," recovered from cache", "\n" ) )
					flush.console()
				}
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
			
			# F_diff_list
			F_diff_list_current_timepoint <- list( F_diff )
			names( F_diff_list_current_timepoint ) <- as.character(timepoints)
		
			# attach F_diff_list_current_timepoint to F_diff_list
			if( !"F_diff_list" %in% ls( envir=pwrLRT.env ) ) {
					F_diff_list <- F_diff_list_current_timepoint
			} else {
					F_diff_list <- c( F_diff_list, F_diff_list_current_timepoint )
			}
			
			# assign into environment
			assign( "F_diff_list", F_diff_list, pos = pwrLRT.env,
													inherits = FALSE, immediate=TRUE)
			
			if( verbose ){
				cat( paste0( "new F_diff for time point ", timepoints," added", "\n" ) )
				cat( paste0( "F_diff in cache: ", "\n" ) )
				print( F_diff_list )
				flush.console()
			}
	  }
  }

  # print F_diff values and df for power calculation
  if( verbose ){
		cat( paste0( "used for power calculation:\n" ) )
		cat( paste0( "   F_diff values: ", paste( F_diff$values, collapse=", " ), "\n" ) )
		cat( paste0( "   df: ", F_diff$df, "\n" ) )
		cat( paste0( "   N: ", N, "\n" ) )
		flush.console()
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
