## Changelog:
# JW: 0.0.32 2022-10-05: added error codes 14-21
# MH 0.0.31 2022-09-05: added error code 13 (type: warning)
# MH 0.0.30 2022-09-02:
#    -- added function error_type
#    -- added code 12 (type: warning)

error_messages_translation <- function (error_code) {
  
  error_messages <- c(
    "T.min smaller than 2", # 1
    "T.min larger or equahl than T.max", # 2
    "N.min smaller than 1", # 3
    "N.max smaller or equal than N.min", #4
    "Matrix with ARCLP must not be an identity matrix", # 5
    "Residual covariance matrix is not positive semi definite", # 6
    "Measurement error covariance matrix is not positive semi definite", # 7
    "Random intercept covariance matrix is not positive semi definite", # 8
    "Random slope covariance matrix is not positive semi definite", # 9
    "A covariance matrix is not positive semi definite", # 10
    "B covariance matrix is not positive semi definite", # 11
    "Optimizer results might be of low accuracy. Try to increase the optimizer accuracy (slider in “Technical details” section) until this message vanishes.", # 12
    # MH 0.0.31 2022-09-05: pop.size.max
	"Optimizer results might be of low accuracy. Please be cautious!",  # 13
	"Please select at least one target parameter.", # 14
	"Variances cannot be set to 0. Please change the Dynamic Residual Variance(s).", # 15
	"Variances cannot be set to 0. Please change the Unique Residual Variance(s).", # 16
	"Variances cannot be set to 0. Please change the Random Intercept Variance(s).", # 17
	"Variances cannot be set to 0. Please change the Random Slope Variance(s).", # 18
	"Variances cannot be set to 0. Please change the Constant Accumulating Factor Variance(s).", # 19
	"Variances cannot be set to 0. Please change the Changing Accumulating Factor Variance(s).", # 20
	"Please don't select all parameters as target parameters." # 21
  )
  
  error_messages[error_code]
  
}

error_type <- function (error_code) {
  
  error_type <- c(
    "error",   # 1
    "error",   # 2
    "error",   # 3
    "error",   # 4
    "error",   # 5
    "error",   # 6
    "error",   # 7
    "error",   # 8
    "error",   # 9
    "error",   # 10
    "error",   # 11
	"warning", # 12
	"warning"  # 13
  )
  
  error_type[error_code]
  
}
