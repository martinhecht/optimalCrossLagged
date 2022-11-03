## Changelog:
# JW: 0.0.43 2022-11-02: changed/deleted error 1-4, added error 23-29
# MH 0.0.42 2022-10-28: changed type of 22 to "note"
# MA 0.0.36 2022-10-07:
#     -- added error code 22
#     -- , updated error_type for the errors 14 to 22
# JW: 0.0.32 2022-10-05: added error codes 14-21
# MH 0.0.31 2022-09-05: added error code 13 (type: warning)
# MH 0.0.30 2022-09-02:
#    -- added function error_type
#    -- added code 12 (type: warning)

error_messages_translation <- function (error_code, minTidentify) {
  
  error_messages <- c(
    paste0("T.min for your chosen model class is ", minTidentify, "."), # 1
    "T.min has to be smaller than T.max.", # 2 deprecated (should not occur)
    "N.min must be greater than 0.", # 3
    "N.min has to be smaller than N.max.", #4 deprecated (should not occur)
    "Matrix with ARCLP must not be an identity matrix (i.e., all AR effects equal 1, all CL effetcs equal 0)", # 5
    "Residual covariance matrix (RES) is not positive semi-definite", # 6
    "Measurement error covariance matrix (UNIQ) is not positive semi-definite", # 7
    "Random intercept covariance matrix (I) is not positive semi-definite", # 8
    "Random slope covariance matrix (S) is not positive semi-definite", # 9
    "Constant Accumulating Factor (A) covariance matrix is not positive semi-definite", # 10
    "Changing Accumulating Factor (B) covariance matrix is not positive semi-definite", # 11
    "Optimizer results might be of low accuracy. Try to increase the precision of the optimizer (see “Technical details” section) until this message vanishes.", # 12
    # MH 0.0.31 2022-09-05: pop.size.max
    "Optimizer results might be of low accuracy. Please be cautious!",  # 13
    "Please select at least one target parameter.", # 14
    "Variances cannot be set to 0. Please change the Dynamic Residual Variance(s).", # 15
    "Variances cannot be set to 0. Please change the Unique Residual Variance(s).", # 16
    "Variances cannot be set to 0. Please change the Random Intercept Variance(s).", # 17
    "Variances cannot be set to 0. Please change the Random Slope Variance(s).", # 18
    "Variances cannot be set to 0. Please change the Constant Accumulating Factor Variance(s).", # 19
    "Variances cannot be set to 0. Please change the Changing Accumulating Factor Variance(s).", # 20
    "Please don't select all parameters as target parameters.", # 21
    "Testing of a variance parameters also includes the test of all related covariance parameters.", # 22
    "Budget must be greater than 0.", # 23
    "N.Cost must be greater than 0.", # 24
    "T.Cost must be greater than 0.", # 25
    "Budget must be greater than the minimum number of time points and persons multiplied with their respective costs. Please increase the budget, the minimum number of time points and/or persons, or decrease the cost per time point and/or person.", # 26
    "Alpha level must be greater than 0 and smaller than 1.", #27
    "AR effects must not be greater than or equal to 1 nor smaller than or equal to -1.", # 28
    "The budget cannot be greater than 1.000.000 because of technical restrictions." # 29 (eg when 10.000.000: "Error : vector memory exhausted (limit reached?)")
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
    "warning", # 13
    "error",   # 14
    "error",   # 15
    "error",   # 16
    "error",   # 17
    "error",   # 18
    "error",   # 19
    "error",   # 20
    "error",   # 21
    "note",    # 22
    "error",   # 23
    "error",   # 24
    "error",   # 25
    "error",   # 26
    "error",   # 27
    "error",   # 28
    "error"    # 29
  )
  
  error_type[error_code]
  
}