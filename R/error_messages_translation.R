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
    "B covariance matrix is not positive semi definite" # 11
  )
  
  error_messages[error_code]
  
}