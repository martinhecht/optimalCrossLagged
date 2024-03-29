# MA: 0.0.44 2023-05-28: added error codes 32
# JW: 0.0.43 2022-11-02: changed/deleted error 1-4, added error 23-29
# MA: 0.0.36 2022-10-07: added error code 22
# JW: 0.0.32 2022-10-05: added error codes 14-21

check_plausability <- function (constraints = constraints, model = model, study = study) {
  
  error_codes <- c()
  
  # check N and T inputs
  if (constraints$T.min < constraints$T.min.identify) {error_codes <- c(error_codes, 1)}
  #if (constraints$T.min > constraints$T.max) {error_codes <- c(error_codes, 2)} # deprecated (should not occur)
  if (constraints$N.min < 1) {error_codes <- c(error_codes, 3)}
  #if (constraints$N.max < constraints$N.min) {error_codes <- c(error_codes, 4)} # deprecated (should not occur)
  
  # check building blocks
  identity_matrix <- diag(1, nrow = nrow(model$specification$input_H1$Gamma$values))
  if (all(model$specification$input_H1$Gamma$values == identity_matrix)) {
    error_codes <- c(error_codes, 5)
  }
  if (!matrixcalc::is.positive.semi.definite(model$specification$input_H1$Omega$values)) {
    error_codes <- c(error_codes, 6)
  }
  if (!is.null(model$specification$input_H1$Psi)) {
    if (!matrixcalc::is.positive.semi.definite(model$specification$input_H1$Psi$values)) {
      error_codes <- c(error_codes, 7)
    }
  }
  if (!is.null(model$specification$input_H1$Theta_I)) {
    if (!matrixcalc::is.positive.semi.definite(model$specification$input_H1$Theta_I$values)) {
      error_codes <- c(error_codes, 8)
    }
  }
  if (!is.null(model$specification$input_H1$Theta_S)) {
    if (!matrixcalc::is.positive.semi.definite(model$specification$input_H1$Theta_S$values)) {
      error_codes <- c(error_codes, 9)
    }
  }
  if (!is.null(model$specification$input_H1$Theta_A)) {
    if (!matrixcalc::is.positive.semi.definite(model$specification$input_H1$Theta_A$values)) {
      error_codes <- c(error_codes, 10)
    }
  }
  if (!is.null(model$specification$input_H1$Theta_B)) {
    if (!matrixcalc::is.positive.semi.definite(model$specification$input_H1$Theta_B$values)) {
      error_codes <- c(error_codes, 11)
    }
  }
  
  if (is.null(model$target.parameters)) {error_codes <- c(error_codes, 14)}
  
  # check whether variances are non-zero
  if (any(diag(model$specification$input_H1$Omega$values) == 0)){
    error_codes <- c(error_codes, 15)
  }
  
  if (any(diag(model$specification$input_H1$Psi$values) == 0)){
    error_codes <- c(error_codes, 16)
  }
  
  if (any(diag(model$specification$input_H1$Theta_I$values) == 0)){
    error_codes <- c(error_codes, 17)
  }
  
  if (any(diag(model$specification$input_H1$Theta_S$values) == 0)){
    error_codes <- c(error_codes, 18)
  }
  
  if (any(diag(model$specification$input_H1$Theta_A$values) == 0)){
    error_codes <- c(error_codes, 19)
  }
  
  if (any(diag(model$specification$input_H1$Theta_B$values) == 0)){
    error_codes <- c(error_codes, 20)
  }
  
  # bei symmetrical matrix: nrow * (nrow+1) / 2 mögliche params (sind in error codes 14-20 zu sehen)
  # bei asymmetrical matrix: nrow*nrow mögliche params
  # kann man sicher noch vereinfachen weil viele matrizen gleiche dimensionen haben
  # alles aufaddieren
  # all.params <- nrow(model$specification$input_H1$Theta_B$values) * (nrow(model$specification$input_H1$Theta_B$values) + 1) / 2
  # if (all.params == model$target.parameters)){
  #   error_codes <- c(error_codes, 21)
  # }
  
  
  # Check if by testing a variance parameter, covariance parameters are tested.
  ## Check is unnecessary for univariate models
  if (nrow(model$specification$input_H1$Gamma$values) > 1) {
    
    if (any(model$target.parameters %in%
            c(diag(model$specification$input_H1$Omega$labels),
              diag(model$specification$input_H1$Psi$labels),
              diag(model$specification$input_H1$Theta_I$labels),
              diag(model$specification$input_H1$Theta_S$labels),
              diag(model$specification$input_H1$Theta_A$labels),
              diag(model$specification$input_H1$Theta$labels)))) {
      error_codes <- c(error_codes, 22)
    }
  }
  
  if (study$budget == 0){
    error_codes <- c(error_codes, 23)
  }
  
  if (study$l2.cost == 0){
    error_codes <- c(error_codes, 24)
  }
  
  if (study$l1.cost == 0){
    error_codes <- c(error_codes, 25)
  }
  
  if (study$budget < (constraints$T.min * study$l1.cost) + (constraints$N.min * study$l2.cost) ){
    error_codes <- c(error_codes, 26)
  }
  
  if (study$alpha == 0 | study$alpha > 1){
    error_codes <- c(error_codes, 27)
  }
  
  if (any(diag(model$specification$input_H1$Gamma$values) >= 1)  | any(diag(model$specification$input_H1$Gamma$values) <= -1) ){
    error_codes <- c(error_codes, 28)
  }
  
  if (study$budget > 1000000){
    error_codes <- c(error_codes, 29)
  }

  # return
  error_codes
  
}