check_plausability <- function (constraints = constraints, model = model) {
  
  error_codes <- c()
  
  # check N and T inputs
  if (constraints$T.min < 2) {error_codes <- c(error_codes, 1)}
  if (constraints$T.min >= constraints$T.max) {error_codes <- c(error_codes, 2)}
  if (constraints$N.min < 1) {error_codes <- c(error_codes, 3)}
  if (constraints$N.max <= constraints$N.min) {error_codes <- c(error_codes, 4)}
  # hier kommen chekcs
  
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

# return
error_codes

}
