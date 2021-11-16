compute_fisher_sparse <- function(timepoints, n_ov, n_process, matrices,
                                  target_parameters = NULL) {
  
  # Calculate dimensions and create indices ----
  n_ov_time <- timepoints * n_ov
  n_process_time <- timepoints * n_process
  n_all <- n_ov_time + n_process_time
  
  
  seq_ov <- seq_len(n_ov)
  seq_ov_time <- seq_len(n_ov_time)
  seq_process <- seq_len(n_process)
  seq_timepoints <- seq_len(timepoints)
  
  
  # Create RAM matrices ----
  RAM_F_values <- matrix(0, nrow = n_ov_time, ncol = n_all)
  RAM_F_values[seq_ov_time, seq_ov_time] <- diag(1, nrow = n_ov_time)
  
  RAM_A_values <- matrix(0, nrow = n_all, ncol = n_all)
  RAM_A_labels <- matrix(NA, nrow = n_all, ncol = n_all)
  
  RAM_S_values <- matrix(0, nrow = n_all, ncol = n_all)
  RAM_S_labels <- matrix(NA, nrow = n_all, ncol = n_all)
  
  RAM_m_values <- matrix(0, nrow = n_all, ncol = 1)
  RAM_m_labels <- matrix(NA, nrow = n_all, ncol = 1)
  
  
  # Fill in parameter matrices ----
  # Add loadings
  for (i in seq_timepoints) {
    RAM_A_values[(i - 1) * n_ov + seq_ov, n_ov_time + (i - 1) * n_process + seq_process] <- matrices$loadings$values
    RAM_A_labels[(i - 1) * n_ov + seq_ov, n_ov_time + (i - 1) * n_process + seq_process] <- matrices$loadings$labels
  }
  
  # Add covariance at T0
  RAM_S_values[(n_ov_time + 1):(n_ov_time + n_process), (n_ov_time + 1):(n_ov_time + n_process)] <- matrices$T0cov$values
  RAM_S_labels[(n_ov_time + 1):(n_ov_time + n_process), (n_ov_time + 1):(n_ov_time + n_process)] <- matrices$T0cov$labels
  
  # Add means at T0
  RAM_m_values[(n_ov_time + 1):(n_ov_time + n_process), 1] <- matrices$T0means$values
  RAM_m_labels[(n_ov_time + 1):(n_ov_time + n_process), 1] <- matrices$T0means$labels
  
  # Add observed variables means
  for (i in seq_timepoints) {
    RAM_m_values[(i - 1) * n_ov + seq_ov, 1] <- matrices$ov_means$values
    RAM_m_labels[(i - 1) * n_ov + seq_ov, 1] <- matrices$ov_means$labels
  }
  
  # Add observed variable covariance
  for (i in seq_timepoints) {
    RAM_S_values[(i - 1) * n_ov + seq_ov, (i - 1) * n_ov + seq_ov] <- matrices$ov_cov$values
    RAM_S_labels[(i - 1) * n_ov + seq_ov, (i - 1) * n_ov + seq_ov] <- matrices$ov_cov$labels
  }
  
  # Add autoregressive and cross-lagged parameters
  for (i in seq_len(timepoints - 1)) {
    RAM_A_values[n_ov_time + i * n_process + seq_process, n_ov_time + (i - 1) * n_process + seq_process] <- matrices$arcl$values
    RAM_A_labels[n_ov_time + i * n_process + seq_process, n_ov_time + (i - 1) * n_process + seq_process] <- matrices$arcl$labels
  }
  
  # Add means of the processes
  for (i in seq_len(timepoints - 1)) {
    RAM_m_values[n_ov_time + i * n_process + seq_process, 1] <- matrices$process_means$values
    RAM_m_labels[n_ov_time + i * n_process + seq_process, 1] <- matrices$process_means$labels
  }
  
  # Add covariance of the processes
  for (i in seq_len(timepoints - 1)) {
    RAM_S_values[n_ov_time + i * n_process + seq_process, n_ov_time + i * n_process + seq_process] <- matrices$process_cov$values
    RAM_S_labels[n_ov_time + i * n_process + seq_process, n_ov_time + i * n_process + seq_process] <- matrices$process_cov$labels
  }
  
  
  # Compute further matrices ----
  B <- solve(diag(1, nrow = n_all) - RAM_A_values)
  E <- B %*% RAM_S_values %*% t(B)
  FB <- RAM_F_values %*% B
  exp_cov <- RAM_F_values %*% E %*% t(RAM_F_values)
  exp_cov_inv <- solve(exp_cov)
  
  
  # Determine model characteristics ----
  ## Does the model has a meanstructure?
  if (all(is.na(RAM_m_labels))) {
    mean_structure <- FALSE
    n_moments <- n_moments_covariance <- n_ov_time * (n_ov_time + 1) / 2
  } else {
    mean_structure <- TRUE
    # Entries of the covariance matrix and mean vector
    n_moments_all <- n_ov_time * (n_ov_time + 3) / 2
    n_moments_covariance <- n_ov_time * (n_ov_time + 1) / 2
  }
  
  
  
  # Get target parameters ----
  if (is.null(target_parameters)) {
    RAM_all_labels <- c(RAM_A_labels, RAM_S_labels, RAM_m_labels)
    target_parameters <- unique(na.omit(RAM_all_labels))
  }
  n_target_parameters <- length(target_parameters)
  seq_target_parameters <- seq_len(n_target_parameters)
  
  
  # Compute partial derivatives of RAM matrices ----
  Zero <- matrix(0, nrow = n_all, ncol = n_all)
  A_deriv <- replicate(n = n_target_parameters, expr = Zero, simplify = FALSE)
  S_deriv <- A_deriv
  
  for (i in seq_target_parameters) {
    A_deriv[[i]][which(RAM_A_labels == target_parameters[i], arr.ind = TRUE)] <- 1
  }
  
  for (i in seq_target_parameters) {
    S_deriv[[i]][which(RAM_S_labels == target_parameters[i], arr.ind = TRUE)] <- 1
  }
  
  if (mean_structure) {
    zero <- matrix(0, nrow = n_all, ncol = 1)
    m_deriv <- replicate(n = n_target_parameters, expr = zero, simplify = FALSE)
    for (i in seq_target_parameters) {
      m_deriv[[i]][which(RAM_m_labels == target_parameters[i], arr.ind = TRUE)] <- 1
    }
  }
  
  
  # Compute Jacobian ----
  jac <- matrix(0, nrow = n_moments_all, ncol = n_target_parameters)
  
  for (i in seq_target_parameters) {
    symm <- FB %*% A_deriv[[i]] %*% E %*% t(RAM_F_values)
    jac[1:n_moments_covariance, i] <- lavaan::lav_matrix_vech(symm + t(symm) + FB %*% S_deriv[[i]] %*% t(FB))
  }
  
  if (mean_structure) {
    for (i in seq_target_parameters) {
      jac[(n_moments_covariance + 1):(n_moments_all), i] <- FB %*% A_deriv[[i]] %*%
        B %*% RAM_m_values + FB %*% m_deriv[[i]]
    }
  }
  
  
  # Calculate weight matrix ----
  Dup <- lavaan::lav_matrix_duplication(n = n_ov_time)
  Dup_sparse <- as(Dup, "sparseMatrix")
  Dup_sparse_t <- as(t(Dup), "sparseMatrix")
  V <- 0.5 * Dup_sparse_t %*% kronecker(X = exp_cov_inv, Y = exp_cov_inv) %*% Dup_sparse
  
  if (mean_structure) {
    V_m_cov <- matrix(data = 0, nrow = n_moments_all, ncol = n_moments_all)
    V_m_cov[1:n_moments_covariance, 1:n_moments_covariance] <- as.matrix(V)
    V_m_cov[(n_moments_covariance + 1):n_moments_all, (n_moments_covariance + 1):n_moments_all] <- exp_cov_inv
    V <- V_m_cov
  }
  
  
  # return Fisher ----
  fisher <- t(jac) %*% V %*% jac
  colnames(fisher) <- rownames(fisher) <- target_parameters
  fisher
  
}