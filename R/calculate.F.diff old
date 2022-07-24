## Changelog:
# MA 0.0.1 2022-02-24: initial programming

## Documentation
#' @title Difference between Fit Functions
#' @description Calculate differences between the fit function of the model
#' under the H1 and the fit function of the model under the H0.
#' @param
#' @param
#' @param
#' @return
#' @keywords internal

calculate.F.diff <- function(timepoints, n_ov, n_process, matrices, 
                             target.parameters = NULL) {
  
  # Assemble SEM of the Building Blocks ----
  
  # Dimensions and indices for RAM matrices ----
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
  
  
  # Populate RAM matrices ----
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
  
  
  # Number of model parameters
  RAM_unique_labels <- unique(na.omit(c(RAM_A_labels, RAM_S_labels, RAM_m_labels)))
  n_parameters <-length(RAM_unique_labels)
  seq_parameters <- seq_len(n_parameters)
  
  
  
  # Compute mean vector and covariance matrix under the H1 ---
  identity_matrix <- diag(1, nrow = n_all)
  F_inv_I_A_H1 <- RAM_F_values %*% solve(identity_matrix - RAM_A_values)
  Sigma_H1 <- F_inv_I_A_H1 %*% RAM_S_values %*% t(F_inv_I_A_H1)
  mu_H1 <- F_inv_I_A_H1 %*% RAM_m_values
  df_H1 <- n_ov_time * (n_ov_time + 3) / 2 - n_parameters
  
  
  
  # Loop over free parameters ----
  
  if (is.null(target.parameters)) {target.parameters <- RAM_unique_labels}
  
  # prepare empty outcome
  n_target_parameters <- length(target.parameters)
  F_diff <- list(values = rep(NA, times = n_target_parameters),
                 df = df_H1)
  names(F_diff$values) <- target.parameters
  
  
  for (i in seq_len(n_target_parameters)) {
    
    # Compute mean vector and covariance matrix under the H0 ----
    RAM_A_values_H0 <- RAM_A_values
    RAM_S_values_H0 <- RAM_S_values
    RAM_m_values_H0 <- RAM_m_values
    
    RAM_A_values_H0[RAM_A_labels == target.parameters[i]] <- 0
    RAM_S_values_H0[RAM_S_labels == target.parameters[i]] <- 0
    RAM_m_values_H0[RAM_m_labels == target.parameters[i]] <- 0
    
    F_inv_I_A_H0 <- RAM_F_values %*% solve(identity_matrix - RAM_A_values_H0)
    Sigma_H0 <- F_inv_I_A_H0 %*% RAM_S_values_H0 %*% t(F_inv_I_A_H0)
    mu_H0 <- F_inv_I_A_H0 %*% RAM_m_values_H0
    
    # noncentrality parameter
    Sigma_H0_inv <- solve(Sigma_H0)
    Sigma_H0_inv_Sigma_H1 <- Sigma_H0_inv %*% Sigma_H1
    F_H1_H0 <- t(mu_H1 - mu_H0) %*% Sigma_H0_inv %*% (mu_H1 - mu_H0) +
      sum(diag(Sigma_H0_inv_Sigma_H1)) -
      log(det(Sigma_H0_inv_Sigma_H1)) -
      n_ov_time
    
    # prepare outcome
    F_diff$values[i] <- F_H1_H0
    
  }
  
  # return
  F_diff
  
}
