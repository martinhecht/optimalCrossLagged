compute_se_mx <- function(N, timepoints, n_ov, n_process, matrices,
                          target_parameters = NULL) {
  
  # Calculate dimensions and create indices ----
  n_ov_time <- timepoints * n_ov
  n_process_time <- timepoints * n_process
  n_all <- n_ov_time + n_process_time
  
  
  seq_ov <- seq_len(n_ov)
  seq_ov_time <- seq_len(n_ov_time)
  seq_process <- seq_len(n_process)
  seq_timepoints <- seq_len(timepoints)
  
  names_process_time <- paste0(rep(paste0(names_process, "_T"), times = timepoints),
                               rep(seq_len(timepoints), each = n_process))
  #names_ov <- paste0("y", seq_len(n_ov))
  names_ov_time <- paste0(rep(paste0(names_ov, "_T"), times = timepoints),
                          rep(seq_len(timepoints), each = n_ov))
  
  
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
  
  
  # Compute model-implied mean and covariance ----
  B <- solve(diag(1, nrow = n_all) - RAM_A_values)
  E <- B %*% RAM_S_values %*% t(B)
  exp_cov <- RAM_F_values %*% E %*% t(RAM_F_values)
  exp_m <- t(RAM_F_values %*% B %*% RAM_m_values)
  
  rownames(exp_cov) <- colnames(exp_cov) <- colnames(exp_m) <- names_ov_time
  
  
  m <- OpenMx::mxModel(
    type = "RAM",
    manifestVars = names_ov_time,
    latentVars = names_process_time,
    OpenMx::mxMatrix(type = "Full",
                     nrow = n_ov_time,
                     ncol = n_all,
                     free = FALSE,
                     values = RAM_F_values,
                     name = "F"),
    OpenMx::mxMatrix(type = "Full",
                     nrow = n_all,
                     ncol = n_all,
                     free = !is.na(RAM_A_labels),
                     values = RAM_A_values,
                     labels = RAM_A_labels,
                     name = "A"),
    OpenMx::mxMatrix(type = "Symm",
                     nrow = n_all,
                     ncol = n_all,
                     free = !is.na(RAM_S_labels),
                     values = RAM_S_values,
                     labels = RAM_S_labels,
                     name = "S"),
    OpenMx::mxMatrix(type = "Full",
                     nrow = 1,
                     ncol = n_all,
                     free = !is.na(RAM_m_labels),
                     values = t(RAM_m_values),
                     labels = t(RAM_m_labels),
                     name = "M"),
    OpenMx::mxExpectationRAM(A = "A", S = "S", F = "F", M = "M",
                             dimnames = c(names_ov_time, names_process_time)),
    OpenMx::mxFitFunctionML(),
    OpenMx::mxData(observed = exp_cov, type = "cov", means = exp_m, numObs = 100)
  )
  
  m <- OpenMx::mxRun(model = m)
  
  if (is.null(target_parameters)) {
    return(m$output$standardErrors)
  } else {
    m$output$standardErrors[target_parameters, ]
  }
 
}