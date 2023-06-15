## Changelog:
# MA 0.1.75 2023-06-15: redefined OpenMx functions to my_mx functions
# MH/MA 0.1.74 2023-06-12: "OpenMx::" added as prefix to function calls
# MA 0.1.73 2023-06-12: initial programming

## Documentation
#' @title Difference between Fit Functions
#' @description Calculate differences between the fit function of the model
#' under the H1 and the fit function of the model under the H0.
#' @param
#' @param
#' @param
#' @return
#' @keywords internal

calculate.F.diff.precise <- function(timepoints, input_H1,
                                     target.parameters = NULL, 
                                     target.parameters.values.H0 = NULL,
                                     return.Sigma = FALSE,
                                     N) {
  
  # Dimensions and indices for RAM matrices ----
  n_ov <- input_H1$n_ov
  n_timepoints <- timepoints
  n_ov_time <- n_timepoints * n_ov
  n_all <- n_ov * (3 * n_timepoints + 4)
  n_tests <- length(target.parameters)
  
  # Test 0 for default
  if (is.null(target.parameters.values.H0)) {
    target.parameters.values.H0 <- rep(0, times = n_tests)
  }
  
  # number of parameters 
  n_parameters <- 0
  n_ov_sqr <- n_ov^2
  n_ov_symm <- n_ov * (n_ov + 1) / 2
  
  if (!is.null(input_H1$Gamma)) {n_parameters <- n_parameters + n_ov_sqr}
  if (!is.null(input_H1$Omega)) {n_parameters <- n_parameters + 2 * n_ov_symm} # Gamma plus T0
  if (!is.null(input_H1$Psi)) {n_parameters <- n_parameters + n_ov_symm}
  if (!is.null(input_H1$Theta_I)) {n_parameters <- n_parameters + n_ov_symm}
  if (!is.null(input_H1$Theta_S)) {n_parameters <- n_parameters + n_ov_symm}
  if (!is.null(input_H1$Theta_IS)) {n_parameters <- n_parameters + n_ov_sqr}
  if (!is.null(input_H1$Theta_A)) {n_parameters <- n_parameters + n_ov_symm + n_ov_sqr} # plus T0
  if (!is.null(input_H1$Theta_B)) {n_parameters <- n_parameters + n_ov_symm + n_ov_sqr} # plus T0 
  if (!is.null(input_H1$Theta_AB)) {n_parameters <- n_parameters + n_ov_sqr}
  
  
  # Indices
  id_ov <- seq_len(n_ov)
  id_ov_time <- seq_len(n_ov_time)
  id_f <- (n_ov_time+1):(2*n_ov_time)
  id_f_star <- (2*n_ov_time+1):(3*n_ov_time)
  id_I <- (3*n_ov_time+1):(3*n_ov_time+n_ov)
  id_S <- (3*n_ov_time+n_ov+1):(3*n_ov_time+2*n_ov)
  id_A <- (3*n_ov_time+2*n_ov+1):(3*n_ov_time+3*n_ov)
  id_B <- (3*n_ov_time+3*n_ov+1):(3*n_ov_time+4*n_ov)
  
  
  # Identity matrices
  I_n_ov <- diag(1, nrow = n_ov)
  I_n_ov2 <- diag(1, nrow = n_ov^2)
  I_n_ov_time <- diag(1, nrow = n_ov_time)
  I_n_all <- diag(1, nrow = n_all)
  
  
  
  
  # Create RAM matrices ----
  RAM_F_values <- matrix(0, nrow = n_ov_time, ncol = n_all)
  RAM_F_values[id_ov_time, id_ov_time] <- I_n_ov_time
  
  RAM_A_values <- RAM_S_values <- matrix(0, nrow = n_all, ncol = n_all)
  RAM_A_labels <- RAM_S_labels <- matrix(NA, nrow = n_all, ncol = n_all)
  RAM_A_free <- RAM_S_free <- matrix(FALSE, nrow = n_all, ncol = n_all)
  
  
  
  # Matrices under the H1 ----
  # Factor loadings f
  RAM_A_values[id_ov_time, id_f] <- I_n_ov_time
  
  # Factor loadings f_star
  RAM_A_values[id_ov_time, id_f_star] <- I_n_ov_time
  
  # Gamma
  if (!is.null(input_H1$Gamma)) {
    for (i in seq_len(n_timepoints - 1)) {
      select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
      RAM_A_values[select+n_ov, select] <- input_H1$Gamma$values
      RAM_A_labels[select+n_ov, select] <- input_H1$Gamma$labels
      RAM_A_free[select+n_ov, select] <- TRUE
    }
  }
  
  # Omega
  # TOVAR
  if (!is.null(input_H1$Omega)) {
    
    ## T0 Var
    RAM_S_values[id_f_star[1:n_ov], id_f_star[1:n_ov]]  <- 
      matrix(as.vector(solve(I_n_ov2 -
                               kronecker(input_H1$Gamma$values, input_H1$Gamma$values)) %*%
                         t(t(as.vector(input_H1$Omega$values)))),
             nrow = n_ov, ncol = n_ov, byrow = TRUE) 
    
    for (i in 1:n_ov) {
      for (j in 1:n_ov) {
        if (j > i) next
        if (i == j) {
          RAM_S_labels[id_f_star[1:n_ov], id_f_star[1:n_ov]][i, j] <- 
            paste0("T0_Omega_", i, "_", j)
        } else {
          RAM_S_labels[id_f_star[1:n_ov], id_f_star[1:n_ov]][i, j] <-
            RAM_S_labels[id_f_star[1:n_ov], id_f_star[1:n_ov]][j, i] <-
            paste0("T0_Omega_", i, "_", j)
        }
        
      }
    }
    
    RAM_S_free[id_f_star[1:n_ov], id_f_star[1:n_ov]] <- TRUE
    
    ## other measurements
    for (i in 2:n_timepoints) {
      select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
      RAM_S_values[select, select] <- input_H1$Omega$values
      RAM_S_labels[select, select] <- input_H1$Omega$labels
      RAM_S_free[select, select] <- TRUE
    }
  }
  
  # Psi
  if (!is.null(input_H1$Psi)) {
    for (i in seq_len(n_timepoints)) {
      select <- id_ov_time[(n_ov*(i-1)+1):(n_ov*i)]
      RAM_S_values[select, select] <- input_H1$Psi$values
      RAM_S_labels[select, select] <- input_H1$Psi$labels
      RAM_S_free[select, select] <- TRUE
    }
  }
  
  # Theta_I
  if (!is.null(input_H1$Theta_I)) {
    # Loadings
    for (i in seq_along(id_I)) {
      loadings <- rep(0, times = n_ov)
      loadings[i] <- 1
      loadings <- rep(loadings, times = n_timepoints)
      RAM_A_values[id_f, id_I[i]] <- loadings
    }
    # Covariance matrix
    RAM_S_values[id_I, id_I] <- input_H1$Theta_I$values
    RAM_S_labels[id_I, id_I] <- input_H1$Theta_I$labels
    RAM_S_free[id_I, id_I] <- TRUE
  }
  
  # Theta_S 
  if (!is.null(input_H1$Theta_S)) {
    # Loadings
    for (i in seq_along(id_I)) {
      loadings <- rep(0, times = n_ov_time)
      for (j in seq_len(n_timepoints - 1)) {
        loadings[i+n_ov*j] <- j
      }
      RAM_A_values[(n_ov_time+1):(2*n_ov_time), id_S[i]] <- loadings
    }
    # Covariance matrix
    RAM_S_values[id_S, id_S] <- input_H1$Theta_S$values
    RAM_S_labels[id_S, id_S] <- input_H1$Theta_S$labels
    RAM_S_free[id_S, id_S] <- TRUE
    
    # Theta_IS
    RAM_S_values[id_I, id_S] <- input_H1$Theta_IS$values
    RAM_S_labels[id_I, id_S] <- input_H1$Theta_IS$labels
    RAM_S_free[id_I, id_S] <- TRUE
    RAM_S_values[id_S, id_I] <- t(input_H1$Theta_IS$values)
    RAM_S_labels[id_S, id_I] <- t(input_H1$Theta_IS$labels)
    RAM_S_free[id_S, id_I] <- TRUE
  }
  
  # Theta_A
  if (!is.null(input_H1$Theta_A)) {
    # Loadings
    # First time point
    RAM_A_values[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_A] <- solve(I_n_ov - input_H1$Gamma$values)
    
    T0_Theta_A_cov <- matrix(NA, nrow = n_ov, ncol = n_ov)
    for (i in 1:n_ov) {
      for (j in 1:n_ov) {
        T0_Theta_A_cov[i, j] <- paste0("T0_eta_", i, "_A_", j)
      }
    }
    
    RAM_S_labels[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_A] <- T0_Theta_A_cov
    RAM_S_free[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_A] <- TRUE
    RAM_S_labels[id_A, (2*n_ov_time+1):(2*n_ov_time+n_ov)] <- t(T0_Theta_A_cov)
    RAM_S_free[id_A, (2*n_ov_time+1):(2*n_ov_time+n_ov)] <- TRUE
    
    for (i in seq_along((id_A))) {
      # This is probably only correct for univariate models
      loadings <- rep(0, times = n_ov)
      loadings[i] <- 1
      loadings <- rep(loadings, times = n_timepoints - 1)
      RAM_A_values[id_f_star[-c(1:n_ov)], id_A[i]] <- loadings
    }
    
    # Covariance matrix
    RAM_S_values[id_A, id_A] <- input_H1$Theta_A$values
    RAM_S_labels[id_A, id_A] <- input_H1$Theta_A$labels
    RAM_S_free[id_A, id_A] <- TRUE
  }
  
  # Theta_B 
  if (!is.null(input_H1$Theta_B)) {
    # Loadings
    # This is probably only correct for univariate models
    RAM_A_values[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_B] <- -input_H1$Gamma$values %*%
      solve((I_n_ov - input_H1$Gamma$values) %*% (I_n_ov - input_H1$Gamma$values)) 
    
    T0_Theta_B_cov <- matrix(NA, nrow = n_ov, ncol = n_ov)
    for (i in 1:n_ov) {
      for (j in 1:n_ov) {
        T0_Theta_B_cov[i, j] <- paste0("T0_eta_", i, "_B_", j)
      }
    }
    
    RAM_S_labels[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_B] <- T0_Theta_B_cov
    RAM_S_free[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_B] <- TRUE
    RAM_S_labels[id_B, (2*n_ov_time+1):(2*n_ov_time+n_ov)] <- t(T0_Theta_B_cov)
    RAM_S_free[id_B, (2*n_ov_time+1):(2*n_ov_time+n_ov)] <- TRUE
    
    for (i in seq_along((id_B))) {
      loadings <- rep(0, times = n_ov_time)
      for (j in seq_len(n_timepoints - 1)) {
        loadings[i+n_ov*j] <- j
      }
      RAM_A_values[id_f_star, id_B[i]] <- loadings
    }
    # Covariance matrix
    RAM_S_values[id_B, id_B] <- input_H1$Theta_B$values
    RAM_S_labels[id_B, id_B] <- input_H1$Theta_B$labels
    RAM_S_free[id_B, id_B] <- TRUE
    
    # Theta_AB
    RAM_S_values[id_A, id_B] <- input_H1$Theta_AB$values
    RAM_S_labels[id_A, id_B] <- input_H1$Theta_AB$labels
    RAM_S_free[id_A, id_B] <- TRUE
    RAM_S_values[id_B, id_A] <- t(input_H1$Theta_AB$values)
    RAM_S_labels[id_B, id_A] <- t(input_H1$Theta_AB$labels)
    RAM_S_free[id_B, id_A] <- TRUE
  }
  
  
  # Compute mean vector and covariance matrix under the H1 ---
  F_inv_I_A_H1 <- RAM_F_values %*% solve(I_n_all - RAM_A_values)
  Sigma_H1 <- F_inv_I_A_H1 %*% RAM_S_values %*% t(F_inv_I_A_H1)
  df_H1 <- n_ov_time * (n_ov_time + 1) / 2 - n_parameters
  
  
  
  # Populate output with H1 model ----
  
  res <- list(values = rep(NA, times = n_tests),
              #df = rep(NA, times = n_tests))
              df = 1)
  # MH 0.0.21 2022-07-24 
  if( return.Sigma ){             
    res <- c( res, list( Sigma_H1 = Sigma_H1,
                         Sigma_H0 = replicate(n = n_tests, expr = matrix(NA),
                                              simplify = FALSE)) )
  }
  
  
  # H0: Loop over free parameters ----
  
  for (par in seq_along(target.parameters)) {
    
    RAM_A_H0_values <- RAM_A_values
    RAM_A_H0_free <- RAM_A_free
    RAM_S_H0_values <- RAM_S_values
    RAM_S_H0_free <- RAM_S_free
    
    
    # Gamma
    if (target.parameters[par] %in% input_H1$Gamma$labels) {
      
      df <- df_H1 + 1 
      
      Gamma_H0_values <- input_H1$Gamma$values
      Gamma_H0_values[input_H1$Gamma$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      Gamma_H0_free <- matrix(TRUE, nrow = n_ov, ncol = n_ov)
      Gamma_H0_free[input_H1$Gamma$labels == target.parameters[par]] <- FALSE
      
      for (i in seq_len(n_timepoints - 1)) {
        select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
        RAM_A_H0_values[select+n_ov, select] <- Gamma_H0_values
        RAM_A_H0_free[select+n_ov, select] <- Gamma_H0_free
      }
    } 
    
    
    # Omega
    # TOVAR
    if (target.parameters[par] %in% input_H1$Omega$labels) {
      Omega_H0_values <- input_H1$Omega$values
      Omega_H0_values[input_H1$Omega$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      Omega_H0_free <- matrix(TRUE, nrow = n_ov, ncol = n_ov)
      Omega_H0_free[input_H1$Omega$labels == target.parameters[par]] <- FALSE
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Omega$labels)) {
        
        df <- df_H1 + n_ov
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Omega$labels))
        Omega_H0_values[pos_par, ] <- Omega_H0_values[, pos_par] <- 0
        Omega_H0[pos_par, pos_par] <- 0.001
        Omega_H0_free[pos_par, pos_par] <- Omega_H0_free[pos_par, ] <- 
          Omega_H0_free[, pos_par] <- FALSE
      } else {
        df <- df_H1 + 1
      }
      
      for (i in 2:n_timepoints) {
        select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
        RAM_S_H0_values[select, select] <- Omega_H0_values
        RAM_S_H0_free[select, select] <- Omega_H0_free
      }
    }
    
    
    # Psi
    if (target.parameters[par] %in% input_H1$Psi$labels) {
      Psi_H0_values <- input_H1$Psi$values
      Psi_H0[input_H1$Psi$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      Psi_H0_free <- matrix(TRUE, nrow = n_ov, ncol = n_ov)
      Psi_H0_free[input_H1$Psi$labels == target.parameters[par]] <- FALSE
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Psi$labels)) {
        
        df <- df_H1 + n_ov
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Psi$labels))
        Psi_H0_values[pos_par, ] <- Psi_H0_values[, pos_par] <- 0
        Psi_H0_values[pos_par, pos_par] <- 0.001
        Psi_H0_free[pos_par, pos_par] <- Psi_H0_free[pos_par, ] <-
          Psi_H0_free[, pos_par] <- FALSE
      } else {
        df <- df_H1 + 1
      }
      
      for (i in seq_len(n_timepoints)) {
        select <- id_ov_time[(n_ov*(i-1)+1):(n_ov*i)]
        RAM_S_H0_values[select, select] <- Psi_H0_values
        RAM_S_H0_free[select, select] <- Psi_H0_free
      }
    }
    
    
    # Theta_I
    if (target.parameters[par] %in% input_H1$Theta_I$labels) {
      Theta_I_H0_values <- input_H1$Theta_I$values
      Theta_I_H0_values[input_H1$Theta_I$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      Theta_I_H0_free <- matrix(TRUE, nrow = n_ov, ncol = n_ov)
      Theta_I_H0_free[input_H1$Theta_I$labels == target.parameters[par]] <- FALSE
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Theta_I$labels)) {
        
        if (is.null(input_H1$Theta_S)) {
          df <- df_H1 + n_ov
        } else {
          df <- df_H1 + 2 * n_ov
        }
        
        # AB covariance matrix
        if (!is.null(input_H1$Theta_S)) {
          df <- df + n_ov
          RAM_S_H0_values[id_I, id_S][pos_par, ] <- 0
          RAM_S_H0_values[id_S, id_I][, pos_par] <- 0
          RAM_S_H0_free[id_I, id_S][pos_par, ] <- FALSE
          RAM_S_H0_free[id_S, id_I][, pos_par] <- FALSE
        }
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Theta_I$labels))
        Theta_I_H0_values[pos_par, ] <- Theta_I_H0_values[, pos_par] <- 0
        Theta_I_H0_values[pos_par, pos_par] <- 0.001
        Theta_I_H0_free[pos_par, pos_par] <- Theta_I_H0_free[pos_par, ] <-
          Theta_I_H0_free[, pos_par] <- FALSE
      } else {
        df <- df_H1 + 1
      }
      
      # Covariance matrix
      RAM_S_H0_values[id_I, id_I] <- Theta_I_H0_values
      RAM_S_H0_free[id_I, id_I] <- Theta_I_H0_free
    }
    
    
    # Theta_S 
    if (target.parameters[par] %in% input_H1$Theta_S$labels) {
      Theta_S_H0_values <- input_H1$Theta_S$values
      Theta_S_H0_values[input_H1$Theta_S$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      Theta_S_H0_free <- matrix(TRUE, nrow = n_ov, ncol = n_ov)
      Theta_S_H0_free[input_H1$Theta_S$labels == target.parameters[par]] <- FALSE
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Theta_S$labels)) {
        
        if (is.null(input_H1$Theta_I)) {
          df <- df_H1 + n_ov
        } else {
          df <- df_H1 + 2 * n_ov
        }
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Theta_S$labels))
        
        # IS covariance matrix
        RAM_S_H0_values[id_I, id_S][, pos_par] <- 0
        RAM_S_H0_values[id_S, id_I][pos_par, ] <- 0
        RAM_S_H0_free[id_I, id_S][, pos_par] <- FALSE
        RAM_S_H0_free[id_S, id_I][pos_par, ] <- FALSE
        
        # Theta_S
        Theta_S_H0_values[pos_par, ] <- Theta_S_H0_values[, pos_par] <- 0
        Theta_S_H0_values[pos_par, pos_par] <- 0.001
        Theta_S_H0_free[pos_par, pos_par] <- Theta_S_H0_free[pos_par, ] <- 
          Theta_S_H0_free[, pos_par] <- FALSE
      } else {
        df <- df_H1 + 1
      }
      
      # Covariance matrix
      RAM_S_H0_values[id_S, id_S] <- Theta_S_H0_values
      RAM_S_H0_free[id_S, id_S] <- Theta_S_H0_free
    }
    
    
    # Theta_IS 
    if (target.parameters[par] %in% input_H1$Theta_IS$labels) {
      
      df <- df_H1 + 1 
      
      Theta_IS_H0_values <- input_H1$Theta_IS$values
      Theta_IS_H0_values[input_H1$Theta_IS$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      Theta_IS_H0_free <- matrix(TRUE, nrow = n_ov, ncol = n_ov)
      Theta_IS_H0_free[input_H1$Theta_IS$labels == target.parameters[par]] <- FALSE
      
      RAM_S_H0_values[id_I, id_S] <- Theta_IS_H0_values
      RAM_S_H0_values[id_S, id_I] <- t(Theta_IS_H0_values)
      
      RAM_S_H0_free[id_I, id_S] <- Theta_IS_H0_free
      RAM_S_H0_free[id_S, id_I] <- t(Theta_IS_H0_free)
    }
    
    
    # Theta_A
    if (target.parameters[par] %in% input_H1$Theta_A$labels) {
      Theta_A_H0_values <- input_H1$Theta_A$values
      Theta_A_H0_values[input_H1$Theta_A$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      Theta_A_H0_free <- matrix(TRUE, nrow = n_ov, ncol = n_ov)
      Theta_A_H0_free[input_H1$Theta_A$labels == target.parameters[par]] <- FALSE
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Theta_A$labels)) {
        
        if (is.null(input_H1$Theta_B)) {
          df <- df_H1 + 2 * n_ov
        } else {
          df <- df_H1 + 3 * n_ov
        }
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Theta_A$labels))
        
        # First time point
        RAM_S_H0_values[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_A][, pos_par] <- 0
        RAM_S_H0_values[id_A, (2*n_ov_time+1):(2*n_ov_time+n_ov)][pos_par, ] <- 0
        
        RAM_S_H0_free[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_A][, pos_par] <- FALSE
        RAM_S_H0_free[id_A, (2*n_ov_time+1):(2*n_ov_time+n_ov)][pos_par, ] <- FALSE
        
        # AB covariance matrix
        if (!is.null(input_H1$Theta_B)) {
          df <- df + n_ov
          RAM_S_H0_values[id_A, id_B][pos_par, ] <- 0
          RAM_S_H0_values[id_B, id_A][, pos_par] <- 0
          RAM_S_H0_free[id_A, id_B][pos_par, ] <- FALSE
          RAM_S_H0_free[id_B, id_A][, pos_par] <- FALSE
        }
        
        
        Theta_A_H0_values[pos_par, ] <- Theta_A_H0_values[, pos_par] <- 0
        Theta_A_H0_values[pos_par, pos_par] <- 0.001
        Theta_A_H0_free[pos_par, pos_par] <- Theta_A_H0_free[pos_par, ] <-
          Theta_A_H0_free[, pos_par] <- FALSE
      } else {
        df <- df_H1 + 1
      }
      
      RAM_S_H0_values[id_A, id_A] <- Theta_A_H0_values
      RAM_S_H0_free[id_A, id_A] <- Theta_A_H0_free
    }
    
    
    # Theta_B 
    if (target.parameters[par] %in% input_H1$Theta_B$labels) {
      Theta_B_H0_values <- input_H1$Theta_B$values
      Theta_B_H0_values[input_H1$Theta_B$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      Theta_B_H0_free <- matrix(TRUE, nrow = n_ov, ncol = n_ov)
      Theta_B_H0_free[input_H1$Theta_B$labels == target.parameters[par]] <- FALSE
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Theta_B$labels)) {
        
        if (is.null(input_H1$Theta_A)) {
          df <- df_H1 + 2 * n_ov
        } else {
          df <- df_H1 + 3 * n_ov
        }
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Theta_A$labels))
        
        # First time point
        RAM_S_H0_values[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_B][, pos_par] <- 0
        RAM_S_H0_values[id_B, (2*n_ov_time+1):(2*n_ov_time+n_ov)][pos_par, ] <- 0
        
        RAM_S_H0_free[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_B][, pos_par] <- FALSE
        RAM_S_H0_free[id_B, (2*n_ov_time+1):(2*n_ov_time+n_ov)][pos_par, ] <- FALSE
        
        # AB covariance matrix
        RAM_S_H0_values[id_A, id_B][, pos_par] <- 0
        RAM_S_H0_values[id_B, id_A][pos_par, ] <- 0
        RAM_S_H0_free[id_A, id_B][, pos_par] <- FALSE
        RAM_S_H0_free[id_B, id_A][pos_par, ] <- FALSE
        
        # Adjust B matrix
        Theta_B_H0_values[pos_par, ] <- Theta_B_H0_values[, pos_par] <- 0
        Theta_B_H0_values[pos_par, pos_par] <- 0.001
        Theta_B_H0_free[pos_par, pos_par] <- Theta_B_H0_free[pos_par, ] <-
          Theta_B_H0_free[, pos_par] <- FALSE
      } else {
        df <- df_H1 + 1
      }
      
      RAM_S_H0_values[id_S, id_S] <- Theta_B_H0_values
      RAM_S_H0_free[id_S, id_S] <- Theta_B_H0_free
    }
    
    
    # Theta_AB
    if (target.parameters[par] %in% input_H1$Theta_AB$labels) {
      
      df <- df_H1 + 1 
      
      Theta_AB_H0_values <- input_H1$Theta_AB$values
      Theta_AB_H0_values[input_H1$Theta_AB$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      Theta_AB_H0_free <- matrix(TRUE, nrow = n_ov, ncol = n_ov)
      Theta_AB_H0_free[input_H1$Theta_AB$labels == target.parameters[par]] <- FALSE
      
      RAM_S_H0_values[id_A, id_B] <- Theta_AB_H0_values
      RAM_S_H0_values[id_B, id_A] <- t(Theta_AB_H0_values)
      RAM_S_H0_free[id_A, id_B] <- Theta_AB_H0_free
      RAM_S_H0_free[id_B, id_A] <- t(Theta_AB_H0_free)
    }
    
    
    
    # Fitting the model ----
    man_vars <- paste0("x", 1:n_ov_time)
    lat_vars <- paste0("l", 1:(n_all - n_ov_time))
    
    dimnames(Sigma_H1) <- list(man_vars, man_vars)
    
    browser()
    
    m <- my_mxModel(model = "H0",
                 my_mxMatrix(type = "Full",
                          free = RAM_A_H0_free,
                          values = RAM_A_H0_values,
                          labels = RAM_A_labels,
                          name = "A"),
                 my_mxMatrix(type = "Symm",
                          free = RAM_S_H0_free,
                          values = round(RAM_S_H0_values, 7),
                          labels = RAM_S_labels,
                          name = "S"),
                 my_mxMatrix(type = "Full",
                          free = FALSE,
                          values = RAM_F_values,
                          name = "F"),
                 my_mxExpectationRAM("A", "S", "F",
                                  dimnames = c(man_vars, lat_vars)),
                 my_mxData(observed = Sigma_H1, type = "cov", numObs = N),
                 my_mxFitFunctionML())
    
    fit <- my_mxRun(m, silent = TRUE)
    
    Sigma_H0 <- my_mxGetExpected(model = fit, component = "covariance")
    
    lambda <- F_ML(S = Sigma_H1, Sigma = Sigma_H0, N = N)
    
    
    
    # Populate output ----
    
    res$values[par] <- lambda
    #res$df[par] <- df
    # MH 0.0.21 2022-07-24 
    if( return.Sigma ){
      res$Sigma_H0[[par]] <- Sigma_H0
      names( res$Sigma_H0 ) <- target.parameters
    }
    
  }
  
  res
  
}
