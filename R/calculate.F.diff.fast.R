## Changelog:
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

calculate.F.diff.fast <- function(timepoints, input_H1, target.parameters = NULL,
                             target.parameters.values.H0 = NULL,
                             return.Sigma = FALSE) {
  
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
  if (!is.null(input_H1$Omega)) {n_parameters <- n_parameters + n_ov_symm}
  if (!is.null(input_H1$Psi)) {n_parameters <- n_parameters + n_ov_symm}
  if (!is.null(input_H1$Theta_I)) {n_parameters <- n_parameters + n_ov_symm}
  if (!is.null(input_H1$Theta_S)) {n_parameters <- n_parameters + n_ov_symm}
  if (!is.null(input_H1$Theta_IS)) {n_parameters <- n_parameters + n_ov_sqr}
  if (!is.null(input_H1$Theta_A)) {n_parameters <- n_parameters + n_ov_symm}
  if (!is.null(input_H1$Theta_B)) {n_parameters <- n_parameters + n_ov_symm}
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
  RAM_F <- matrix(0, nrow = n_ov_time, ncol = n_all)
  RAM_F[id_ov_time, id_ov_time] <- I_n_ov_time
  
  RAM_A <- RAM_S <- matrix(0, nrow = n_all, ncol = n_all)
  
  
  # Populate RAM matrices ----
  # Factor loadings f
  RAM_A[id_ov_time, id_f] <- I_n_ov_time
  
  # Factor loadings f_star
  RAM_A[id_ov_time, id_f_star] <- I_n_ov_time
  
  # Gamma
  if (!is.null(input_H1$Gamma)) {
    for (i in seq_len(n_timepoints - 1)) {
      select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
      RAM_A[select+n_ov, select] <- input_H1$Gamma$values
    }
  }
  
  # Omega
  # TOVAR
  if (!is.null(input_H1$Omega)) {
    RAM_S[id_f_star[1:n_ov], id_f_star[1:n_ov]]  <- 
      matrix(as.vector(solve(I_n_ov2 -
                               kronecker(input_H1$Gamma$values, input_H1$Gamma$values)) %*%
                         t(t(as.vector(input_H1$Omega$values)))),
             nrow = n_ov, ncol = n_ov, byrow = TRUE) 
    for (i in 2:n_timepoints) {
      select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
      RAM_S[select, select] <- input_H1$Omega$values
    }
  }
  
  # Psi
  if (!is.null(input_H1$Psi)) {
    for (i in seq_len(n_timepoints)) {
      select <- id_ov_time[(n_ov*(i-1)+1):(n_ov*i)]
      RAM_S[select, select] <- input_H1$Psi$values
    }
  }
  
  # Theta_I
  if (!is.null(input_H1$Theta_I)) {
    # Loadings
    for (i in seq_along(id_I)) {
      loadings <- rep(0, times = n_ov)
      loadings[i] <- 1
      loadings <- rep(loadings, times = n_timepoints)
      RAM_A[id_f, id_I[i]] <- loadings
    }
    # Covariance matrix
    RAM_S[id_I, id_I] <- input_H1$Theta_I$values
  }
  
  # Theta_S 
  if (!is.null(input_H1$Theta_S)) {
    # Loadings
    for (i in seq_along(id_I)) {
      loadings <- rep(0, times = n_ov_time)
      for (j in seq_len(n_timepoints - 1)) {
        loadings[i+n_ov*j] <- j
      }
      RAM_A[(n_ov_time+1):(2*n_ov_time), id_S[i]] <- loadings
    }
    # Covariance matrix
    RAM_S[id_S, id_S] <- input_H1$Theta_S$values
    
    # Theta_IS
    RAM_S[id_I, id_S] <- input_H1$Theta_IS$values
    RAM_S[id_S, id_I] <- t(input_H1$Theta_IS$values)
  }
  
  # Theta_A
  if (!is.null(input_H1$Theta_A)) {
    # Loadings
    # First time point
    RAM_A[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_A] <- solve(I_n_ov - input_H1$Gamma$values)
    for (i in seq_along((id_A))) {
      # This is probably only correct for univariate models
      loadings <- rep(0, times = n_ov)
      loadings[i] <- 1
      loadings <- rep(loadings, times = n_timepoints - 1)
      RAM_A[id_f_star[-c(1:n_ov)], id_A[i]] <- loadings
    }
    # Covariance matrix
    RAM_S[id_A, id_A] <- input_H1$Theta_A$values
  }
  
  # Theta_B 
  if (!is.null(input_H1$Theta_B)) {
    # Loadings
    # This is probably only correct for univariate models
    RAM_A[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_S] <- -input_H1$Gamma$values %*%
      solve((I_n_ov - input_H1$Gamma$values) %*% (I_n_ov - input_H1$Gamma$values)) 
    for (i in seq_along((id_S))) {
      loadings <- rep(0, times = n_ov_time)
      for (j in seq_len(n_timepoints - 1)) {
        loadings[i+n_ov*j] <- j
      }
      RAM_A[id_f_star, id_B[i]] <- loadings
    }
    # Covariance matrix
    RAM_S[id_B, id_B] <- input_H1$Theta_B$values
    
    # Theta_AB
    RAM_S[id_A, id_B] <- input_H1$Theta_AB$values
    RAM_S[id_B, id_A] <- t(input_H1$Theta_AB$values)
  }
  
  
  # Compute mean vector and covariance matrix under the H1 ---
  F_inv_I_A_H1 <- RAM_F %*% solve(I_n_all - RAM_A)
  Sigma_H1 <- F_inv_I_A_H1 %*% RAM_S %*% t(F_inv_I_A_H1)
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
  
  
  # Loop over free parameters ----
  
  for (par in seq_along(target.parameters)) {
    
    RAM_A_H0 <- RAM_A
    RAM_S_H0 <- RAM_S
    
    
    # Gamma
    if (target.parameters[par] %in% input_H1$Gamma$labels) {
      
      df <- df_H1 + 1 
      
      Gamma_H0 <- input_H1$Gamma$values
      Gamma_H0[input_H1$Gamma$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      for (i in seq_len(n_timepoints - 1)) {
        select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
        RAM_A_H0[select+n_ov, select] <- Gamma_H0
      }
      
      # T0VAR
      if (!is.null(input_H1$Omega)) {
        RAM_S_H0[id_f_star[1:n_ov], id_f_star[1:n_ov]] <-   
          matrix(as.vector(solve(
            I_n_ov2 - 
              kronecker(Gamma_H0, Gamma_H0)) %*%
              t(t(as.vector(input_H1$Omega$values)))),
            nrow = n_ov, ncol = n_ov, byrow = TRUE) 
      }
      
      # Effect of A on the first measurement
      if (!is.null(input_H1$Theta_A)) {
        for (i in seq_along((id_A))) {
          # First time point
          RAM_A_H0[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_A] <-
            solve(I_n_ov - Gamma_H0)
        }
      }
      
      # Effect of B on the first measurement
      if (!is.null(input_H1$Theta_B)) {
        for (i in seq_along((id_S))) {
          RAM_A_H0[(2*n_ov_time+1):(2*n_ov_time+n_ov), id_S] <-
            -Gamma_H0 %*%
            solve((I_n_ov - Gamma_H0) %*% 
                    (I_n_ov - Gamma_H0)) 
        }
      }
      
    } # Gamma ends here
    
    
    # Omega
    # TOVAR
    if (target.parameters[par] %in% input_H1$Omega$labels) {
      Omega_H0 <- input_H1$Omega$values
      Omega_H0[input_H1$Omega$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Omega$labels)) {
        
        df <- df_H1 + n_ov
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Omega$labels))
        Omega_H0[pos_par, ] <- Omega_H0[, pos_par] <- 0
        Omega_H0[pos_par, pos_par] <- 0.001
      } else {
        df <- df_H1 + 1
      }
      
      # T0VAR
      RAM_S_H0[id_f_star[1:n_ov], id_f_star[1:n_ov]] <-   
        matrix(as.vector(solve(
          I_n_ov2 - kronecker(input_H1$Gamma$values, input_H1$Gamma$values)) %*%
            t(t(as.vector(Omega_H0)))),
          nrow = n_ov, ncol = n_ov, byrow = TRUE) 
      for (i in 2:n_timepoints) {
        select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
        RAM_S_H0[select, select] <- Omega_H0
      }
    }
    
    
    # Psi
    if (target.parameters[par] %in% input_H1$Psi$labels) {
      Psi_H0 <- input_H1$Psi$values
      Psi_H0[input_H1$Psi$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Psi$labels)) {
        
        df <- df_H1 + n_ov
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Psi$labels))
        Psi_H0[pos_par, ] <- Psi_H0[, pos_par] <- 0
        Psi_H0[pos_par, pos_par] <- 0.001
      } else {
        df <- df_H1 + 1
      }
      
      for (i in seq_len(n_timepoints)) {
        select <- id_ov_time[(n_ov*(i-1)+1):(n_ov*i)]
        RAM_S_H0[select, select] <- Psi_H0
      }
    }
    
    
    # Create Theta_IS_H0 here, because it can be changed by target.parameters
    # in Theta_I or Theta_S
    if (!is.null(input_H1$Theta_IS)) {
      Theta_IS_H0 <- input_H1$Theta_IS$values
    }
    
    
    # Theta_I
    if (target.parameters[par] %in% input_H1$Theta_I$labels) {
      Theta_I_H0 <- input_H1$Theta_I$values
      Theta_I_H0[input_H1$Theta_I$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Theta_I$labels)) {
        
        if (is.null(input_H1$Theta_S)) {
          df <- df_H1 + n_ov
        } else {
          df <- df_H1 + 2 * n_ov
        }
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Theta_I$labels))
        Theta_I_H0[pos_par, ] <- Theta_I_H0[, pos_par] <- 0
        Theta_I_H0[pos_par, pos_par] <- 0.001
        
        # Effect on Theta_IS
        if (!is.null(input_H1$Theta_IS)) {
          Theta_IS_H0[pos_par, ] <- 0
        }
        
      } else {
        df <- df_H1 + 1
      }
      
      # Covariance matrix
      RAM_S_H0[id_I, id_I] <- Theta_I_H0
    }
    
    
    # Theta_S 
    if (target.parameters[par] %in% input_H1$Theta_S$labels) {
      Theta_S_H0 <- input_H1$Theta_S$values
      Theta_S_H0[input_H1$Theta_S$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Theta_S$labels)) {
        
        if (is.null(input_H1$Theta_I)) {
          df <- df_H1 + n_ov
        } else {
          df <- df_H1 + 2 * n_ov
        }
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Theta_S$labels))
        Theta_S_H0[pos_par, ] <- Theta_S_H0[, pos_par] <- 0
        Theta_S_H0[pos_par, pos_par] <- 0.001
        
        # Effect on Theta_IS
        if (!is.null(input_H1$Theta_IS)) {
          Theta_IS_H0[, pos_par] <- 0
        }
        
      } else {
        df <- df_H1 + 1
      }
      
      # Covariance matrix
      RAM_S_H0[id_S, id_S] <- Theta_S_H0
    }
    
    
    # Theta_IS 
    if (target.parameters[par] %in% input_H1$Theta_IS$labels) {
      Theta_IS_H0[input_H1$Theta_IS$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      df <- df_H1 + 1 
      
      RAM_S_H0[id_I, id_S] <- Theta_IS_H0
      RAM_S_H0[id_S, id_I] <- t(Theta_IS_H0)
    }
    
    
    # Create Theta_AB_H0 here, because it can be changed by target.parameters
    # in Theta_A or Theta_B
    if (!is.null(input_H1$Theta_AB)) {
      Theta_AB_H0 <- input_H1$Theta_AB$values
    }
    
    
    # Theta_A
    if (target.parameters[par] %in% input_H1$Theta_A$labels) {
      Theta_A_H0 <- input_H1$Theta_A$values
      Theta_A_H0[input_H1$Theta_A$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Theta_A$labels)) {
        
        if (is.null(input_H1$Theta_B)) {
          df <- df_H1 + n_ov
        } else {
          df <- df_H1 + 2* n_ov
        }
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Theta_A$labels))
        Theta_A_H0[pos_par, ] <- Theta_A_H0[, pos_par] <- 0
        Theta_A_H0[pos_par, pos_par] <- 0.001
        
        # Effect on Theta_AB
        if (!is.null(input_H1$Theta_AB)) {
          Theta_AB_H0[pos_par, ] <- 0
        }
        
      } else {
        df <- df_H1 + 1
      }
      
      RAM_S_H0[id_A, id_A] <- Theta_A_H0
    }
    
    
    # Theta_B 
    if (target.parameters[par] %in% input_H1$Theta_B$labels) {
      Theta_B_H0 <- input_H1$Theta_B$values
      Theta_B_H0[input_H1$Theta_B$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      # Adjustment for testing a variance parameter
      if (target.parameters[par] %in% diag(input_H1$Theta_B$labels)) {
        
        if (is.null(input_H1$Theta_A)) {
          df <- df_H1 + n_ov
        } else {
          df <- df_H1 + 2* n_ov
        }
        
        pos_par <- which(target.parameters[par] == diag(input_H1$Theta_B$labels))
        Theta_B_H0[pos_par, ] <- Theta_B_H0[, pos_par] <- 0
        Theta_B_H0[pos_par, pos_par] <- 0.001
        
        # Effect on Theta_AB
        if (!is.null(input_H1$Theta_AB)) {
          Theta_AB_H0[, pos_par] <- 0
        }
        
      } else {
        df <- df_H1 + 1
      }
      
      RAM_S_H0[id_B, id_B] <- Theta_B_H0
    } 
    
    
    # Theta_AB
    if (target.parameters[par] %in% input_H1$Theta_AB$labels) {
      Theta_AB_H0[input_H1$Theta_AB$labels == target.parameters[par]] <-
        target.parameters.values.H0[par]
      
      df <- df_H1 + 1 
      
      RAM_S_H0[id_A, id_B] <- Theta_AB_H0
      RAM_S_H0[id_B, id_A] <- t(Theta_AB_H0)
    }
    
    
    F_inv_I_A_H0 <- RAM_F %*% solve(I_n_all - RAM_A_H0)
    Sigma_H0 <- F_inv_I_A_H0 %*% RAM_S_H0 %*% t(F_inv_I_A_H0)
    Sigma_H0_inv <- solve(Sigma_H0)
    Sigma_H0_inv_Sigma_H1 <- Sigma_H0_inv %*% Sigma_H1
    
    # Noncentrality parameter
    FF_H1_H0 <- sum(diag(Sigma_H0_inv_Sigma_H1)) -
      log(det(Sigma_H0_inv_Sigma_H1)) -
      n_ov_time
    
    # Populate output 
    
    res$values[par] <- FF_H1_H0
    #res$df[par] <- df
    # MH 0.0.21 2022-07-24 
	if( return.Sigma ){
		res$Sigma_H0[[par]] <- Sigma_H0
		names( res$Sigma_H0 ) <- target.parameters
    }
	
  }
  
  res
  
}
