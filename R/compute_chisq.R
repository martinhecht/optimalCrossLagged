compute_chisq <- function(input_H1, input_H0, alpha) {
  
  # Dimensions and indices for RAM matrices ----
  n_ov <- input_H1$n_ov
  n_timepoints <- input_H1$timepoints
  n_ov_time <- n_timepoints * n_ov
  n_all <- n_ov * (3 * n_timepoints + 4)
  n_tests <- length(input_H0$matrices)
  
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
  if (!is.null(input_H1$Gamma_star)) {n_parameters <- n_parameters + n_ov_sqr}
  
  
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
      RAM_A[select+n_ov, select] <- input_H1$Gamma
    }
  }
  
  # Omega
  # TOVAR
  if (!is.null(input_H1$Omega)) {
    RAM_S[id_f_star[1:n_ov], id_f_star[1:n_ov]]  <- solve(
      I_n_ov - input_H1$Gamma %*% input_H1$Gamma) %*% input_H1$Omega
    for (i in 2:n_timepoints) {
      select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
      RAM_S[select, select] <- input_H1$Omega
    }
  }
  
  # Psi
  if (!is.null(input_H1$Psi)) {
    for (i in seq_len(n_timepoints)) {
      select <- id_ov_time[(n_ov*(i-1)+1):(n_ov*i)]
      RAM_S[select, select] <- input_H1$Psi
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
    RAM_S[id_I, id_I] <- input_H1$Theta_I
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
    RAM_S[id_S, id_S] <- input_H1$Theta_S
    
    # Theta_IS
    RAM_S[id_I, id_S] <- input_H1$Theta_IS
    RAM_S[id_S, id_I] <- t(input_H1$Theta_IS)
  }
  
  # Theta_A
  if (!is.null(input_H1$Theta_A)) {
    # Loadings
    for (i in seq_along((id_A))) {
      # This is probably only correct for univariate models
      RAM_A[2*n_ov_time+i, id_A[i]] <- 1 / (1 - diag(input_H1$Gamma)[i])
      loadings <- rep(0, times = n_ov)
      loadings[i] <- 1
      loadings <- rep(loadings, times = n_timepoints - 1)
      RAM_A[id_f_star[-c(1:n_ov)], id_A[i]] <- loadings
    }
    # Covariance matrix
    RAM_S[id_A, id_A] <- input_H1$Theta_I
  }
  
  # Theta_B 
  if (!is.null(input_H1$Theta_S)) {
    # Loadings
    # This is probably only correct for univariate models
    for (i in seq_along((id_S))) {
      RAM_A[2*n_ov_time+i, id_S[i]] <- -input_H1$Gamma[i, i] / 
        (1 - input_H1$Gamma[i, i])^2
      loadings <- rep(0, times = n_ov_time)
      for (j in seq_len(n_timepoints - 1)) {
        loadings[i+n_ov*j] <- j
      }
      RAM_A[(n_ov_time+1):(2*n_ov_time), id_B[i]] <- loadings
    }
    # Covariance matrix
    RAM_S[id_S, id_S] <- input_H1$Theta_S
    
    # Theta_AB
    RAM_S[id_A, id_B] <- input_H1$Theta_AB
    RAM_S[id_B, id_A] <- t(input_H1$Theta_AB)
  }
  
  
  # Compute mean vector and covariance matrix under the H1 ---
  F_inv_I_A_H1 <- RAM_F %*% solve(I_n_all - RAM_A)
  Sigma_H1 <- F_inv_I_A_H1 %*% RAM_S %*% t(F_inv_I_A_H1)
  df_H1 <- n_ov_time * (n_ov_time + 1) / 2 - n_parameters
  
  # Critical value
  ## Assuming mean structure
  critical_value <- qchisq(p = 1 - alpha,
                           df = df_H1)
  
  
  
  # Populate output with H1 model ----
  
  res <- list(H1 = list(critical_value = critical_value,
                        df_H1 = df_H1,
                        Sigma_H1 = Sigma_H1),
              H0 = list(n_tests = n_tests,
                        FF_H1_H0 = rep(NA, times = n_tests),
                        Sigma_H0 = replicate(n = n_tests, expr = matrix(NA), simplify = FALSE),
                        N = NA,
                        power = rep(NA, times = n_tests)))
  
  
  # Loop over free parameters ----
  
  for (par in seq_len(length(input_H0$matrices))) {
    
    RAM_A_H0 <- RAM_A
    RAM_S_H0 <- RAM_S
    
    # Gamma
    if (input_H0$building_block[[par]] == "Gamma") {
      for (i in seq_len(n_timepoints - 1)) {
        select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
        RAM_A_H0[select+n_ov, select] <- input_H0$matrices[[par]]
      }
      
      # T0VAR
      RAM_S_H0[id_f_star[1:n_ov], id_f_star[1:n_ov]]  <- solve(
        I_n_ov - input_H0$matrices[[par]] %*% input_H0$matrices[[par]]) %*%
        input_H1$Omega
      
      # Effect of A on the first measurement
      if (!is.null(input_H1$Theta_A)) {
        for (i in seq_along((id_A))) {
          # This is probably only correct for univariate models
          RAM_A_H0[2*n_ov_time+i, id_A[i]] <- 1 / (1 - diag(input_H0$matrices[[par]])[i])
        }
      }
      
      # Effect of B on the first measurement
      if (!is.null(input_H1$Theta_B)) {
        for (i in seq_along((id_S))) {
          RAM_A_H0[2*n_ov_time+i, id_S[i]] <- -input_H0$matrices[[par]][i, i] / 
            (1 - input_H0$matrices[[par]][i, i])^2
        }
      }
      
    } # Gamma ends here
    
    # Omega
    # TOVAR
    if (input_H0$building_block[[par]] == "Omega") {
      # T0VAR
      RAM_S_H0[id_f_star[1:n_ov], id_f_star[1:n_ov]]  <- solve(
        I_n_ov - input_H1$Gamma %*% input_H1$Gamma) %*% input_H0$matrices[[par]]
      for (i in 2:n_timepoints) {
        select <- id_f_star[(n_ov*(i-1)+1):(n_ov*i)]
        RAM_S_H0[select, select] <- input_H0$matrices[[par]]
      }
    }
    
    # Psi
    if (input_H0$building_block[[par]] == "Psi") {
      for (i in seq_len(n_timepoints)) {
        select <- id_ov_time[(n_ov*(i-1)+1):(n_ov*i)]
        RAM_S_H0[select, select] <- input_H0$matrices[[par]]
      }
    }
    
    # Theta_I
    if (input_H0$building_block[[par]] == "Theta_I") {
      # Covariance matrix
      RAM_S_H0[id_I, id_I] <- input_H0$matrices[[par]]
    }
    
    # Theta_S 
    if (input_H0$building_block[[par]] == "Theta_I") {
      # Covariance matrix
      RAM_S_H0[id_S, id_S] <- input_H0$matrices[[par]]
    }
    
    # Theta_IS 
    if (input_H0$building_block[[par]] == "Theta_I") {
      RAM_S_H0[id_I, id_S] <- input_H0$matrices[[par]]
      RAM_S_H0[id_S, id_I] <- t(input_H0$matrices[[par]])
    }
    
    # Theta_A
    if (input_H0$building_block[[par]] == "Theta_A") {
      RAM_S_H0[id_A, id_A] <- input_H0$matrices[[par]]
    }
    
    # Theta_B 
    if (input_H0$building_block[[par]] == "Theta_B") {
      RAM_S_H0[id_S, id_S] <- input_H0$matrices[[par]]
    }
    
    # Theta_AB
    if (input_H0$building_block[[par]] == "Theta_AB") {
      RAM_S_H0[id_A, id_B] <- input_H0$matrices[[par]]
      RAM_S_H0[id_B, id_A] <- t(input_H0$matrices[[par]])
    }
    
    F_inv_I_A_H0 <- RAM_F %*% solve(I_n_all - RAM_A_H0)
    Sigma_H0 <- F_inv_I_A_H0 %*% RAM_S_H0 %*% t(F_inv_I_A_H0)
    Sigma_H0_inv <- solve(Sigma_H0)
    Sigma_H0_inv_Sigma_H1 <- Sigma_H0_inv %*% Sigma_H1
    
    # Fitting function
    FF_H1_H0 <- sum(diag(Sigma_H0_inv_Sigma_H1)) -
      log(det(Sigma_H0_inv_Sigma_H1)) -
      n_ov_time
    
    # Populate output with H0 models ----
    
    res$H0$FF_H1_H0[par] <- FF_H1_H0
    res$H0$Sigma_H0[[par]] <- Sigma_H0
    
  }
  
  res
  
}
