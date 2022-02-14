## Changelog:
# MH 0.0.2 2022-02-14:

compute_se_oertzen<- function(N, timepoints, n_ov, n_process, matrices,
                              cppf.env,
							  target.parameters = NULL) {
  
  # MH 0.0.2 2022-02-14 
  require( Rcpp )
  require( RcppArmadillo )

  ## function definition mm, mmm, mmmm, minv in calc.power() >= 0.0.3 2022-01-10
  ## cppf.env contains all Rcpp functions, get them
  # ls(name, envir = cppf.env, all.names = FALSE, pattern, sorted = TRUE)
  mm <- get( "mm", envir = cppf.env, mode = "function", inherits = FALSE)
  mmm <- get( "mmm", envir = cppf.env, mode = "function", inherits = FALSE)
  mmmm <- get( "mmmm", envir = cppf.env, mode = "function", inherits = FALSE)
  minv <- get( "minv", envir = cppf.env, mode = "function", inherits = FALSE)
  
  
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
  
  
  # Get model information ----
  
  # Does the model has mean structure?
  # How many elements are in the model-implied mean vector and covariance
  ## matrix?
  #if (all(is.na(RAM_m_labels))) {
  #+  mean_structure <- FALSE
  #  n_moments <- n_moments_covariance <- n_ov_time * (n_ov_time + 1) / 2
  #} else {
  #  mean_structure <- TRUE
  #  n_moments_all <- n_ov_time * (n_ov_time + 3) / 2
  #  n_moments_covariance <- n_ov_time * (n_ov_time + 1) / 2
  #}
  #n_moments_all <- n_ov_time * (n_ov_time + 3) / 2
  #n_moments_covariance <- n_ov_time * (n_ov_time + 1) / 2
  
  # Number of model parameters
  RAM_unique_labels <- unique(na.omit(c(RAM_A_labels, RAM_S_labels, RAM_m_labels)))
  n_parameters <-length(RAM_unique_labels)
  seq_parameters <- seq_len(n_parameters)
  
# browser()  
  # Compute fixed matrices ----
  # MH 0.0.2 2022-02-14
  # B <- solve(diag(1, nrow = n_all) - RAM_A_values)
  B <- minv(diag(1, nrow = n_all) - RAM_A_values)
  # Bm <- B %*% RAM_m_values
  Bm <- mm( B, RAM_m_values )
  # E <- B %*% RAM_S_values %*% t(B)
  E <- mm( mm( B, RAM_S_values ), t(B) )
  # FB <- RAM_F_values %*% B
  FB <- mm( RAM_F_values, B )
  # EFt <- E %*% t(RAM_F_values)
  EFt <- mm( E, t(RAM_F_values) )
  BtFt <- t(FB)
  # exp_cov <- RAM_F_values %*% E %*% t(RAM_F_values)
  exp_cov <- mmm( RAM_F_values, E, t(RAM_F_values) )
  # exp_cov_inv <- solve(exp_cov)
  exp_cov_inv <- minv(exp_cov)
  
  
  # Compute partial derivatives of RAM matrices ----
  Zero <- matrix(0, nrow = n_all, ncol = n_all)
  A_deriv <- replicate(n = n_parameters, expr = Zero, simplify = FALSE)
  S_deriv <- A_deriv
  
  for (i in seq_parameters) {
    A_deriv[[i]][which(RAM_A_labels == RAM_unique_labels[i], arr.ind = TRUE)] <- 1
  }
  
  for (i in seq_parameters) {
    S_deriv[[i]][which(RAM_S_labels == RAM_unique_labels[i], arr.ind = TRUE)] <- 1
  }
  
  #if (mean_structure) {
  zero <- matrix(0, nrow = n_all, ncol = 1)
  m_deriv <- replicate(n = n_parameters, expr = zero, simplify = FALSE)
  for (i in seq_parameters) {
    m_deriv[[i]][which(RAM_m_labels == RAM_unique_labels[i], arr.ind = TRUE)] <- 1
    #  }
  }
  
  
  # Compute Fisher information matrix ----
  fisher <- matrix(0, nrow = n_parameters, ncol = n_parameters)
  
  # Prepare lists with with terms depending on the elements of theta
  fisher_terms <- list()
  for (i in seq_parameters) {
    # MH 0.0.2 2022-02-14
	# A_deriv_EFt <- A_deriv[[i]] %*% EFt
	A_deriv_EFt <- mm( A_deriv[[i]], EFt )
    # FB_A_deriv <- FB %*% A_deriv[[i]]
    FB_A_deriv <- mm( FB, A_deriv[[i]] )
    # FB_A_deriv_B <- FB_A_deriv %*% B
    FB_A_deriv_B <- mm( FB_A_deriv, B )
    # S_deriv_BtFt <- S_deriv[[i]] %*% BtFt
    S_deriv_BtFt <- mm( S_deriv[[i]], BtFt )
    # symm <- FB %*% A_deriv_EFt
    symm <- mm( FB, A_deriv_EFt )
    # Sigma_deriv <- symm + t(symm) + FB %*% S_deriv_BtFt
    Sigma_deriv <- symm + t(symm) + mm( FB, S_deriv_BtFt )
    # mu_deriv <- FB_A_deriv %*% Bm + FB %*% m_deriv[[i]]
    mu_deriv <- mm( FB_A_deriv, Bm ) + mm( FB, m_deriv[[i]] )
    # exp_cov_inv_Sigma_deriv <- exp_cov_inv %*% Sigma_deriv
    exp_cov_inv_Sigma_deriv <- mm( exp_cov_inv, Sigma_deriv )
    
    fisher_terms[[i]] <- list(A_deriv_EFt = A_deriv_EFt,
                              FB_A_deriv = FB_A_deriv,
                              FB_A_deriv_B = FB_A_deriv_B,
                              S_deriv_BtFt = S_deriv_BtFt,
                              Sigma_deriv = Sigma_deriv,
                              mu_deriv = mu_deriv,
                              exp_cov_inv_Sigma_deriv = exp_cov_inv_Sigma_deriv)
  }
  
  for (i in seq_parameters) {
    
    for (j in seq_len(i)) {
      
      # Second-order derivative of Sigma
	  # MH 0.0.2 2022-02-14
      Sigma_deriv_ij <- 
        # fisher_terms[[i]]$FB_A_deriv_B %*% fisher_terms[[j]]$A_deriv_EFt +
        mm( fisher_terms[[i]]$FB_A_deriv_B, fisher_terms[[j]]$A_deriv_EFt ) +
        # fisher_terms[[j]]$FB_A_deriv_B %*% fisher_terms[[i]]$A_deriv_EFt +
        mm( fisher_terms[[j]]$FB_A_deriv_B, fisher_terms[[i]]$A_deriv_EFt )+
        # fisher_terms[[i]]$FB_A_deriv_B %*% fisher_terms[[j]]$S_deriv_BtFt +
        mm( fisher_terms[[i]]$FB_A_deriv_B, fisher_terms[[j]]$S_deriv_BtFt ) +
        # fisher_terms[[i]]$FB_A_deriv %*% E %*% t(A_deriv[[j]]) %*% BtFt +
        mmmm( fisher_terms[[i]]$FB_A_deriv, E, t(A_deriv[[j]]), BtFt ) +
        # fisher_terms[[j]]$FB_A_deriv_B %*% fisher_terms[[i]]$S_deriv_BtFt
        mm( fisher_terms[[j]]$FB_A_deriv_B, fisher_terms[[i]]$S_deriv_BtFt )
      Sigma_deriv_ij <- Sigma_deriv_ij + t(Sigma_deriv_ij)

# browser()      
      # MH 0.0.2 2022-02-14
	  fisher[i, j] <-
        # N/2 * OpenMx::tr(fisher_terms[[j]]$exp_cov_inv_Sigma_deriv %*%
        N/2 * OpenMx::tr( mm( fisher_terms[[j]]$exp_cov_inv_Sigma_deriv,
                           # fisher_terms[[i]]$exp_cov_inv_Sigma_deriv) +
                           fisher_terms[[i]]$exp_cov_inv_Sigma_deriv ) ) +
        # OpenMx::tr(fisher_terms[[i]]$exp_cov_inv_Sigma_deriv %*%
        OpenMx::tr( mm( fisher_terms[[i]]$exp_cov_inv_Sigma_deriv,
                     # fisher_terms[[j]]$exp_cov_inv_Sigma_deriv -
                     fisher_terms[[j]]$exp_cov_inv_Sigma_deriv ) -
                     # 0.5*exp_cov_inv %*%  Sigma_deriv_ij) +
                     mm( 0.5*exp_cov_inv, Sigma_deriv_ij ) ) +
        # N*t(fisher_terms[[i]]$mu_deriv) %*% exp_cov_inv %*% 
        mmm( N*t(fisher_terms[[i]]$mu_deriv), exp_cov_inv,
        # fisher_terms[[j]]$mu_deriv
        fisher_terms[[j]]$mu_deriv )
    }
  }
  
  # Put lower triangs into upper triangle
  fisher[upper.tri(fisher)] <- t(fisher)[upper.tri(fisher)]
  
  # Prepare output ----
  colnames(fisher) <- rownames(fisher) <- RAM_unique_labels
  # MH 0.0.2 2022-02-14
  # acov <- solve(fisher)
  acov <- minv(fisher)
  colnames(acov) <- colnames(fisher)
  rownames(acov) <- rownames(fisher)
  
  if (is.null(target.parameters)) {
    target.parameters <- RAM_unique_labels
  }
  
  sqrt(diag(acov))[target.parameters]
  
}

### development
# Rdir <- "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R"
# Rfiles <- list.files( Rdir, pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("compute_se_oertzen.R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R","Make RAM matrices.R") ]
# Rfiles <- file.path( Rdir, Rfiles )
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

# example 2
# model <- generate_model_example2()

# se <- compute_se_oertzen( 
						  ##N=model$N,
						  # N=80,
						  ##timepoints=model$timepoints,
						  # timepoints=15,
						  # n_ov=model$n_ov,
						  # n_process=model$n_process,
						  # matrices=model$matrices,
						  # target.parameters="arcl_eta1eta2" )
# se



