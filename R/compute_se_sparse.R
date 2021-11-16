compute_se_sparse <- function(N, timepoints, n_ov, n_process, matrices,
                              target_parameters = NULL) {
  
  fisher <- compute_fisher_sparse(timepoints = timepoints, n_ov = n_ov,
                                  n_process = n_process, matrices = matrices)
  
  acov <- solve(N * fisher)
  
  sqrt(diag(acov))
  
}
