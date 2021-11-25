## Changelog:
# MH 0.0.1 2021-11-03: copied chunks from Input - Two Processes with Two Indicator Each.R

## Documentation
#' @title
#' @description
#' @param
#' @param
#' @param
#' @return
#' @keywords internal

## Function definition
generate_model_example2 <- function(){
		
		# Number of individuals and measurement time points
		N <- 100
		timepoints <- 5

		# Number of observed variables and latent processes
		n_ov <- 4
		n_process <- 2

		# Parameter values
		loadings_values <- c(1, 0.8, 0, 0, 0, 0, 1, 1.2)
		loadings_free <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
		T0cov_values <- c(1, 0.2, 0.2, 1)
		T0cov_free <- c(TRUE, TRUE, TRUE, TRUE)
		T0means_values <- c(-1, 1)
		T0means_free <- c(TRUE, TRUE)
		ov_means_values <- c(0, 0)
		ov_means_free <- c(FALSE, FALSE)
		ov_cov_values <- c(diag(x = 0.25, nrow = n_ov))
		ov_cov_free <- c(diag(x = TRUE, nrow = n_ov))
		arcl_values <- c(0.5, 0.1, 0.1, 0.5)
		arcl_free <- c(TRUE, TRUE, TRUE, TRUE)
		process_means_values <- c(-1, 1)
		process_means_free <- c(TRUE, TRUE)
		process_cov_values <- c(0.75, 0.25, 0.25, 0.75)
		process_cov_free <- c(TRUE, TRUE, TRUE, TRUE)


		# Target parameter for which standard errors are to be estimated
		target_parameters <- NULL


		# Calculate standard errors ----

		# Create variable names
		names_process <- paste0("eta", seq_len(n_process))
		names_ov <- paste0("y", seq_len(n_ov))

		# Generate matrices with parameters

		## Factor loadings
		loadings <- list(
		  values = matrix(loadings_values, nrow = n_ov, ncol = n_process),
		  labels = matrix(NA, nrow = n_ov, ncol = n_process)
		)

		for (i in seq_len(n_ov)) {
		  for (j in seq_len(n_process)) {
			loadings$labels[i, j] <- paste0("loading_", names_ov[i], names_process[j])
		  }
		}

		loadings$labels[!loadings_free] <- NA


		## Covariance at T0
		T0cov <- list(
		  values = matrix(T0cov_values, nrow = n_process, ncol = n_process),
		  labels = matrix(NA, nrow = n_process, ncol = n_process)
		)

		for (i in seq_len(n_process)) {
		  for (j in seq_len(n_process)) {
			T0cov$labels[i, j] <- paste0("T0cov_", names_process[i], names_process[j])
		  }
		}

		T0cov$labels[!T0cov_free] <- NA

		if (n_process > 1) {
		  T0cov$labels[upper.tri(T0cov$labels)] <- t(T0cov$labels)[upper.tri(T0cov$labels)]
		}


		## T0means
		T0means <- list(
		  values = matrix(T0means_values, nrow = n_process, ncol = 1),
		  labels = matrix(NA, nrow = n_process, ncol = 1)
		)

		for (i in seq_len(n_process)) {
		  T0means$labels[i, 1] <- paste0("T0means_", names_process[i])
		}

		T0means$labels[!T0means_free] <- NA


		## Observed variable means
		ov_means <- list(
		  values = matrix(ov_means_values, nrow = n_ov, ncol = 1),
		  labels = matrix(NA, nrow = n_ov, ncol = 1)
		)

		for (i in seq_len(n_ov)) {
		  ov_means$labels[i, 1] <- paste0("T0means_", names_process[i])
		}

		ov_means$labels[!ov_means_free] <- NA


		## Observed variable covariance
		ov_cov <- list(
		  values = matrix(ov_cov_values, nrow = n_ov, ncol = n_ov),
		  labels = matrix(NA, nrow = n_ov, ncol = n_ov)
		)

		for (i in seq_len(n_ov)) {
		  for (j in seq_len(n_ov)) {
			ov_cov$labels[i, j] <- paste0("ov_cov_", names_ov[i], names_ov[j])
		  }
		}

		ov_cov$labels[!ov_cov_free] <- NA

		if (n_ov > 1) {
		  ov_cov$labels[upper.tri(ov_cov$labels)] <- t(ov_cov$labels)[upper.tri(ov_cov$labels)]
		}


		## Autoregressive and cross-lagged effects
		arcl <- list(
		  values = matrix(arcl_values, nrow = n_process, ncol = n_process),
		  labels = matrix(NA, nrow = n_process, ncol = n_process)
		)

		for (i in seq_len(n_process)) {
		  for (j in seq_len(n_process)) {
			arcl$labels[i, j] <- paste0("arcl_", names_process[i], names_process[j])
		  }
		}

		arcl$labels[!arcl_free] <- NA


		## Means of the processes
		process_means <- list(
		  values = matrix(process_means_values, nrow = n_process, ncol = 1),
		  labels = matrix(NA, nrow = n_process, ncol = 1)
		)

		for (i in seq_len(n_process)) {
		  process_means$labels[i, 1] <- paste0("process_means_", names_process[i])
		}

		process_means$labels[!process_means_free] <- NA


		## Covariance of the processes
		process_cov <- list(
		  values = matrix(process_cov_values, nrow = n_process, ncol = n_process),
		  labels = matrix(NA, nrow = n_process, ncol = n_process)
		)

		for (i in seq_len(n_process)) {
		  for (j in seq_len(n_process)) {
			process_cov$labels[i, j] <- paste0("process_cov_", names_process[i], names_process[j])
		  }
		}

		process_cov$labels[!process_cov_free] <- NA

		if (n_process > 1) {
		  process_cov$labels[upper.tri(process_cov$labels)] <- t(process_cov$labels)[upper.tri(process_cov$labels)]
		}


		## Put all matrices in a list
		matrices <- list(loadings = loadings,
						 T0cov = T0cov,
						 T0means = T0means,
						 ov_means = ov_means,
						 ov_cov = ov_cov,
						 arcl = arcl,
						 process_means = process_means,
						 process_cov = process_cov
		)

		# return
		ret <- list( N = N,
		             timepoints = timepoints,
					 n_ov = n_ov,
					 names_ov = names_ov,
					 n_process = n_process,
                     names_process = names_process,
					 matrices = matrices
					)
		
		return( ret )
}

### development
# Rfiles <- list.files( "c:/Users/martin/Dropbox/84_optimalclpm/04_martinhecht/R", pattern="*.R" )
# Rfiles <- Rfiles[ !Rfiles %in% c("generate_input_example2.R","app.R","Input - Single Process with a Single Indicator.R","Input - Two Processes with Two Indicator Each.R") ]
# for( Rfile in Rfiles ){
	# source( Rfile )
# }

# inp <- generate_input_example2()
# inp


### test
# require( testthat )
# test_file("../tests/testthat/XXXXX.R")
