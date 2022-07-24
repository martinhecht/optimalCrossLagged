## Changelog:
# MH 0.0.4 2022-01-15: renamed generate_model_example2 to generate.model.example.2
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
generate.model.example.3 <- function(){
		
		# Number of individuals and measurement time points
		N <- 100
		timepoints <- 5

		input_H1 <- list(
		  model = " Factor CLPM",
		  n_ov = 2, # number of repeatedly observed manifest variable (single indicators)
		  Gamma = list(values = matrix(c(0.5, 0.1, 0.1, 0.5), nrow = 2, ncol = 2),
		               labels = matrix(c("ARCL_1_1", "ARCL_2_1", "ARCL_1_2",
		                                 "ARCL_2_2"), nrow = 2, ncol = 2)),
		  Omega = list(values = matrix(c(0.75, 0.25, 0.25, 0.75), nrow = 2, ncol = 2),
		               labels = matrix(c("RES_1_1", "RES_2_1", "RES_2_1", "RES_2_2"),
		                               nrow = 2, ncol = 2)),
		  Psi = list(values = matrix(c(0.25, 0, 0, 0.25), nrow = 2, ncol = 2),
		             labels = matrix(c("UNIQ_1_1", "UNIQ_2_1", "UNIQ_2_1", "UNIQ_2_2"),
		                             nrow = 2, ncol = 2)),
		  Theta_I = NULL, # I
		  Theta_S = NULL, # S
		  Theta_IS = NULL, # IS
		  Theta_A = NULL, # A
		  Theta_B = NULL, # B
		  Theta_AB = NULL # AB
		)

		names_process <- paste0("eta", seq_len(input_H1$n_ov))
		
		# return
		ret <- list(N = N,
		            timepoints = timepoints,
		            names_process = names_process,
		            input_H1 = input_H1
		)
		
		return(ret)
}
