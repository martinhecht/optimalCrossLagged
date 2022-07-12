# Set up ----
rm(list = ls())
setwd("C:/Users/manue/OneDrive/Forschung/Projekte/Optimal Cross Lagged/Unified Framework")

# load functions
source("compute_chisq.R")
source("compute_power.R")


# Factor CLPM ----

input_H1 <- list(
  model = " Factor CLPM",
  alpha = 0.05,
  timepoints = 5,
  n_ov = 2, # number of repeatedly observed manifest variable (single indicators)
  Gamma = matrix(c(0.5, 0.1, 0.1, 0.5), nrow = 2, ncol = 2),
  Omega = matrix(c(0.75, 0.25, 0.25, 0.75), nrow = 2, ncol = 2),
  Psi = matrix(c(0.25, 0, 0, 0.25), nrow = 2, ncol = 2),
  Theta_I = NULL,
  Theta_S = NULL,
  Theta_IS = NULL,
  Theta_A = NULL,
  Theta_B = NULL,
  Theta_AB = NULL,
  Gamma_star = NULL
)

input_H0 <- list(
  building_block = c("Gamma", "Omega"),
  matrices = list(
    Gamma = matrix(c(0.5, 0, 0.1, 0.5), nrow = 2, ncol = 2),
    Omega = matrix(c(0.75, 0, 0, 0.75), nrow = 2, ncol = 2)
  )
)

chisq_values <- compute_chisq(input_H1 = input_H1, input_H0 = input_H0,
                              alpha = 0.05)

# Covariance matrix under H1
round(chisq_values$H1$Sigma_H1, digits = 3)
# Covariance matrix under H0 (first constraint)
round(chisq_values$H0$Sigma_H0[[1]], digits = 3)
# Covariance matrix under H0 (second constraint)
round(chisq_values$H0$Sigma_H0[[2]], digits = 3)

res <- compute_power(N = 100, chisq_values = chisq_values)

res


# RI-CLPM ----

input_H1 <- list(
  model = "RI-CLPM",
  alpha = 0.05,
  timepoints = 5,
  n_ov = 2, # number of repeatedly observed manifest variable (single indicators)
  Gamma = matrix(c(0.5, 0.1, 0.1, 0.5), nrow = 2, ncol = 2),
  Omega = matrix(c(0.75, 0.25, 0.25, 0.75), nrow = 2, ncol = 2),
  Psi = NULL,
  Theta_I = matrix(c(0.5, 0.25, 0.25, 0.5), nrow = 2, ncol = 2),
  Theta_S = NULL,
  Theta_IS = NULL,
  Theta_A = NULL,
  Theta_B = NULL,
  Theta_AB = NULL,
  Gamma_star = NULL
)

input_H0 <- list(
  building_block = c("Gamma", "Theta_I"),
  matrices = list(
    Gamma = matrix(c(0.5, 0, 0.1, 0.5), nrow = 2, ncol = 2),
    Theta_I = matrix(c(0.5, 0, 0, 0.5), nrow = 2, ncol = 2)
  )
)

chisq_values <- compute_chisq(input_H1 = input_H1, input_H0 = input_H0,
                              alpha = 0.05)

# Covariance matrix under H1
round(chisq_values$H1$Sigma_H1, digits = 3)
# Covariance matrix under H0 (first constraint)
round(chisq_values$H0$Sigma_H0[[1]], digits = 3)
# Covariance matrix under H0 (second constraint)
round(chisq_values$H0$Sigma_H0[[2]], digits = 3)

res <- compute_power(N = 100, chisq_values = chisq_values)

res
