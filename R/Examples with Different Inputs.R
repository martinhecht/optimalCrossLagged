# Factor CLPM ----

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

timepoints <- 5
target.parameters <- c("ARCL_2_1", "RES_2_1")
target.parameters.values.H0 <- c(0, 0)

res <- calculate.F.diff(timepoints = timepoints,
                        input_H1 = input_H1,
                        target.parameters = target.parameters,
                        target.parameters.values.H0 = target.parameters.values.H0
)

# Covariance matrix under H1
round(res$Sigma_H1, digits = 3)
# Covariance matrix under H0 (first constraint)
round(res$Sigma_H0[[1]], digits = 3)
# Covariance matrix under H0 (second constraint)
round(res$Sigma_H0[[2]], digits = 3)



# RI-CLPM ----

input_H1 <- list(
  model = "RI-CLPM",
  n_ov = 2, # number of repeatedly observed manifest variable (single indicators)
  Gamma = list(values = matrix(c(0.5, 0.1, 0.1, 0.5), nrow = 2, ncol = 2),
               labels = matrix(c("ARCL_1_1", "ARCL_2_1", "ARCL_1_2",
                                 "ARCL_2_2"), nrow = 2, ncol = 2)),
  Omega = list(values = matrix(c(0.75, 0.25, 0.25, 0.75), nrow = 2, ncol = 2),
               labels = matrix(c("RES_1_1", "RES_2_1", "RES_2_1", "RES_2_2"),
                               nrow = 2, ncol = 2)),
  Psi = NULL,
  Theta_I = list(values = matrix(c(0.5, 0.25, 0.25, 0.5), nrow = 2, ncol = 2),
                 labels = matrix(c("I_1_1", "I_2_1", "I_2_1", "I_2_2"),
                                 nrow = 2, ncol = 2)),
  Theta_S = NULL,
  Theta_IS = NULL,
  Theta_A = NULL,
  Theta_B = NULL,
  Theta_AB = NULL
)

timepoints <- 5
target.parameters <- c("ARCL_2_1", "I_2_1", "RES_2_1")
target.parameters.values.H0 <- c(0, 0, 0)

# Covariance matrix under H1
round(res$Sigma_H1, digits = 3)
# Covariance matrix under H0 (first constraint)
round(res$Sigma_H0[[1]], digits = 3)
# Covariance matrix under H0 (second constraint)
round(res$Sigma_H0[[2]], digits = 3)
# Covariance matrix under H0 (third constraint)
round(res$Sigma_H0[[3]], digits = 3)
