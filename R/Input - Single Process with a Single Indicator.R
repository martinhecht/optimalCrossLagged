# Set up ----
rm(list = ls())
source("compute_se_mx.R")


# User Input ----

# Target parameter for which standard errors are to be estimated
target_parameters <- NULL # NULL: all standard errors are computed

# Number of individuals and measurement time points
N <- 100
timepoints <- 5

# Number of observed variables and latent processes
n_ov <- 1
n_process <- 1

# Parameter values
loadings_values <- 1
loadings_free <- FALSE
T0cov_values <- 1
T0cov_free <- TRUE
T0means_values <- 0.5
T0means_free <- TRUE
ov_means_values <- 0
ov_means_free <- FALSE
ov_cov_values <- 0.25
ov_cov_free <- TRUE
arcl_values <- 0.5
arcl_free <- TRUE
process_means_values <- 0.5
process_means_free <- TRUE
process_cov_values <- 0.75
process_cov_free <- TRUE


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


# Compute standard errors
SE <- compute_se_mx(N = 100, timepoints = 5, n_ov = n_ov, n_process = n_process,
                    matrices = matrices)
SE

compute_se_mx(N = 100, timepoints = 5, n_ov = n_ov, n_process = n_process,
              matrices = matrices, target_parameters = "arcl_eta1eta1")


# Runtime ----
system.time(compute_se_mx(N = 100, timepoints = 10, n_ov = n_ov, n_process = n_process,
                          matrices = matrices))
system.time(compute_se_mx(N = 100, timepoints = 50, n_ov = n_ov, n_process = n_process,
                          matrices = matrices))
system.time(compute_se_mx(N = 100, timepoints = 100, n_ov = n_ov, n_process = n_process,
                          matrices = matrices))

system.time(compute_se_mx(N = 1000, timepoints = 10, n_ov = n_ov, n_process = n_process,
                          matrices = matrices))
system.time(compute_se_mx(N = 1000, timepoints = 50, n_ov = n_ov, n_process = n_process,
                          matrices = matrices))
system.time(compute_se_mx(N = 1000, timepoints = 100, n_ov = n_ov, n_process = n_process,
                          matrices = matrices))



# Compare analytical standard errors with empirical ----

# Simulation parameters
N = 100
timepoints = 5

# Population model
source("Make RAM matrices.R")

m_pop <- OpenMx::mxModel(
  manifestVars = names_ov_time,
  latentVars = names_process_time,
  type = "RAM",
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

runs <- 250
results <- data.frame(matrix(NA, nrow = runs, ncol = 6))

for (i in 1:runs){
  
  Data <- OpenMx::mxGenerateData(model = m_pop, nrows = N)
  colnames(Data) <- names_ov_time
  m_sample <- OpenMx::mxModel(model = m_pop,
                              OpenMx::mxData(observed = Data, type = "raw"))
  fit <- OpenMx::mxTryHard(model = m_sample)
  results[i, ] <- fit$output$standardErrors
  
}

colnames(results) <- rownames(fit$output$standardErrors)

boxplot(results, ylim = c(0, 1))
points(x = 1:6, y = SE, pch = 4, col = "red", cex = 3)
