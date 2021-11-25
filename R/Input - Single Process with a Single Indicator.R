# Set up ----
setwd("C:/Users/arnoldmz/OneDrive/Forschung/Projekte/Optimal Design ACLPM")
rm(list = ls())
source("compute_se_mx.R")
source("compute_se_oertzen.R")
source("compute_se_sparse.R")

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


# Check standard errors ----
se_mx <- compute_se_mx(N = 100, timepoints = 5, n_ov = n_ov, n_process = n_process,
                              matrices = matrices)
se_oertzen <- compute_se_oertzen(N = 100, timepoints = 5, n_ov = n_ov, n_process = n_process,
                          matrices = matrices)
se_sparse <- compute_se_sparse(N = 100, timepoints = 5, n_ov = n_ov, n_process = n_process,
                                 matrices = matrices)

SE <- cbind("mx" = se_mx, "oertzen" = se_oertzen, "sparse" = se_sparse)


# Runtime ----

runs <- 25
timepoints <- c(3, 5, 10, 20, 30)
results <- data.frame(matrix(NA, nrow = runs * timepoints, ncol = 4))

counter <- 1
for (i in 1:runs) {
  for (j in timepoints) {
    
    res_mx <-  system.time(compute_se_mx(N = 1000, timepoints = j, n_ov = n_ov, n_process = n_process,
                                         matrices = matrices))
    res_oertzen <- system.time(compute_se_oertzen(N = 1000, timepoints = j, n_ov = n_ov, n_process = n_process,
                                                  matrices = matrices))
    res_sparse <- system.time(compute_se_sparse(N = 1000, timepoints = j, n_ov = n_ov, n_process = n_process,
                                                matrices = matrices))
    
    results[counter, 1] <- j
    results[counter, 2] <- res_mx[3]
    results[counter, 3] <- res_oertzen[3]
    results[counter, 4] <- res_sparse[3]
    
    counter <- counter + 1
    
  }
}

colnames(results) <- c("timepoints", "mx", "oertzen", "sparse")

boxplot(subset(results[, 2:4], subset = results$timepoints == 3), main = "10 time points")
boxplot(subset(results[, 2:4], subset = results$timepoints == 5), main = "10 time points")
boxplot(subset(results[, 2:4], subset = results$timepoints == 10), main = "10 time points")
boxplot(subset(results[, 2:4], subset = results$timepoints == 20), main = "20 time points")
boxplot(subset(results[, 2:4], subset = results$timepoints == 30), main = "30 time points")
