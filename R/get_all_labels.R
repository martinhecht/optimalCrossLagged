get_all_labels <- function(input_H1) {
  
  parameter_labels <- c()
  
  if (!is.null(input_H1$Gamma)) {parameter_labels <- c(parameter_labels,
                                                       c(input_H1$Gamma$labels))}
  if (!is.null(input_H1$Omega)) {parameter_labels <- c(parameter_labels,
                                                       c(input_H1$Omega$labels))}
  if (!is.null(input_H1$Psi)) {parameter_labels <- c(parameter_labels,
                                                     c(input_H1$Psi$labels))}
  if (!is.null(input_H1$Theta_I)) {parameter_labels <- c(parameter_labels,
                                                         c(input_H1$Theta_I$labels))}
  if (!is.null(input_H1$Theta_S)) {parameter_labels <- c(parameter_labels,
                                                         c(input_H1$Theta_S$labels))}
  if (!is.null(input_H1$Theta_IS)) {parameter_labels <- c(parameter_labels,
                                                          c(input_H1$Theta_IS$labels))}
  if (!is.null(input_H1$Theta_A)) {parameter_labels <- c(parameter_labels,
                                                         c(input_H1$Theta_A$labels))}
  if (!is.null(input_H1$Theta_B)) {parameter_labels <- c(parameter_labels,
                                                         c(input_H1$Theta_B$labels))}
  if (!is.null(input_H1$Theta_AB)) {parameter_labels <- c(parameter_labels,
                                                          c(input_H1$Theta_AB$labels))}
  
  unique(parameter_labels)
  
}
