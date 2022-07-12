compute_power <- function(N, chisq_values) {
  
  for (i in seq_len(chisq_values$H0$n_tests)) {
    
    # noncentrality parameter
    lambda <- N * chisq_values$H0$FF_H1_H0[i]
    
    # Power
    chisq_values$H0$power[i] <- pchisq(q = chisq_values$H1$critical_value,
                                       df = chisq_values$H1$df_H1,
                                       lower.tail = FALSE,
                                       ncp = lambda)
    
  }
  
  chisq_values$H0$N <- N
  
  chisq_values
  
}