## Changelog:
# MA 0.0.1 2022-02-24: initial programming

## Documentation
#' @title Calculate power
#' @description Function calculates the power of a likelihood ratio test.
#' @param
#' @param
#' @param
#' @return
#' @keywords internal

calculate.power.LRT <- function(alpha, N, F_diff) {
  
  # Critical value
  critical_value <- qchisq(p = 1 - alpha, df = F_diff$df)
  
  # prepare outcome
  power <- F_diff$values
  
  # calculate power
  for (i in seq_along(power)) {
    power[i] <- pchisq(q = critical_value, df = F_diff$df, lower.tail = FALSE,
                       ncp = N * F_diff$values[i])
  }

  # return outcome
  power

}
