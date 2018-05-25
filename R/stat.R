
harmonic_mean <- function(x, ...) {
  1/mean(1/x, ...)
}

cagr <- function(x) {
  require(dplyr)
  (last(x) / first(x))^(1/(length(x) - 1)) - 1
}

pct_change <- function(new, old) {
  if (old == 0) {
    stop("reference value (old) cannot be zero")
  } else {
    (new - old)/old
  }
}

